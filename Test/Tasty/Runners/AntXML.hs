{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
-- | Run a 'Tasty.TestTree' and produce an XML file summarising the test results
-- in the same schema that would be produced by Apache Ant's JUnit test runner.
-- This schema can be intepreted by the Jenkins continuous integration server,
-- amongst other tools.
module Test.Tasty.Runners.AntXML (antXMLRunner, AntXMLPath(..) ) where

import Control.Applicative
import Control.Arrow (first)
import Control.Monad.Trans.Class (lift)
import Data.Maybe (fromMaybe)
import Data.Monoid (Monoid(..), Endo(..), Sum(..))
import Data.Proxy (Proxy(..))
import Data.Semigroup.Applicative (Traversal(..))
import Data.Tagged (Tagged(..))
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Generics.Deriving.Monoid (memptydefault, mappenddefault)

import qualified Control.Concurrent.STM as STM
import qualified Control.Monad.State as State
import qualified Data.Functor.Compose as Functor
import qualified Data.IntMap as IntMap
import qualified Test.Tasty.Options as Tasty
import qualified Test.Tasty.Runners as Tasty
import qualified Text.XML.Light as XML

--------------------------------------------------------------------------------
newtype AntXMLPath = AntXMLPath FilePath
  deriving (Typeable)

instance Tasty.IsOption (Maybe AntXMLPath) where
  defaultValue = Nothing
  parseValue = Just . Just . AntXMLPath
  optionName = Tagged "xml"
  optionHelp = Tagged "A file path to store the test results in Ant-compatible XML"


--------------------------------------------------------------------------------
data Summary = Summary { summaryFailures :: Sum Int
                       , summaryErrors :: Sum Int
                       , summarySuccesses :: Sum Int
                       , xmlRenderer :: Endo XML.Element
                       } deriving (Generic)

instance Monoid Summary where
  mempty = memptydefault
  mappend = mappenddefault


--------------------------------------------------------------------------------
{-|

  To run tests using this ingredient, use 'Tasty.defaultMainWithIngredients',
  passing 'antXMLRunner' as one possible ingredient. This ingredient will run
  tests if you pass the @--xml@ command line option. For example,
  @--xml=junit.xml@ will run all the tests and generate @junit.xml@ as output.

-}
antXMLRunner :: Tasty.Ingredient
antXMLRunner = Tasty.TestReporter optionDescription runner
 where
  optionDescription = [ Tasty.Option (Proxy :: Proxy (Maybe AntXMLPath)) ]
  runner options testTree = do
    AntXMLPath path <- Tasty.lookupOption options

    return $ \statusMap ->
      let

        runTest _ testName _ = Traversal $ Functor.Compose $ do
          i <- State.get

          summary <- lift $ STM.atomically $ do
            status <- STM.readTVar $
              fromMaybe (error "Attempted to lookup test by index outside bounds") $
                IntMap.lookup i statusMap

            let testCaseAttributes = map (uncurry XML.Attr . first XML.unqual)
                  [ ("name", testName ) ]

                mkSummary contents =
                  mempty { xmlRenderer = Endo
                             (`appendChild` XML.node (XML.unqual "testcase") contents)
                         }

                mkSuccess = (mkSummary testCaseAttributes) { summarySuccesses = Sum 1 }

                mkFailure reason =
                  mkSummary ( testCaseAttributes
                            , XML.node (XML.unqual "failure") reason
                            )

            case status of
              -- If the test is done, generate XML for it
              Tasty.Done result
                | Tasty.resultSuccessful result -> pure mkSuccess
                | otherwise ->
                    case resultException result of
                      Just e  -> pure $ (mkFailure (show e)) { summaryErrors = Sum 1 }
                      Nothing -> pure $
                        if resultTimedOut result
                          then (mkFailure "TimeOut") { summaryErrors = Sum 1 }
                          else (mkFailure (Tasty.resultDescription result))
                               { summaryFailures = Sum 1 }

              -- Otherwise the test has either not been started or is currently
              -- executing
              _ -> STM.retry

          Const summary <$ State.modify (+ 1)

        runGroup groupName children = Traversal $ Functor.Compose $ do
          Const soFar <- Functor.getCompose $ getTraversal children
          let grouped = appEndo (xmlRenderer soFar) $
                XML.node (XML.unqual "testsuite") $
                  XML.Attr (XML.unqual "name") groupName

          pure $ Const
            soFar { xmlRenderer = Endo (`appendChild` grouped)
                  }

      in do
        (Const summary, tests) <-
          flip State.runStateT 0 $ Functor.getCompose $ getTraversal $
           Tasty.foldTestTree
             Tasty.trivialFold { Tasty.foldSingle = runTest, Tasty.foldGroup = runGroup }
             options
             testTree

        return $ \elapsedTime -> do

          writeFile path $
            XML.showTopElement $
              appEndo (xmlRenderer summary) $
                XML.node
                  (XML.unqual "testsuites")
                  [ XML.Attr (XML.unqual "errors")
                      (show . getSum . summaryErrors $ summary)
                  , XML.Attr (XML.unqual "failures")
                      (show . getSum . summaryFailures $ summary)
                  , XML.Attr (XML.unqual "tests") (show tests)
                  , XML.Attr (XML.unqual "time") (show elapsedTime)
                  ]

          return (getSum ((summaryFailures `mappend` summaryErrors) summary) == 0)

  appendChild parent child =
    parent { XML.elContent = XML.elContent parent ++ [ XML.Elem child ] }

  resultException r =
    case Tasty.resultOutcome r of
         Tasty.Failure (Tasty.TestThrewException e) -> Just e
         _ -> Nothing

  resultTimedOut r =
    case Tasty.resultOutcome r of
         Tasty.Failure (Tasty.TestTimedOut _) -> True
         _ -> False
