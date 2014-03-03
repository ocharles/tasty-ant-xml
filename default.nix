{}:
with import <nixpkgs> {};
let
  inherit (haskellPackages) aeson ansiTerminal async attoparsec cabal
    cabalInstall_1_18_0_2 deepseq doctest
    filepath genericDeriving HUnit lens logict mtl optparseApplicative
    reducers regexPosix
    semigroups
    simpleReflect snap stm tagged
    text unorderedContainers utf8String vector xml tasty;

in cabal.mkDerivation (self: {
  pname = "tasty-ant-xml";
  version = "1.0.0";
  src = ./.;
  buildDepends = [ genericDeriving reducers tasty xml ];
  buildTools = [ cabalInstall_1_18_0_2 ];
})
