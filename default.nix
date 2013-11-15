{}:
with import <nixpkgs> {};
let
  inherit (haskellPackages) aeson ansiTerminal async attoparsec cabal
    cabalInstall_1_18_0_2 deepseq doctest
    filepath genericDeriving HUnit lens logict mtl optparseApplicative
    reducers regexPosix
    semigroups
    simpleReflect snap stm tagged
    text unorderedContainers utf8String vector xml;

  tasty = cabal.mkDerivation (self: {
    pname = "tasty";
    version = "0.4.0.1";
    sha256 = "04nnjg04520lvjm8h2ma0ihm4bz6p0ppk445i8gmn82ixwan76h0";
    buildDepends = [
      ansiTerminal deepseq mtl optparseApplicative regexPosix stm tagged
    ];
    meta = {
      description = "Modern and extensible testing framework";
      license = self.stdenv.lib.licenses.mit;
      platforms = self.ghc.meta.platforms;
    };
  });

in cabal.mkDerivation (self: {
  pname = "tasty-ant-xml";
  version = "1.0.0";
  src = ./.;
  buildDepends = [ genericDeriving reducers tasty xml ];
  buildTools = [ cabalInstall_1_18_0_2 ];
})
