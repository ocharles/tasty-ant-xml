{ cabal, genericDeriving, mtl, stm, tagged, tasty, transformers
, xml
}:

cabal.mkDerivation (self: {
  pname = "tasty-ant-xml";
  version = "1.0.1";
  src = ./.;
  buildDepends = [
    genericDeriving mtl stm tagged tasty transformers xml
  ];
  meta = {
    homepage = "http://github.com/ocharles/tasty-ant-xml";
    description = "Render tasty output to XML for Jenkins";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
