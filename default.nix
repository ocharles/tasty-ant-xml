{ mkDerivation, base, containers, generic-deriving, ghc-prim, mtl
, stdenv, stm, tagged, tasty, transformers, xml
}:
mkDerivation {
  pname = "tasty-ant-xml";
  version = "1.0.1";
  src = ./.;
  buildDepends = [
    base containers generic-deriving ghc-prim mtl stm tagged tasty
    transformers xml
  ];
  homepage = "http://github.com/ocharles/tasty-ant-xml";
  description = "Render tasty output to XML for Jenkins";
  license = stdenv.lib.licenses.bsd3;
}
