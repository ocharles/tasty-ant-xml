{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, containers, directory, filepath
      , generic-deriving, ghc-prim, mtl, stdenv, stm, tagged, tasty
      , transformers, xml
      }:
      mkDerivation {
        pname = "tasty-ant-xml";
        version = "1.1.3";
        src = ./.;
        libraryHaskellDepends = [
          base containers directory filepath generic-deriving ghc-prim mtl
          stm tagged tasty transformers xml
        ];
        homepage = "http://github.com/ocharles/tasty-ant-xml";
        description = "Render tasty output to XML for Jenkins";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
