let
  pkgs = import <nixpkgs> {};
  haskellPackages = pkgs.haskellPackages.override {
    extension = self: super: {
      tastyAntXml = self.callPackage ./. {};
    };
  };

in pkgs.myEnvFun {
     name = haskellPackages.tastyAntXml.name;
     buildInputs = [
       pkgs.curl
       pkgs.git
       pkgs.openssh
       (haskellPackages.ghcWithPackages (hs: ([
         hs.cabalInstall
         hs.hscolour
       ] ++ hs.tastyAntXml.propagatedNativeBuildInputs)))
     ];
   }
