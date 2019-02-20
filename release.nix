{
  nixpkgs  ? import <nixos-unstable>
, compiler ? "ghc822"
}:

let
  config = {
    packageOverrides = pkgs: rec {
      haskell = pkgs.haskell // {
        packages = pkgs.haskell.packages // {
          "${compiler}" = pkgs.haskell.packages."${compiler}".override {
            overrides = haskellPackagesNew: haskellPackagesOld: rec {
              hleap             = haskellPackagesNew.callPackage        ../hleap/default.nix { };
              kafka-device      = haskellPackagesNew.callPackage ../kafka-device/default.nix { };
              kafka-device-leap = haskellPackagesNew.callPackage               ./default.nix { };
            };
          };
        };
      };
    };
  };
  pkgs = nixpkgs { inherit config; };
in
  {
    kafka-device-leap = pkgs.haskell.packages."${compiler}".kafka-device-leap;
  }
