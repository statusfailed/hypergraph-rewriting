{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, array, base, containers, logict, mtl, stdenv
      , tasty, tasty-hunit, vector
      }:
      mkDerivation {
        pname = "hypergraph-rewriting";
        version = "0.1.0.0";
        src = ./.;
        libraryHaskellDepends = [
          array base containers logict mtl vector
        ];
        testHaskellDepends = [ base containers logict tasty tasty-hunit ];
        license = stdenv.lib.licenses.mit;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
