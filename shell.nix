{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, curl, scalpel, stdenv, text, webdriver
      }:
      mkDerivation {
        pname = "financial-news-scraper";
        version = "0.1.0.0";
        src = ./.;
        libraryHaskellDepends = [ base curl scalpel text webdriver ];
        homepage = "https://github.com/v0d1ch/financial-news-scraper#readme";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
