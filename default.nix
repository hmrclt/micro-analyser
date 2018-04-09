{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, base, bytestring, conduit, containers
      , directory, http-conduit, http-types, lens, lens-aeson, MissingH
      , network-info, optparse-applicative, regex-compat, resourcet
      , stdenv, text, time, vector
      }:
      mkDerivation {
        pname = "micro-analyser";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [
          aeson base bytestring conduit containers directory http-conduit
          http-types lens lens-aeson MissingH network-info
          optparse-applicative regex-compat resourcet text time vector
        ];
        description = "Allows inspection of microservices on MDTP";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
