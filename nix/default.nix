{ pkgs ? import ./sources.nix {}, doCheck ? false }:


  let
    inherit (pkgs) lib stdenv ocamlPackages;
    genSrc = { dirs, files }: lib.filterGitSource {
      src = ./..;
      inherit dirs;
      files = files ++ [ "dune-project" ];
    };
    buildPackage = args: ocamlPackages.buildDunePackage ({
      version = "0.0.1-dev";
      inherit doCheck;
      useDune2 = true;
    } // args);
in

with ocamlPackages;

rec {
  bson = buildPackage {
    pname = "bson";

    src = genSrc {
      dirs = [ "bson" ];
      files = [ "bson.opam" ];
    };

    propagatedBuildInputs = [
      angstrom
      faraday
   ];
  };

  mongo = buildPackage {
    pname = "mongo";

    src = genSrc {
      dirs = [ "src" ];
      files = [ "mongo.opam" ];
    };

    propagatedBuildInputs = [ bson ];
  };

  mongo-lwt = buildPackage {
    pname = "mongo-lwt";

    src = genSrc {
      dirs = [ "lwt" ];
      files = [ "mongo-lwt.opam" ];
    };

    propagatedBuildInputs = [
      mongo
      lwt
      mirage-crypto
      gluten-lwt
      pbkdf
      base64
    ];
  };

  mongo-lwt-unix = buildPackage {
    pname = "mongo-lwt-unix";

    src = genSrc {
      dirs = [ "lwt-unix" ];
      files = [ "mongo-lwt-unix.opam" ];
    };

    propagatedBuildInputs = [
      mongo-lwt
      gluten-lwt-unix
    ];
  };

  ppx_deriving_bson = buildPackage {
    pname = "ppx_deriving_bson";

    src = genSrc {
      dirs = [ "ppx" ];
      files = [ "ppx_deriving_bson.opam" ];
    };

    propagatedBuildInputs = [
      ppxlib
      ppx_deriving
      bson
    ];
  };
}

