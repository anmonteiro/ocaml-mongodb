{ pkgs, stdenv, lib, ocamlPackages, static ? false, doCheck }:

with ocamlPackages;

rec {
  piaf = buildDunePackage {
    pname = "piaf";
    version = "0.0.1-dev";

    src = lib.filterGitSource {
      src = ./..;
      dirs = [ "lib" "lib_test" ];
      files = [ "dune-project" "piaf.opam" ];
    };

    useDune2 = true;

    propagatedBuildInputs = [
      bigstringaf
      uri
      angstrom
      faraday
      gluten-lwt-unix

      alcotest
      alcotest-lwt
      ppxlib
      ppx_deriving

      lwt
      mirage-crypto
      pbkdf
      base64
    ];

    inherit doCheck;

    meta = {
      description = "Client library for HTTP/1.X / HTTP/2 written entirely in OCaml.";
      license = stdenv.lib.licenses.bsd3;
    };
  };
}

