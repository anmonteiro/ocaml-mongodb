{ ocamlVersion ? "4_11" }:

let
  overlays =
    /Users/anmonteiro/projects/nix-overlays;
    # builtins.fetchTarball
      # https://github.com/anmonteiro/nix-overlays/archive/86c05aa.tar.gz;

in

  import "${overlays}/sources.nix" {
    overlays = [
      (import overlays)
      (self: super: {
        ocamlPackages = super.ocaml-ng."ocamlPackages_${ocamlVersion}";

        pkgsCross.musl64.pkgsStatic = super.pkgsCross.musl64.pkgsStatic.appendOverlays [(self: super: {
          ocamlPackages = super.ocaml-ng."ocamlPackages_${ocamlVersion}";
        })];
      })
    ];
  }
