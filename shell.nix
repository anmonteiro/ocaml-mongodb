let
  pkgs = import ./nix/sources.nix {};
  inherit (pkgs) lib;
  mongoPkgs = pkgs.recurseIntoAttrs (import ./nix { inherit pkgs; });
  mongoDrvs = lib.filterAttrs (_: value: lib.isDerivation value) mongoPkgs;

  filterDrvs = inputs:
    lib.filter
      (drv:
        # we wanna filter our own packages so we don't build them when entering
        # the shell. They always have `pname`
        !(lib.hasAttr "pname" drv) ||
        drv.pname == null ||
        !(lib.any (name: name == drv.pname || name == drv.name) (lib.attrNames mongoDrvs)))
      inputs;

in
  with pkgs;

  (mkShell {
    inputsFrom = lib.attrValues mongoDrvs;
    buildInputs = with ocamlPackages; [ merlin ocamlformat utop pkgs.mongodb-4_2 ];
  }).overrideAttrs (o : {
    propagatedBuildInputs = filterDrvs o.propagatedBuildInputs;
    buildInputs = filterDrvs o.buildInputs;
  })
