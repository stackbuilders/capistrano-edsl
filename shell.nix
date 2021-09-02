let
  sources = import ./nix/sources.nix {};
  pkgs = import sources.nixpkgs {};
in pkgs.mkShell {
  name = "capistrano_edsl";
  buildInputs = [
    pkgs.stack
  ];
}
