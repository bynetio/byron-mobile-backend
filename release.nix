let
  packages = import ./.;

  inherit (packages) pkgs plutus-starter;
  project = plutus-starter.haskell.project;
  exe = project.mobile-wallet-backend.components.exes.mobile-wallet-backend;
in
{
  mobile-wallet-backend = (import ./nix/release.nix).mkDockerImage pkgs exe;
}