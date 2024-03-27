{ config, lib, pkgs, inputs, ... }:
with lib;
{
  nixpkgs.overlays = [ inputs.emacs-overlay.overlay ];

  home-manager.useGlobalPkgs = true;
  home-manager.useUserPackages = true;
  home-manager.users.bascht = import ./home.nix;

}
