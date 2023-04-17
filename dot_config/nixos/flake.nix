{
  inputs.nixos-hardware.url = "github:NixOS/nixos-hardware/master";
  inputs.bascht-private.url = "git+https://git.dorhamm.me/bascht/nixos-private.git?ref=main";
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-22.11";

  outputs = { self, nixpkgs, nixos-hardware, bascht-private }: {
    nixosConfigurations.pierogi = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [
        nixos-hardware.nixosModules.lenovo-thinkpad-t14s
        ./configuration.nix
        ./thinkpads.nix
        ./boot.nix
        ./network.nix
        ./security.nix
        ./host-pierogi.nix
        bascht-private.udev-pierogi
        bascht-private.trieste
        bascht-private.wifi
      ];
    };
    nixosConfigurations.apfelstrudel = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [
        nixos-hardware.nixosModules.lenovo-thinkpad-t14s
        ./configuration.nix
        ./thinkpads.nix
        ./boot.nix
        ./network.nix
        ./security.nix
        ./host-apfelstrudel.nix
        bascht-private.udev-apfelstrudel
        bascht-private.alfaview
        bascht-private.wifi
      ];
    };
    nixosConfigurations.flaki = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [
        ./configuration.nix
        ./hardware-configuration.nix
        ./wifi.nix
        ./scanberry.nix
        ./flaki-host.nix
      ];
    };
  };
}
