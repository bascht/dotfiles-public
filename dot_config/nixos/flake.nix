{
  inputs.nixos-hardware.url = "github:NixOS/nixos-hardware/master";
  inputs.bascht-private.url = "/home/bascht/Code/nixos-private.git";

  outputs = { self, nixpkgs, nixos-hardware, bascht-private }: {
    nixosConfigurations.pierogi = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [
        nixos-hardware.nixosModules.lenovo-thinkpad-t14s
        ./configuration.nix
        ./thinkpads.nix
        ./boot.nix
        ./network.nix
        ./host-pierogi.nix
        bascht-private.udev-pierogi
        bascht-private.trieste
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
