{
  inputs.nixos-hardware.url = "github:NixOS/nixos-hardware/master";
  inputs.bascht-private.url = "git+https://git.dorhamm.me/bascht/nixos-private.git?ref=main";
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-22.11";
  inputs.home-manager = {
    url = "github:nix-community/home-manager";
    inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, nixpkgs, nixos-hardware, bascht-private, home-manager }: {
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
    nixosConfigurations.zurek = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [
        ./configuration.nix
        ./boot.nix
        ./network.nix
        ./security.nix
        ./host-zurek.nix
        bascht-private.udev-zurek
      ];
    };
    nixosConfigurations.flaki = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [
        ./configuration.nix
        ./scanberry.nix
        ./boot.nix
        ./network.nix
        ./security.nix
        ./host-flaki.nix
        bascht-private.udev-flaki
        bascht-private.trieste
        bascht-private.wifi
      ];
    };
  };
}
