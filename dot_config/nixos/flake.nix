{
  inputs.nixos-hardware.url = "github:NixOS/nixos-hardware/master";
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.05";
  inputs.home-manager = {
    url = "github:nix-community/home-manager/release-23.05";
    inputs.nixpkgs.follows = "nixpkgs";
  };
  inputs.bascht-private = {
    url = "git+https://git.dorhamm.me/bascht/nixos-private.git?ref=main";
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
        home-manager.nixosModules.home-manager
        ./home-manager.nix
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
        ./futros.nix
        ./boot.nix
        ./network.nix
        ./security.nix
        ./host-zurek.nix
        bascht-private.udev-zurek
        bascht-private.ssh-boot-zurek
      ];
    };
    nixosConfigurations.flaki = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [
        ./configuration.nix
        ./futros.nix
        ./scanberry.nix
        ./boot.nix
        ./network.nix
        ./security.nix
        ./host-flaki.nix
        bascht-private.udev-flaki
        bascht-private.ssh-boot-flaki
        bascht-private.trieste
        bascht-private.wifi
        home-manager.nixosModules.home-manager
        ./home-manager.nix

      ];
    };
  };
}
