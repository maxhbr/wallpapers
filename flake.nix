{
  description = "my wallpapers";

  inputs.nixpkgs.url = github:NixOS/nixpkgs/nixos-unstable;

  outputs = { self, nixpkgs, ... }@inputs: let
    pkgs = import nixpkgs { system = "x86_64-linux"; };
    my-wallpapers = pkgs.callPackage ./default.nix {};
  in {
    defaultPackage.x86_64-linux = my-wallpapers;
    nixosModule = { config, lib, pkgs, ... }: {
      config = (lib.mkIf config.services.xserver.enable {
        nixpkgs.overlays = [
          (_: _: {
            my-wallpapers = my-wallpapers;
          })
        ];
        home-manager.sharedModules = [{
          home.packages = [ my-wallpapers ];
          services.random-background = {
            enable = true;
            imageDirectory = "${my-wallpapers}/share";
            display = "scale";
            interval = "10min";
          };
        }];

        services.xserver.displayManager.lightdm = {
          background = "${my-wallpapers}/share/romben3.png";
          # sessionCommands = ''
          #   ${pkgs.my-wallpapers}/bin/myRandomBackground &disown
          # '';
        };
      });
    };
  };
}
