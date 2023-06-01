{ lib, pkgs, config, ... }:
with lib;
let
  cfg = config.services.scanberry;
in
{
  options.services.scanberry = {
    enable = mkEnableOption "scanberry service";
  };

  config = mkIf cfg.enable {
    systemd.user.services.scanberry = {
      wantedBy = [ "multi-user.target" ];
      serviceConfig.ExecStart = "/home/bascht/Code/scanberry/scanberry";
    };
  };
}
