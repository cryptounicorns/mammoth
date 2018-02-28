{ config, lib, pkgs, ... }:

with lib;

let
  name = "mammoth";
  cfg = config.services."${name}";
  pkg = (pkgs.callPackage ./default.nix { });
  in {
  options = with types; {
    services."${name}" = {
      enable = mkEnableOption "Mammoth HTTP interface for time-series data";
      user = mkOption {
        default = name;
        type = string;
        description = ''
          User name to run service from.
        '';
      };
      group = mkOption {
        default = name;
        type = string;
        description = ''
          Group name to run service from.
        '';
      };
    };
  };

  config = mkIf cfg.enable {
    users.extraUsers."${name}" = {
      name = name;
      group = cfg.group;
      uid = config.cryptounicorns.ids.uids."${name}";
    };

    users.extraGroups."${name}" = {
      name = name;
      gid = config.cryptounicorns.ids.gids."${name}";
    };

    systemd.services."${name}" = {
      enable = true;

      wants    = [ "influxdb.service" ];
      wantedBy = [ "multi-user.target" ];
      after    = [ "network.target" ];

      serviceConfig = {
        Type = "simple";
        User = name;
        Group = name;
        ExecStart = "${pkg}/bin/${name}";
        Restart = "on-failure";
        RestartSec = 1;
      };
    };
  };
}
