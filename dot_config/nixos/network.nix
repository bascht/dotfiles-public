{ config, pkgs, lib, ... }:

{
  imports = [  ];
  config = {
    systemd.network = {
      enable = true;
      wait-online.anyInterface = true;

      networks = {
        "100-ethernet" = {
          matchConfig.Name = "ethernet";
          DHCP = "yes";
          networkConfig = {
            IPv6AcceptRA = true;
            LinkLocalAddressing = "yes";
          };
          dhcpV4Config = {
            UseDomains = true;
          };
          dhcpConfig = {
            UseDNS = true;
          };
        };

        "100-dorhamm-docking" = {
          matchConfig.Name = "dorhamm-docking";
          DHCP = "yes";
          networkConfig = {
            IPv6AcceptRA = true;
            LinkLocalAddressing = "yes";
          };
          dhcpV4Config = {
            UseDomains = true;
          };
          dhcpConfig = {
            UseDNS = true;
          };
        };

        "100-wifi" = {
          matchConfig.Name = "wifi";
          DHCP = "yes";
          networkConfig = {
            IPv6AcceptRA = true;
            LinkLocalAddressing = "yes";
          };
          dhcpV4Config = {
            UseDomains = true;
            RouteMetric = 1200;
          };
          dhcpConfig = {
            UseDNS = true;
          };
        };
      };
    };

  };
}
