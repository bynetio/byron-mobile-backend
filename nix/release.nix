let
  appConfig = ../app.config;
  mkDockerImage = pkgs: package:
    pkgs.dockerTools.buildImage {
      name = "mobile-wallet-backend";
      tag = "latest";
      contents = [ package pkgs.bash pkgs.bashInteractive pkgs.coreutils ];
      runAsRoot = ''
        mkdir -p /etc/mobile-wallet-backend/
        cp ${appConfig} /etc/mobile-wallet-backend/app.config
      '';
      config = { Cmd = [ "/bin/mobile-wallet-backend" "start-app" "--config" "/etc/mobile-wallet-backend/app.config" ]; };
    };
in
{
  inherit mkDockerImage;
}
