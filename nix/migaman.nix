perSystem: {
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.programs.migaman;
  tomlFormat = pkgs.formats.toml {};
in {
  options.programs.migaman = {
    enable = lib.mkEnableOption "Migaman email alias manager";

    package = lib.mkOption {
      type = lib.types.package;
      default = perSystem.config.packages.migaman;
      description = "The package to use for migaman.";
    };

    settings = lib.mkOption {
      type = tomlFormat.type;
      default = {};
      example = lib.literalExpression ''
        {
          migadu.auth = {
            account = "john.doe@example.com";
            keyCommand = "pass show migadu-key";
          };
          migaman = {
            database = "''${config.xdg.dataHome}/migaman/db.sqlite3";
            defaults = {
              name = "John Doe";
              domain = "example.com";
              target = "john";
            };
          };
        }
      '';
      description = ''
        Configuration written to
        {file}`$XDG_CONFIG_HOME/migaman.toml`.

        See <https://github.com/ozkutuk/migaman/blob/main/migaman.toml.sample>
        for the full list of options.
      '';
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = [cfg.package];

    xdg.configFile."migaman.toml" = lib.mkIf (cfg.settings != {}) {
      source = tomlFormat.generate "migaman-config" cfg.settings;
    };
  };
}
