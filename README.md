# migaman

![demo](demo.gif)

Migaman is a CLI email alias manager for [Migadu][migadu] users. It allows you
to generate and manage per-service email addresses with ease, allowing you to
protect your primary email address from spam without sacrificing convenience.

Migaman is similar to services like [addy.io][addy], however with a different
set of trade-offs. Migaman only works with Migadu compared to other services
which are provider-agnostic. In turn, Migaman directly uses the Migadu API for
its operations. This way, Migaman cuts the middle-man from the process, whereas
the other services need to intercept the email traffic to proxy them to the
recipient.

## Installation

### Option 1: Nix

If you have the [Nix][nix] package manager with Flakes support enabled, you can
run Migaman with:

```bash
$ nix run github:ozkutuk/migaman
```

If you want to install Migaman to your system rather than always running from
the GitHub source, please refer to the Nix manual.

### Option 2: Building from source

This option involves installing the Haskell toolchain, and the easiest way to do
so is via [GHCup][ghcup]. Please follow the instructions on the GHCup website to
proceed with the installation.

Once the toolchain is installed, compile and install the executable to your
system:

```bash
$ cabal install exe:migaman
```

Afterwards, you can run the program:

```bash
$ migaman
```

### Option 3: home-manager

If you have a [home-manager][home-manager] installation set up with Flakes, you
can use the home-manager module to install _and configure_ Migaman:

```nix
{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    
    migaman.url = "github:ozkutuk/migaman";
  };
  outputs = { nixpkgs, home-manager, migaman, ... }:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
    in {
      homeConfigurations.jdoe = home-manager.lib.homeManagerConfiguration {
        inherit pkgs;

        modules = [
          migaman.homeManagerModules.default

          {
            # See "migaman.toml.sample" for the full list of options
            programs.migaman = {
              enable = true;
              settings = {
                migadu.auth = {
                  account = "john.doe@example.com";
                  keyCommand = "pass show migadu-api-key";
                };
                migaman = {
                  database = "${config.xdg.dataHome}/migaman/db.sqlite3";
                };
              };
            };
          }
        ];
      };
    };
}
```

## Usage

The first time you run Migaman, it will generate a configuration file and prompt
you to fill in the mandatory fields. Once you fill it, you can start using
Migaman to manage your aliases. For convenience's sake, the following commands
assume that you have filled in the default values for `domain`, `name` and
`target` fields. However, Migaman allows all of them to be provided as CLI
options if desired.

If you have been previously using the "identities" feature of Migadu manually to
manage your aliases, you can import them to Migaman's database via `migaman
import`. Otherwise, let's start by generating an alias:

```bash
$ migaman generate untrusted-service
x83vk1gv0a@example.com

# ...or you can directly pipe the output to your CLI clipboard
# utility of choice:
# $ migaman generate untrusted-service | wl-copy
```

Migaman has generated a new Migadu "identity" that you can use while registering
to the "untrusted-service". You can confirm that it is generated and active by
listing all the aliases:

```bash
$ migaman list
+-------------------++------------------------+-----------------------+---------+
|                   ||                  email |                target | enabled |
+===================++========================+=======================+=========+
| untrusted-service || x83vk1gv0a@example.com | blackhole@example.com |       X |
+-------------------++------------------------+-----------------------+---------+
```

If at some point you are bothered by the emails you recieve on that alias, you
can simply disable it:

```bash
migaman disable untrusted-service
```

That's about it. You can see the full list of commands supported by running
`migaman --help`.


[migadu]: https://www.migadu.com/
[addy]: https://addy.io/
[nix]: https://nixos.org/
[ghcup]: https://www.haskell.org/ghcup/
[home-manager]: https://github.com/nix-community/home-manager
