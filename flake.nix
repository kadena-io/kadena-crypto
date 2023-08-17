{
  description = "Kadena's Crypto Primitives";

  inputs = {
    # nixpkgs.follows = "haskellNix/nixpkgs";
    nixpkgs.url = "github:NixOS/nixpkgs?rev=4d2b37a84fad1091b9de401eb450aae66f1a741e";
    haskellNix.url = "github:input-output-hk/haskell.nix";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    flake-utils.lib.eachSystem
      [ "x86_64-linux" "x86_64-darwin"
        "aarch64-linux" "aarch64-darwin" ] (system:
    let
      pkgs = import nixpkgs {
        inherit system overlays;
        inherit (haskellNix) config;
      };
      flake = pkgs.pact-crypto.flake {
        # crossPlatforms = p: [ p.ghcjs ];
      };
      overlays = [ haskellNix.overlay
        (final: prev: {
          pact-crypto =
            final.haskell-nix.project' {
              src = ./.;
              compiler-nix-name = "ghc962";
              shell.tools = {
                cabal = {};
                haskell-language-server = {};
                # hlint = {};
              };
              shell.buildInputs = with pkgs; [
                openssl
              ];
              # shell.crossPlatforms = p: [ p.ghcjs ];
            };
        })
      ];
      # This package depends on other packages at buildtime, but its output does not
      # depend on them. This way, we don't have to download the entire closure to verify
      # that those packages build.
      mkCheck = name: package: pkgs.runCommand ("check-"+name) {} ''
        echo ${name}: ${package}
        echo works > $out
      '';
    in flake // rec {
      packages.default = flake.packages."kadena-crypto:lib:kadena-crypto";

      devShell = pkgs.haskellPackages.shellFor {
        buildInputs = with pkgs.haskellPackages; [
          cabal-install
          haskell-language-server
          # hlint
        ];

        withHoogle = true;
      };
      packages.check = pkgs.runCommand "check" {} ''
        echo ${mkCheck "pact-crypto" packages.default}
        echo ${mkCheck "devShell" flake.devShell}
        echo works > $out
      '';

    });
}
