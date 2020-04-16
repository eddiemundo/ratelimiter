let
  hs-compiler = "ghc883";
  hs = nixpkgs.haskell;
  hs-pkgs = hs.packages.${hs-compiler};

  hs-overlay = self: super: {
    my-hs-pkgs = super.haskell.packages.${hs-compiler}.override {
      overrides = self: super: {
        ghcide = nixpkgs.haskell.lib.dontCheck (self.callCabal2nix
          "ghcide"
          (builtins.fetchGit {
            url = "https://github.com/digital-asset/ghcide.git";
            rev = "39605333c34039241768a1809024c739df3fb2bd";
          }) {}
        );
        hie-bios = nixpkgs.haskell.lib.dontCheck (self.callHackageDirect {
          pkg = "hie-bios";
          ver = "0.4.0";
          sha256 = "19lpg9ymd9656cy17vna8wr1hvzfal94gpm2d3xpnw1d5qr37z7x";
        } {});
        haskell-lsp = nixpkgs.haskell.lib.dontCheck (self.callHackageDirect {
          pkg = "haskell-lsp";
          ver = "0.21.0.0";
          sha256 = "1j6nvaxppr3wly2cprv556yxr220qw1ghd3ac139iw16ihfjvz8a";
        } {});
        haskell-lsp-types = nixpkgs.haskell.lib.dontCheck (self.callHackageDirect {
          pkg = "haskell-lsp-types";
          ver = "0.21.0.0";
          sha256 = "0vq7v6k9szmwxh2haphgzb3c2xih6h5yyq57707ncg0ha75bhlll";
        } {});
        cabal-install = nixpkgs.callPackage ./cabal-3.2.0.0.nix { inherit nixpkgs; };
      };
    };
  };

  nixpkgs = import (import ./nixpkgs.nix) {
    overlays = [ hs-overlay ];
  };
in
  nixpkgs.mkShell {
    buildInputs = [ hs.compiler.${hs-compiler} nixpkgs.my-hs-pkgs.cabal-install nixpkgs.my-hs-pkgs.ghcide ];
  }


