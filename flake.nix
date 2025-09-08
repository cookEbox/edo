{
  description = "edo: A qualified do library for IO (Either e a) combinator";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils"; 
  };
  outputs = { nixpkgs, flake-utils, ... }: 
    flake-utils.lib.eachDefaultSystem (system: 
    let 
      pkgs = nixpkgs.legacyPackages.${system};
      hp = pkgs.haskellPackages;
      edoPkg = hp.callCabal2nix "edo" ./. { };
    in {
      packages = { 
        default = edoPkg;
        edo = edoPkg;
      };

      devShells.default = hp.shellFor {
        packages = p: [ edoPkg ]; 
        withHoogle = true;
        buildInputs = [
          pkgs.cabal-install
          pkgs.hlint
          pkgs.ormolu
          pkgs.ghcid
          hp.haskell-language-server 
        ];
      };
      apps.hoogle = let
        serve = pkgs.writeShellApplication {
          name = "edo-hoogle";
          runtimeInputs = [
            pkgs.cabal-install
            hp.hoogle
            pkgs.findutils
            pkgs.coreutils
          ];
          text = ''
            set -euo pipefail
            cabal clean

            echo "[edo-hoogle] generating HTML + Hoogle docs via cabal…"
            cabal haddock --haddock-html --haddock-hoogle --haddock-quickjump 

            echo "[edo-hoogle] locating hoogle txt…"
            # Prefer the package's own txt (e.g. .../doc/html/edo/edo.txt)
            TXT=$(find dist-newstyle -type f -path "*/doc/html/*/edo.txt" | head -n1 || true)

            # Fallback: any *.txt under doc/html (handles component naming differences)
            if [ -z "''${TXT:-}" ]; then
              TXT=$(find dist-newstyle -type f -path "*/doc/html/*/*.txt" | head -n1 || true)
            fi

            if [ -z "''${TXT:-}" ]; then
              echo "ERROR: couldn't find a Hoogle .txt under dist-newstyle/**/doc/html/"
              exit 1
            fi

            DIR=$(dirname "''${TXT}")
            mkdir -p .hoogle
            DB=".hoogle/edo.hoo"

            echo "[edo-hoogle] building DB from: ''${DIR}"
            hoogle generate --database "''${DB}" --local="''${DIR}"

            PORT="''${HOOGLE_PORT:-8080}"
            echo "[edo-hoogle] serving http://localhost:$PORT  (Ctrl+C to stop)"
            # --local is boolean "offline" mode; not a path.
            hoogle server --database "''${DB}" --local --port "$PORT"
          '';
        };
      in {
        type = "app";
        program = "${serve}/bin/edo-hoogle";
      };

      apps.hoogle-merge = let
        gen = pkgs.writeShellApplication {
          name = "edo-hoogle-merge";
          runtimeInputs = [
            pkgs.cabal-install
            hp.hoogle
            pkgs.findutils
            pkgs.coreutils
          ];
          text = ''
            set -euo pipefail
            cabal clean

            cabal haddock --haddock-html --haddock-hoogle --haddock-quickjump

            TXT=$(find dist-newstyle -type f -path "*/doc/html/*/edo.txt" | head -n1 || true)
            if [ -z "''${TXT:-}" ]; then
              TXT=$(find dist-newstyle -type f -path "*/doc/html/*/*.txt" | head -n1 || true)
            fi
            if [ -z "''${TXT:-}" ]; then
              echo "ERROR: couldn't find a Hoogle .txt under dist-newstyle/**/doc/html/"
              exit 1
            fi

            DIR=$(dirname "''${TXT}")
            mkdir -p .hoogle
            DB=".hoogle/edo.hoo"

            echo "[edo-hoogle-merge] generating local DB at $DB from $DIR"
            hoogle generate --database "''${DB}" --local="''${DIR}"
            echo "Run: hoogle server --database $DB --local --port 8080"
          '';
        };
      in {
        type = "app";
        program = "${gen}/bin/edo-hoogle-merge";
      };
    }
  );

}
