{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    nixpkgs.follows = "quasar/nixpkgs";
    quasar.url = "github:queezle42/quasar";
  };

  outputs = { self, nixpkgs, quasar }:
  with nixpkgs.lib;
  let
    systems = platforms.unix;
    forAllSystems = genAttrs systems;
    getHaskellPackages = pkgs: pattern: pipe pkgs.haskell.packages [
      attrNames
      (filter (x: !isNull (strings.match pattern x)))
      (sort (x: y: x>y))
      (map (x: pkgs.haskell.packages.${x}))
      head
    ];
  in {
    packages = forAllSystems (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [
            self.overlays.default
            quasar.overlays.default
          ];
        };
        haskellPackages = getHaskellPackages pkgs "ghc94.";
        results = {
          qty = haskellPackages.qty;
        };
      in results // {
        default = pkgs.linkFarm "qty-all" (results // mapAttrs' (k: v: nameValuePair "${k}-doc" (v.doc or pkgs.emptyDirectory)) results);
      }
    );

    apps = forAllSystems (system: {
      default = {
        type = "app";
        program = "${self.packages.${system}.qty}/bin/qty";
      };
    });

    overlays.default = final: prev: {
      haskell = prev.haskell // {
        packageOverrides = hfinal: hprev: prev.haskell.packageOverrides hfinal hprev // {
          qty = hfinal.generateOptparseApplicativeCompletions ["qty"]
            (hfinal.callCabal2nix "qty" ./qty {});
        };
      };
    };

    devShells = forAllSystems (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [
            self.overlays.default
            quasar.overlays.default
          ];
        };
        haskellPackages = getHaskellPackages pkgs "ghc94.";
      in {
        default = haskellPackages.shellFor {
          packages = hpkgs: [
            hpkgs.qty
          ];
          nativeBuildInputs = [
            haskellPackages.haskell-language-server
            pkgs.cabal-install
            pkgs.hlint

            # in addition, for ghcid-wrapper
            pkgs.entr
            pkgs.ghcid
            pkgs.zsh
          ];
        };
      }
    );
  };
}
