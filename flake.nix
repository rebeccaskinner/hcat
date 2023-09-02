{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        hsPkgs = pkgs.haskellPackages;
        hsDevPkgs = hsPkgs.ghcWithPackages(p: with p;
          [ # packages used in effective haskell
            bytestring
            base64-bytestring
            text
            containers
            vector
            time
            unix
            mtl
            transformers
            process

            # testing packages
            hspec
            hspec-expectations
          ]);

        hcat = returnShellEnv:
          import ./package.nix { inherit pkgs hsPkgs returnShellEnv;};

        shellPackages =
          (with hsPkgs; [fourmolu ghcid hlint]) ++
          [ hsDevPkgs ];

      in {
        packages.hcat = hcat false;
        apps.hcat = { type = "app"; program = "${self.packages.${system}.hcat}/bin/hcat"; };
        defaultPackage = self.packages.${system}.hcat;
        defaultApp = self.apps.${system}.hcat;
        devShell = pkgs.mkShell {
          inputsFrom = [(hcat true)] ++ shellPackages;
          buildInputs = shellPackages;
        };
      });
}
