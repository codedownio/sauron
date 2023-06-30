{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/release-23.05";

  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }@inputs:
    # flake-utils.lib.eachDefaultSystem (system:
    flake-utils.lib.eachSystem ["x86_64-linux"] (system:
      let
        pkgs = import nixpkgs { inherit system; };
        default = pkgs.haskell.packages.ghc8107.callPackage ./default.nix {};
      in
        rec {
          packages = rec {
            inherit default;
          };

          defaultPackage = packages.default;
        }
    );
}
