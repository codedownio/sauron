{
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.gitignore = {
    url = "github:hercules-ci/gitignore.nix";
    inputs.nixpkgs.follows = "nixpkgs";
  };
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/release-23.11";

  outputs = { self, flake-utils, gitignore, haskellNix, nixpkgs }:
    # flake-utils.lib.eachDefaultSystem (system:
    flake-utils.lib.eachSystem ["x86_64-linux"] (system:
      let
        overlays = [
          haskellNix.overlay
          (import ./nix/fix-ghc-pkgs-overlay.nix)
        ];

        pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };

        src = gitignore.lib.gitignoreSource ./.;

        flake = (pkgs.hixProject "ghc964" src).flake {};
        flakeStatic = (pkgs.pkgsCross.musl64.hixProject "ghc964" src).flake {};

      in
        {
          packages = rec {
            inherit pkgs flake flakeStatic;

            inherit (pkgs) cabal2nix;

            normal = flake.packages."sauron:exe:sauron";
            static = flakeStatic.packages."sauron:exe:sauron";

            print-nixpkgs = pkgs.writeShellScriptBin "print-nixpkgs.sh" "echo ${pkgs.path}";
          };
        }
    );
}
