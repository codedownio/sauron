{
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.gitignore = {
    url = "github:hercules-ci/gitignore.nix";
    inputs.nixpkgs.follows = "nixpkgs";
  };
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";

  outputs = { self, flake-utils, gitignore, haskellNix, nixpkgs }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [
            haskellNix.overlay
            (import ./nix/fix-ghc-pkgs-overlay.nix)
          ];
          inherit (haskellNix) config;
        };

        src = gitignore.lib.gitignoreSource ./.;

        compilerNixName = "ghc9122";

        flake = (pkgs.haskell-nix.hix.project {
          inherit src;
          evalSystem = "x86_64-linux";
          compiler-nix-name = compilerNixName;
          projectFileName = "stack.yaml";
          modules = [
            (import ./nix/fix-ghc-pkgs-module.nix)
            (import ./nix/os-string-module.nix)
            {
              packages.sauron.components.exes.sauron.configureFlags = [
                ''--ghc-options="-pgml g++"''
              ];
              packages.sauron.components.exes.sauron.build-tools = [pkgs.gcc];
              packages.sauron.components.exes.sauron.dontStrip = false;
              packages.sauron.components.exes.sauron.postInstall = pkgs.lib.optionalString (pkgs.stdenv.hostPlatform.isDarwin) ''
                # Haskell.nix seems to automatically strip on Linux, but we need this on macOS
                strip "$out/bin/sauron"

                # For debugging
                otool -L $out/bin/sauron

                # Recursively gather and fix all dylib dependencies
                ${./nix/gather-dylibs.sh} $out/bin/sauron $out/bin "@executable_path/"
              '';
            }
          ];
        }).flake {};

        flakeDarwinStatic = (pkgs.haskell-nix.hix.project {
          inherit src;
          evalSystem = "x86_64-linux";
          compiler-nix-name = compilerNixName;
          projectFileName = "stack.yaml";
          modules = [
            (import ./nix/fix-ghc-pkgs-module.nix)
            (import ./nix/os-string-module.nix)
            {
              packages.sauron.components.exes.sauron.configureFlags = [
                ''--ghc-options="-pgml g++ -optl-L${pkgs.pkgsStatic.gmp}/lib -optl-L${pkgs.pkgsStatic.ncurses}/lib -optl-L${pkgs.pkgsStatic.pcre}/lib -optl-L${pkgs.pkgsStatic.libffi}/lib -optl-L${pkgs.pkgsStatic.gettext}/lib"''
                ''--ghc-options="-optl-lgmp -optl-lncursesw -optl-lpcre -optl-lffi -optl-lintl"''
                "--disable-shared"
              ];
              packages.sauron.components.exes.sauron.build-tools = [pkgs.gcc];
              packages.sauron.components.exes.sauron.dontStrip = false;

              # Override C library dependencies to use static versions
              packages.sauron.components.exes.sauron.libs = with pkgs; [
                pkgsStatic.gmp
                pkgsStatic.ncurses
                pkgsStatic.pcre
                pkgsStatic.libffi
                pkgsStatic.gettext
              ];

              packages.sauron.components.exes.sauron.postInstall = ''
                # Strip the binary
                strip "$out/bin/sauron"

                # Verify linking - should show system libraries only, no nix store dylibs
                echo "=== Checking final binary dependencies ==="
                otool -L "$out/bin/sauron"

                # Count nix store references - should be minimal (ideally just system libs)
                nix_refs=$(otool -L "$out/bin/sauron" | grep -c "/nix/store/" || echo "0")
                echo "Nix store references found: $nix_refs"

                # This is acceptable as long as there are no dylib files being shipped
                # The goal is to not need separate dylib files
                if [ -d "$out/bin" ] && find "$out/bin" -name "*.dylib" | grep -q .; then
                  echo "ERROR: Dylib files found in output!"
                  find "$out/bin" -name "*.dylib"
                  exit 1
                fi

                echo "SUCCESS: No separate dylib files needed"
              '';
            }
          ];
        }).flake {};

        flakeStatic = (pkgs.pkgsCross.musl64.haskell-nix.hix.project {
          inherit src;
          evalSystem = "x86_64-linux";
          compiler-nix-name = compilerNixName;
          projectFileName = "stack.yaml";
          modules = [
            (import ./nix/fix-ghc-pkgs-module.nix)
            (import ./nix/os-string-module.nix)
            {
              packages.sauron.components.exes.sauron.configureFlags = [
                ''--ghc-options="-pgml g++"''
              ];
              packages.sauron.components.exes.sauron.dontStrip = false;

              packages.sauron.components.exes.sauron.enableShared = false;
              packages.sauron.components.exes.sauron.libs = [];
              packages.sauron.components.exes.sauron.build-tools = [pkgs.pkgsCross.musl64.gcc];
            }
          ];
        }).flake {};

        flakeWindows = (pkgs.pkgsCross.mingwW64.haskell-nix.hix.project {
          inherit src;
          compiler-nix-name = compilerNixName;
          projectFileName = "stack.yaml";
          modules = [
            (import ./nix/os-string-module.nix)
            {
              packages.bitvec.components.library.configureFlags = [
                "-f -simd"
              ];
              packages.basement.components.library.configureFlags = [
                "--gcc-option=-Wno-int-conversion" # Fix for newer GCC
              ];
              packages.sauron.components.exes.sauron.configureFlags = [
                "--ld-option=-Wl,--allow-multiple-definition"
              ];
            }
          ];
        }).flake {};

      in
        {
          devShells = {
            default = pkgs.mkShell {
              buildInputs = with pkgs; [
                gmp
                ncurses
                pcre
                pkg-config
                zlib

                pkgs.haskell.compiler.ghc9122
                (pkgs.haskell-language-server.override { supportedGhcVersions = ["9122"]; })
              ];
            };
          };

          packages = rec {
            inherit pkgs flake flakeStatic flakeDarwinStatic;

            inherit (pkgs) cabal2nix;

            normal = flake.packages."sauron:exe:sauron";
            static = flakeStatic.packages."sauron:exe:sauron";
            darwin-static = flakeDarwinStatic.packages."sauron:exe:sauron";
            windows = flakeWindows.packages."sauron:exe:sauron";

            print-nixpkgs = pkgs.writeShellScriptBin "print-nixpkgs.sh" "echo ${pkgs.path}";
          };
        }
    );
}
