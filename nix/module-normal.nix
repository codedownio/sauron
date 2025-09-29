{ gcc
, lib
, stdenv
}:

{
  packages.sauron.components.exes.sauron.configureFlags = [
    ''--ghc-options="-pgml g++"''
  ];
  packages.sauron.components.exes.sauron.build-tools = [gcc];
  packages.sauron.components.exes.sauron.dontStrip = false;
  packages.sauron.components.exes.sauron.postInstall = lib.optionalString (stdenv.hostPlatform.isDarwin) ''
    # Haskell.nix seems to automatically strip on Linux, but we need this on macOS
    strip "$out/bin/sauron"

    # For debugging
    otool -L $out/bin/sauron

    # Recursively gather and fix all dylib dependencies
    ${./gather-dylibs.sh} $out/bin/sauron $out/bin "@executable_path/"
  '';
}
