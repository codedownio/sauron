{ clang
, pkgsStatic
}:

{
  packages.sauron.components.exes.sauron.configureFlags = [
    ''--ghc-options="-pgml clang++ -optl-L${pkgsStatic.gmp}/lib -optl-L${pkgsStatic.ncurses}/lib -optl-L${pkgsStatic.pcre}/lib -optl-L${pkgsStatic.libffi}/lib -optl-L${pkgsStatic.gettext}/lib"''
    ''--ghc-options="-optl-lgmp -optl-lncursesw -optl-lpcre -optl-lffi -optl-lintl"''
    "--disable-shared"
  ];
  packages.sauron.components.exes.sauron.build-tools = [clang];
  packages.sauron.components.exes.sauron.dontStrip = false;

  # Override C library dependencies to use static versions
  packages.sauron.components.exes.sauron.libs = [
    pkgsStatic.gmp
    pkgsStatic.ncurses
    pkgsStatic.pcre
    pkgsStatic.libffi
    pkgsStatic.gettext
  ];

  packages.sauron.components.exes.sauron.postInstall = ''
    strip "$out/bin/sauron"

    echo "Dynamic library dependencies before patching:"
    otool -L "$out/bin/sauron"

    # Fix a few more dylibs
    source "${./fix-dylib.sh}"
    fix_dylib "$out/bin/sauron" "libstdc++.6.dylib" "/usr/lib/libstdc++.dylib"
    fix_dylib "$out/bin/sauron" "libc++.1.0.dylib" "/usr/lib/libc++.dylib"
    fix_dylib "$out/bin/sauron" "libc++abi.1.0.dylib" "/usr/lib/libc++abi.dylib"
    fix_dylib "$out/bin/sauron" "libz.dylib" "/usr/lib/libz.dylib"
    fix_dylib "$out/bin/sauron" "libiconv.2.dylib" "/usr/lib/libiconv.2.dylib"

    if otool -L "$out/bin/sauron" | tail -n +2 | grep -q "/nix/store"; then
      echo "ERROR: Found nix store references in binary:"
      otool -L "$out/bin/sauron" | tail -n +2 | grep "/nix/store"
      exit 1
    fi
  '';
}
