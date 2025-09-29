{ }:

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
