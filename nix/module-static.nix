{ pkgsCross }:

{
  packages.sauron.components.exes.sauron.configureFlags = [
    ''--ghc-options="-pgml g++"''
  ];
  packages.sauron.components.exes.sauron.dontStrip = false;

  packages.sauron.components.exes.sauron.enableShared = false;
  packages.sauron.components.exes.sauron.libs = [];
  packages.sauron.components.exes.sauron.build-tools = [pkgsCross.musl64.gcc];
}
