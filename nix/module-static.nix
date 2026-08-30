{ pkgsStatic }:

{ lib, ... }:

let
  # build-tools splices pkgsStatic.gcc to the build->musl cross wrapper, and that
  # wrapper only installs prefixed binaries. There is no plain g++ on PATH, so
  # naming one gets "gcc: could not execute: g++" out of --with-gcc.
  cxx = "${pkgsStatic.stdenv.cc.targetPrefix}g++";

  # GHC's boot terminfo registers `extra-libraries: tinfo`, but its library-dirs
  # only cover GHC's own tree, and GHC resolves a package's extra-libraries against
  # that package's registered library-dirs -- adding ncurses to the consuming
  # component's extra-lib-dirs does not help. A static by default GHC has to
  # resolve tinfo for real when running splices, so it falls back to
  # dlopen("libtinfo.so"), which a static ncurses does not provide:
  #   Error loading shared library libtinfo.so: No such file or directory
  #
  # haskell.nix already handles this for a Hackage-built terminfo --
  # configuration-nix.nix attaches ncurses to it, which puts the ncurses dir in
  # terminfo's own registration. So let terminfo be reinstalled rather than using
  # GHC's boot copy. haskeline has to come along: it is the only boot package
  # depending on terminfo, and leaving it non-reinstallable would leave the db
  # inconsistent. Nothing in the compiler's db depends on haskeline in turn.
  reinstallForTerminfo = [ "terminfo" "haskeline" ];
in

{
  # See reinstallForTerminfo above. Filtered rather than respelled so this stays in
  # sync with fix-ghc-pkgs-module.nix, which is regenerated per compiler version.
  nonReinstallablePkgs = lib.mkForce (
    lib.filter
      (p: !(lib.elem p reinstallForTerminfo))
      (import ./fix-ghc-pkgs-module.nix).nonReinstallablePkgs
  );

  packages = {
    # time-hourglass and hourglass are both in the plan and both export
    # hourglass_clock_calendar, which a static GHC's runtime linker rejects when it
    # loads both for a splice.
    time-hourglass.patches = [ ./time-hourglass-rename-c-symbol.patch ];

    # bitvec's simd flag dispatches through __builtin_cpu_supports, whose
    # __cpu_model lives in libgcc. The static GHC's TH loader has no libgcc to
    # resolve it against, so loading bitvec's objects dies with
    #   Failed to lookup symbol: __cpu_model
    # module-windows.nix turns the same flag off for the same reason.
    bitvec.components.library.configureFlags = [ "-f -simd" ];

    sauron.components.exes.sauron = {
      configureFlags = [
        ''--ghc-options="-pgml ${cxx}"''
      ];
      dontStrip = false;

      enableShared = false;
      libs = [];
      build-tools = [ pkgsStatic.gcc ];
    };
  };
}
