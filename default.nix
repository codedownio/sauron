{ mkDerivation, base, brick, containers, github, lib, microlens
, microlens-th, stm, string-interpolate, text, time, unliftio
, unliftio-core, vector, vty
}:
mkDerivation {
  pname = "sauron";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base brick containers github microlens microlens-th stm
    string-interpolate text time unliftio unliftio-core vector vty
  ];
  executableHaskellDepends = [
    base brick containers github microlens stm string-interpolate text
    unliftio unliftio-core vty
  ];
  testHaskellDepends = [
    base brick containers github microlens stm string-interpolate text
    unliftio unliftio-core vty
  ];
  homepage = "https://github.com/codedownio/sauron#readme";
  license = lib.licenses.bsd3;
  mainProgram = "sauron";
}
