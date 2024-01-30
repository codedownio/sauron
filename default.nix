{ mkDerivation, aeson, base, brick, containers, exceptions
, filepath, github, lib, microlens, microlens-th, mtl
, optparse-applicative, process, relude, safe-exceptions, stm
, string-interpolate, text, time, unliftio, unliftio-core, vector
, vty, vty-crossplatform, yaml
}:
mkDerivation {
  pname = "sauron";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base brick containers github microlens microlens-th mtl process
    relude stm string-interpolate text time unliftio unliftio-core
    vector vty vty-crossplatform
  ];
  executableHaskellDepends = [
    aeson base brick containers exceptions filepath github microlens
    mtl optparse-applicative process relude safe-exceptions stm
    string-interpolate text unliftio unliftio-core vector vty
    vty-crossplatform yaml
  ];
  testHaskellDepends = [
    base brick containers github microlens mtl process relude stm
    string-interpolate text unliftio unliftio-core vector vty
    vty-crossplatform
  ];
  homepage = "https://github.com/codedownio/sauron#readme";
  license = lib.licenses.bsd3;
  mainProgram = "sauron";
}
