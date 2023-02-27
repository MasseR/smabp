{ mkDerivation, aeson, amazonka, amazonka-s3, base, bytestring
, conduit, containers, cryptonite, dhall, directory, either
, filepath, generic-lens, hashable, hedgehog, hspec, hspec-hedgehog
, lens, lib, mtl, optparse-generic, temporary, text, transformers
, typed-process, unordered-containers
}:
mkDerivation {
  pname = "smabp";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    amazonka amazonka-s3 base bytestring conduit containers directory
    filepath generic-lens lens mtl optparse-generic temporary text
    typed-process unordered-containers
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    aeson amazonka amazonka-s3 base bytestring conduit containers
    cryptonite dhall either filepath generic-lens hashable hedgehog
    hspec hspec-hedgehog lens temporary text transformers
    unordered-containers
  ];
  license = "unknown";
  mainProgram = "smabp";
}
