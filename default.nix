{ mkDerivation, amazonka, amazonka-s3, base, bytestring, containers
, directory, filepath, lens, lib, mtl, optparse-generic, temporary
, text, typed-process
}:
mkDerivation {
  pname = "smabp";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    amazonka amazonka-s3 base bytestring containers directory filepath
    lens mtl optparse-generic temporary text typed-process
  ];
  executableHaskellDepends = [ base ];
  license = "unknown";
  mainProgram = "smabp";
}
