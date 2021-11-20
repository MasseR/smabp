{ mkDerivation, base, bytestring, lens, lib, mtl, optparse-generic
, text, typed-process
}:
mkDerivation {
  pname = "smabp";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring lens mtl optparse-generic text typed-process
  ];
  executableHaskellDepends = [ base ];
  license = "unknown";
  hydraPlatforms = lib.platforms.none;
}
