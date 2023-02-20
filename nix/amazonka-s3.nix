{ mkDerivation, amazonka-core, amazonka-test, base, bytestring
, case-insensitive, fetchgit, lens, lib, tasty, tasty-hunit, text
, time, unordered-containers
}:
mkDerivation {
  pname = "amazonka-s3";
  version = "2.0";
  src = fetchgit {
    url = "https://github.com/brendanhay/amazonka.git";
    sha256 = "1wq0wyk6mgrcx3jr6js7s1y80bz8d8sv81p1b4k5wjwf17yv6rk6";
    rev = "f610f8c95e190edf86606c5f86485d198671beb0";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/lib/services/amazonka-s3; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [ amazonka-core base lens text ];
  testHaskellDepends = [
    amazonka-core amazonka-test base bytestring case-insensitive tasty
    tasty-hunit text time unordered-containers
  ];
  homepage = "https://github.com/brendanhay/amazonka";
  description = "Amazon Simple Storage Service SDK";
  license = lib.licenses.mpl20;
}
