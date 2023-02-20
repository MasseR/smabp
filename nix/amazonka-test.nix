{ mkDerivation, aeson, amazonka-core, base, bifunctors, bytestring
, case-insensitive, conduit, conduit-extra, fetchgit, groom
, http-client, http-types, lib, process, resourcet, tasty
, tasty-hunit, template-haskell, temporary, text, time
, unordered-containers, yaml
}:
mkDerivation {
  pname = "amazonka-test";
  version = "2.0";
  src = fetchgit {
    url = "https://github.com/brendanhay/amazonka.git";
    sha256 = "1wq0wyk6mgrcx3jr6js7s1y80bz8d8sv81p1b4k5wjwf17yv6rk6";
    rev = "f610f8c95e190edf86606c5f86485d198671beb0";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/lib/amazonka-test; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    aeson amazonka-core base bifunctors bytestring case-insensitive
    conduit conduit-extra groom http-client http-types process
    resourcet tasty tasty-hunit template-haskell temporary text time
    unordered-containers yaml
  ];
  homepage = "https://github.com/brendanhay/amazonka";
  description = "Common functionality for Amazonka library test-suites";
  license = lib.licenses.mpl20;
}