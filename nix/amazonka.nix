{ mkDerivation, aeson, amazonka-core, amazonka-sso, amazonka-sts
, base, bytestring, conduit, directory, exceptions, fetchgit
, http-client, http-conduit, http-types, ini, lens, lib, resourcet
, retry, text, time, transformers, unordered-containers, uuid
}:
mkDerivation {
  pname = "amazonka";
  version = "2.0";
  src = fetchgit {
    url = "https://github.com/brendanhay/amazonka.git";
    sha256 = "1wq0wyk6mgrcx3jr6js7s1y80bz8d8sv81p1b4k5wjwf17yv6rk6";
    rev = "f610f8c95e190edf86606c5f86485d198671beb0";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/lib/amazonka; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    aeson amazonka-core amazonka-sso amazonka-sts base bytestring
    conduit directory exceptions http-client http-conduit http-types
    ini lens resourcet retry text time transformers
    unordered-containers uuid
  ];
  homepage = "https://github.com/brendanhay/amazonka";
  description = "Comprehensive Amazon Web Services SDK";
  license = lib.licenses.mpl20;
}
