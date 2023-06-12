{ mkDerivation, appendful, autodocodec, autodocodec-yaml, base
, containers, criterion, genvalidity, genvalidity-containers
, genvalidity-criterion, genvalidity-sydtest
, genvalidity-sydtest-aeson, genvalidity-time, genvalidity-uuid
, lib, mtl, pretty-show, QuickCheck, random, safe-coloured-text
, sydtest, sydtest-discover, time, uuid
}:
mkDerivation {
  pname = "genvalidity-appendful";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    appendful base containers genvalidity genvalidity-containers
    genvalidity-time QuickCheck
  ];
  testHaskellDepends = [
    appendful autodocodec autodocodec-yaml base containers
    genvalidity-sydtest genvalidity-sydtest-aeson genvalidity-uuid mtl
    pretty-show QuickCheck random safe-coloured-text sydtest time uuid
  ];
  testToolDepends = [ sydtest-discover ];
  benchmarkHaskellDepends = [
    appendful base criterion genvalidity-criterion
  ];
  homepage = "https://github.com/NorfairKing/appendful#readme";
  license = lib.licenses.mit;
}
