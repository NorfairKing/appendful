{ mkDerivation, aeson, autodocodec, base, containers, deepseq, lib
, mtl, validity, validity-containers
}:
mkDerivation {
  pname = "appendful";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson autodocodec base containers deepseq mtl validity
    validity-containers
  ];
  homepage = "https://github.com/NorfairKing/appendful#readme";
  license = lib.licenses.mit;
}
