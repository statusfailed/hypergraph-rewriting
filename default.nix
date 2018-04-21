{ mkDerivation, array, base, bimap, containers, exceptions, lens
, logict, miso, mtl, stdenv, tasty, tasty-hunit, text, vector
}:
mkDerivation {
  pname = "hypergraph-rewriting";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    array base bimap containers exceptions lens logict miso mtl text
    vector
  ];
  testHaskellDepends = [
    base bimap containers logict tasty tasty-hunit text
  ];
  license = stdenv.lib.licenses.mit;
}
