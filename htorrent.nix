{ mkDerivation, Cabal, bencoding, bytestring, stdenv, base, mtl,
  containers, semigroups, text, cabal-install
} : mkDerivation {
  pname = "htorrent";
  version = "0.0.1";
  src = ./.;
  buildTools = [
    cabal-install
  ];
  setupHaskellDepends = [ base Cabal ];
  libraryHaskellDepends = [
    base bytestring bencoding containers mtl text semigroups
  ];
  testHaskellDepends = [
    base
  ];
  homepage = "https://github.com/kendricktan/htorrent";
  description = "Torrent Client in Haskell";
  license = stdenv.lib.licenses.mit;
}
