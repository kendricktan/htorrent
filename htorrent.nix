{ mkDerivation, Cabal, cryptohash, binary, bencoding, bytestring, stdenv, base, mtl,
  containers, network, random, semigroups, text, network-uri, cabal-install
} : mkDerivation {
  pname = "htorrent";
  version = "0.0.1";
  src = ./.;
  buildTools = [
    cabal-install
  ];
  setupHaskellDepends = [ base Cabal ];
  libraryHaskellDepends = [
    base binary bytestring bencoding containers mtl network network-uri
    text random semigroups
  ];
  testHaskellDepends = [
    base
  ];
  homepage = "https://github.com/kendricktan/htorrent";
  description = "Torrent Client in Haskell";
  license = stdenv.lib.licenses.mit;
}
