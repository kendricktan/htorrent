pkgs: (self: super:

let
  sources = {
    bencoding = import ./nix/bencoding.nix;
  };

in
{
  bencoding = self.callCabal2nix "bencoding" sources.bencoding {};
})
