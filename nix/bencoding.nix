let
  initialNixpkgs = import <nixpkgs> {};

  sources = rec {
    bencoding-pinned = initialNixpkgs.pkgs.lib.importJSON ./bencoding.json;
    bencoding = initialNixpkgs.pkgs.fetchFromGitHub {
      owner = "sergv";
      repo = "bencoding";
      inherit (bencoding-pinned) rev sha256;
    };
  };
in
  sources.bencoding
