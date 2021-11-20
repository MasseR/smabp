{ nixpkgs ? import <nixpkgs> {} }:

with nixpkgs;

let
  easy-hls-src = fetchFromGitHub {
    owner = "ssbothwell";
    repo = "easy-hls-nix";
    inherit (builtins.fromJSON (builtins.readFile ./easy-hls-nix.json)) rev sha256;
  };
  easy-hls = callPackage easy-hls-src { ghcVersions = [ hp.ghc.version ]; };
  hp = haskellPackages.extend (self: super: {
    reddit_pub = self.callPackage ./. {};
  });

in

hp.shellFor {
  packages = h: [h.reddit_pub];
  withHoogle = true;
  buildInputs = [
    ffmpeg
    exiftool
    entr
    cabal-install
    haskellPackages.hlint
    stylish-haskell
    ghcid
    easy-hls

    sqlite-interactive
    rrdtool

    jq

    haskellPackages.graphmod
  ];
}

