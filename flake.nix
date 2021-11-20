{
  description = "Smart audiobook player organizer";

  inputs = {
    easy-hls = {
      url = "github:jkachmar/easy-hls-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    flake-utils = {
      url = "github:numtide/flake-utils";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, flake-utils, easy-hls }:
  {
    overlay = final: prev: {
      haskellPackages = prev.haskellPackages.override ( old: {
        overrides = final.lib.composeExtensions ( old.overrides or (_: _: {})) (f: p: {
          smabp = f.callPackage ./. {};
        });
      } );
    };
  }
    //
    flake-utils.lib.eachSystem ["x86_64-linux" "x86_64-darwin"] ( system:
      let
        pkgs = import nixpkgs { inherit system; overlays = [ self.overlay ]; };
        hp = pkgs.haskellPackages;
        hls = (easy-hls.withGhcs [ hp.ghc.version ] ).${system};
      in
      rec {

        packages = { inherit (hp) smabp; };
        defaultPackage = packages.smabp;

        apps.kobodl = {
          type = "app";
          program =
            let
              wrapped = with pkgs; runCommandNoCC "smabp" { nativeBuildInputs = [ makeWrapper ]; } ''
                mkdir -p $out/bin
                cp ${packages.smabp}/bin/smabp $out/bin
                wrapProgram $out/bin/smabp --prefix PATH : ${lib.makeBinPath [ffmpeg exiftool]}
                '';
            in "${wrapped}/bin/smabp";
        };

        defaultApp = apps.kobodl;

        devShell =
          hp.shellFor {
            packages = h: [h.smabp];
            withHoogle = true;
            buildInputs = with pkgs; [
              entr
              cabal-install
              hp.hlint
              stylish-haskell
              ghcid
              hls

              hp.graphmod
            ];
          };
        }
    );
}
