{
  description = "Smart audiobook player organizer";

  inputs = {
    nixpkgs = { url = "github:NixOS/nixpkgs/nixos-unstable"; };
    flake-utils = {
      url = "github:numtide/flake-utils";
    };
  };

  outputs = { self, nixpkgs, flake-utils }:
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
        pkgs = nixpkgs.legacyPackages.${system};
        hp = pkgs.haskellPackages.override ( old: {
        overrides = pkgs.lib.composeExtensions ( old.overrides or (_: _: {})) (f: p: {
          smabp = f.callPackage ./. {};

          # Need to update amazonka...
          amazonka-core = f.callPackage nix/amazonka-core.nix {};
          amazonka-s3 = f.callPackage nix/amazonka-s3.nix {};
          amazonka-test = f.callPackage nix/amazonka-test.nix {};
          amazonka = f.callPackage nix/amazonka.nix {};
          amazonka-sso = f.callPackage nix/amazonka-sso.nix {};
          amazonka-sts = f.callPackage nix/amazonka-sts.nix {};
        });
      } );
      in
      rec {

        packages = { inherit (hp) smabp; };
        defaultPackage = packages.smabp;

        apps.smabp = let
              wrapped = with pkgs; runCommandNoCC "smabp" { nativeBuildInputs = [ makeWrapper ]; } ''
                mkdir -p $out/bin
                cp ${packages.smabp}/bin/smabp $out/bin
                wrapProgram $out/bin/smabp --prefix PATH : ${lib.makeBinPath [ffmpeg exiftool]}
                '';
        in
        flake-utils.lib.mkApp { drv = wrapped; };

        apps.default = apps.smabp;

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
              hp.haskell-language-server

              hp.graphmod

              sops

              ffmpeg
              exiftool
            ];
          };
        }
    );
}
