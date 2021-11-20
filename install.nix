with (import <nixpkgs> {});

let
  smabp_raw = haskellPackages.callPackage ./. {};
  smabp = runCommandNoCC "smabp" { nativeBuildInputs = [ makeWrapper ]; } ''
    mkdir -p $out/bin
    cp ${smabp_raw}/bin/smabp $out/bin
    wrapProgram $out/bin/smabp --prefix PATH : ${lib.makeBinPath [ffmpeg exiftool]}
  '';

in

{ smabp = smabp; }
