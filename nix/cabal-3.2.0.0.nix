{ nixpkgs }:
nixpkgs.stdenv.mkDerivation {
  name = "cabal-install";
  src = nixpkgs.fetchurl {
    url = "https://downloads.haskell.org/~cabal/cabal-install-3.2.0.0/cabal-install-3.2.0.0-x86_64-unknown-linux.tar.xz";
    sha256 = "1b1qqw4f8jj2ylk0jk7icfbrnh2z83dnm9lrxyq7rhv5237zgl9j";
  };
  nativeBuildInputs = [ nixpkgs.autoPatchelfHook ];
  buildInputs = [
    nixpkgs.glibc
    nixpkgs.zlib
  ];
  dontConfigure = true;
  dontBuild = true;
  unpackPhase = "true";
  #phases = ["installPhase" "patchPhase"];
  installPhase = ''
    mkdir -p $out/bin
    tar -xf $src
    cp cabal $out/bin/cabal
    chmod +x $out/bin/cabal
  '';
}
