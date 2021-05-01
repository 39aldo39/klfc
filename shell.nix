{ ghcVer ? "ghc8104" }:
let
  nixpkgs = import ./nix/nixpkgs.nix { inherit ghcVer; };
in
  nixpkgs.haskell.packages."${ghcVer}".shellFor {
    packages = p: with p; [
      keyboard-layout-files-creator
    ];

    nativeBuildInputs = with nixpkgs.haskell.packages."${ghcVer}"; [
      cabal-install
      Cabal
      haskell-language-server
    ] ++ (with nixpkgs; [ cabal2nix niv glibcLocales ]);
    withHoogle = true;
  }
