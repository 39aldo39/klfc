{ ghcVer ? "ghc8104" }:
let nixpkgs = import ./nix/nixpkgs.nix { inherit ghcVer; };
in
  nixpkgs.haskell.packages."${ghcVer}".keyboard-layout-files-creator
