{ ghcVer ? "ghc8104" }:
let sources = import ./sources.nix;

    overlay = final: prev: let
      inherit (prev.haskell.lib) overrideCabal;
      inherit (prev.nix-gitignore) gitignoreSource;
      in {
        haskell = prev.haskell // {
          packages = prev.haskell.packages // {
            "${ghcVer}" = prev.haskell.packages."${ghcVer}".override {
              overrides = hfinal: hprev: {
                keyboard-layout-files-creator = overrideCabal
                  (hprev.callCabal2nix "keyboard-layout-files-creator" (gitignoreSource [] ../.) {})
                  (old: {
                    buildTools = [ final.makeWrapper ];
                    postInstall = ''
                      # small hack to ensure locale works properly with no
                      # environment changes on non-nixos distros

                      wrapProgram $out/bin/klfc --set LOCALE_ARCHIVE ${final.glibcLocales}/lib/locale/locale-archive
                    '';
                  });
              };
            };
          };
        };
    };
in
  import sources.nixpkgs {
    overlays = [ overlay ];
    config = { allowBroken = true; };
  }
