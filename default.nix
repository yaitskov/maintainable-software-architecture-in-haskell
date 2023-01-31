{ system ? builtins.currentSystem or "x86_64-linux", ghc ? "ghc924",
  withProfiling ? false }:

let
  nix = import ./nix;

  pkgs = nix.pkgSetForSystem system {
    config = {
      allowBroken = true;
      allowUnfree = true;
    };
  };

  inherit (pkgs) lib;

  inherit (pkgs.haskell.lib)
    enableSharedExecutables
    disableLibraryProfiling disableExecutableProfiling
    enableLibraryProfiling enableExecutableProfiling ;

  controlProfiling = drv:
    if withProfiling then
      builtins.trace "profiling is ON"
        (enableLibraryProfiling (enableExecutableProfiling drv))
    else
      disableLibraryProfiling (disableExecutableProfiling drv);

  haskellPkgSetOverlay = pkgs.callPackage ./nix/haskell/overlay.nix {
    inherit (nix) sources;
  };

  project-name = "maintainable-software-architecture-in-haskell";

  sourceRegexes = [
    "^src.*$"
    "^app.*$"
    ("^" + project-name + ".*$")
    "^test.*$"
    "^.*\\.cabal$"
    "Build.hs"
    "Setup.hs"
    "^LICENSE$"
  ];

  overlay = _hfinal: _hprev: { ${project-name} = haskellPkgs.callCabal2nix project-name (lib.sourceByRegex ./. sourceRegexes) {}; };
  baseHaskellPkgs = pkgs.haskell.packages.${ghc};
  haskellOverlays = [ haskellPkgSetOverlay overlay ];
  haskellPkgs = baseHaskellPkgs.override (old: {
    overrides =
      builtins.foldl' pkgs.lib.composeExtensions (old.overrides or (_: _: { }))
      haskellOverlays;
  });

  haskellLanguageServer =
    pkgs.haskell.lib.overrideCabal haskellPkgs.haskell-language-server
    (_: { enableSharedExecutables = true; });

  shell = haskellPkgs.shellFor {
    packages = p: [ p.${project-name} ];

    # include any recommended tools
    nativeBuildInputs = [ haskellLanguageServer ] ++ (with pkgs; [
      cabal-install
      ghcid
      hlint
      niv
      llvm
      hpack
    ]);
  };

  # $regex-match-these-but-not-those = haskellPkgs.regex-match-these-but-not-those;
in {
  inherit haskellPkgs;
  inherit ghc;
  inherit pkgs;
  inherit shell;
  ${project-name} = haskellPkgs.${project-name};
  inherit haskellOverlays;
  inherit haskellLanguageServer;
}
