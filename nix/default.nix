rec {
  sources = import ./sources.nix;

  # A pinned version of nixpkgs, widely used and hopefully well cached.
  defaultNixpkgs = import sources.nixpkgs;

  pkgSetForSystem = system: args:
    defaultNixpkgs (args // { inherit system; });

  # `pkgSetForSystem` for the current system.
  pkgSet = pkgSetForSystem builtins.currentSystem;
}
