{ seaaye-spec = 1;
  package-name = "hxtools";
  targets =
  {
    hackage-8-10 = {
      resolver = "hackage";
      index-state = "2021-07-01T00:00:00Z";
      ghc-ver = "ghc8107";
    };
    hackage-9-01 = {
      resolver = "hackage";
      index-state = "2021-07-01T00:00:00Z";
      ghc-ver = "ghc901";
      enabled = false;
    };
    hackage-9-02 = {
      resolver = "hackage";
      index-state = "2021-11-01T00:00:00Z";
      ghc-ver = "ghc921";
      enabled = false;
    };
  };
  module-flags = [
    # N.B.: There are haskell-nix module options. See the haskell-nix docs
    #       for details. Also, be careful about typos: In many cases you
    #       will not get errors but the typo'd flag will just not have any
    #       effect!
    # { packages.my-package.flags.my-package-examples-examples = true; }
  ];
  default-target = "hackage-8-10";
  do-check-hackage = "hackage.haskell.org";
  do-check-changelog = "changelog.md";
  # local-config-path = ./nix/local-config.nix;
}
