let
  haskellNixSrc = builtins.fetchTarball
      https://github.com/input-output-hk/haskell.nix/archive/e3933cbb701e5bc61c18f620a4fd43c55f5c026e.tar.gz;
  haskellNix = import haskellNixSrc { version = 2; };
  nixpkgsSrc = haskellNix.sources.nixpkgs-2105;
in
{ nixpkgs ? import nixpkgsSrc haskellNix.nixpkgsArgs
, ghc-ver ? "ghc8107"
, index-state ? "2021-07-01T00:00:00Z"
, index-sha256 ? null
, plan-sha256 ? null
, materialized ? null
}:
with nixpkgs;
let
  gitignoreSrc = nixpkgs.fetchFromGitHub {
    owner = "hercules-ci";
    repo = "gitignore.nix";
    rev = "c4662e662462e7bf3c2a968483478a665d00e717";
    sha256 = "sha256:1npnx0h6bd0d7ql93ka7azhj40zgjp815fw2r6smg8ch9p7mzdlx";
  };
  inherit (import gitignoreSrc { inherit (nixpkgs) lib; }) gitignoreFilter;
  cleanedSource = nixpkgs.lib.cleanSourceWith {
    name = "hxtools";
    src = ./.;
    filter = p: t:
      let baseName = baseNameOf (toString p);
      in gitignoreFilter ./../.. p t
      && baseName != ".gitignore"
      && baseName != "nix"
      && baseName != "ci-out"
      && (builtins.match ".*\.nix" baseName == null);
  };
  sdist = nixpkgs.stdenvNoCC.mkDerivation {
    name = "hxtools" + "-sdist";
    src = cleanedSource;
    buildInputs = [ nixpkgs.bash nixpkgs.cabal-install ];
    phases = [ "unpackPhase" "buildPhase" ];
    buildPhase = ''
      mkdir -p $out
      cabal sdist -o $out
    '';
  };
  sdist-unpacked = nixpkgs.stdenvNoCC.mkDerivation {
    name = "hxtools" + "-sdist-unpacked";
    src = cleanedSource;
    buildInputs = [
      nixpkgs.bash
      nixpkgs.gnutar
    ];
    phases = [ "buildPhase" ];
    buildPhase = ''
      mkdir -p "$out"
      tar -xz -f "${sdist}"/*.tar.gz --strip-components=1 -C "$out"
      for f in "$src"/stack*.yaml; do cp "$f" "$out"; done
    '';
  };
  package-nix = nixpkgs.haskell-nix.callCabalProjectToNix {
    src = sdist-unpacked;
    inherit index-state index-sha256 plan-sha256 materialized;
    compiler-nix-name = ghc-ver;
  };
  package-plan = nixpkgs.haskell-nix.importAndFilterProject { inherit (package-nix) projectNix sourceRepos src; };
  hsPkgs =
    let pkg-set = nixpkgs.haskell-nix.mkCabalProjectPkgSet
              { plan-pkgs = package-plan;
                modules = [
                  { ghc.package = nixpkgs.haskell-nix.compiler.${ghc-ver}; }
                  { packages.hxtools.src = nixpkgs.haskell-nix.cleanSourceHaskell { src = cleanedSource; }; }
                ];
              };
    in pkg-set.config.hsPkgs;
in hsPkgs.hxtools.components.exes

