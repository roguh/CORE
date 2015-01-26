{ pkgs ? (import <nixpkgs> {})
, haskellPackages ? pkgs.haskellPackages_ghc763
}:
let
  hsEnv = haskellPackages.ghcWithPackages (hsPkgs : (with hsPkgs ; [
    parsec
    text
    haskeline
  ]));
in
haskellPackages.cabal.mkDerivation (self: {
  pname = "core";
  version = "0.1.0.0";
  isLibrary = false;
  isExecutable = true;
  src = ./.;
  buildDepends = [
    hsEnv ];
  extraCmds = ''
    $(grep export ${hsEnv.outPath}/bin/ghc)
  '';
  meta = {
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
