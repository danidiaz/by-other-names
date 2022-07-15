{ ps ? import <nixpkgs> {} }:
  ps.mkShell {
    buildInputs = [(ps.haskellPackages.ghcWithPackages (ps: [ps.aeson])) ps.cabal-install ps.haskell-language-server];
}
