{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.05";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  outputs = { self, nixpkgs, flake-utils }:
    (flake-utils.lib.eachDefaultSystem (system:
    let haskell_packages = nixpkgs.legacyPackages.${system}.haskell.packages; in
    {
      # Build an output for each version of GHC in the nixpkgs snapshot
      packages = nixpkgs.lib.attrsets.genAttrs (nixpkgs.lib.attrNames haskell_packages) (ghc_version:
        # Use 'cabal build' to compile the Haskell
        # (but with GHC & Haskell libraries from the nixpkgs snapshot)
        haskell_packages.${ghc_version}.developPackage {
          root = ./.;
        });
    }));
}
