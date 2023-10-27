{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.05";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  outputs = { self, nixpkgs , flake-utils }:
    (flake-utils.lib.eachDefaultSystem (system:
    let all_ghc_versions = nixpkgs.lib.attrNames nixpkgs.legacyPackages.${system}.haskell.packages; in
    {
      # Build an output for each version of GHC in the nixpkgs snapshot
      packages = nixpkgs.lib.attrsets.genAttrs all_ghc_versions (ghc_version:
        # Use 'cabal build' to compile the Haskell
        # (but with GHC & haskell libraries from the nixpkgs snapshot)
        nixpkgs.legacyPackages.${system}.haskell.packages.${ghc_version}.developPackage {
          root = ./.;
        });
    }));
}
