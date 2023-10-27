{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.05";
  outputs = { self, nixpkgs }:
    let
      system = "x86_64-linux";
      all_ghc_versions = nixpkgs.lib.attrNames nixpkgs.legacyPackages.${system}.haskell.packages;
    in
    {
      packages.${system} = (
        # Build the an output for each version of GHC in the nixpkgs snapshot
        nixpkgs.lib.attrsets.genAttrs all_ghc_versions (ghc_version:
          # Use 'cabal build' to compile the Haskell
          # (but with GHC & haskell libraries from the nixpkgs snapshot)
          nixpkgs.legacyPackages.${system}.haskell.packages.${ghc_version}.developPackage {
            root = ./.;
          }
        )
      );
    };
}
