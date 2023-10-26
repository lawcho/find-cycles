{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.05";
  outputs = { self, nixpkgs }:
    let
      system = "x86_64-linux";
      ghc_version = "ghc927";
      # ghc_version = "ghc928"; # Tested 26 Oct 2023, builds fast
      # ghc_version = "ghc927"; # Tested 26 Oct 2023, builds slowly (120 dependencies from source)
    in
    {
      # Default behaviour of 'nix build' is like 'cabal build'
      # (but with haskell libraries from the nixpkgs snapshot)
      packages.${system}.default =
        nixpkgs.legacyPackages.${system}.haskell.packages.${ghc_version}.developPackage {
          root = ./.;
        };
    };
}
