{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.05";
  outputs = { self, nixpkgs }:
    let system = "x86_64-linux";
    in
    {
      # Default behaviour of 'nix build' is like 'cabal build'
      # (but with haskell libraries from the nixpkgs snapshot)
      packages.${system}.default =
        nixpkgs.legacyPackages.${system}.haskellPackages.developPackage {
          root = ./.;
        };
    };
}
