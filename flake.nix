{
  description = "Solving programming problems from different sources in different languages.";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }: flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = import nixpkgs { inherit system; };
    in
    {
      devShells.haskell = pkgs.mkShell
        rec {
          name = "haskell-dev";

          inputsFrom = [ (pkgs.haskellPackages.callCabal2nix "programming" ./. { }).env ];

          buildInputs = with pkgs.haskellPackages; [
            ghc
            cabal-install
          ];
        };
    }
  );
}
