{
  description = "flake-parts based dev environment for xeus-haskell";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.05";
    flake-parts.url = "github:hercules-ci/flake-parts";
  };

  outputs = { self, nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit self nixpkgs; } {
      systems = [ "x86_64-linux" "aarch64-linux" ];

      perSystem = { pkgs, system, ... }: let
        pythonDeps = with pkgs.python3Packages; [
          pytest
          jupyter_kernel_test
          nbval
          pytest-rerunfailures
        ];

        haskellDeps = with pkgs.haskellPackages; [
          ghc
          cabal-install
          hlint
          hspec
        ];

        buildDeps = with pkgs; [
          make
          cmake
          gcc
        ];

        hostDeps = with pkgs; [
          xeus-zmq
          nlohmann_json
          cppzmq
        ];

      in {
        devShells.default = pkgs.mkShell {
          name = "xeus-haskell-dev";

          buildInputs =
            buildDeps
            ++ hostDeps
            ++ haskellDeps
            ++ pythonDeps
            ++ [ pkgs.micromamba ];

          shellHook = ''
            echo "🚀 Welcome to the xeus-haskell dev environment"
            echo "System: ${system}"
            export CC=${pkgs.stdenv.cc.targetPrefix}cc
            export CXX=${pkgs.stdenv.cc.targetPrefix}c++
          '';
        };
      };
    };
}
