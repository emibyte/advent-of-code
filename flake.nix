{
  description = "ocaml flake";

  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

  outputs = { self, nixpkgs }:
    let
      supportedSystems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
      forEachSupportedSystem = f: nixpkgs.lib.genAttrs supportedSystems (system: f {
        pkgs = import nixpkgs { inherit system; };
      });
    in
    {
      devShells = forEachSupportedSystem ({ pkgs }: {
        default = pkgs.mkShell {
          packages = with pkgs; [
            ocamlPackages.ocaml
            ocamlPackages.dune_3
            ocamlPackages.ppx_inline_test
            ocamlPackages.ocp-indent

            ocamlPackages.findlib
            ocamlPackages.utop
            ocamlPackages.odoc
            ocamlPackages.ocaml-lsp
            ocamlformat

            ocamlPackages.base
            ocamlPackages.core
            ocamlPackages.janeStreet.stdio
          ];
        };
      });
    };
}
