{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/032bc6539bd5f14e9d0c51bd79cfe9a055b094c3";
    rust-overlay = {
      url = "github:oxalica/rust-overlay/ebc7823c3ffde594c7733113042b72694d996de9";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, rust-overlay }:
    let
      mkInputs = system: {
        pkgs = nixpkgs.legacyPackages.${system}.extend (rust-overlay.overlays.default);
      };
      forAllSystems = fn:
        with nixpkgs.lib; attrsets.genAttrs systems.flakeExposed (system: fn (mkInputs system));
    in
    {
      devShells = forAllSystems (inputs: with inputs; {
        default = pkgs.mkShellNoCC {
          packages = [
            (pkgs.rust-bin.stable.latest.default.override {
              extensions = [ "rust-src" ];
            })
          ];
        };
      });
    };
}
