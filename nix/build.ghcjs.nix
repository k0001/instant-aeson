# this file can be used with nix-build

with (import <nixpkgs> {}).pkgs;
with (import <nixpkgs/pkgs/development/haskell-modules/lib.nix> { inherit pkgs; });

let hs = haskell-ng.packages.ghcjs.override {
    overrides = self: super: {
      instant-aeson = self.callPackage ./default.nix {};
      instant-generics = haskell-ng.lib.overrideCabal super.instant-generics (drv: {
        src = fetchFromGitHub {
            owner = "k0001";
            repo = "instant-generics";
            rev = "3f46e7da4667bcdc0ed637c8141ace4330076862";
            sha256 = "0h79xlws17cgcc4zwh9z95wscxa57gy9hc48mz7b7j1vipyzx1i5";
        };
      });
    };
  };
in hs.instant-aeson
