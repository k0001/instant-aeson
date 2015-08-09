{ mkDerivation
, stdenv

  # haskell deps
, base
, aeson
, instant-generics
, tasty
, tasty-quickcheck
}:

mkDerivation {
  pname = "instant-aeson";
  version = "0.2";
  homepage = "https://github.com/k0001/instant-aeson";
  description = "Generic Aeson instances through instant-generics";
  license = stdenv.lib.licenses.bsd3;
  src = ../.;
  isLibrary = true;
  isExecutable = false;
  doHaddock = true;
  buildDepends = [base aeson instant-generics tasty tasty-quickcheck];
}
