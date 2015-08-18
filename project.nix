{ mkDerivation, base, containers, filepath, haskonf, mtl, stdenv
, text, time, turtle
}:
mkDerivation {
  pname = "blog";
  version = "0.0.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  buildDepends = [
    base containers filepath haskonf mtl text time turtle
  ];
  description = "CLI to instantiate/publish hakyll posts";
  license = stdenv.lib.licenses.bsd2;
}
