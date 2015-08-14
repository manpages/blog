{ mkDerivation, base, haskonf, stdenv, turtle }:
mkDerivation {
  pname = "blog";
  version = "0.0.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  buildDepends = [ base haskonf turtle ];
  description = "Reflective configuration library";
  license = stdenv.lib.licenses.bsd2;
}
