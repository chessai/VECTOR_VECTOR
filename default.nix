{ mkDerivation, base, stdenv }:
mkDerivation {
  pname = "vector";
  version = "0.0.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base ];
  executableHaskellDepends = [ base ];
  jailbreak = true;
  description = "Dependently-typed linked list implementation";
  license = stdenv.lib.licenses.mit;
}
