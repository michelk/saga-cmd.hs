{ mkDerivation, base, cmdargs, containers, directory, filepath
, process, stdenv, text, unix
}:
mkDerivation {
  pname = "bindings-saga-cmd";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  buildDepends = [
    base cmdargs containers directory filepath process text unix
  ];
  homepage = "https://github.com/michelk/bindings-saga-cmd.hs";
  description = "Wrapping saga_cmd";
  license = stdenv.lib.licenses.gpl3;
}
