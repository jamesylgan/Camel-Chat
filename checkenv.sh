#!/bin/bash

environment=good

OPAM_LOCATION="$(command -v opam)"
if [[ "$OPAM_LOCATION" == "" ]]; then
  echo "OPAM is NOT available.  This is bad."
  environment=bad
else
  echo "OPAM is available.  Good."
fi

OCAMLC_VERSION="$(ocamlc --version 2>&1)"
if [[ "$OCAMLC_VERSION" == "4.05.0" ]]; then
  echo "OCaml compiler version 4.05.0 is active.  Good."
else
  echo "OCaml compiler version 4.05.0 is NOT active.  This is bad."
  environment=bad
fi

OUNIT_VERSION="$(opam info ounit -f installed-version 2>&1)"
if [[ "$OUNIT_VERSION" =~ "2.0.0" && "$OUNIT_VERSION" =~ "4.05.0" ]]; then
  echo "OUnit version 2.0.0 is active.  Good."
else
  echo "OUnit version 2.0.0 is NOT active.  This is bad."
  environment=bad
fi

CORE_VERSION="$(opam info core -f installed-version 2>&1)"
if [[ "$CORE_VERSION" =~ "0.9.1" && "$CORE_VERSION" =~ "4.05.0" ]]; then
  echo "Core version 0.9.1 is active.  Good."
else
  echo "Core version 0.9.1 is NOT active.  This is bad."
  environment=bad
fi

ASYNC_VERSION="$(opam info async -f installed-version 2>&1)"
if [[ "$ASYNC_VERSION" =~ "0.9.0" && "$ASYNC_VERSION" =~ "4.05.0" ]]; then
  echo "Async version 0.9.0 is active.  Good."
else
  echo "Async version 0.9.0 is NOT active.  This is bad."
  environment=bad
fi

ANSITERMINAL_VERSION="$(opam info ansiterminal -f installed-version 2>&1)"
if [[ "$ANSITERMINAL_VERSION" =~ "0.7" && "$ANSITERMINAL_VERSION" =~ "4.05.0" ]]; then
  echo "ANSITerminal version 0.7 is active.  Good."
else
  echo "ANSITerminal version 0.7 is NOT active.  This is bad."
  environment=bad
fi

if [[ "$environment" == good ]]; then
  cat <<EOF
===========================================================
Your OCaml environment looks good to me.  Congratulations!
===========================================================
EOF
else
  cat <<EOF
===========================================================
WARNING

Your OCaml environment looks broken to me.  The code that
you submit might not compile on the grader's machine,
leading to heavy penalties.  Please fix your OCaml
environment. Check the error messages above carefully to
determine what is wrong with your environment.  See a
consultant for help if you cannot determine what is wrong.
===========================================================
EOF
fi
