#!/bin/sh -eu

case "$CL_IMPL" in
	sbcl)
		sbcl --script main.lisp;;
	ecl-interp)
		ecl --shell main.lisp;;
	ecl-compile)
		ecl --compile main.lisp >/dev/null
		ecl --shell main.fas
		rm main.fas;;
	ccl)
		ccl -l main.lisp --eval '(ccl:quit)';;
esac
