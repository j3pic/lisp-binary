It's not a good idea to run the `run-tests` script locally. It assumes it's okay to do things like `rm -rf ~/quicklisp`, 
which is quite okay to do in the Docker environment it was designed for.

Instead, evaluate `(load "basic-tests.lisp")` from your Lisp and then call `(lisp-binary-test::run-test)`.
