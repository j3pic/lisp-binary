It's not a good idea to run the `run-tests` script locally. It assumes it's okay to do things like `rm -rf ~/quicklisp`, 
which is quite okay to do in the Docker environment it was designed for.

Instead, evaluate this:

```
(push '*default-pathname-defaults* asdf:*central-registry*)
(ql:quickload :lisp-binary-test)
(lisp-binary-test::run-test)
```

Make sure your Lisp's `*default-pathname-defaults*` points to the test directory,
or else try using a variable that does point there.
