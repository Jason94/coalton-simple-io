# Coalton-Simple-Io - Simple IO monad for Coalton.

## Usage

```lisp
(coalton-toplevel
  (declare greet (String -> IO Unit))
  (define (greet name)
    (write-line (<> "Hello, " name)))
    
  (declare sleep (Integer -> IO Unit))
  (define (sleep s)
    (wrap-io
          (lisp :a (s)
            (cl:sleep s))
          Unit)))
          
(coalton
  (run!
    (do
      (sleep 1)
      (greet "Lisp"))))
```

## Installation

`coalton-simple-io` is not on Quicklisp, but you can easily install it by checing it out to your `local-projects` directory:

```bash
git clone https://github.com/Jason94/coalton-simple-io.git ~/quicklisp/local-projects/coalton-simple-io
```

and then: (1) in your project's `.asd` file, list `coalton-simple-io` as a requirement, or (2) execute in your REPL:
```lisp
(ql:quickload "coalton-simple-io")
```

## Author

* Jason Walker (Jason0@pm.me)

## Copyright

Copyright (c) 2025 Jason Walker (Jason0@pm.me)

## License

Licensed under the MIT License.
