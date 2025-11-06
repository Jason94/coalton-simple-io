# Coalton-Simple-Io - Simple IO monad for Coalton.

## Example Usage

```lisp
  (declare sum-file (IO Integer))
  (define sum-file
    (do
     (write-line "Writing data file...")
     write-data-file
     (write-line "Done writing file...")
     (input-chan <- mv:new-empty-chan)
     (ints-chan <- mv:new-empty-chan)
     (sum-mvar <- mv:new-empty-mvar)
     (write-line "Forking threads...")
     (fork (reader-thread input-chan))
     (do-loop-times (_ n-workers)
       (fork (parser-thread input-chan ints-chan)))
     (fork (summer-thread ints-chan sum-mvar))
     (write-line "Waiting for sum...")
     (sum <- (mv:take-mvar sum-mvar))
     (write-line (<> "Calculated sum: " (into sum)))
     (pure sum)))
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
