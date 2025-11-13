# `coalton-io` - Functional Applications in Coalton

_coalton-io_ provides tools to write pure, functional programs in Coalton that can perform necessary tasks like:
* Mutable data
* Random numbers
* Terminal & File IO
* Multithreading
* Safely sharing data between threads

_coalton-io_ also provides all of this functionality for free if you want to write your own underlying effect type.

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


`coalton-io` depends on a later version of Coalton than the current Quicklisp release (as of 11/13/2025). You can easily install it by checking it out to your `local-projects` directory:

```bash
git clone https://github.com/coalton-lang/coalton.git ~/quicklisp/local-projects/coalton
```

Once you have the latest version of Coalton, you can install `coalton-io` from [Ultralisp](https://ultralisp.org/). See the Ultralisp website for setup instructions. Once the "ultralisp" distribution is set up, simply install it with:

```lisp
(ql:quickload "coalton-io")
```

## Feature Breakdown

_coalton-io_ provides the following features in these packages:

* `io/term`   - Read/write to the Console
* `io/file`   - Read/write to files and interact with the file system
* `io/random` - Generate random numbers
* `io/mut`    - Use unsynchronized (non-thread safe) mutable variables in pure code
* `io/unique` - Generate guaranteed unique values (thread safe)
* `io/thread` - Fork new threads which run their own `IO` operations
* `io/atomic` - Atomic mutable variables for sharing state across threads
* `io/mvar`   - Provides `MVar`s (synchronized single-value mutable stores to hand off data between threads) and `MChan`s (thread safe FIFO queues to stream data between threads)
* `io/future` - Provides Futures, with standard `await` semantics

If you just want to use `IO` to write an application, use `io/simple-io` to get the standard `IO` type.

If you want to write _your own_ effect type, use `io/monad-io` and `io/io-all` to cover your own type with all of the features in the list above.

## Examples

_coalton-io_ has two example programs to demonstrate how to use the `IO` type:

* [Hangman](examples/hangman.lisp) - Play a game of hangman in the terminal. Shows `IO` basics and terminal IO.
* [Channels & Threading](examples/channels-threading.lisp) - Multithreaded application to process an input data file. Shows how to mix different `IO` effects, multithreading, and passing data safely between threads.
