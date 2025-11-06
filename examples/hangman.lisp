(cl:in-package :cl-user)
(defpackage :simple-io/examples/hangman
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/monad/identity
   #:coalton-library/monad/statet
   #:coalton-library/monad/environment
   #:coalton-library/experimental/do-control-core
   #:coalton-library/experimental/do-control-loops
   #:simple-io/io
   #:simple-io/term)
  (:local-nicknames
   (:lp #:coalton-library/experimental/do-control-loops-adv)
   (:itr #:coalton-library/iterator)
   (:l #:coalton-library/list)
   (:tp #:coalton-library/tuple)
   (:s #:coalton-library/string)))

(in-package :simple-io/examples/hangman)

(named-readtables:in-readtable coalton:coalton)

;;
;; Helper Functions
;;

(coalton-toplevel
  (declare contains? (Eq :a => :a -> List :a -> Boolean))
  (define (contains? elt lst)
    (match lst
      ((Nil) False)
      ((Cons x rst)
       (if (== x elt)
           True
           (contains? elt rst)))))

  (declare str-contains? (Char -> String -> Boolean))
  (define (str-contains? c s)
    (match (itr:find! (== c) (s:chars s))
      ((Some _) True)
      ((None) False)))

  (declare chars-list (String -> List Char))
  (define (chars-list str)
    (itr:collect! (s:chars str)))
  )

;;
;; Hangman Program
;;

(coalton-toplevel

  (declare first-char (String -> Optional Char))
  (define (first-char str)
    (itr:next! (s:chars str)))

  (define-struct HangmanConf
    (num-guesses UFix)
    ;; We don't want to have to leave the dictionary in memory during
    ;; the whole program, so we ask for an IO operation that can get
    ;; a random word (possibly from a file) and return it.
    (get-random-word (IO String)))

  (define-struct HangmanState
    (guessed-chars (List Char))
    (num-wrong-guesses UFix))

  (declare inc-wrong-guesses (HangmanState -> HangmanState))
  (define (inc-wrong-guesses st)
    (HangmanState (.guessed-chars st) (+ 1 (.num-wrong-guesses st))))

  (define-type-alias HangmanM (EnvT HangmanConf (StateT HangmanState IO)))

  ;; Workaround for:
  ;; https://github.com/coalton-lang/coalton/issues/1656
  (declare get-random-word_ (HangmanConf -> HangmanM String))
  (define (get-random-word_ conf)
    (lift (lift (.get-random-word conf))))

  ;; Workaround for:
  ;; https://github.com/coalton-lang/coalton/issues/1656
  (declare num-guesses_ (HangmanConf -> UFix))
  (define (num-guesses_ conf)
    (.num-guesses conf))

  ;; Workaround for:
  ;; https://github.com/coalton-lang/coalton/issues/1656
  (declare guessed-chars_ (HangmanState -> List Char))
  (define (guessed-chars_ st)
    (.guessed-chars st))

  ;; Workaround for:
  ;; https://github.com/coalton-lang/coalton/issues/1656
  (declare num-wrong-guesses_ (HangmanState -> UFix))
  (define (num-wrong-guesses_ st)
    (.num-wrong-guesses st))

  (declare run-hangman (HangmanConf -> HangmanM :a -> :a))
  (define (run-hangman conf m)
    (run!
     (map tp:snd
          (run-stateT
           (run-envT m conf)
           (HangmanState
            Nil
            0)))))

  (define-type Guess
    (WordGuess String)
    (LetterGuess Char)
    (InputError String))

  (declare parse-guess (MonadState HangmanState :m => String -> :m Guess))
  (define (parse-guess input)
    (if (> (s:length input) 1)
        (pure (WordGuess input))
        (do-match (first-char input)
          ((None) (pure (InputError "Must enter a guess!")))
          ((Some c)
           (already-guessed <- (map guessed-chars_ get))
           (if (contains? c already-guessed)
               (pure (InputError (<> (<> "Already guessed " (into c)) "!")))
               (pure (LetterGuess c)))))))

  ;; (declare enter-letter-guess (String -> Char -> HangmanM Unit))
  (define (enter-letter-guess secret-word c)
    "Store the guessed letter and increment the number of wrong guesses."
    (modify
     (fn (st)
       (HangmanState
        (Cons c (guessed-chars_ st))
        (if (str-contains? c secret-word)
            (num-wrong-guesses_ st)
            (+ 1 (num-wrong-guesses_ st)))))))

  ;; (declare write-status (String -> HangmanM Unit))
  (define (write-status secret-word)
    (do
     ((HangmanState guessed n-wrong) <- get)
     (write-line "You have now guessed:")
     (do-foreach (c guessed)
       (write (<> (into c) " ")))
     (write-line "")
     (write-line "Secret Word:")
     (do-foreach (c (chars-list secret-word))
       (write
        (<> (if (contains? c guessed)
                (into c)
                "_")
            " ")))
     (write-line "")
     (num-guesses <- (asks num-guesses_))
     (let remaining-guesses = (- num-guesses n-wrong))
     (write-line (<> (<> "You have " (into remaining-guesses)) " remaining incorrect guesses."))
     (write-line "")))

  (declare over-and-failed? ((MonadState HangmanState :m)
                             (MonadEnvironment HangmanConf :m)
                             => :m Boolean))
  (define over-and-failed?
    (do
     ((HangmanState _ n-wrong) <- get)
     (num-guesses <- (asks num-guesses_))
     (pure (>= n-wrong num-guesses))))

  (define failure-msg "You ran out of guesses. Better luck next time!")

  (declare hangman (HangmanM Unit))
  (define hangman
    (do
     (get-random-word <- (asks get-random-word_))
     (secret-word <- get-random-word)
     (write-line "Please enter a full word to make a guess at the answer.")
     (write-line "Otherwise, enter a single letter to make a letter guess.")
     (lp:do-loop
       (input <- read-line)
       (matchM (parse-guess input)
         ((InputError msg)
          (write-line (<> "Invalid guess: " msg)))
         ((LetterGuess c)
          (enter-letter-guess secret-word c))
         ((WordGuess w)
          (do-if (/= w secret-word)
              (modify inc-wrong-guesses)
            (write-line (<> "The word was " secret-word))
            (write-line "You won!")
            lp:break-loop)))
       (do-whenM over-and-failed?
         (write-line failure-msg)
         lp:break-loop)
       (write-status secret-word))
      ))
  )


(cl:defun play ()
  (coalton (run-hangman (HangmanConf 4 (pure "pizza")) hangman)))
