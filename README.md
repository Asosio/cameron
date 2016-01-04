# #lang s-exp cameron

## WAT ?
Cameron is a dialect of Racket which (largely[1]) retains the syntax of Racket proper while providing a few niceties to make writing Racket code more enjoyable.

Presently Cameron provides:
* automatic access to threading macros (from racket/threading)
* def/defn syntax a lá Clojure
* (in progress) abstract API over core data structures (vectors/lists/dictionaries/sets)

Immediate goals with Cameron:
* finish abstraction over core data structures
* Write a set of "extensions" for writing web services (json parsing, environment variables etc) which utilises Cameron
* *Drum up new features as appropriate*

## Why
Racket has been described as "a language for creating languages" - cameron is inspired by aspects of Clojure that we liked and refines those we did not :)
Cameron is not meant to thoroughly overhaul Racket, nor do we aim to emulate Clojure - we just want to write the nicest[2] code we can.

## What's the deal with the name ?
![<3](http://images.amcnetworks.com/amc.com/wp-content/uploads/2015/06/halt-and-catch-fire-episode-204-post-cameron-davis-800x600.jpg)

**[1]** We've adopted a Clojure-esque way of defining variables (def) and functions (defn)

**[2]** Subject to biases - you're free to create your own language (or work with us!)
