#lang scribble/manual
@(require racket/sandbox
          scribble/eval
          (for-label threading
                     cameron/defs
                     racket))

@title{#lang cameron}
@defmodule[cameron/defs]
Cameron is a dialect of Racket which (largely) retains the syntax of Racket proper while providing a few niceties to make writing Racket code more enjoyable.

@hyperlink["https://github.com/Asosio/cameron" "Get the Source"].
@margin-note{Cameron is only tested against Racket v6.3 and later.}

@[table-of-contents]

@section{Why?}
Having worked on a (to our minds) larger project using several languages & frameworks, we've come to appreciate how different languages
make different tradeoffs to more succinctly express some things as opposed to others.
@margin-note{Interestingly, human languages also display tradeoffs in expressiveness, see
@hyperlink["http://www.newyorker.com/magazine/2012/12/24/utopian-for-beginners" "Utopian For Beginners"], paragraph 24.}

It seems plausible then, that certain languages lend themselves better to some problem domains than others.
Having examined several languages, we started studying Lisp.
The appeal comes from its spartan syntax and macro system which results in little-to-no distinction between pre-made language features, third-party or even entirely bespoke additions to the language.
Racket, sometimes described as "a language for creating languages" takes a step further, offering a superb macro system, flexible import/export system and the ability to define dialects which can alter the reader to introduce new syntax if desired.

Cameron reflects our attempt to adjust Racket to our preferences while hopefully remaining general enough to be of interest to others. Our aims can be summarised as thus:

@itemlist[#:style 'ordered
          @item{Try very hard not to alter existing Racket behaviour}
          @item{Make it as easy as possible to construct & manipulate data structures}
          @item{Emphasize consistency and general applicability}]

Feel free to fork, extend and borrow from Cameron to fashion a Racket dialect entirely suited to your preferences and needs.

@;--------------------------------------------------------------------

@;--------------------------------------------------------------------
@section{New & Changed Syntax}
@subsection{Definitions: @racket[def], @racket[defn], @racket[case-fn], ...}
@subsubsection{Defining variables}
@defform[(def id expr)]{
  Binds @racket[id] to the result of @racket[expr].}

From:
@racketblock[(define x 10)]
To:
@racketblock[(def x 10)]

@subsubsection{Defining functions}
@defform/subs[(defn head [args] body ...+)
              ([head id
                     (head args)]
               [args (code:line arg ...)
                     (code:line arg ... & rest-id)]
               [arg arg-id
                    (arg-id default-expr)
                    (code:line keyword arg-id)
                    (code:line keyword (arg-id default-expr))])]{
Bind @racket{id} to a procedure.}

From:
@racketblock[(define (sum a . args)
               (apply + a args))]
To:
@racketblock[(defn sum [a & args]
               (apply + a args))]

@subsubsection[#:tag "fn"]{Lambda (anonymous) functions}
@defform/subs[(fn kw-formals body ...+)
              ([kw-formals [arg ...]
                           [arg ...+ & rest-id]]
               [arg id
                    [id default-expr]
                    (code:line keyword id)
                    (code:line keyword [id default-expr])])]

@racketblock[(lambda (label . msgs)
                 (println (string-append label ": " (string-join msgs ", "))))]

@racketblock[(fn [label & msgs]
                 (println (string-append label ": " (string-join msgs ", "))))]

As an aside, the following in Racket:
@racketblock[(lambda args (string-append args))]

Should be written in Cameron as:
@racketblock[(fn [& args] (string-append args))]

@subsubsection{Defining conditional procecure}
As might be expected, @racket[case-lambda] is replaced by @racket[case-fn]. As for regular anonymous functions (@racket[fn]), the ampersand (@tt{&}) is used to express rest-arguments.

@defform/subs[(case-fn (formals body ...+) ...)
              ([formals [id ...]
                        [id ... & rest-id]])]

From:
@racketblock[
 (case-lambda
   ([a b] (+ a b))
   ([a . args] (apply + a args)))]

To:
@racketblock[
 (case-fn
   ([a b] (+ a b))
   ([a & args] (apply + a args)))]

@;--------------------------------------------------------------------
@section{Generic Indexing & Assignment Operations}

@subsection{Indexing by key: @racket[key], @racket[key-set], @racket[key-rm]}
@defform[(key data-structure ref [failure-result])]{
  Returns the value referenced by @tt{ref}, if available. If not, @tt{failure-result} will be returned, if supplied, otherwise a contract violation occurs. Supported by @racket[dict].}

@defform/subs[(key-set data-structure assignment ...+)
              ([assignment ref
                           (code:line ref value)])]{
  Adds entries to @racket[data-structure]. If @racket[data-structure] is a @racket[dict], then an assignment is expressed as @racket[ref value] whereas if it is a @racket[set], an assignment is the key itself (@racket[ref]). If @racket[data-structure] is mutable the return-value is itself, otherwise, a new immutable data-structure of the same type is returned.}

@defform[(key-rm data-structure ref ...+)]{
  Removes entries from @racket[data-structure]. If @racket[data-structure] is mutable the return-value is itself, otherwise, a new immutable data-structure of the same type is returned.}

@subsection{Indexing by position: @racket[nth], @racket[nth-set]}

@defform[(nth data-structure pos)]

@defform/subs[(nth-set data-structure assignment ...)
              ([assignment (code:line pos value)])]{
Update structure with the supplied assignments. If @racket[data-structure] is mutable the return-value is itself, otherwise, a new immutable data-structure of the same type is returned.}

@;frozen? has? empty? len 