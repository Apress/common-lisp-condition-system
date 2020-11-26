# Appendix F: Hyperlinking the Common Lisp Condition System

## Preface

It turns out that the hyperlinks in the first edition of *The Common Lisp Condition System* did not receive all the attention that they needed. Some hyperlinks in the online version of the book point to wrong locations, whereas the printed version of the book has no hyperlinks printed as footnotes or otherwise, and therefore misses the hyperlinks altogether.

Because of these inconsistencies, this appendix was created and published. Instead of an errata that would gather all the links in a single list along with pointers to the book pages to which they belong, we have decided to create an online appendix that gathers and describes all of these links into what we hope will serve as a single, consistent resource for external materials that describe and/or touch and/or support the Common Lisp condition system.

We apologize for the inconvenience and hope that this appendix alleviates the issue to some extent.

## Front Matter

The preface of the book (0.4) mentions a Symbolics dialect of Lisp Machine Lisp. An online version of its manual is available at https://hanshuebner.github.io/lmman/frontpage.html and its chapter dedicated to error handling is available at  https://hanshuebner.github.io/lmman/errors.xml - these documents should provide some historical reference for the current version of the Common Lisp condition system.

The previous work that describes the Common Lisp condition system includes articles from:
* Kent M. Pitman - https://www.nhplace.com/kent/Papers/Condition-Handling-2001.html,
* Peter Seibel - http://www.gigamonkeys.com/book/beyond-exception-handling-conditions-and-restarts.html,
* Chaitanya Gupta - https://lisper.in/restarts,
* Justin Grant - https://imagine27.com/error-handling-or-the-emporors-old-clothes/,
* Timmy Jose - https://z0ltan.wordpress.com/2016/08/06/conditions-and-restarts-in-common-lisp/,
* Jacek Złydach - http://jacek.zlydach.pl/blog/2019-07-24-algebraic-effects-you-can-touch-this.html,
* Robin Schroer - https://sulami.github.io/posts/common-lisp-restarts/,
* the collaborative effort behind the Common Lisp cookbook - https://lispcookbook.github.io/cl-cookbook/error_handling.html.

The source code and appendixes for *The Common Lisp Condition System* are all available at https://github.com/Apress/common-lisp-condition-system.

## Chapter 1

The section about dynamic variables (1.1) mentions a *stack* data structure - a description of this data structure is available (among descriptions in other places) on Wikipedia at https://en.wikipedia.org/wiki/Stack_(abstract_data_type).

The section about dynamic variables in Common Lisp (1.1.2) refers to *Practical Common Lisp* by Peter Seibel for detailing the way in which dynamic variables may be defined and rebound in Common Lisp. This chapter is available at http://www.gigamonkeys.com/book/variables.html#dynamic-aka-special-variables.

The section about non-local transfers of control (1.2) mentions that standard flow of control uses an *inorder* tree traversal. A description of this tree traversal ordering, including comparisons to other possible orderings, is available e.g. at https://www.geeksforgeeks.org/tree-traversals-inorder-preorder-and-postorder/.

The section related to `go` and `tagbody` (1.2.1) contains the first use of the `format` function for outputting formatted text; again, we refer to *Practical Common Lisp* with the respective chapter being available at http://www.gigamonkeys.com/book/a-few-format-recipes.html.

## Chapter 2

The initial section (2.1) contains a link to a technique called *hooking* which is then described in detail later in the book; Wikipedia has a page for this technique available at https://en.wikipedia.org/wiki/Hooking.

The section that introduces the term *condition handling* (2.2) quotes the Common Lisp HyperSpec, an online rendition of the Common Lisp standard - in particular, to a part that describes the general concepts of the condition system. That part is available at http://clhs.lisp.se/Body/09_a.htm.

The section which introduces the concept of exception handling (2.2.1.1) describes the concepts of *inheritance* and *multiple inheritance*, both of which apply to Common Lisp conditions. An example problem that is solved by multiple inheritance is *the diamond problem*, described on Wikipedia at https://en.wikipedia.org/wiki/Multiple_inheritance#The_diamond_problem.

The section that details computing and invoking choices (2.3.2.4) mentions a term called a *thunk function*; again, we link to the Wikipedia page for that term, available at https://en.wikipedia.org/wiki/Thunk.

The section describing custom debuggers (2.9.5) has a missing link to the `trivial-custom-debugger` library, which is available on GitHub at https://github.com/phoe/trivial-custom-debugger. This section also mentions Quicklisp (https://quicklisp.org), a Common Lisp library manager.

The section related to backtraces (2.9.8) has a missing link to Dissect (https://shinmera.github.io/dissect/), a portability library for producing backtraces that are then available to be processed as standard Lisp data.

## Chapter 3

The initial paragraphs of chapter 3 mention that the condition system defined in *The Common Lisp Condition System* is adapted from Portable Condition System (PCS, https://github.com/phoe/portable-condition-system), which, in turn, was adapted from the original condition system written for the first version of CL by Kent M. Pitman (http://www.nhplace.com/kent/CL/Revision-18.lisp). It also states that the test suite for PCS was adapted from the ANSI-TEST test suite (https://gitlab.common-lisp.net/ansi-test/ansi-test).

The section mentioning integrating PCS with the host condition system (3.12.1) links to an ASDF system named `portable-condition-system.integration` (https://github.com/phoe/portable-condition-system/tree/master/integration) which provides a means of integrating the host's condition system with PCS.

## Chapter 4

The initial paragraphs of chapter 4 link to Kent M. Pitman's original implementation of the condition system on his homepage (http://www.nhplace.com/kent/CL/Revision-18.txt) and in the PCS repository (https://github.com/phoe/portable-condition-system/blob/master/Revision-18.lisp).

The section describing the purpose of the condition system (4.1) describes a situation in which a space probe, hundreds of millions of miles away from the programmer, was debugged thanks to the fact that it was running a Lisp REPL. The full story of this episode is available at http://flownet.com/gat/jpl-lisp.html.

The section about the separation of concerns (4.3) contains links to several Common Lisp implementations of hooks. Those are:
* `cl-events` - https://github.com/deadtrickster/cl-events/,
* `cl-hooks` - https://github.com/scymtym/architecture.hooks,
* `modularize-hooks` - https://github.com/Shinmera/modularize-hooks.

The sections about algebraic effects contains multiple links, being:
* a description of algebraic effects in general - https://www.sciencedirect.com/science/article/pii/S1571066115000705,
* links to implementations of algebraic effects in Haskell (https://hackage.haskell.org/package/effect-handlers), Eff (https://www.eff-lang.org/), and Javascript (https://overreacted.io/algebraic-effects-for-the-rest-of-us/),
* a repetition of a previously mentioned article by Jacek Złydach - http://jacek.zlydach.pl/blog/2019-07-24-algebraic-effects-you-can-touch-this.html.

The section mentioning speed as a downside of the condition system (4.5.3) links to an article that describes the possibility of implementing a backtracking algorithm via Common Lisp conditions (http://directed-procrastination.blogspot.se/2011/05/backtracking-with-common-lisp-condition.html) and to a later response that compares this approach with using the Common Lisp control flow primitives directly (https://gist.github.com/nikodemus/b461ab9146a3397dd93e).

The section describing the downsides of the condition system's introspection facilities (4.5.5) contains a series of links:
* Dissect (https://github.com/Shinmera/dissect), the utility for fetching backtrace information, 
* A trio of libraries that implement their own debuggers by means of hooking into implementation-dependent code:
  * Swank (https://github.com/slime/slime/tree/master/swank),
  * Slynk (https://github.com/joaotavora/sly/blob/master/slynk/),
  * `trivial-custom-debugger` (https://github.com/phoe/trivial-custom-debugger).

The section describing smaller issues contains a link to a Git commit that removes conditions from the Rust programming language, along with some rationale in the commit message. This commit is available at https://github.com/rust-lang/rust/commit/454882dcb7fdb03867d695a88335e2d2c8f7561a.

The section summarizing the downsides of the Common Lisp condition system links to *What Made Lisp Different*, an essay by Paul Graham describing the aspects of Common Lisp which have not trickled into programming languages nowadays considered mainstream. This article is available at `http://www.paulgraham.com/diff.html` and the condition system would be a good addition to that list.

The above section also contains condition systems implemented as libraries for other programming languages:
* Python - https://github.com/svetlyak40wt/python-cl-conditions,
* Clojure - https://github.com/clojureman/special,
* Ruby - https://github.com/michaeljbishop/mulligan,
* Perl - https://metacpan.org/release/Worlogog-Incident and https://metacpan.org/release/Worlogog-Restart.

Again in that section, there is a linked article which describes the implementation of a simplified condition system in Clojure, available at https://gist.github.com/msgodf/6f4e43c112b8e89eee3d.

The section describing collecting results in a testing library (4.6.1) contains a link to the testing library `should-test`, available at https://github.com/vseloved/should-test with the concrete example available at https://github.com/vseloved/should-test/blob/48facb9/should-test.lisp#L119.

## Chapter 5

Appendix A (5.1) mentions that dynamic variables (or *fluid variables*, as they are called there) are available in the Rust programming language as a library named `fluid-let`, available at https://docs.rs/fluid-let/0.1.0/fluid_let/.
