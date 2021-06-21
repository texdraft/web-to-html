# web-to-html
Conversion of Knuth's `WEB` to HTML.

Currently only &ldquo;Phase 1&rdquo; (see a [comment I wrote elsewhere]([comment](https://github.com/shreevatsa/webWEB/discussions/1#discussioncomment-856044))) is implemented. In this phase, the `WEB` file is fully read and split up into tokens.

# How to use it

Prerequisites: You must have a Common Lisp implementation and [ASDF](https://common-lisp.net/project/asdf/) (which likely comes with your Lisp).

First, download the project into a place where ASDF can see it (such as `~/common-lisp/` on \*nix). Next, start a Lisp session and do

```lisp
(require "asdf")
(asdf:load-system "web-to-html")
(in-package :web-to-html)
```

Now you have the program loaded. The entry point for Phase 1 is the [`Phase-1`](https://github.com/texdraft/web-to-html/blob/main/phase-1.lisp#L979) function, which has the following syntax:

```
(Phase-1 WEB-file [change-file])
```

where *WEB-file* and *change-file* are streams. By default, *change-file* is `(make-string-input-stream "")`, i.e., an empty stream. An example usage would be

```
(with-open-file (input "tex.web")
  (Phase-1 input))
```

After executing the above expression, you can use the following functions to inspect the collected data:

- [`(get-nth-section n)`](https://github.com/texdraft/web-to-html/blob/main/common.lisp#L279) returns the representation of the `n`th section.
- [`(section-text section)`](https://github.com/texdraft/web-to-html/blob/main/common.lisp#L16) returns the token list associated with a section object.
- [`(lookup-module name)`](https://github.com/texdraft/web-to-html/blob/main/common.lisp#L206) returns the `module` object corresponding to the module named `name`.
- [`(lookup-prefix prefix)`](https://github.com/texdraft/web-to-html/blob/main/common.lisp#L238) returns the `module` object uniquely identified by the given `prefix`.
- [`(module-name module)`](https://github.com/texdraft/web-to-html/blob/main/common.lisp#L8) returns the name associated with a given `module` object.
- [`(module-definitions module)`](https://github.com/texdraft/web-to-html/blob/main/common.lisp#L8) returns a list of section numbers where the given module is defined.
- [`(map-modules function)`](https://github.com/texdraft/web-to-html/blob/main/common.lisp#L261) calls the `function` on each module. They are traversed in tree order.

To get the full text of a module, you might run

```lisp
(let ((definitions (module-definitions module)))
  (loop for definition in definitions
        appending (section-text (get-nth-section definition))))
```

# Weird stuff

Here are some things you might notice about the code:

- There are no abbreviations, except when the abbreviation is a single character long. This is a stylistic preference.
- A lot is unused; it is included in preparation of Phase 2 and Phase 3.
- I use `defstruct` instead of `defclass`. My rationale is that `defclass` is best used when the full facilities of CLOS are desired, such as inheritance (although `defstruct` can do this), generic functions, and the method combination; `defstruct` should be used for what is called &ldquo;Plain Old Data&rdquo; in C++. For semantic analysis and HTML generation, CLOS will be used.
- Comments use the [&ldquo;incorrect&rdquo; quotation marks](https://www.cl.cam.ac.uk/~mgk25/ucs/quotes.html) <code>&#x0060;&#x0060;</code> and `''`. I would just write `“` and `”`, but I've decided to make the program Unicode-agnostic, and I can't bear the sight of `"`.
- The condition system isn't used for errors in Phase 1. Eventually there will be a whole heirarchy of condition types, but for now errors are simply reported.
