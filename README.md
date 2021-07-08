# web-to-html
Conversion of Knuth's `WEB` to HTML.

Currently only &ldquo;Phase 1&rdquo; and &ldquo;Phase 2&rdquo; (see a [comment I wrote elsewhere](https://github.com/shreevatsa/webWEB/discussions/1#discussioncomment-856044)) are implemented.

# How to use it

Prerequisites: You must have a Common Lisp implementation and [ASDF](https://common-lisp.net/project/asdf/) (which likely comes with your Lisp).

First, download the project into a place where ASDF can see it (such as `~/common-lisp/` on \*nix). Next, start a Lisp session and do

```lisp
(require "asdf")
(asdf:load-system "web-to-html")
(in-package :web-to-html)
```

Now you have the program loaded. The entry point for Phase 1 is the `Phase-1` function, which has the following syntax:

```
(Phase-1 WEB-file [change-file])
```

where `WEB-file` and `change-file` are streams. By default, `change-file` is `(make-string-input-stream "")`, i.e., an empty stream. An example usage would be

```
(with-open-file (input "tex.web")
  (Phase-1 input))
```

After executing the above expression, you can use the following functions to inspect the collected data:

- `(get-nth-section n)` returns the representation of the `n`th section.
- `(section-text section)` returns the token list associated with a section object.
- `(lookup-module name)` returns the `module` object corresponding to the module named `name`.
- `(lookup-prefix prefix)` returns the `module` object uniquely identified by the given `prefix` (don't include “`...`”).
- `(module-name module)` returns the name associated with a given `module` object.
- `(module-definitions module)` returns a list of section numbers where the given module is defined.
- `(map-modules function)` calls the `function` on each module. They are traversed in tree order, but you shouldn't depend on this.

To get the full text of a module `module`, you might run

```lisp
(let ((definitions (module-definitions module)))
  (loop for definition in definitions
        appending (section-Pascal-part (get-nth-section definition))))
```

Running Phase 2 is easier; all you need to do is evaluate `(Phase-2)`, provided that Phase 1 has already completed. However, most of Knuth's programs will need to be changed, because they have meta-comments (`@{`&hellip;`@}`) that confuse Phase 2. For `WEAVE` and `TANGLE` it suffices to remove the reference to the “Compiler directives” module, and the `'BREAKPOINT'` comment in TeX's `debug_help` must also be removed.

# Weird stuff

Here are some things you might notice about the code:

- There are no abbreviations, except when the abbreviation is a single character long. This is a stylistic preference.
- Comments use the [&ldquo;incorrect&rdquo; quotation marks](https://www.cl.cam.ac.uk/~mgk25/ucs/quotes.html) <code>&#x0060;&#x0060;</code> and `''`. I would just write `“` and `”`, but I've decided to make the program Unicode-agnostic, and I can't bear the sight of `"`.
- The condition system isn't used for errors in Phase 1. Eventually there will be a whole heirarchy of condition types, but for now errors are simply reported.
- There is a mysterious `:long-distance` token `type`, apparently corresponding to an `@n` control code. This is intended to be used to specify the section in which an identifier should be identified, in case it is ambiguous.
