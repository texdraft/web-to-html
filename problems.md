# Trouble spots in WEB programs

Here are a few issues I've run into while working with `WEB` programs.

## Compiler directives

Many of Knuth's programs have a &langle;Compiler directives&rdquo; module, containing instructions to the Pascal compiler in the form of a “pragmatic comment”. For example, section 9 of TeX has

```
@<Compiler directives@>=
@{@&$C-,A+,D-@} {no range check, catch arithmetic overflow, no debug overhead}
@!debug @{@&$C+,D+@}@+ gubed {but turn everything on when debugging}
```

The problem is that the contents of meta-comments in `@{`…`@}` are treated as regular Pascal code by `WEAVE` and by my program, rather than as actual comments. This is necessary to allow code between `debug` and `gubed`, `stat` and `stats`, and `init` and `tini` to be formatted properly. Thus the first tokens the parser sees are

> `:dollar-sign`, `:identifier` “`C`”, `:minus`, `:comma`, `:identifier` “`A`”, `:plus`, `:identifier` “`D`”, `:minus`,

but of course it's looking for `program`.

My suggestion is to use a piece of verbatim Pascal text like `@={$C-,A+,D-}@>`, which will not be seen by the parser.

## mtype

Section 4 of TeX begins with the following definitions.

```
@d mtype==t@&y@&p@&e {this is a \.{WEB} coding trick:}
@f mtype==type {`\&{mtype}' will be equivalent to `\&{type}'}
@f type==true {but `|type|' will not be treated as a reserved word}
```

You can probably agree that there is no reasonable way to have the parser recognize `t@&y@&p@&e` as `type`. The solution I've adopted removes the definitions (they will be “faked” in the output) and changes the code in `install-reserved-words` (defined in `phase-2.lisp`) to remove `type` as a reserved word and to treat `mtype` as synonymous to `type`.

## float_constant

TeX has a macro called `float_constant`, defined as

```
@d float_constant(#) == #.0 {convert |integer| constant to |real|}
```

Phase 1 treats this as `:macro-parameter`, `:dot`, `:decimal-integer`, so no expansion will be valid Pascal. I just remove the definition for now.

## saved(−1)

In several places, TeX refers to `saved(-1)`. Here `saved` is a macro, and the expression expands to

```
save_stack[save_ptr+-1].int
```

My initial parser followed Pascal's expression syntax closely and did not accept `save_ptr+-1`. The solution was simple: modify the parser to allow a &langle;term&rangle; to begin with a sign. (`TANGLE` converts `+-1` into `-1`, achieving essentially the same result.)

## BREAKPOINT

In TeX's `debug_help`, the pragmatic comment `@{'BREAKPOINT'@}` appears. The parser ends up seeing a random, unexpected `:Pascal-string` in the middle of a compound statement. As with the compiler directives, I suggest changing it to `@={'BREAKPOINT'}@>`.

## Online graphic output

In section 566 of Metafont, the following code occurs:

```
@{@+@!screen_pixel:array[screen_row,screen_col] of pixel_color@t; @>@}
```

The semicolon in this “declaration” (which is hidden from the Pascal compiler in the `TANGLE`d output) is faked by appearing in `@t...@>`, in order to prevent a line break. My parser doesn't see the semicolon and complains when it finds `screen_started` (the next global variable). The solution is to use a “real” semicolon in place of the verbatim TeX text.

## t_of_the_way

Metafont's `t_of_the_way` macro is defined like this:

```
@d t_of_the_way_end(#)==#,t@=)@>
@d t_of_the_way(#)==#-take_fraction@=(@>#-t_of_the_way_end
```

If the parentheses weren't in `@=...@>`, `TANGLE` would not process the program correctly, since it expects all replacement texts to have balanced parentheses. My program does not place such a restriction; although this is definitely a bug, it means that we can get away with writing the parentheses literally.

An alternative, which would be used if I decide to require balanced parentheses, is to make verbatim Pascal text be visible to the parser, by a mechanism similar to that used with meta-comments: add `:begin-verbatim-Pascal` and `:end-verbatim-Pascal` token types and treat what comes between them as regular Pascal text; then in `phase-2.lisp` have `get-next` skip the delimiting tokens. However, additional state would have to be added to the lexer in order to react to `@>` correctly in such a situation. (On the other hand, the text could be collected first and then tokenized, as with module names.)
