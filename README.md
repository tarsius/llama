Compact syntax for short lambdas.

Unfortunately compact syntax for short `lambda`s won't be added to
Emacs anytime soon.  IMO the arguments as to why we would like to have
that has been layed out convincingly but the proposal has been
rejected anyway.

Several packages exist that implement anonymous function literals,
but until now they all either are waiting for a patch to the C part
be merged into Emacs, or they depart too far from the ideal syntax.

This package is another attempt.

> **Update:** I have come up with a second syntax, which does not put
> anything *before* the opening parenthesis and looks like `(l'foo %)`.
> Unlike with my original `llama` idea I am no longer suggesting making
> it look like some character(s) appear before the paren using font-lock
> or similar trickery (see below).  If you so desire, then you could
> however use `prettify-symbols-mode` to display it as e.g. `(ƒ'foo
> %)`.  This approach is implemented in [`l`](https://git.sr.ht/~tarsius/l).

In a stroke of luck I discovered a loophole that allows us to have
almost the syntax that we want without having to convince anyone.

    $(foo %)        is what I would have used if it were up to me.
    #(foo %)        works just as well for me.
    ##(foo %)       is similar enough to that.
    (##foo %)       is the loophole that I discovered.

Even though there is no space between the second `#` and `foo`, this
last form is read as a list with three arguments `(## foo %)` and it
is also indented the way we want!

    (##foo %
           bar)

This is good enough for me, but with a bit of font-lock trickery,
we can even get it to be display like this:

    ##(foo %
           bar)

This is completely optional and you have to opt-in by enabling
`llama-mode` (or the global variant `global-llama-mode`).

An unfortunate edge-case exists that you have to be aware off; if
no argument is placed on the same line as the function, then Emacs
does not indent as we would want it too:

    (##foo                                         ##(foo
     bar)        which llama-mode displays as       bar)

I recommend that in this case you simply write this instead:

    (## foo                                        ##( foo
        bar)     which llama-mode displays as          bar)

It is my hope that this package helps to eventually get similar
syntax into Emacs itself, by demonstrating that this is useful and
that people want to use it.

*Also see the [announcement](https://emacsair.me/2021/01/28/llama-0.1).*

*Also see [`l`](https://git.sr.ht/~tarsius/l), my second attempt at
faking such syntax.*
