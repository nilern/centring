# Namespaces

# ns

    (ns ns-name: <symbol>)

Make `ns-name` the current namespace.

Compiles to `%set-ns!`.

# require

    (require rspecs: <require-spec>*)

Perform require-actions demanded by `rspecs`:

    <require-spec> ::= <symbol>
                     | (as <require-spec> <symbol>)
                     | (use <require-spec>)
                     | (only <require-spec> <symbol>*)
                     | (except <require-spec> <symbol>*)
                     | (rename <require-spec> (<symbol> <symbol>)*)
                     | (-> <require-spec> <require-spec'>*)

`ctr.lang` and `ctr.core` are `use`:d implicitly. This can be overridden by
adding explicit require-specs for them.

Examples of `require`:

    (require (-> mod (as foo) use (only x y))
             (-> ctr.lang use (except fold)))

Using `only`, `except` or `rename` on a require spec that hasn't undergone `use`
is an error (to avoid silent bugs).

`require` can be compiled to use just:

* `%require!` (`<symbol>`)
* `%alias!` (`as`)
* `%import!` (`use`) -- fill current import set
* `%refer!` (`only` and `except`)
* `%rename!` (`rename`)
* `%end-import!` -- add refers to current ns, empty current import set

# ffi-require

    (ffi-require frspecs: <ffi-require-spec>*)

where

    <ffi-require-spec> ::= <string>
                         | (as <ffi-require-spec> <symbol>)

Load a foreign library if it is not already loaded and optionally give it a name
to be used by `ffi-fn`. (Usually loading a foreign library without naming it is
not very useful.)

Compiles to `%ffi-require!`.

# def and def-

    (def name: <symbol> <expr>)

Set the global named by `name` in the current namespace and make it public.

    (def- name: <symbol> <expr>)

Set the global named by `name` in the current namespace and make it private.

Can be compiled to `%set-global!`.

# Restrictions

`ns`, `require`, `ffi-require` and `def(-)` should only appear at the toplevel. It is
standard practice to place them only at the beginning of files, so that each file
contains one namespace. In fact this is the only arrangement which will work
smoothly without the use of `load` or `include` (which might or might not be
provided by the implementation).
