# PostScript Transformer

A simple library for doing basic transformations on shapes 
represented as a subset of PostScript language.

Detailed requirements are presented in [`Assignment.md`](Assignment.md) (in Polish).

The [`Lib`](src/Lib.hs) file that defines picture representation format
and its possible transformations (rotation and translation).
PostScript subset parser is defined in [`Main`](app/Main.hs) file, while the tests are present in
[`test`](test) directory. Note that none of the tests are my work, they supplied 
with the assignment and have only been lightly modified.
