# rusty-pl0
A compiler + interpreter for the pl0 programming language in rust.

pl0 is an educational subset of pascal.

[See pl/0 syntax, grammar, and semantics](https://en.wikipedia.org/wiki/PL/0)

To run:
- install rust compiler
- install cargo
- navigate to the root project directory
- `cargo run < test.pl0` to test a syntactically valid pl/0 program
- `cargo run < err.pl0` to test a program with errors

As seen above the program reads from stdin.

`testKEY.out` and `errKEY.out` act as expected outputs to compare to when making changes.  


Written as a challenge from the Programming Languages (CSC330 @ UVic) proffessor Dr. Mantis Cheng.
