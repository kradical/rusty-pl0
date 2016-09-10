// PL/0 compiler with code generation
// translated from pascal Program 5.6 in Algorithms + Data Structures = Programs

#![allow(dead_code)]
#![allow(unused_variables)]
#![allow(unused_mut)]

use std::collections::{HashSet, HashMap};
use std::fs::File;
use std::path::Path;
use std::io::Read;
use std::error::Error;

const NORW: usize = 11;   // no. of reserved words
const TXMAX: usize = 100; // length of identifier table
const NMAX: usize = 14;   // max. no. of digits in numbers
const MAX_IDENTIFIER_LEN: usize = 10;     // length of identifiers
const AMAX: usize = 2047; // maximum address
const LEVMAX: usize = 3;  // maximum depth of block nesting
const CXMAX: usize = 200; // size of code array

#[derive(Clone, Copy, Eq, PartialEq, Hash)]
enum Symbol {
    Nul,
    Ident,
    Number(usize),
    Plus,
    Minus,
    Times,
    Slash,
    OddSym,
    Eql,
    Neq,
    Lss,
    Leq,
    Gtr,
    Geq,
    LParen,
    RParen,
    Comma,
    Semicolon,
    Period,
    Becomes,
    BeginSym,
    EndSym,
    IfSym,
    ThenSym,
    WhileSym,
    DoSym,
    CallSym,
    ConstSym,
    VarSym,
    ProcSym,
}

enum Obj {
    Constant(usize), // val
    Variable(usize, usize), // level, adr
    Prozedure(usize, usize), // level, adr
}

// Instructions
#[derive(Hash, Eq, PartialEq)]
enum Fct {
    Lit,
    Opr,
    Lod,
    Sto,
    Cal,
    Ret,
    Int,
    Jmp,
    Jpc,
}

type SymSet = HashSet<Symbol>;
type Alfa = [char; MAX_IDENTIFIER_LEN];

// lit 0,a: load constant a
// opr 0,a: execute operation a
// lod l,a: load variable l,a
// sto l,a: store variable l,a
// cal l,a: call procedure a at level l
// ret 0,0; return from current procedure
// int 0,a: increment t-register by a
// jmp 0,a: jump to a
// jpc 0,a: jump conditional to a
struct Instruction {
    f: Fct,
    l: usize, // [0, LEVMAX)
    a: usize, // [0, AMAZ)
}

struct Entry {
    name: Alfa,
    kind: Obj,
}

fn main() {
    let mut sym: Symbol; // last symbol read
    let mut id: Alfa;    // last identifier read
    let mut num: usize;    // last number read
    let mut err = 0;
    let mut cx = 0;     // code allocation index
    let mut a: Alfa;
    let mut code: [Instruction; CXMAX + 1]; //ndx 0..CXMAX in pascal

    let mut table: [Entry; TXMAX];

    let mut mnemonic = HashMap::<Fct, String>::new();
    mnemonic.insert(Fct::Lit, String::from(" lit "));
    mnemonic.insert(Fct::Opr, String::from(" opr "));
    mnemonic.insert(Fct::Lod, String::from(" lod "));
    mnemonic.insert(Fct::Sto, String::from(" sto "));
    mnemonic.insert(Fct::Cal, String::from(" cal "));
    mnemonic.insert(Fct::Ret, String::from(" ret "));
    mnemonic.insert(Fct::Int, String::from(" int "));
    mnemonic.insert(Fct::Jmp, String::from(" jmp "));
    mnemonic.insert(Fct::Jpc, String::from(" jpc "));

    // page(output); what do

    let path = Path::new("test.pl0");
    let mut input_file = match File::open(&path) {
        Err(y) => panic!("error opening input file: {}", y.description()),
        Ok(f) => f,
    };

    let mut input_str = String::new();

    match input_file.read_to_string(&mut input_str) {
        Err(y) => panic!("error reading iput file: {}", y.description()),
        Ok(_) => {},
    }

    block(0, 0, &mut input_str);
    // if sym<> period then
    //   error(9);
    // if err = 0 then
    //   interpret
    // else
    //   write( err, ' errors found' );
}

/// Error Handler
fn error(n: usize) {

}

/// Tokenizer / Scanner
fn get_symbol(mut input: &mut String) -> Symbol {
    fn get_char(input: &mut String) -> char {
        // if input is exhausted println!("Program incomplete") and exit
        // else get the next character, write it to output and return it
        return input.remove(0);
    }

    fn is_identifier(c: char) -> bool {
        is_identifier_start(c) || is_number(c)
    }

    fn is_identifier_start(c: char) -> bool {
        c >= 'a' && c <= 'z'
    }

    fn is_number(c: char) -> bool {
        c >= '0' && c <= '9'
    }

    let mut current = get_char(&mut input);
    while current == ' ' {
        current = get_char(&mut input);
    }

    let mut identifier = String::new();
    if is_identifier_start(current) {
        // identifier or reserved word
        let k = 0;
        while is_identifier(current) {
            if k < MAX_IDENTIFIER_LEN {
                identifier.push(current);
            }
            current = get_char(&mut input);
        }

        return match &identifier[..] {
            "begin" => Symbol::BeginSym,
            "call" => Symbol::CallSym,
            "const" => Symbol::ConstSym,
            "do" => Symbol::DoSym,
            "end" => Symbol::EndSym,
            "if" => Symbol::IfSym,
            "odd" => Symbol::OddSym,
            "procedure" => Symbol::ProcSym,
            "then" => Symbol::ThenSym,
            "var" => Symbol::VarSym,
            "while" => Symbol::WhileSym,
            _ => Symbol::Ident,
        }
    } else if is_number(current) {
        let mut num_string = String::new();
        while is_number(current) {
            num_string.push(current);
            current = get_char(&mut input);
        }
        let number = match num_string.parse::<usize>() {
            Err(y) => panic!("parsing invalid number: {}", y.description()),
            Ok(n) => n,
        };

        if number > 99999999999999 {
            error(30);
        }

        return Symbol::Number(number)
    } else if current == ':' {
        current = get_char(&mut input);
        if current == '=' {
            // pascal has a getch here
            return Symbol::Becomes;
        } else {
            return Symbol::Nul;
        }
    } else if current == '<' {
        current = get_char(&mut input);
        if current == '=' {
            // pascal has a getch here
            return Symbol::Leq;
        } else {
            return Symbol::Lss
        }
    } else if current == '>' {
        current = get_char(&mut input);
        if current == '=' {
            // pascal has a getch here
            return Symbol::Geq;
        } else {
            return Symbol::Gtr
        }
    } else {
        return match current {
            '+' => Symbol::Plus,
            '-' => Symbol::Minus,
            '*' => Symbol::Times,
            '/' => Symbol::Slash,
            '(' => Symbol::LParen,
            ')' => Symbol::RParen,
            '=' => Symbol::Eql,
            ',' => Symbol::Comma,
            '.' => Symbol::Period,
            '#' => Symbol::Neq,
            ';' => Symbol::Semicolon,
            _ => Symbol::Nul,
        }
    }
}

/// Code Generator
fn gen(x: Fct, y: usize, z: usize, cx: &mut usize) -> Instruction {
    if *cx > CXMAX {
        print!("Program too long");
        //TODO exit program
    }

    *cx += 1;
    Instruction {
        f: x,
        l: y,
        a: z,
    }
}

/// Parser
fn test(s1: &SymSet, s2: &SymSet, n: usize) {

}

/// Parser
fn block(lev: usize, tx: usize, mut input: &mut String) {
    /// List code generated for this block
    /// Debug generated byte code
    fn listcode() {

    }

    fn enter() {

    }

    fn const_declaration() {

    }

    fn vardeclaration() {

    }

    fn statement(fsys: SymSet) {
        fn expression(fsys: SymSet) {
            fn term(fsys: SymSet) {
                fn factor(fsys: SymSet) {

                }
            }
        }

        fn condition(fsys: SymSet) {

        }
    }

    let mut declbegsys = SymSet::new();
    declbegsys.insert(Symbol::ConstSym);
    declbegsys.insert(Symbol::VarSym);
    declbegsys.insert(Symbol::ProcSym);

    let mut statbegsys = SymSet::new();
    statbegsys.insert(Symbol::BeginSym);
    statbegsys.insert(Symbol::CallSym);
    statbegsys.insert(Symbol::IfSym);
    statbegsys.insert(Symbol::WhileSym);

    let mut facbegsys = SymSet::new();
    facbegsys.insert(Symbol::Ident);
    facbegsys.insert(Symbol::Number(0));
    facbegsys.insert(Symbol::LParen);

    let mut symset = SymSet::new();
    symset.insert(Symbol::Period);
    symset.insert(Symbol::BeginSym);
    symset.insert(Symbol::CallSym);
    symset.insert(Symbol::IfSym);
    symset.insert(Symbol::WhileSym);
    symset.insert(Symbol::ConstSym);
    symset.insert(Symbol::VarSym);
    symset.insert(Symbol::ProcSym);

    let mut code = Vec::new();

    let dx = 3; // data allocation index
    let tx0 = tx; // initial table index
    let cx0 = 0; // initial code index

    let mut cx = 0; // might have to move to main scope

    code.push(gen(Fct::Jmp, 0, 0, &mut cx));

    if lev > LEVMAX {
        error(32);
    }

    let mut current = get_symbol(&mut input);

    loop {
        if current == Symbol::ConstSym {
            current = get_symbol(&mut input);

            loop {
                const_declaration();
                while current == Symbol::Comma {
                    current = get_symbol(&mut input);
                    const_declaration()
                }

                if current == Symbol::Semicolon {
                    current = get_symbol(&mut input);
                } else {
                    error(5);
                }

                if current == Symbol::Ident {
                    break;
                }
            }
        }

        if current == Symbol::VarSym {
            current = get_symbol(&mut input);
            loop {
                vardeclaration();

                while current == Symbol::Comma {
                    current = get_symbol(&mut input);
                    vardeclaration();
                }

                if current == Symbol::Semicolon {
                    current = get_symbol(&mut input);
                } else {
                    error(5);
                }

                if current == Symbol::Comma {
                    break;
                }
            }
        }

        while current == Symbol::ProcSym {
            current = get_symbol(&mut input);

            if current == Symbol::Ident {
                enter(); //TODO enter(prozedure);
                current = get_symbol(&mut input);
            } else {
                error(4);
            }

            if current == Symbol::Semicolon {
                current = get_symbol(&mut input);
            } else {
                error(5);
            }

            block(lev + 1, tx, &mut input); //TODO add semicolon to set here

            if current == Symbol::Semicolon {
                current = get_symbol(&mut input);
                test(&symset, &symset, 6); //TODO test(statbegsys+[ident,procsym],fsys,6)
            } else {
                error(5);
            }
        }

        test(&declbegsys, &declbegsys, 7); //TODO test(statbegsys+[ident],declbegsys,7)

        if declbegsys.contains(&current) {
            break;
        }
    }

    //TODO
    // code[table[tx0].adr].a:= cx;
    // with table[tx0] do
    // begin
    //   adr:= cx; (*start adr of code*)
    // end;
    // cx0:= cx;
    // gen(int,0,dx);
    // statement([semicolon,endsym]+fsys);
    // gen(ret,0,0);
    // test(fsys,[],8);
    // if err = 0 then listcode;

}

/// Byte Code Interpreter
fn interpret() {

}
