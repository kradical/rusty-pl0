// PL/0 compiler with code generation
// translated from pascal Program 5.6 in Algorithms + Data Structures = Programs

#![allow(dead_code)]
#![allow(unused_variables)]
#![allow(unused_mut)]

use std::collections::{HashSet};
use std::fs::File;
use std::path::Path;
use std::io::Read;
use std::error::Error;
use std::fmt;

type SymSet = HashSet<Symbol>;

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
    Number,
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
    Constant,
    Variable,
    Prozedure,
}

// Instructions
// lit 0,a: load constant a
// opr 0,a: execute operation a
// lod l,a: load variable l,a
// sto l,a: store variable l,a
// cal l,a: call procedure a at level l
// ret 0,0; return from current procedure
// int 0,a: increment t-register by a
// jmp 0,a: jump to a
// jpc 0,a: jump conditional to a
#[derive(Hash, Eq, PartialEq, Copy, Clone)]
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
    Noop,
}
impl std::fmt::Debug for Fct {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let fct = match *self {
            Fct::Lit => String::from("Lit"),
            Fct::Opr => String::from("Opr"),
            Fct::Lod => String::from("Lod"),
            Fct::Sto => String::from("Sto"),
            Fct::Cal => String::from("Cal"),
            Fct::Ret => String::from("Ret"),
            Fct::Int => String::from("Int"),
            Fct::Jmp => String::from("Jmp"),
            Fct::Jpc => String::from("Jpc"),
            Fct::Noop => String::from("Nop"),
        };
        write!(f, "{}", fct)
    }
}

struct Instruction {
    f: Fct,
    l: usize, // [0, LEVMAX)
    a: usize, // [0, AMAZ)
}
impl std::fmt::Debug for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, " {:?}  {}   {}  ", self.f, self.l, self.a)
    }
}

struct Entry {
    name: String,
    kind: Obj,
    val: usize,
    level: usize,
    adr: usize,
}

struct Parser {
    code_words: Vec<Option<Instruction>>,
    ident_table: Vec<Option<Entry>>,
    current_symbol: Symbol,
    current_level: usize,
    last_id: String,
    last_num: usize,
    input: String,
    error_count: usize,
    code_index: usize,
    data_index: usize,
    table_index: usize,
    declbegsys: SymSet,
    statbegsys: SymSet,
    facbegsys: SymSet,
}

fn main() {
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

    let mut parser = Parser {
        code_words: Vec::with_capacity(CXMAX),
        ident_table: Vec::with_capacity(TXMAX),
        input: input_str,
        current_symbol: Symbol::Nul,
        current_level: 0,
        last_id: String::new(),
        last_num: 0,
        error_count: 0,
        code_index: 0,
        data_index: 0,
        table_index: 0,
        declbegsys: SymSet::new(),
        statbegsys: SymSet::new(),
        facbegsys: SymSet::new(),
    };

    for _ in 0..CXMAX {
        parser.code_words.push(None);
    }

    for _ in 0..TXMAX {
        parser.ident_table.push(None);
    }

    parser.declbegsys.insert(Symbol::ConstSym);
    parser.declbegsys.insert(Symbol::VarSym);
    parser.declbegsys.insert(Symbol::ProcSym);

    parser.statbegsys.insert(Symbol::BeginSym);
    parser.statbegsys.insert(Symbol::CallSym);
    parser.statbegsys.insert(Symbol::IfSym);
    parser.statbegsys.insert(Symbol::WhileSym);

    parser.facbegsys.insert(Symbol::Ident);
    parser.facbegsys.insert(Symbol::Number);
    parser.facbegsys.insert(Symbol::LParen);

    let mut symset = SymSet::new();
    symset.insert(Symbol::Period);
    symset.insert(Symbol::BeginSym);
    symset.insert(Symbol::CallSym);
    symset.insert(Symbol::IfSym);
    symset.insert(Symbol::WhileSym);
    symset.insert(Symbol::ConstSym);
    symset.insert(Symbol::VarSym);
    symset.insert(Symbol::ProcSym);

    parser.block(symset);
    //TODO
    // if sym<> period then
    //   error(9);
    // if err = 0 then
    //   interpret
    // else
    //   write( err, ' errors found' );
}

impl Parser {
    /// Error Handler
    fn error(&mut self, n: usize) {
        self.error_count += 1;
        //TODO print error messages
    }

    fn get_char(&mut self) -> char {
        //TODO
        // if input is exhausted println!("Program incomplete") and exit
        // else get the next character, write it to output and return it
        return self.input.remove(0);
    }

    /// Tokenizer / Scanner
    /// Reads from self.input and sets self.current_symbol
    fn get_symbol(&mut self) {
        fn is_identifier(c: char) -> bool {
            is_identifier_start(c) || is_number(c)
        }

        fn is_identifier_start(c: char) -> bool {
            c >= 'a' && c <= 'z'
        }

        fn is_number(c: char) -> bool {
            c >= '0' && c <= '9'
        }

        let mut current = self.get_char();
        while current == ' ' {
            current = self.get_char();
        }

        let mut identifier = String::new();
        if is_identifier_start(current) {
            // identifier or reserved word
            let k = 0;
            while is_identifier(current) {
                if k < MAX_IDENTIFIER_LEN {
                    identifier.push(current);
                }
                current = self.get_char();
            }

            self.current_symbol = match &identifier[..] {
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
            };
            self.last_id = identifier;
            return;
        } else if is_number(current) {
            let mut num_string = String::new();
            while is_number(current) {
                num_string.push(current);
                current = self.get_char();
            }
            let number = match num_string.parse::<usize>() {
                Err(y) => panic!("parsing invalid number: {}", y.description()),
                Ok(n) => n,
            };

            if number > 99999999999999 {
                self.error(30);
            }

            self.current_symbol = Symbol::Number;
            self.last_num = number;
            return;
        } else if current == ':' {
            current = self.get_char();
            if current == '=' {
                // pascal has a getch here
                self.current_symbol = Symbol::Becomes;
                return;
            } else {
                self.current_symbol = Symbol::Nul;
                return;
            }
        } else if current == '<' {
            current = self.get_char();
            if current == '=' {
                // pascal has a getch here
                self.current_symbol = Symbol::Leq;
                return;
            } else {
                self.current_symbol = Symbol::Lss;
                return;
            }
        } else if current == '>' {
            current = self.get_char();
            if current == '=' {
                // pascal has a getch here
                self.current_symbol = Symbol::Geq;
                return;
            } else {
                self.current_symbol = Symbol::Gtr;
                return;
            }
        } else {
            self.current_symbol = match current {
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
            };
            return;
        }
    }

    /// Code Generator
    fn gen(&mut self, x: Fct, y: usize, z: usize) -> Instruction {
        if self.code_index > CXMAX {
            panic!("Program too long");
        }

        self.code_words[self.code_index] = Some(Instruction {
            f: x,
            l: y,
            a: z,
        });

        self.code_index += 1;
        Instruction {
            f: x,
            l: y,
            a: z,
        }
    }

    /// Parser
    fn test(&mut self, s1: &SymSet, s2: &SymSet, n: usize) {
        if !s1.contains(&self.current_symbol) {
            self.error(n);
            while !s1.contains(&self.current_symbol) && s2.contains(&self.current_symbol) {
                self.get_symbol();
            }
        }
    }

    /// Find identifier location in table
    fn position(&self, id: String) -> usize {
        for (i, entry) in self.ident_table.iter().enumerate() {
            match *entry {
                Some(ref e) if e.name == id => return i,
                _ => {},
            }
        }

        return TXMAX;
    }

    /// function to declare a constant
    fn const_declaration(&mut self) {
        if self.current_symbol == Symbol::Ident {
            self.get_symbol();

            if self.current_symbol == Symbol::Eql || self.current_symbol == Symbol::Becomes {
                if self.current_symbol == Symbol::Becomes{
                    self.error(1);
                }

                self.get_symbol();

                if self.current_symbol == Symbol::Number {
                    self.enter(Obj::Constant);
                    self.get_symbol();
                } else {
                    self.error(2);
                }
            } else {
                self.error(4);
            }
        }
    }

    /// Variable declaration
    fn var_declaration(&mut self) {
        if self.current_symbol == Symbol::Ident {
            self.enter(Obj::Variable);
            self.get_symbol();
        } else {
            self.error(4);
        }
    }

    /// Parser
    fn block(&mut self, fsys: SymSet) {
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

        self.current_level += 1;
        self.data_index = 3; // data allocation index
        let tx0 = self.table_index; // initial table index

        match self.ident_table[self.table_index] {
            Some(ref mut i) => i.adr = self.code_index,
            None => {},
        }

        self.gen(Fct::Jmp, 0, 0);

        if self.current_level > LEVMAX {
            self.error(32);
        }

        self.get_symbol();

        loop {
            if self.current_symbol == Symbol::ConstSym {
                self.get_symbol();

                loop {
                    self.const_declaration();
                    while self.current_symbol == Symbol::Comma {
                        self.get_symbol();
                        self.const_declaration();
                    }

                    if self.current_symbol == Symbol::Semicolon {
                        self.get_symbol();
                    } else {
                        self.error(5);
                    }

                    if self.current_symbol == Symbol::Ident {
                        break;
                    }
                }
            }

            if self.current_symbol == Symbol::VarSym {
                self.get_symbol();
                loop {
                    self.var_declaration();

                    while self.current_symbol == Symbol::Comma {
                        self.get_symbol();
                        self.var_declaration();
                    }

                    if self.current_symbol == Symbol::Semicolon {
                        self.get_symbol();
                    } else {
                        self.error(5);
                    }

                    if self.current_symbol == Symbol::Comma {
                        break;
                    }
                }
            }

            while self.current_symbol == Symbol::ProcSym {
                self.get_symbol();

                if self.current_symbol == Symbol::Ident {
                    self.enter(Obj::Prozedure);
                    self.get_symbol();
                } else {
                    self.error(4);
                }

                if self.current_symbol == Symbol::Semicolon {
                    self.get_symbol();
                } else {
                    self.error(5);
                }

                let mut new_fsys = fsys.clone();
                new_fsys.insert(Symbol::Semicolon);

                self.block(new_fsys);

                if self.current_symbol == Symbol::Semicolon {
                    self.get_symbol();

                    let mut statbegsys1 = self.statbegsys.clone();
                    statbegsys1.insert(Symbol::Ident);
                    statbegsys1.insert(Symbol::ProcSym);
                    self.test(&statbegsys1, &fsys, 6);
                } else {
                    self.error(5);
                }
            }

            // messy, fix test function maybe
            let mut statbegsys2 = self.statbegsys.clone();
            let declbegsys = self.declbegsys.clone();
            statbegsys2.insert(Symbol::Ident);
            self.test(&statbegsys2, &declbegsys, 7);

            if self.declbegsys.contains(&self.current_symbol) {
                break;
            }
        }

        match self.ident_table[tx0] {
            Some(ref mut e) => {
                match self.code_words[e.adr] {
                    Some(ref mut i) => i.a = self.code_index,
                    None => {},
                }
                e.adr = self.code_index;
            },
            None => {},
        }

        let cx0 = self.code_index;

        let dx = self.data_index;
        self.gen(Fct::Int, 0, dx);

        let mut new_fsys2 = fsys.clone();
        new_fsys2.insert(Symbol::Semicolon);
        new_fsys2.insert(Symbol::EndSym);
        statement(new_fsys2);

        self.gen(Fct::Ret, 0, 0);

        self.test(&fsys, &SymSet::new(), 8);

        if self.error_count == 0{
            self.listcode(cx0);
        }

        self.current_level -= 1;
    }

    /// List code generated for this block
    /// Debug generated byte code
    fn listcode(&self, code_start: usize) {
        for i in code_start..self.code_index - 1 {
            match self.code_words[i] {
                Some(ref instr) => println!("{:?}", instr),
                None => {},
            };
        }
    }

    /// Enter object into table
    fn enter(&mut self, k: Obj) {
        self.table_index += 1;
        self.ident_table[self.table_index] = match k {
            Obj::Constant => {
                if self.last_num > AMAX {
                    self.error(31);
                    self.last_num = 0;
                }

                Some(Entry {
                    name: self.last_id.clone(),
                    kind: Obj::Constant,
                    val: self.last_num,
                    level: 0,
                    adr: 0,
                })
            },
            Obj::Variable => {
                let e = Some(Entry {
                    name: self.last_id.clone(),
                    kind: Obj::Variable,
                    val: 0,
                    level: self.current_level,
                    adr: self.data_index,
                });

                self.data_index += 1;
                e
            },
            Obj::Prozedure => Some(Entry {
                name: self.last_id.clone(),
                kind: Obj::Prozedure,
                val: 0,
                level: self.current_level,
                adr: 0,
            })
        }
    }
}

/// Byte Code Interpreter
fn interpret() {

}
