// PL/0 compiler with code generation
// translated from pascal Program 5.6 in Algorithms + Data Structures = Programs

use std::collections::{HashSet};
use std::fs::File;
use std::path::Path;
use std::io::Read;
use std::error::Error;
use std::fmt;

type SymSet = HashSet<Symbol>;

const TXMAX: usize = 100; // length of identifier table
const MAX_IDENTIFIER_LEN: usize = 10;     // length of identifiers
const AMAX: usize = 2047; // maximum address
const LEVMAX: usize = 3;  // maximum depth of block nesting
const CXMAX: usize = 200; // size of code array

#[derive(Clone, Copy, Eq, PartialEq, Hash, Debug)]
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

#[derive(Clone, PartialEq)]
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

#[derive(Clone)]
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
    current_char: char,
    current_symbol: Symbol,
    current_level: usize,
    last_id: String,
    last_num: usize,
    input: String,
    code_index: usize,
    data_index: usize,
    table_index: usize,
    error_count: usize,
    char_count: usize,
    declbegsys: SymSet,
    statbegsys: SymSet,
    facbegsys: SymSet,
}

fn initialize_parser(input: String) -> Parser {
    let mut parser = Parser {
        code_words: Vec::with_capacity(CXMAX),
        ident_table: Vec::with_capacity(TXMAX),
        input: input,
        current_char: ' ',
        current_symbol: Symbol::Nul,
        current_level: 0,
        last_id: String::new(),
        last_num: 0,
        code_index: 0,
        data_index: 0,
        table_index: 0,
        error_count: 0,
        char_count: 0,
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

    parser
}

fn initialize_symset() -> SymSet {
    let mut symset = SymSet::new();
    symset.insert(Symbol::Period);
    symset.insert(Symbol::BeginSym);
    symset.insert(Symbol::CallSym);
    symset.insert(Symbol::IfSym);
    symset.insert(Symbol::WhileSym);
    symset.insert(Symbol::ConstSym);
    symset.insert(Symbol::VarSym);
    symset.insert(Symbol::ProcSym);

    symset
}

fn main() {
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

    let mut parser = initialize_parser(input_str);
    let symset = initialize_symset();

    parser.get_symbol();
    parser.block(symset);

    if parser.current_symbol != Symbol::Period {
        parser.error(9);
    }
    if parser.error_count == 0 {
        interpret();
    } else {
        print!("{} errors found", parser.error_count);
    }
    println!("");
}

impl Parser {
    /// Error Handler
    fn error(&mut self, n: usize) {
        let mut spaces = String::new();

        for _ in 0..self.char_count {
            spaces.push(' ');
        }

        print!("*****{}^ ", spaces);

        match n {
            1 => println!("use = instead of :="),
            2 => println!("= must be followed by a number"),
            3 => println!("identifier must be followed by ="),
            4 => println!("const, var, procedure must be followed by an identifier"),
            5 => println!("semicolon or comma missing"),
            6 => println!("incorrect symbol after procedure declaration"),
            7 => println!("statement expected"),
            8 => println!("incorrect symbol after statement part in block"),
            9 => println!("period expected"),
           10 => println!("semicolon between statements is missing"),
           11 => println!("undeclared identifier"),
           12 => println!("assignment to constant or procedure is not allowed"),
           13 => println!("assignment operator := expected"),
           14 => println!("call must be followed by an identifier"),
           15 => println!("call of a constant or a variable is meaningless"),
           16 => println!("then expected"),
           17 => println!("semicolon or end expected"),
           18 => println!("do expected"),
           19 => println!("incorrect symbol following statement"),
           20 => println!("relational operator expected"),
           21 => println!("expression must not contain a procedure identifier"),
           22 => println!("right paranthesis missing"),
           23 => println!("the preceding factor cannot be followed by this symbol"),
           24 => println!("an expression cannot begin with this symbol"),
           30 => println!("this number is too large"),
           31 => println!("this constant is too large"),
           32 => println!("too many lexical levels"),
           _ => {},
        }
        self.error_count += 1;
    }

    fn get_char(&mut self) {
        self.current_char = self.input.remove(0);

        if self.current_char == '\n' {
            println!("");
            self.char_count = 0;
        } else {
            print!("{}", self.current_char);
            self.char_count += 1;
        }
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

        while self.current_char.is_whitespace() {
            self.get_char();
        }

        let mut identifier = String::new();
        if is_identifier_start(self.current_char) {
            // identifier or reserved word
            loop {
                if identifier.len() < MAX_IDENTIFIER_LEN {
                    identifier.push(self.current_char);
                }
                self.get_char();

                if !is_identifier(self.current_char) {
                    break;
                }
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
        } else {
            if is_number(self.current_char) {
                self.current_symbol = Symbol::Number;

                let mut num_string = String::new();
                while is_number(self.current_char) {
                    num_string.push(self.current_char);
                    self.get_char();
                }
                let number = match num_string.parse::<usize>() {
                    Err(y) => panic!("parsing invalid number: {}", y.description()),
                    Ok(n) => n,
                };

                if number > 99999999999999 {
                    self.error(30);
                }

                self.last_num = number;
            } else {
                if self.current_char == ':' {
                    self.get_char();
                    if self.current_char == '=' {
                        self.current_symbol = Symbol::Becomes;
                        self.get_char();
                    } else {
                        self.current_symbol = Symbol::Nul;
                    }
                } else {
                    if self.current_char == '<' {
                        self.get_char();
                        if self.current_char == '=' {
                            self.current_symbol = Symbol::Leq;
                            self.get_char();
                        } else {
                            self.current_symbol = Symbol::Lss;
                        }
                    } else {
                        if self.current_char == '>' {
                            self.get_char();
                            if self.current_char == '=' {
                                self.current_symbol = Symbol::Geq;
                                self.get_char();
                            } else {
                                self.current_symbol = Symbol::Gtr;
                            }
                        } else {
                            self.current_symbol = match self.current_char {
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
                            self.get_char();
                        }
                    }
                }
            }
        }
    }

    /// Code Generator
    fn gen(&mut self, x: Fct, y: usize, z: usize) {
        if self.code_index > CXMAX {
            panic!("Program too long");
        }

        self.code_words[self.code_index] = Some(Instruction {
            f: x,
            l: y,
            a: z,
        });

        self.code_index += 1;
    }

    /// Parser
    fn test(&mut self, s1: &SymSet, s2: &SymSet, n: usize) {
        if !s1.contains(&self.current_symbol) {
            self.error(n);
            while !(s1.contains(&self.current_symbol) || s2.contains(&self.current_symbol)) {
                self.get_symbol();
            }
        }
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

    /// Find identifier location in table
    fn position(&self, id: &str) -> usize {
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
                self.error(3);
            }
        } else {
            self.error(4);
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

    fn factor(&mut self, fsys: SymSet) {
        let symbol_set = self.facbegsys.clone();
        self.test(&symbol_set, &fsys, 24);

        while self.facbegsys.contains(&self.current_symbol) {
            if self.current_symbol == Symbol::Ident {
                let i = self.position(&self.last_id);
                if i == TXMAX {
                    self.error(11);
                } else {
                    // entry should exist, safe to unwrap
                    let entry = self.ident_table[i].clone().unwrap();
                    let level = self.current_level;

                    match entry.kind {
                        Obj::Constant => { self.gen(Fct::Lit, 0, entry.val); },
                        Obj::Variable => { self.gen(Fct::Lod, level - entry.level, entry.adr); },
                        Obj::Prozedure => { self.error(21); },
                    }
                    self.get_symbol();
                }
            } else {
                if self.current_symbol == Symbol::Number {
                    if self.last_num > AMAX {
                        self.error(31);
                        self.last_num = 0;
                    }
                    let last_num = self.last_num;
                    self.gen(Fct::Lit, 0, last_num);
                    self.get_symbol();
                } else {
                    if self.current_symbol == Symbol::LParen {
                        self.get_symbol();

                        let mut new_fsys = fsys.clone();
                        new_fsys.insert(Symbol::RParen);
                        self.expression(new_fsys);

                        if self.current_symbol == Symbol::RParen {
                            self.get_symbol();
                        } else {
                            self.error(22);
                        }
                    }
                }
            }

            let mut lparen_set = SymSet::new();
            lparen_set.insert(Symbol::LParen);
            self.test(&fsys, &lparen_set, 23);
        }
    }

    fn term(&mut self, fsys: SymSet) {
        let mut new_fsys1 = fsys.clone();
        new_fsys1.insert(Symbol::Times);
        new_fsys1.insert(Symbol::Slash);
        self.factor(new_fsys1);

        while self.current_symbol == Symbol::Times || self.current_symbol == Symbol::Slash {
            let mulop = self.current_symbol;
            self.get_symbol();

            let mut new_fsys2 = fsys.clone();
            new_fsys2.insert(Symbol::Times);
            new_fsys2.insert(Symbol::Slash);
            self.factor(new_fsys2);

            if mulop == Symbol::Times {
                self.gen(Fct::Opr, 0, 4);
            } else {
                // divide
                self.gen(Fct::Opr, 0, 5);
            }
        }
    }

    fn expression(&mut self, fsys: SymSet) {
        fn is_plus_minus(s: Symbol) -> bool {
            s == Symbol::Plus || s == Symbol::Minus
        }

        if is_plus_minus(self.current_symbol) {
            let addop = self.current_symbol;

            self.get_symbol();

            let mut new_set1 = fsys.clone();
            new_set1.insert(Symbol::Plus);
            new_set1.insert(Symbol::Minus);
            self.term(new_set1);

            if addop == Symbol::Minus {
                self.gen(Fct::Opr, 0, 1);
            }
        } else {
            let mut new_set2 = fsys.clone();
            new_set2.insert(Symbol::Plus);
            new_set2.insert(Symbol::Minus);
            self.term(new_set2);
        }

        while is_plus_minus(self.current_symbol) {
            let addop = self.current_symbol;

            self.get_symbol();

            let mut new_set3 = fsys.clone();
            new_set3.insert(Symbol::Plus);
            new_set3.insert(Symbol::Minus);
            self.term(new_set3);

            if addop == Symbol::Plus {
                self.gen(Fct::Opr, 0, 2);
            } else {
                // minus
                self.gen(Fct::Opr, 0, 2);
            }
        }
    }

    fn condition(&mut self, fsys: SymSet) {
        fn is_conditional(s: Symbol) -> bool {
            s == Symbol::Eql || s == Symbol::Neq || s == Symbol::Lss ||
            s == Symbol::Leq || s == Symbol::Gtr || s == Symbol::Geq
        }
        if self.current_symbol == Symbol::OddSym {
            self.get_symbol();
            self.expression(fsys);
            self.gen(Fct::Opr, 0, 6) // odd
        } else {
            let mut new_set1 = fsys.clone();
            new_set1.insert(Symbol::Eql);
            new_set1.insert(Symbol::Neq);
            new_set1.insert(Symbol::Lss);
            new_set1.insert(Symbol::Gtr);
            new_set1.insert(Symbol::Leq);
            new_set1.insert(Symbol::Geq);
            self.expression(new_set1);

            if !is_conditional(self.current_symbol) {
                self.error(20);
            } else {
                let relop = self.current_symbol;
                self.get_symbol();
                self.expression(fsys);
                match relop {
                    Symbol::Eql => self.gen(Fct::Opr, 0, 8), // =
                    Symbol::Neq => self.gen(Fct::Opr, 0, 9), // #
                    Symbol::Lss => self.gen(Fct::Opr, 0, 10), // <
                    Symbol::Geq => self.gen(Fct::Opr, 0, 11), // >=
                    Symbol::Gtr => self.gen(Fct::Opr, 0, 12), // >
                    Symbol::Leq => self.gen(Fct::Opr, 0, 13), // <=
                    _ => {},
                }
            }
        }
    }

    fn statement(&mut self, fsys: SymSet) {
        let test_symset = fsys.clone();

        if self.current_symbol == Symbol::Ident {
            let mut i = self.position(&self.last_id);

            if i == TXMAX {
                self.error(11);
            } else {
                //entry should exist so unwrap should be safe
                let entry = self.ident_table[i].clone().unwrap();
                if entry.kind != Obj::Variable {
                    self.error(12);
                    i = 0;
                }
            }

            self.get_symbol();

            if self.current_symbol == Symbol::Becomes {
                self.get_symbol();
            } else {
                self.error(13);
            }

            self.expression(fsys);

            if i != 0 {
                // as above should be safe to unwrap
                let entry = self.ident_table[i].clone().unwrap();
                let level = self.current_level;
                self.gen(Fct::Sto, level - entry.level, entry.adr);
            }
        } else {
            if self.current_symbol == Symbol::CallSym {
                self.get_symbol();
                if self.current_symbol != Symbol::Ident {
                    self.error(14);
                } else {
                    let i = self.position(&self.last_id);
                    if i == TXMAX {
                        self.error(11);
                    } else {
                        // entry should exist and be safe to unwrap
                        let entry = self.ident_table[i].clone().unwrap();
                        let level = self.current_level;
                        if entry.kind == Obj::Prozedure {
                            self.gen(Fct::Cal, level - entry.level, entry.adr);
                        } else {
                            self.error(15);
                        }
                        self.get_symbol();
                    }
                }
            } else {
                if self.current_symbol == Symbol::IfSym {
                    self.get_symbol();

                    let mut new_symset = fsys.clone();
                    new_symset.insert(Symbol::ThenSym);
                    new_symset.insert(Symbol::DoSym);
                    self.condition(new_symset);

                    if self.current_symbol == Symbol::ThenSym {
                        self.get_symbol();
                    } else {
                        self.error(16);
                    }

                    let init_code_index = self.code_index;
                    self.gen(Fct::Jpc, 0, 0);
                    self.statement(fsys);

                    let entry = &mut self.code_words[init_code_index];
                    match *entry {
                        Some(ref mut e) => e.a = self.code_index,
                        None => {},
                    }
                } else {
                    if self.current_symbol == Symbol::BeginSym {
                        self.get_symbol();

                        let mut new_symset1 = fsys.clone();
                        new_symset1.insert(Symbol::Semicolon);
                        new_symset1.insert(Symbol::EndSym);
                        self.statement(new_symset1);

                        while self.statbegsys.contains(&self.current_symbol) || self.current_symbol == Symbol::Semicolon {
                            if self.current_symbol == Symbol::Semicolon {
                                self.get_symbol();
                            } else {
                                self.error(10);
                            }

                            let mut new_symset2 = fsys.clone();
                            new_symset2.insert(Symbol::Semicolon);
                            new_symset2.insert(Symbol::EndSym);
                            self.statement(new_symset2);
                        }

                        if self.current_symbol == Symbol::EndSym {
                            self.get_symbol();
                        } else {
                            self.error(17);
                        }
                    } else {
                        if self.current_symbol == Symbol::WhileSym {
                            let code_index1 = self.code_index;

                            self.get_symbol();
                            let mut new_symset3 = fsys.clone();
                            new_symset3.insert(Symbol::DoSym);
                            self.condition(new_symset3);

                            let code_index2 = self.code_index;
                            self.gen(Fct::Jpc, 0, 0);

                            if self.current_symbol == Symbol::DoSym {
                                self.get_symbol();
                            } else {
                                self.error(18);
                            }

                            self.statement(fsys);
                            self.gen(Fct::Jmp, 0, code_index1);
                            let entry = &mut self.code_words[code_index2];
                            match *entry {
                                Some(ref mut e) => e.a = self.code_index,
                                None => {},
                            }
                        }
                    }
                }
            }
        }
        self.test(&test_symset, &SymSet::new(), 19);
    }

    /// Parser
    fn block(&mut self, fsys: SymSet) {
        self.current_level += 1;
        self.data_index = 3; // data allocation index
        let init_table_index = self.table_index; // initial table index

        match self.ident_table[self.table_index] {
            Some(ref mut i) => i.adr = self.code_index,
            None => {},
        }

        self.gen(Fct::Jmp, 0, 0);

        if self.current_level > LEVMAX {
            self.error(32);
        }

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

                    if self.current_symbol != Symbol::Ident {
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

                    if self.current_symbol != Symbol::Ident {
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

                let mut new_set1 = fsys.clone();
                new_set1.insert(Symbol::Semicolon);
                self.block(new_set1);

                if self.current_symbol == Symbol::Semicolon {
                    self.get_symbol();

                    let mut new_set2 = self.statbegsys.clone();
                    new_set2.insert(Symbol::Ident);
                    new_set2.insert(Symbol::ProcSym);
                    self.test(&new_set2, &fsys, 6);
                } else {
                    self.error(5);
                }
            }

            let declbegsys1 = self.declbegsys.clone();
            let mut statbegsys1 = self.statbegsys.clone();
            statbegsys1.insert(Symbol::Ident);
            self.test(&statbegsys1, &declbegsys1, 7);

            if !self.declbegsys.contains(&self.current_symbol) {
                break;
            }
        }

        match self.ident_table[init_table_index] {
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

        let mut new_set3 = fsys.clone();
        new_set3.insert(Symbol::Semicolon);
        new_set3.insert(Symbol::EndSym);
        self.statement(new_set3);

        self.gen(Fct::Ret, 0, 0);

        self.test(&fsys, &SymSet::new(), 8);

        if self.error_count == 0{
            self.listcode(cx0);
        }

        self.current_level -= 1;
    }
}

/// Byte Code Interpreter
fn interpret() {

}

//TODO
//  -add line numbers / format output exactly
//  -add Ret 0 0 to the end of funcions
//  -write interpreter
