// PL/0 compiler with code generation
// translated from pascal Program 5.6 in Algorithms + Data Structures = Programs

use std::collections::{HashSet};
use std::io::{self, Read};
use std::error::Error;
use std::fmt;

type SymSet = HashSet<Symbol>;

const TXMAX: usize = 100; // length of identifier table
const MAX_IDENTIFIER_LEN: usize = 10;     // length of identifiers
const AMAX: usize = 2047; // maximum address
const LEVMAX: usize = 3;  // maximum depth of block nesting
const CXMAX: usize = 200; // size of code array
const STACKSIZE: usize = 500; // interpreter stack size

enum ParseError {
    Compare,
    CompareAfter,
    CompareBefore,
    IdentifierAfter,
    CommaMiss,
    ProcedureAfter,
    ExpectedStatement,
    StatementSymbol,
    EndProgram,
    StatementSeperator,
    UndeclaredIdentifier,
    NoConstAssign,
    Assign,
    CallAfter,
    CallConst,
    ExpectedThen,
    ExpectedEnd,
    ExpectedDo,
    StatementAfter,
    ExpectedRelation,
    ProcedureReserved,
    RightParenMiss,
    FactorAfter,
    ExpressionBegin,
    NumOverflow,
    ConstOverflow,
    LexicalOverflow,
}

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
    Procedure,
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
            Fct::Lit => "lit",
            Fct::Opr => "opr",
            Fct::Lod => "lod",
            Fct::Sto => "sto",
            Fct::Cal => "cal",
            Fct::Ret => "ret",
            Fct::Int => "int",
            Fct::Jmp => "jmp",
            Fct::Jpc => "jpc",
        };
        write!(f, "{}", fct)
    }
}

struct Instruction {
    function: Fct,
    level: usize, // [0, LEVMAX)
    address: usize, // [0, AMAZ)
}
impl std::fmt::Debug for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, " {:?}{:4}{:5}", self.function, self.level, self.address)
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
    data_indices: Vec<usize>,
    table_index: usize,
    error_count: usize,
    err_msgs: Vec<String>,
    char_count: usize,
    line_start: bool,
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
        data_indices: Vec::new(),
        table_index: 0,
        error_count: 0,
        err_msgs: Vec::new(),
        char_count: 0,
        line_start: true,
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
    let mut input_str = String::new();
    match io::stdin().read_to_string(&mut input_str) {
        Err(y) => panic!("error reading iput file: {}", y.description()),
        Ok(_) => {},
    }

    let mut parser = initialize_parser(input_str);
    let symset = initialize_symset();

    parser.get_symbol();
    parser.block(symset);

    if parser.current_symbol != Symbol::Period {
        parser.error(ParseError::EndProgram);
    }
    if parser.error_count == 0 {
        interpret(&parser.code_words);
    } else {
        print!("\n{} errors found", parser.error_count);
    }
    println!("");
}

impl Parser {
    /// Error Handler
    fn error(&mut self, n: ParseError) {
        let mut err_msg = String::from("*****");

        for _ in 0..self.char_count {
            err_msg.push(' ');
        }

        err_msg.push_str("^ ");

        let err_str = match n {
            ParseError::Compare => "use = instead of :=",
            ParseError::CompareAfter => "= must be followed by a number",
            ParseError::CompareBefore => "identifier must be followed by =",
            ParseError::IdentifierAfter => "const, var, procedure must be followed by an identifier",
            ParseError::CommaMiss => "semicolon or comma missing",
            ParseError::ProcedureAfter => "incorrect symbol after procedure declaration",
            ParseError::ExpectedStatement => "statement expected",
            ParseError::StatementSymbol => "incorrect symbol after statement part in block",
            ParseError::EndProgram => "period expected",
            ParseError::StatementSeperator => "semicolon between statements is missing",
            ParseError::UndeclaredIdentifier => "undeclared identifier",
            ParseError::NoConstAssign => "assignment to constant or procedure is not allowed",
            ParseError::Assign => "assignment operator := expected",
            ParseError::CallAfter => "call must be followed by an identifier",
            ParseError::CallConst => "call of a constant or a variable is meaningless",
            ParseError::ExpectedThen => "then expected",
            ParseError::ExpectedEnd => "semicolon or end expected",
            ParseError::ExpectedDo => "do expected",
            ParseError::StatementAfter => "incorrect symbol following statement",
            ParseError::ExpectedRelation => "relational operator expected",
            ParseError::ProcedureReserved => "expression must not contain a procedure identifier",
            ParseError::RightParenMiss => "right paranthesis missing",
            ParseError::FactorAfter => "the preceding factor cannot be followed by this symbol",
            ParseError::ExpressionBegin => "an expression cannot begin with this symbol",
            ParseError::NumOverflow => "this number is too large",
            ParseError::ConstOverflow => "this constant is too large",
            ParseError::LexicalOverflow => "too many lexical levels",
        };
        err_msg.push_str(err_str);

        self.err_msgs.push(err_msg);
        self.error_count += 1;
    }

    fn get_char(&mut self) {
        self.current_char = self.input.remove(0);

        if self.line_start {
            for msg in self.err_msgs.drain(..) {
                println!("{}", msg);
            }

            print!("{:5} ", self.code_index);
            self.char_count = 0;
        } else {
            self.char_count += 1;
        }

        print!("{}", self.current_char);

        self.line_start = self.current_char == '\n';
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
                    self.error(ParseError::NumOverflow);
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
    fn generate_instruction(&mut self, f: Fct, l: usize, a: usize) {
        if self.code_index > CXMAX {
            panic!("Program too long");
        }

        self.code_words[self.code_index] = Some(Instruction {
            function: f,
            level: l,
            address: a,
        });

        self.code_index += 1;
    }

    /// Parser
    fn test(&mut self, s1: &SymSet, s2: &SymSet, e: ParseError) {
        if !s1.contains(&self.current_symbol) {
            self.error(e);
            while !(s1.contains(&self.current_symbol) || s2.contains(&self.current_symbol)) {
                self.get_symbol();
            }
        }
    }

    /// List code generated for this block
    /// Debug generated byte code
    fn list_code(&self, code_start: usize) {
        for i in code_start..self.code_index {
            match self.code_words[i] {
                Some(ref instr) => println!("{}{:?}", i, instr),
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
                    self.error(ParseError::ConstOverflow);
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
                let data_indices = self.data_indices.pop().unwrap();
                let e = Some(Entry {
                    name: self.last_id.clone(),
                    kind: Obj::Variable,
                    val: 0,
                    level: self.current_level,
                    adr: data_indices,
                });

                self.data_indices.push(data_indices + 1);
                e
            },
            Obj::Procedure => Some(Entry {
                name: self.last_id.clone(),
                kind: Obj::Procedure,
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
                    self.error(ParseError::Compare);
                }

                self.get_symbol();

                if self.current_symbol == Symbol::Number {
                    self.enter(Obj::Constant);
                    self.get_symbol();
                } else {
                    self.error(ParseError::CompareAfter);
                }
            } else {
                self.error(ParseError::CompareBefore);
            }
        } else {
            self.error(ParseError::IdentifierAfter);
        }
    }

    /// Variable declaration
    fn var_declaration(&mut self) {
        if self.current_symbol == Symbol::Ident {
            self.enter(Obj::Variable);
            self.get_symbol();
        } else {
            self.error(ParseError::IdentifierAfter);
        }
    }

    fn factor(&mut self, fsys: SymSet) {
        let symbol_set = self.facbegsys.clone();
        self.test(&symbol_set, &fsys, ParseError::ExpressionBegin);

        while self.facbegsys.contains(&self.current_symbol) {
            if self.current_symbol == Symbol::Ident {
                let i = self.position(&self.last_id);
                if i == TXMAX {
                    self.error(ParseError::UndeclaredIdentifier);
                } else {
                    // entry should exist, safe to unwrap
                    let entry = self.ident_table[i].clone().unwrap();
                    let level = self.current_level;

                    match entry.kind {
                        Obj::Constant => { self.generate_instruction(Fct::Lit, 0, entry.val); },
                        Obj::Variable => { self.generate_instruction(Fct::Lod, level - entry.level, entry.adr); },
                        Obj::Procedure => { self.error(ParseError::ProcedureReserved); },
                    }
                }
                self.get_symbol();
            } else {
                if self.current_symbol == Symbol::Number {
                    if self.last_num > AMAX {
                        self.error(ParseError::ConstOverflow);
                        self.last_num = 0;
                    }
                    let last_num = self.last_num;
                    self.generate_instruction(Fct::Lit, 0, last_num);
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
                            self.error(ParseError::RightParenMiss);
                        }
                    }
                }
            }

            let mut lparen_set = SymSet::new();
            lparen_set.insert(Symbol::LParen);
            self.test(&fsys, &lparen_set, ParseError::FactorAfter);
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
                self.generate_instruction(Fct::Opr, 0, 4);
            } else {
                // divide
                self.generate_instruction(Fct::Opr, 0, 5);
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
                self.generate_instruction(Fct::Opr, 0, 1);
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
                self.generate_instruction(Fct::Opr, 0, 2);
            } else {
                // minus
                self.generate_instruction(Fct::Opr, 0, 3);
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
            self.generate_instruction(Fct::Opr, 0, 6) // odd
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
                self.error(ParseError::ExpectedRelation);
            } else {
                let relop = self.current_symbol;
                self.get_symbol();
                self.expression(fsys);
                match relop {
                    Symbol::Eql => self.generate_instruction(Fct::Opr, 0, 8), // =
                    Symbol::Neq => self.generate_instruction(Fct::Opr, 0, 9), // #
                    Symbol::Lss => self.generate_instruction(Fct::Opr, 0, 10), // <
                    Symbol::Geq => self.generate_instruction(Fct::Opr, 0, 11), // >=
                    Symbol::Gtr => self.generate_instruction(Fct::Opr, 0, 12), // >
                    Symbol::Leq => self.generate_instruction(Fct::Opr, 0, 13), // <=
                    _ => {},
                }
            }
        }
    }

    fn statement(&mut self, fsys: SymSet) {
        let test_symset = fsys.clone();

        match self.current_symbol {
            Symbol::Ident => {
                let mut i = self.position(&self.last_id);

                if i == TXMAX {
                    self.error(ParseError::UndeclaredIdentifier);
                } else {
                    //entry should exist so unwrap should be safe
                    let entry = self.ident_table[i].clone().unwrap();
                    if entry.kind != Obj::Variable {
                        self.error(ParseError::NoConstAssign);
                        i = 0;
                    }
                }

                self.get_symbol();

                if self.current_symbol == Symbol::Becomes {
                    self.get_symbol();
                } else {
                    self.error(ParseError::Assign);
                }

                self.expression(fsys);

                if i != 0 {
                    // as above should be safe to unwrap
                    let entry = self.ident_table[i].clone().unwrap();
                    let level = self.current_level;
                    self.generate_instruction(Fct::Sto, level - entry.level, entry.adr);
                }
            },
            Symbol::CallSym => {
                self.get_symbol();
                if self.current_symbol != Symbol::Ident {
                    self.error(ParseError::CallAfter);
                } else {
                    let i = self.position(&self.last_id);
                    if i == TXMAX {
                        self.error(ParseError::UndeclaredIdentifier);
                    } else {
                        // entry should exist and be safe to unwrap
                        let entry = self.ident_table[i].clone().unwrap();
                        let level = self.current_level;
                        if entry.kind == Obj::Procedure {
                            self.generate_instruction(Fct::Cal, level - entry.level, entry.adr);
                        } else {
                            self.error(ParseError::CallConst);
                        }
                        self.get_symbol();
                    }
                }
            },
            Symbol::IfSym => {
                self.get_symbol();

                let mut new_symset = fsys.clone();
                new_symset.insert(Symbol::ThenSym);
                new_symset.insert(Symbol::DoSym);
                self.condition(new_symset);

                if self.current_symbol == Symbol::ThenSym {
                    self.get_symbol();
                } else {
                    self.error(ParseError::ExpectedThen);
                }

                let init_code_index = self.code_index;
                self.generate_instruction(Fct::Jpc, 0, 0);
                self.statement(fsys);

                let entry = &mut self.code_words[init_code_index];
                match *entry {
                    Some(ref mut e) => e.address = self.code_index,
                    None => {},
                }
            },
            Symbol::BeginSym => {
                let program_main = self.code_index - 1;
                self.get_symbol();

                let mut new_symset1 = fsys.clone();
                new_symset1.insert(Symbol::Semicolon);
                new_symset1.insert(Symbol::EndSym);
                self.statement(new_symset1);

                while self.statbegsys.contains(&self.current_symbol) || self.current_symbol == Symbol::Semicolon {
                    if self.current_symbol == Symbol::Semicolon {
                        self.get_symbol();
                    } else {
                        self.error(ParseError::StatementSeperator);
                    }

                    let mut new_symset2 = fsys.clone();
                    new_symset2.insert(Symbol::Semicolon);
                    new_symset2.insert(Symbol::EndSym);
                    self.statement(new_symset2);
                }

                if self.current_symbol == Symbol::EndSym {
                    self.get_symbol();
                    if self.current_symbol == Symbol::Period {
                        let entry = &mut self.code_words[0];
                        match *entry {
                            Some(ref mut e) => e.address = program_main,
                            None => {},
                        }
                    }
                } else {
                    self.error(ParseError::ExpectedEnd);
                }
            },
            Symbol::WhileSym => {
                let code_index1 = self.code_index;

                self.get_symbol();
                let mut new_symset3 = fsys.clone();
                new_symset3.insert(Symbol::DoSym);
                self.condition(new_symset3);

                let code_index2 = self.code_index;
                self.generate_instruction(Fct::Jpc, 0, 0);

                if self.current_symbol == Symbol::DoSym {
                    self.get_symbol();
                } else {
                    self.error(ParseError::ExpectedDo);
                }

                self.statement(fsys);
                self.generate_instruction(Fct::Jmp, 0, code_index1);
                let entry = &mut self.code_words[code_index2];
                match *entry {
                    Some(ref mut e) => e.address = self.code_index,
                    None => {},
                }
            },
            _ => {},
        }

        self.test(&test_symset, &SymSet::new(), ParseError::StatementAfter);
    }

    /// Parser
    fn block(&mut self, fsys: SymSet) {
        self.current_level += 1;
        self.data_indices.push(3); // data allocation index
        let init_table_index = self.table_index; // initial table index

        match self.ident_table[self.table_index] {
            Some(ref mut i) => i.adr = self.code_index,
            None => {},
        }

        self.generate_instruction(Fct::Jmp, 0, 0);

        if self.current_level > LEVMAX {
            self.error(ParseError::LexicalOverflow);
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
                        self.error(ParseError::CommaMiss);
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
                        self.error(ParseError::CommaMiss);
                    }

                    if self.current_symbol != Symbol::Ident {
                        break;
                    }
                }
            }

            while self.current_symbol == Symbol::ProcSym {
                self.get_symbol();

                if self.current_symbol == Symbol::Ident {
                    self.enter(Obj::Procedure);
                    self.get_symbol();
                } else {
                    self.error(ParseError::IdentifierAfter);
                }

                if self.current_symbol == Symbol::Semicolon {
                    self.get_symbol();
                } else {
                    self.error(ParseError::CommaMiss);
                }

                let mut new_set1 = fsys.clone();
                new_set1.insert(Symbol::Semicolon);
                self.block(new_set1);

                if self.current_symbol == Symbol::Semicolon {
                    self.get_symbol();

                    let mut new_set2 = self.statbegsys.clone();
                    new_set2.insert(Symbol::Ident);
                    new_set2.insert(Symbol::ProcSym);
                    self.test(&new_set2, &fsys, ParseError::ProcedureAfter);
                } else {
                    self.error(ParseError::CommaMiss);
                }
            }

            let declbegsys1 = self.declbegsys.clone();
            let mut statbegsys1 = self.statbegsys.clone();
            statbegsys1.insert(Symbol::Ident);
            self.test(&statbegsys1, &declbegsys1, ParseError::ExpectedStatement);

            if !self.declbegsys.contains(&self.current_symbol) {
                break;
            }
        }

        match self.ident_table[init_table_index] {
            Some(ref mut e) => {
                match self.code_words[e.adr] {
                    Some(ref mut i) => i.address = self.code_index,
                    None => {},
                }
                e.adr = self.code_index;
            },
            None => {},
        }

        let code_index_init = self.code_index;

        let data_index = self.data_indices.pop().unwrap();
        self.generate_instruction(Fct::Int, 0, data_index);

        let mut new_set3 = fsys.clone();
        new_set3.insert(Symbol::Semicolon);
        new_set3.insert(Symbol::EndSym);
        self.statement(new_set3);

        self.generate_instruction(Fct::Ret, 0, 0);

        self.test(&fsys, &SymSet::new(), ParseError::StatementSymbol);

        if self.error_count == 0{
            self.list_code(code_index_init);
        }

        self.current_level -= 1;
    }
}

/// Byte Code Interpreter
fn interpret(code_words: &[Option<Instruction>]) {
    println!("Start PL/0");

    let mut stack_index = 0;
    let mut base = 1;
    let mut address = 0;
    let mut stack: [i64; STACKSIZE] = [0; STACKSIZE];

    loop {
        let instruction_pointer = &code_words[address];
        address += 1;

        match *instruction_pointer {
            Some(ref e) => {
                match e.function {
                    Fct::Lit => {
                        stack_index += 1;
                        stack[stack_index] = e.address as i64;
                    },
                    Fct::Opr => {
                        // operators
                        match e.address {
                            1 => stack[stack_index] = -stack[stack_index], // negate
                            2 => {
                                stack_index -= 1;
                                stack[stack_index] = stack[stack_index] + stack[stack_index + 1];
                            }, // add
                            3 => {
                                stack_index -= 1;
                                stack[stack_index] = stack[stack_index] - stack[stack_index + 1];
                            }, // subtract
                            4 => {
                                stack_index -= 1;
                                stack[stack_index] = stack[stack_index] * stack[stack_index + 1];
                            }, // multiply
                            5 => {
                                stack_index -= 1;
                                stack[stack_index] = stack[stack_index] / stack[stack_index + 1];
                            }, // divide
                            6 => {
                                stack[stack_index] = (stack[stack_index] % 2 != 0) as i64;
                            }, // odd
                            8 => {
                                stack_index -= 1;
                                stack[stack_index] = (stack[stack_index] == stack[stack_index + 1]) as i64;
                            }, // =
                            9 => {
                                stack_index -= 1;
                                stack[stack_index] = (stack[stack_index] != stack[stack_index + 1]) as i64;
                            }, // #
                            10 => {
                                stack_index -= 1;
                                stack[stack_index] = (stack[stack_index] < stack[stack_index + 1]) as i64;
                            }, // <
                            11 => {
                                stack_index -= 1;
                                stack[stack_index] = (stack[stack_index] >= stack[stack_index + 1]) as i64;
                            }, // >=
                            12 => {
                                stack_index -= 1;
                                stack[stack_index] = (stack[stack_index] > stack[stack_index + 1]) as i64;
                            }, // >
                            13 => {
                                stack_index -= 1;
                                stack[stack_index] = (stack[stack_index] <= stack[stack_index + 1]) as i64;
                            }, // <=
                            _ => {},
                        }
                    },
                    Fct::Lod => {
                        stack_index += 1;
                        stack[stack_index] = stack[find_base(e.level, base, &stack) + e.address]
                    },
                    Fct::Sto => {
                        let base = find_base(e.level, base, &stack);
                        stack[base + e.address] = stack[stack_index];
                        println!("{}", stack[stack_index]);
                        stack_index -= 1;
                    },
                    Fct::Cal => {
                        stack[stack_index + 1] = find_base(e.level, base, &stack) as i64;
                        stack[stack_index + 2] = base as i64;
                        stack[stack_index + 3] = address as i64;
                        base = stack_index + 1;
                        address = e.address;
                    },
                    Fct::Ret => {
                        stack_index = base - 1;
                        address = stack[stack_index + 3] as usize;
                        base = stack[stack_index + 2] as usize;
                    },
                    Fct::Int => stack_index = stack_index + e.address,
                    Fct::Jmp => address = e.address,
                    Fct::Jpc => {
                        if stack[stack_index] == 0 {
                            address = e.address;
                        }
                        stack_index -= 1;
                    },
                }
            },
            None => {},
        }

        if address == 0 {
            break;
        }
    }
    print!("End PL/0");
}

fn find_base(mut level: usize, base_init: usize, stack: &[i64; STACKSIZE]) -> usize {
    let mut base = base_init;

    while level > 0 {
        base = stack[base] as usize;
        level -= 1;
    }

    base
}

// TODO
//  -used fixed sized arrays for the fixed stuff
//  -add error enum instead of #'s
