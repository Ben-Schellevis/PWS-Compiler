use core::panic;
use std::{default, fs};

#[derive(Copy, Clone, Debug)]
enum TokenType {
    //If else for etc.
    Keyword,

    //Name of variable or function
    Identifier,

    //String or number given by the code
    Literal,

    //any of the mathmatical or logical opertors
    Operator,

    None,
}

struct Token {
    type_: TokenType,
    data: String,
}

#[derive(PartialEq)]
struct Pattern {
    patt: Vec<char>,
}
impl PartialEq<char> for Pattern {
    fn eq(&self, other: &char) -> bool {
        if self.patt.contains(other) {
            return true;
        }
        return false;
    }
}

#[derive(PartialEq)]
enum CharPattern {
    Pat(Pattern),
    Str(&'static str),
}

struct SymbolTable {
    data: Vec<CharPattern>,
}

#[derive(Debug)]
struct Node {
    connections: Vec<(Node, char)>,
    is_end: bool,
    token_type: TokenType,
}

impl Node {
    fn new(defualt_data: TokenType, end: bool) -> Node {
        return Node {
            connections: vec![],
            is_end: end,
            token_type: defualt_data,
        };
    }

    fn insert(&mut self, _match: char, end: bool) -> &mut Node {
        let i = &self
            .connections
            .iter()
            .position(|x| -> bool { x.1 == _match });

        let node: &mut Node = match i {
            Some(n) => &mut self.connections[n.clone()].0,
            None => {
                let n = Node::new(self.token_type, end);

                self.connections.push((n, _match));

                &mut self.connections.last_mut().unwrap().0
            }
        };

        return node;
    }
}

#[derive(Debug)]
struct DFA<'a> {
    state: Option<&'a Node>,
    start_node: Node,
}

impl<'a> DFA<'a> {
    fn new(table: &'a SymbolTable) -> DFA<'a> {
        let mut start_node = Node::new(TokenType::None, false);
        for p in &table.data {
            match p {
                CharPattern::Str(str) => {
                    let mut it = str.chars().peekable();
                    let mut node = &mut start_node;
                    while let Some(c) = it.next() {
                        if it.peek().is_none() {
                            node = node.insert(c, true);
                        } else {
                            node = node.insert(c, false);
                        }
                    }
                }
                CharPattern::Pat(pat) => panic!("WIP"),
            }
        }
        DFA {
            state: None,
            start_node,
        }
    }

    fn solve_next(&'a mut self, c: Option<char>) -> Option<TokenType> {
        let node = self.state.unwrap_or_else(|| &self.start_node);
        let checks: Vec<char> = node.connections.iter().map(|x| x.1).collect();
        let mut position = None;
        if let Some(c) = c{
            position = checks.iter().position(|x| *x == c);
        }
        if let Some(i) = position {
            let node = &node.connections[i].0;
            self.state = Some(node);
            None
        } else {
            if node.is_end {
                return Some(TokenType::Keyword);
            }
            panic!("Invalid charter for current set");
        }
    }
}

struct Lexer<'a> {
    //the output of the lexer
    pub tokens: Vec<Token>,

    chars: Vec<char>,

    dfa: DFA<'a>
}

impl<'a> Lexer<'a> {
    fn new(code: String, dfa: DFA)  -> Lexer {
        let chars = code.chars().collect();


        Lexer {
            tokens: vec![],
            chars,
            dfa
        }
    }

    fn next_char(&self) -> Option<char> {
        let mut r: Option<char> = None;
        for c in &self.chars {
            match c {
                ' ' | '\n' | '\r' => {
                    continue;
                }
                _ => {
                    r = Some(c.clone());
                    break;
                }
            }
        }
        return r;
    }

    fn next_token(&'a mut self) {
        let token:Token;
        let mut x = self.dfa.solve_next(self.next_char());
        while let None = x {
            x = self.dfa.solve_next(self.next_char());
        }

    }
}

fn main() {
    let raw_text = fs::read_to_string("/mnt/c/programming/files/rust/PWS-Compiler/source.txt")
        .expect("Should have been able to read the file");

    use CharPattern::Str;
    let strings = vec![Str("if"), Str("+"), Str("else"), Str("ifs"), Str("irs")];
    let table = SymbolTable { data: strings };
    let dfa = DFA::new(&table);
    println!("{dfa:#?}");
    //let lexer = Lexer::new(raw_text);
}
