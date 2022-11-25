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

    Unkown,
}

#[derive(Debug)]
struct Token {
    type_: TokenType,
    data: String,
}
#[derive(Debug, PartialEq)]
struct CharPattern {
    patt: Vec<char>,
    infinite: bool,
}
impl PartialEq<char> for CharPattern {
    fn eq(&self, other: &char) -> bool {
        if self.patt.contains(other) {
            return true;
        }
        return false;
    }
}
struct StringPattern {
    pub patt: Vec<CharPattern>,
}

enum Pattern {
    Pat(StringPattern),
    Str(String),
}

impl Pattern {
    fn new_literal(patrn: String) -> Pattern {
        Pattern::Str(patrn)
    }

    fn new_pattern(pattern: Vec<(Vec<char>, bool)>) -> Pattern {
        let mut char_patterns: Vec<CharPattern> = vec![];

        for patt in pattern {
            let char_pattern = CharPattern {
                patt: patt.0,
                infinite: patt.1,
            };
            char_patterns.push(char_pattern);
        }
        return Pattern::Pat(StringPattern {
            patt: char_patterns,
        });
    }
}

struct SymbolTable {
    data: Vec<Pattern>,
}

#[derive(Debug)]
struct Node {
    connections: Vec<(Node, CharPattern)>,
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

    fn insert(&mut self, _match: CharPattern, end: bool) -> &mut Node {
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
    start_node: &'a Node,
    state: &'a Node,
}

impl<'a> DFA<'a> {
    fn new(table: SymbolTable, start_node: &'a mut Node) -> DFA<'a> {
        let mut strings: Vec<String> = vec![];
        let mut patterns: Vec<StringPattern> = vec![];
        for p in table.data {
            match p {
                Pattern::Str(str) => {
                    strings.push(str);
                }
                Pattern::Pat(pat) => {
                    patterns.push(pat);
                }
            }
        }

        for string in strings {
            //Get all chars of the string
            let mut it = string.chars().peekable();

            //Get the start node
            let mut node = &mut *start_node;

            //Loop over all the chars
            let mut string_pattern = None;
            let mut first_iter = true;
            let mut i = 0;
            while let Some(c) = it.next() {
                //Check if their is another char after this one.
                let mut end = it.peek().is_none();

                if first_iter {
                    //Check if any String patterns macht with the char

                    let i = patterns.iter().position(|x| x.patt[0] == c);

                    if let Some(i) = i {
                        //let vec = &mut patterns[i].patt[0].patt;

                        //let index = vec.iter().position(|x| *x == c).expect("Char not in vec even tough contains returned true.");
                        //vec.remove(index);

                        string_pattern = Some(&patterns[i]);
                        end = true;
                    }
                } else if let Some(pattern) = string_pattern {
                    let elem = &pattern.patt[i];
                    if *elem == c {
                        end = true;
                    } else {
                        string_pattern = None;
                    }
                }

                //Create CharPattern from the char
                let char_pattern = CharPattern {
                    patt: vec![c],
                    infinite: false,
                };

                //Create the node
                first_iter = false;

                i += 1;
                node = node.insert(char_pattern, end);
            }
        }

        for pattern in patterns {
            let mut node = &mut *start_node;
            let mut it = pattern.patt.into_iter().peekable();
            while let Some(char_pattern) = it.next() {
                let end = it.peek().is_none();
                node = node.insert(char_pattern, end);
            }
        }

        /*
        for p in &table.data {
            match p {
                Pattern::Str(str) => {
                    let mut it = str.chars().peekable();
                    let mut node = &mut *start_node;
                    while let Some(c) = it.next() {
                        let end = it.peek().is_none();
                        let char_pattern = CharPattern{
                            patt: vec![c],
                            infinite: false
                        };
                        node = node.insert(char_pattern, end);
                    }
                }
                Pattern::Pat(pat) => {
                    //let patterns = pat.patt;

                    //for char_pattern  in patterns {
                        //let position = start_node.connections.iter().position(|x| x.1 == char_pattern);
                    //}
                }
            }
        }
        */
        DFA {
            state: start_node,
            start_node: start_node,
        }
    }

    fn solve_next(&mut self, c: Option<char>) -> Option<TokenType> {
        let node = self.state;
        let checks: Vec<&CharPattern> = node.connections.iter().map(|x| &x.1).collect();
        let mut position = None;
        if let Some(c) = c {
            position = checks.iter().position(|x| **x == c);
        }
        if let Some(i) = position {
            let node = &node.connections[i].0;
            self.state = node;
            None
        } else {
            if node.is_end {
                return Some(TokenType::Keyword);
            }
            panic!("Invalid charter for current set");
        }
    }

    fn reset(&mut self) {
        self.state = self.start_node;
    }
}

struct Lexer<'a> {
    //the output of the lexer
    pub tokens: Vec<Token>,

    chars: Vec<char>,
    dfa: DFA<'a>,
    file_index: usize,
}

impl<'a> Lexer<'a> {
    fn new(code: String, patterns: Vec<Pattern>, starting_node: &'a mut Node) -> Lexer<'a> {
        let chars = code.chars().collect();
        let table = SymbolTable { data: patterns };
        let dfa = DFA::new(table, starting_node);

        println!("{dfa:#?}");
        Lexer {
            tokens: vec![],
            chars,
            dfa,
            file_index: 0,
        }
    }

    fn next_char(&mut self) -> Option<char> {
        let mut r: Option<char> = None;
        let mut i = self.file_index;
        for c in self.chars.iter().skip(self.file_index) {
            i += 1;
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
        self.file_index = i;
        return r;
    }

    fn next_token(&mut self) -> Option<Token> {
        let mut string = String::new();

        let ch = self.next_char();
        let mut x = self.dfa.solve_next(ch);

        if let Some(ch) = ch {
            string.push(ch);
        } else {
            return None;
        }

        while let None = x {
            let ch = self.next_char();
            if let Some(ch) = ch {
                string.push(ch);
            }
            x = self.dfa.solve_next(ch);
        }
        self.file_index -= 1;
        string.pop();

        let res = x.unwrap();

        let token = Token {
            type_: res,
            data: string,
        };

        println!("{token:#?}");

        self.dfa.reset();

        Some(token)
    }
}

fn main() {
    let raw_text = fs::read_to_string("/mnt/c/programming/files/rust/PWS-Compiler/source.txt")
        .expect("Should have been able to read the file");

    use Pattern::Str;

    let mut node = Node::new(TokenType::Keyword, false);
    let strings = vec![Pattern::Pat(StringPattern {
        patt: vec![CharPattern {
            patt: vec!['+','-'],
            infinite: false
        },
        CharPattern{
            patt: vec!['0','1','2','3','4','5','6','7','8','9'],
            infinite: true
        }],
    })];
    let mut lex = Lexer::new(raw_text, strings, &mut node);
}



