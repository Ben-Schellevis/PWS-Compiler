use core::panic;
use std::fs;

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
#[derive(Debug, PartialEq, Clone)]
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
    pub type_: TokenType
}

enum Pattern {
    Pat(StringPattern),
    Str((String,TokenType)),
}

impl Pattern {

    fn new_pattern(pattern: Vec<(String, bool)>, type_: TokenType) -> Pattern {
        let mut char_patterns: Vec<CharPattern> = vec![];

        for patt in pattern {
            let char_pattern = CharPattern {
                patt: patt.0.chars().collect(),
                infinite: patt.1,
            };
            char_patterns.push(char_pattern);
        }
        return Pattern::Pat(StringPattern {
            patt: char_patterns,
            type_
        });
    }
}

struct SymbolTable {
    data: Vec<Pattern>,
}

#[derive(Debug)]
struct Node {
    connections: Vec<(Node, CharPattern)>,
    infinite: Option<CharPattern>,
    is_end: bool,
    token_type: TokenType,
}

impl Node {
    fn new(defualt_data: TokenType, end: bool, infinite: Option<CharPattern>) -> Node {
        return Node {
            connections: vec![],
            infinite,
            is_end: end,
            token_type: defualt_data,
        };
    }

    fn insert(
        &mut self,
        _match: CharPattern,
        end: bool,
        infinite: Option<CharPattern>,
        type_: TokenType
    ) -> &mut Node {
        let i = &self
            .connections
            .iter()
            .position(|x| -> bool { x.1 == _match });

        let node: &mut Node = match i {
            Some(n) => &mut self.connections[n.clone()].0,
            None => {
                let n = Node::new(type_, end, infinite);

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
        let mut strings: Vec<(String,TokenType)> = vec![];
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

        for (string, type_) in strings {
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

                    let pos = patterns.iter().position(|x| x.patt[0] == c);

                    if let Some(pos) = pos {
                        let vec = &mut patterns[pos].patt[0].patt;

                        let index = vec
                            .iter()
                            .position(|x| *x == c)
                            .expect("Char not in vec even tough contains returned true.");
                        vec.remove(index);

                        string_pattern = Some(&patterns[pos]);
                        end = true;
                    }
                } else if let Some(pattern) = string_pattern {
                    let elem = &pattern.patt[i];
                    if *elem == c {
                        end = true;
                    } else {
                        string_pattern = None;
                    }
                    i += 1;
                }

                //Create CharPattern from the char
                let char_pattern = CharPattern {
                    patt: vec![c],
                    infinite: false,
                };

                first_iter = false;

                //Create the node
                node = node.insert(char_pattern, end, None, type_);
            }
        }

        for pattern in patterns {
            let mut node = &mut *start_node;
            let mut it = pattern.patt.into_iter().peekable();
            while let Some(char_pattern) = it.next() {
                let end = it.peek().is_none();
                let mut inf = None;
                if char_pattern.infinite {
                    inf = Some(char_pattern.clone());
                }
                node = node.insert(char_pattern, end, inf, pattern.type_);
            }
        }

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

        // check if we have a infite connection:

        if let Some(i) = position {
            let node = &node.connections[i].0;
            self.state = node;
            None
        } else {
            if let Some(inf) = &node.infinite {
                if let Some(c) = c {
                    if *inf == c {
                        return None;
                    }
                }
            }
            if node.is_end {
                return Some(node.token_type);
            }
            panic!("Invalid charter for current set {:#?}", c);
        }
    }

    fn reset(&mut self) {
        self.state = self.start_node;
    }
}

struct Lexer<'a> {
    chars: Vec<char>,
    dfa: DFA<'a>,
    file_index: usize,
}

impl<'a> Lexer<'a> {
    fn new(code: String, patterns: Vec<Pattern>, starting_node: &'a mut Node) -> Lexer<'a> {
        let chars = code.chars().collect();
        let table = SymbolTable { data: patterns };
        let dfa = DFA::new(table, starting_node);

        Lexer {
            chars,
            dfa,
            file_index: 0,
        }
    }

    fn next_char(&mut self) -> Option<char> {
        let mut r: Option<char> = None;
        let i = self.file_index;
        for c in self.chars.iter().skip(self.file_index) {
            match c {
                ' ' | '\n' | '\r' => {
                    self.file_index = i + 1;
                    return None
                }
                _ => {
                    r = Some(c.clone());
                    break;
                }
            }
        }
        self.file_index = i + 1;
        return r;
    }

    fn next_token(&mut self) -> Option<Token> {
        let mut string = String::new();

        let ch = self.next_char();
        let mut to_push = ch?;
        let mut x = self.dfa.solve_next(ch);


        while let None = x {
            string.push(to_push);
            let ch = self.next_char();
            if let Some(ch) = ch {
                to_push = ch;
            }
            x = self.dfa.solve_next(ch);
        }
        self.file_index -= 1;

        let res = x.unwrap();

        let token = Token {
            type_: res,
            data: string,
        };

        self.dfa.reset();

        Some(token)
    }
}

macro_rules! pat {
    ( $( $x:expr, $y:expr ),* , type: $z:expr ) => {
        Pattern::new_pattern(vec![ $( ($x.to_owned(), $y) ),* ], $z)
    };
}

fn main() {
    let raw_text = fs::read_to_string("/mnt/c/programming/files/rust/PWS-Compiler/source.txt")
        .expect("Should have been able to read the file");

    let mut node = Node::new(TokenType::Unkown, false, None);

    let strings = vec![
        pat!("1234567890", true, type: TokenType::Literal),

        pat!("abcdefghijklmnopqrstuvwxyz", true, type: TokenType::Identifier),
        pat!("abcdefghijklmnopqrstuvwxyz", true, "(", false, ")", false, type: TokenType::Keyword),
        pat!("+-", false,  type: TokenType::Operator),
        Pattern::Str(("if".to_owned(),TokenType::Keyword)),
        Pattern::Str(("===".to_owned(),TokenType::Operator)),
    ];

    let mut lex = Lexer::new(raw_text, strings, &mut node);

    let c = lex.next_token();
    println!("{:#?}", c);

    let c = lex.next_token();
    println!("{:#?}", c);

    let c = lex.next_token();
    println!("{:#?}", c);

    let c = lex.next_token();
    println!("{:#?}", c);

    let c = lex.next_token();
    println!("{:#?}", c);
}
