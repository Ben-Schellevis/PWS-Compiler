#[macro_use]
pub mod lexer {
    use core::{panic};
    use std::str::Chars;

    #[derive(Copy, Clone, Debug, PartialEq)]
    pub enum Literal {
        Bool,
        Number,
        String,
    }

    #[derive(Copy, Clone, Debug, PartialEq)]
    pub enum TokenType {
        //If else for etc.
        Keyword,

        //Name of variable or function
        Identifier,

        //String or number given by the code
        Literal(Literal),

        //any of the mathmatical or logical opertors
        Operator,

        Deliminator,

        Unkown,
    }

    #[derive(Debug, Clone)]
    pub struct Token {
        pub type_: TokenType,
        pub data: String,
    }

    #[derive(Debug, Clone)]
    pub struct CharPattern {
        patt: Vec<char>,
        infinite: bool,
    }
    impl CharPattern {
        fn includes_all(&self, other: &CharPattern) -> bool {
            for c in &other.patt {
                if !(self.patt.contains(c)) {
                    return false;
                }
            }
            return true;
        }

        fn remove(&mut self, patter: &CharPattern) {
            for c in &patter.patt {
                let pos = self.patt.iter().position(|x| *x == *c).unwrap();
                self.patt.remove(pos);
            }
        }

        fn equals(&mut self, other: &CharPattern) -> bool {
            let res = true;
            if self.patt.len() != other.patt.len() {
                return false;
            }

            for c in &other.patt {
                if !(self.patt.contains(c)) {
                    return false;
                }
            }
            for c in &self.patt {
                if !(other.patt.contains(c)) {
                    return false;
                }
            }

            return res;
        }
    }

    impl PartialEq<char> for CharPattern {
        fn eq(&self, other: &char) -> bool {
            if self.patt.contains(other) {
                return true;
            }
            return false;
        }
    }
    pub struct StringPattern {
        pub patt: Vec<CharPattern>,
        pub type_: TokenType,
    }

    pub enum Pattern {
        Pat(StringPattern),
        Str((String, TokenType)),
    }

    impl Pattern {
        pub fn new_pattern(pattern: Vec<(String, bool)>, type_: TokenType) -> Pattern {
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
                type_,
            });
        }
    }

    struct SymbolTable {
        data: Vec<Pattern>,
    }

    #[derive(Debug, Clone)]
    pub struct Node {
        connections: Vec<(Node, CharPattern)>,
        infinite: Option<CharPattern>,
        is_end: bool,
        token_type: TokenType,
    }

    impl Node {
        pub fn new(defualt_data: TokenType, end: bool, infinite: Option<CharPattern>) -> Node {
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
            type_: TokenType,
        ) -> &mut Node {
            // find any clashing patterns
            let i = &self
                .connections
                .iter()
                .enumerate()
                .filter(|(_, x)|  -> bool { x.1.includes_all(&_match) })
                .map(|(index, _)| index)
                .collect::<Vec<_>>();
                //.position(|x| -> bool { x.1.includes_all(&_match) });

            let node: &mut Node = match i.len() {
                
                0 => {
                    let n = Node::new(type_, end, infinite);

                    self.connections.push((n, _match));

                    &mut self.connections.last_mut().unwrap().0
                }
                _ => {
                    for n in i {
                        let node = &mut self.connections[n.clone()];
                        if node.1.equals(&_match.clone()) {
                            if !node.0.is_end {
                                node.0.token_type = type_;
                                node.0.is_end = true;
                            }
                            return &mut self.connections[n.clone()].0;
                        } else {
                            let mut type_ = type_;
                            let mut end = end;
                            node.1.remove(&_match);
    
                            //if this is not an end node make sure it type is changed
                            if !end {
                                type_ = node.0.token_type;
                                end = true;
                            }
    
                            let mut n = Node::new(type_, end, infinite.clone());
                            n.connections.push(node.clone());
                            self.connections.push((n, _match.clone()));
                        }

                    }
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
            let mut strings: Vec<(String, TokenType)> = vec![];
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

            for (string, type_) in strings {
                //Get all chars of the string
                let mut it = string.chars().peekable();

                //Get the start node
                let mut node = &mut *start_node;

                //Loop over all the chars
                while let Some(c) = it.next() {
                    //Check if their is another char after this one.
                    let end = it.peek().is_none();

                    //Create CharPattern from the char
                    let char_pattern = CharPattern {
                        patt: vec![c],
                        infinite: false,
                    };

                    //Create the node
                    node = node.insert(char_pattern, end, None, type_);
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

    pub struct Lexer<'a> {
        dfa: DFA<'a>,
        iter: Chars<'a>,
        last_char: Option<char>,
        peek_state: Option<Token>,
    }

    impl<'a> Lexer<'a> {
        pub fn new(
            iter: Chars<'a>,
            patterns: Vec<Pattern>,
            starting_node: &'a mut Node,
        ) -> Lexer<'a> {
            let table = SymbolTable { data: patterns };
            let dfa = DFA::new(table, starting_node);
            Lexer {
                dfa,
                iter,
                last_char: None,
                peek_state: None,
            }
        }

        fn nextchar(&mut self) -> Option<char> {
            let cha = self.iter.next();
            match cha {
                Some('\n') | Some(' ') | Some('\r') | None => {
                    self.last_char = None;
                    return None;
                }
                Some(_) => {
                    self.last_char = cha;
                    return cha;
                }
            }
        }

        fn nextcharvalid(&mut self) -> Option<char> {
            let mut c = None;
            if let Some(prev) = self.last_char {
                self.last_char = None;
                return Some(prev);
            }
            while let Some(cha) = self.iter.next() {
                match cha {
                    '\n' | ' ' | '\r' => {
                        continue;
                    }
                    _ => {
                        c = Some(cha);
                        break;
                    }
                }
            }
            return c;
        }

        pub fn peek_next_token(&mut self) -> Option<Token> {
            let tok = self.next_token();
            self.peek_state = tok.clone();
            return tok;
        }

        pub fn next_token(&mut self) -> Option<Token> {
            if let Some(token) = self.peek_state.clone() {
                self.peek_state = None;
                return Some(token);
            }

            let mut string = String::new();
            let ch = self.nextcharvalid()?;
            let mut to_push = ch;
            let mut x = self.dfa.solve_next(Some(ch));

            while let None = x {
                string.push(to_push);
                let ch = self.nextchar();
                if let Some(ch) = ch {
                    to_push = ch;
                }
                x = self.dfa.solve_next(ch);
            }

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

    macro_rules! str_pat {
        ($($x:expr),+, type: $t:expr) => {
        vec![$(Pattern::Str(($x.to_owned(), $t))),+]
    }
    }
}
