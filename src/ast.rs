pub mod ast {
    use std::vec;

    use crate::lexer::lexer::{Lexer, Token, TokenType};

    #[derive(Debug)]
    enum Literal {
        Number(i64),
        Text(String),
        Bool(bool),
    }
    impl Literal {
        fn new(dat: String) -> Literal {
            let num = dat.parse::<i64>();
            if let Ok(num) = num {
                return Literal::Number(num);
            }
            let boolean = dat.parse::<bool>();
            if let Ok(boolean) = boolean {
                return Literal::Bool(boolean);
            }
            return Literal::Text(dat);
        }
    }

    #[derive(Debug)]
    struct Variable {
        name: String,
    }

    #[derive(Debug)]
    struct Block {
        data: Vec<AstNode>,
        variables: Vec<Variable>,
        parent: Option<usize>,
    }

    impl Block {
        fn has_variable(&self, name: String) -> bool {
            let x = self.variables.iter().find(|&x| x.name == name);
            match x {
                Some(_) => true,
                None => false,
            }
        }

        fn add_variable(&mut self, name: String) {
            if self.has_variable(name.clone()) {
                panic!("Variable with name already exsits in this scope")
            }
            self.variables.push(Variable { name });
        }
    }

    #[derive(Debug)]
    struct Operator {
        left: Box<AstNode>,
        right: Box<AstNode>,
        type_: String,
    }

    #[derive(Debug)]
    enum AstNode {
        Block(Block),
        If(Box<AstNode>, Box<AstNode>),
        Literal(Literal),
        Operator(Operator),
        Identifier(Variable),
        Unknown,
    }

    #[derive(Debug)]
    struct Precedence {
        add: i32,
        minus: i32,
        multi: i32,
        divide: i32,
        or: i32,
        and: i32,
        less: i32,
        greater: i32,
    }

    impl Precedence {
        fn get(&self, op: String) -> i32 {
            match op.as_str() {
                "+" => return self.add,
                "-" => return self.minus,
                "*" => return self.multi,
                "/" => return self.divide,
                "|" => return self.or,
                "&" => return self.and,
                "<" => return self.less,
                ">" => return self.greater,
                _ => return i32::MAX,
            }
        }
    }

    const PRECEDENCE: Precedence = Precedence {
        add: 1,
        minus: 1,
        multi: 2,
        divide: 2,
        or: 0,
        and: 0,
        less: -1,
        greater: -1,
    };

    pub struct tree<'a> {
        lex: Lexer<'a>,
        program: Vec<Block>,
    }

    impl<'a> tree<'a> {
        pub fn new(mut lex: Lexer<'a>) -> tree<'a> {
            tree {
                lex,
                program: vec![Block {
                    data: vec![],
                    variables: vec![],
                    parent: None,
                }],
            }
        }
        pub fn build(&mut self) {
            let done = self.parse_block(0 as usize);
            println!("{done:#?}")
        }

        fn parse_exp(&mut self, toks: Vec<Token>, scope: usize) -> AstNode {
            let mut tokens = toks.clone().into_iter().enumerate();

            let mut operators = vec![];
            let mut bracket_count = 0;

            while let Some((i, next_token)) = tokens.next() {
                match next_token.type_ {
                    TokenType::Keyword => {
                        panic!("Did not exepect keyword {}", next_token.data);
                    }
                    TokenType::Identifier => {
                        if self.program[scope].has_variable(next_token.data.clone()) {
                            if toks.len() == 1 {
                                return AstNode::Identifier(Variable {
                                    name: next_token.data.clone(),
                                });
                            }
                        } else {
                            panic!("{} not defined", next_token.data);
                        }
                    }
                    TokenType::Literal => {
                        if toks.len() == 1 {
                            return AstNode::Literal(Literal::new(next_token.data));
                        }
                    }
                    TokenType::Operator => {
                        operators.push((i, bracket_count));
                    }
                    TokenType::Deliminator => match next_token.data.clone().as_str() {
                        "(" => bracket_count += 1,
                        ")" => bracket_count -= 1,
                        _ => panic!("Did not exepect {}", next_token.data),
                    },
                    TokenType::Unkown => todo!(),
                }
            }

            let least_brackets = operators
                .iter()
                .min_by(|x, y| x.1.cmp(&y.1))
                .unwrap_or(&(0, 0))
                .1;

            let all_least_brackets: Vec<(usize, i32)> = operators
                .iter()
                .filter(|x| x.1 == least_brackets)
                .cloned()
                .collect();

            if all_least_brackets.len() > 1 {
                let mut all_operators = vec![];
                for op in all_least_brackets {
                    let tok = toks[op.0].clone();
                    all_operators.push((PRECEDENCE.get(tok.data.clone()), op.0))
                }
                let op = all_operators
                    .iter()
                    .min_by(|x, y| x.0.cmp(&y.0))
                    .expect("No operator")
                    .1;

                let tok = toks[op].clone();
                let left =
                    Box::new(self.parse_exp(toks[least_brackets as usize..op].to_vec(), scope));
                let right = Box::new(self.parse_exp(
                    toks[op + 1..toks.len() - least_brackets as usize].to_vec(),
                    scope,
                ));
                let node = AstNode::Operator(Operator {
                    left,
                    right,
                    type_: tok.data,
                });
                return node;
            } else if all_least_brackets.len() == 1 {
                let op = all_least_brackets[0].0;
                let tok = toks[op].clone();
                let left =
                    Box::new(self.parse_exp(toks[least_brackets as usize..op].to_vec(), scope));
                let right = Box::new(self.parse_exp(
                    toks[op + 1..toks.len() - least_brackets as usize].to_vec(),
                    scope,
                ));
                let node = AstNode::Operator(Operator {
                    left,
                    right,
                    type_: tok.data,
                });
                return node;
            } else {
                panic!("Expected logical exp got nothing",)
            }
        }

        fn parse_keyword(&mut self, tok: String, scope: usize) -> AstNode {
            match tok.as_str() {
                "if" => {
                    let mut next_token = self.lex.next_token().expect("exepected do");
                    let mut logical_nodes = vec![];
                    while next_token.data != "do" {
                        logical_nodes.push(next_token);
                        next_token = self.lex.next_token().expect("exepected do");
                    }
                    let logical_exp = self.parse_exp(logical_nodes, scope);
                    let block = self.parse_block(scope);
                    AstNode::If(Box::new(logical_exp), Box::new(AstNode::Block(block)))
                }
                "let" => {
                    let mut ident = self.lex.next_token().expect("Expected an Identifier");
                    if !(ident.type_ == TokenType::Identifier) {
                        panic!(
                            "Expected an Identifier not the {:#?}, {}",
                            ident.type_, ident.data
                        );
                    }
                    self.program[scope].add_variable(ident.data.clone());

                    let tok = self
                        .lex
                        .next_token()
                        .expect(&format!("Expected a \"=\" after let {} ", ident.data));

                    if !(tok.data == "=" && tok.type_ == TokenType::Operator) {
                        panic!("Expected a \"=\" after let {} ", ident.data);
                    }

                    let mut exp = vec![];
                    let mut next_token = self.lex.next_token().expect("exepected ;");
                    while next_token.data != ";" {
                        exp.push(next_token);
                        next_token = self.lex.next_token().expect("exepected ;");
                    }
                    let exp = self.parse_exp(exp, scope);

                    AstNode::Operator(Operator {
                        left: Box::new(AstNode::Identifier(Variable { name: ident.data })),
                        right: Box::new(exp),
                        type_: "=".to_owned(),
                    })
                }
                _ => panic!("Unknow keyword"),
            }
        }

        //after we find a do or at the start of the program
        fn parse_block(&mut self, scope: usize) -> Block {
            let mut scope = scope;
            let token = self.lex.next_token().expect("No tokens");
            let mut start = Block {
                data: vec![],
                variables: vec![],
                parent: Some(scope),
            };
            match token.type_ {
                TokenType::Keyword => {
                    if token.data == "end" {
                        return start;
                    }
                    let new_node = self.parse_keyword(token.data, self.program.len() - 1);
                    start.data.push(new_node);
                    start
                }
                TokenType::Identifier => todo!(),
                TokenType::Literal => todo!(),
                TokenType::Operator => todo!(),
                TokenType::Deliminator => todo!(),
                TokenType::Unkown => todo!(),
            }
        }
    }
}

