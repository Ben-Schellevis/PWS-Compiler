pub mod ast {
    use std::{clone, error::Error, fmt, vec};

    use crate::lexer::lexer::{Lexer, Token, TokenType};

    #[derive(Debug, Clone, PartialEq)]
    enum VarType {
        Number,
        Text,
        Bool,
        Function,
    }

    #[derive(Debug, Clone)]
    struct Literal {
        type_: VarType,
        data: String,
    }
    impl Literal {
        fn try_new(data: String, type_: VarType) -> Literal {
            match type_ {
                VarType::Number => {
                    if let Ok(_) = data.parse::<i64>() {
                        return Literal { type_, data };
                    } else {
                        panic!("{} is not a number", data);
                    }
                }
                VarType::Text => return Literal { type_, data },
                VarType::Bool => {
                    if let Ok(_) = data.parse::<bool>() {
                        return Literal { type_, data };
                    } else {
                        panic!("{} is not a bool", data);
                    }
                }
                VarType::Function => {
                    panic!("literal cant be a function")
                }
            }
        }
    }

    #[derive(Debug, Clone)]
    struct Variable {
        name: String,
        type_: VarType,
    }

    #[derive(Debug, Clone)]
    struct Block {
        data: Vec<AstNode>,
        variables: Vec<Variable>,
        parent: Option<usize>,
    }

    impl Block {
        fn get_variable<'a>(
            &'a self,
            name: String,
            parents: &'a Vec<Block>,
        ) -> Option<&'a Variable> {
            let x = self.variables.iter().find(|&x| x.name == name);
            match x {
                Some(var) => Some(var),
                None => match self.parent {
                    Some(index) => {
                        return parents[index].get_variable(name, parents);
                    }
                    None => None,
                },
            }
        }

        fn add_variable(&mut self, name: String, type_: VarType) {
            self.variables.push(Variable { name, type_ });
        }
    }

    struct CallOperator {
        args: Vec<AstNode>,
        name: String,
    }

    #[derive(Debug, Clone)]
    struct BinaryOperator {
        left: Box<AstNode>,
        right: Box<AstNode>,
        type_: String,
    }

    #[derive(Debug, Clone)]
    struct Function {
        name: String,
        inner_block: usize,
        vrariables: i64,
    }
    #[derive(Debug, Clone)]
    enum AstNode {
        Block(usize),
        Function(Function),
        If(Box<AstNode>, Box<AstNode>),
        Literal(Literal),
        Operator(BinaryOperator),
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
        not: i32,
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
                "!" => return self.not,
                _ => return i32::MAX,
            }
        }
        fn get_type(op: String) -> (VarType, VarType) {
            match op.as_str() {
                "+" => return (VarType::Number,VarType::Number) ,
                "-" => return (VarType::Number,VarType::Number),
                "*" => return (VarType::Number,VarType::Number),
                "/" => return (VarType::Number,VarType::Number),
                "|" => return (VarType::Bool,VarType::Bool),
                "&" => return (VarType::Bool,VarType::Bool),
                "<" => return (VarType::Number,VarType::Bool),
                ">" => return (VarType::Number,VarType::Bool),
                "!" => return (VarType::Bool,VarType::Bool),
                _ => panic!("undifiend"),
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
        not: 5,
    };

    #[derive(Debug)]
    enum Errors {
        TokenExpected(Token, String),
        TokenTypeExpected(Token, TokenType),
    }

    impl fmt::Display for Errors {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            match self {
                Errors::TokenExpected(_, s) => write!(f, "Expected {}", s),
                Errors::TokenTypeExpected(token, type_) => {
                    write!(f, "Expected a {:#?} got token {:#?}", type_, token)
                }
            }
        }
    }
    impl Error for Errors {}
    pub struct Tree<'a> {
        lex: Lexer<'a>,
        program: Vec<Block>,
    }

    impl<'a> Tree<'a> {
        pub fn new(lex: Lexer<'a>) -> Tree<'a> {
            Tree {
                lex,
                program: vec![Block {
                    data: vec![],
                    variables: vec![],
                    parent: None,
                }],
            }
        }

        pub fn build(&mut self) {
            let done = self.parse_block(0 as usize, None);
            println!("{:#?}", self.program[done]);
            println!("{:#?}", self.program)
        }

        fn parse_type(name: String) -> VarType {
            match name.as_str() {
                "int" => VarType::Number,
                "bool" => VarType::Bool,
                "string" => VarType::Text,
                _ => todo!(),
            }
        }

        fn expect_token_type(&mut self, type_: TokenType) -> Result<Token, Errors> {
            let token = self.lex.next_token().ok_or(Errors::TokenTypeExpected(
                Token {
                    type_: TokenType::Unkown,
                    data: "".to_owned(),
                },
                type_,
            ))?;

            if !(token.type_ == type_) {
                return Err(Errors::TokenTypeExpected(token, type_));
            } else {
                return Ok(token);
            }
        }

        fn expect_exact_token(&mut self, type_: TokenType, data: String) -> Result<Token, Errors> {
            let token = self.lex.next_token().ok_or(Errors::TokenExpected(
                Token {
                    type_: TokenType::Unkown,
                    data: "".to_owned(),
                },
                data.clone(),
            ))?;
            if !(token.data == data && token.type_ == type_) {
                return Err(Errors::TokenExpected(token, data.clone()));
            }
            Ok(token)
        }

        fn parse_exp(&mut self, toks: Vec<Token>, type_: VarType, scope: usize) -> AstNode {
            let mut tokens = toks.clone().into_iter().enumerate();

            let mut operators = vec![];
            let mut bracket_count = 0;

            while let Some((i, next_token)) = tokens.next() {
                match next_token.type_ {
                    TokenType::Keyword => {
                        panic!("Did not exepect keyword {}", next_token.data);
                    }
                    TokenType::Identifier => {
                        let var = self.program[scope]
                            .get_variable(next_token.data.clone(), &self.program);
                        if let Some(var) = var {
                            if toks.len() == 1 {
                                if !(var.type_ == type_) {
                                    panic!("{} is not of type {:#?}", var.name.clone(), type_);
                                }
                                return AstNode::Identifier(Variable {
                                    type_: var.type_.clone(),
                                    name: next_token.data.clone(),
                                });
                            }
                        } else {
                            panic!("{} not defined", next_token.data);
                        }
                    }
                    TokenType::Literal => {
                        if toks.len() == 1 {
                            return AstNode::Literal(Literal::try_new(next_token.data, type_));
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

            let least_brackets = {
                operators
                    .iter()
                    .min_by(|x, y| x.1.cmp(&y.1))
                    .unwrap_or(&(0, 0))
                    .1
            };

            let all_least_brackets: Vec<(usize, i32)> = {
                operators
                    .iter()
                    .filter(|x| x.1 == least_brackets)
                    .cloned()
                    .collect()
            };

            let mut all_operators = vec![];
            for op in all_least_brackets {
                let tok = toks[op.0].clone();
                all_operators.push((PRECEDENCE.get(tok.data.clone()), op.0))
            }

            let op = {
                all_operators
                    .iter()
                    .min_by(|x, y| x.0.cmp(&y.0))
                    .expect("No operator")
                    .1
            };

            let tok = toks[op].clone();
            let types = Precedence::get_type(tok.data.clone());
            if types.1 != type_ {
                panic!("Operator {} does not provide type {:#?}", tok.data.clone(), types.1)
            }
            let left = Box::new(self.parse_exp(
                toks[least_brackets as usize..op].to_vec(),
                types.clone().0,
                scope,
            ));
            let right = Box::new(self.parse_exp(
                toks[op + 1..toks.len() - least_brackets as usize].to_vec(),
                types.clone().0,
                scope,
            ));
            let node = AstNode::Operator(BinaryOperator {
                left,
                right,
                type_: tok.data.clone(),
            });
            return node;
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
                    let logical_exp = self.parse_exp(logical_nodes, VarType::Bool, scope);
                    let block = self.parse_block(scope, None);
                    AstNode::If(Box::new(logical_exp), Box::new(AstNode::Block(block)))
                }
                "let" => {
                    let mut ident = self.expect_token_type(TokenType::Identifier).unwrap();

                    self.expect_exact_token(TokenType::Deliminator, ":".to_owned())
                        .unwrap();

                    let type_ = self.expect_token_type(TokenType::Identifier).unwrap().data;
                    let type_ = Tree::<'a>::parse_type(type_);
                    self.program[scope].add_variable(ident.data.clone(), type_.clone());

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
                    let exp = self.parse_exp(exp, type_.clone(), scope);

                    AstNode::Operator(BinaryOperator {
                        left: Box::new(AstNode::Identifier(Variable {
                            name: ident.data,
                            type_: type_,
                        })),
                        right: Box::new(exp),
                        type_: "=".to_owned(),
                    })
                }
                "func" => {
                    let function_name = self.expect_token_type(TokenType::Identifier).unwrap();

                    self.program[scope].add_variable(function_name.data.clone(), VarType::Function);

                    self.expect_exact_token(TokenType::Deliminator, "(".to_owned())
                        .unwrap();

                    let mut function_args = vec![];
                    let mut token = self.expect_token_type(TokenType::Identifier);
                    while let Ok(ref arg) = token {
                        self.expect_exact_token(TokenType::Deliminator, ":".to_owned())
                            .unwrap();
                        let type_token = self.expect_token_type(TokenType::Identifier).unwrap();
                        let type_ = Tree::<'a>::parse_type(type_token.data);
                        function_args.push((arg.data.clone(), type_));
                        token = self.expect_token_type(TokenType::Identifier);
                    }

                    if let Err(Errors::TokenTypeExpected(tok, _)) = token {
                        if !(tok.data == ")".to_owned()) {
                            panic!("Expectd )")
                        }
                    } else {
                        panic!("Expectd )")
                    }

                    let amount = function_args.len() as i64;

                    let mut funcblock = Block {
                        data: vec![],
                        variables: vec![],
                        parent: Some(scope),
                    };
                    for (name, type_) in function_args {
                        let var = Variable { name, type_ };
                        funcblock.variables.push(var);
                    }

                    let inner_code = self.parse_block(scope, Some(funcblock));
                    AstNode::Function(Function {
                        name: function_name.data,
                        inner_block: inner_code,
                        vrariables: amount,
                    })
                }
                _ => panic!("Unknown keyword"),
            }
        }

        //after we find a do or at the start of the program
        fn parse_block(&mut self, scope: usize, block: Option<Block>) -> usize {
            let mut scope = scope;
            let mut start = block.unwrap_or(Block {
                data: vec![],
                variables: vec![],
                parent: Some(scope),
            });
            self.program.push(start);
            let innerscope = self.program.len() - 1;

            loop {
                let token = self.lex.next_token().expect("expected end");
                match token.type_ {
                    TokenType::Keyword => {
                        if token.data == "end" {
                            return innerscope;
                        }
                        if token.data == "do" {
                            let new_node = self.parse_block(innerscope, None);
                            self.program[innerscope].data.push(AstNode::Block(new_node));
                        } else {
                            let new_node = self.parse_keyword(token.data, innerscope);
                            self.program[innerscope].data.push(new_node);
                        }
                    }
                    TokenType::Identifier => {
                        let var = self.program[innerscope]
                            .get_variable(token.data.clone(), &self.program);
                        if let None = var {
                            panic!("{} not defined", token.data);
                        }
                        let var = var.unwrap();
                        let next_token = self.lex.next_token().expect("expected operator");
                        if !(next_token.type_ == TokenType::Operator) && !(next_token.data == "=") {
                            panic!("expected operator")
                        }

                        let left = Box::new(AstNode::Identifier(var.clone()));

                        let mut exp = vec![];
                        let mut next_token = self.lex.next_token().expect("exepected ;");
                        while next_token.data != ";" {
                            exp.push(next_token);
                            next_token = self.lex.next_token().expect("exepected ;");
                        }

                        let right = Box::new(self.parse_exp(exp, var.type_.clone(), innerscope));
                        self.program[innerscope]
                            .data
                            .push(AstNode::Operator(BinaryOperator {
                                left,
                                right,
                                type_: "=".to_owned(),
                            }));
                    }
                    TokenType::Literal => todo!(),
                    TokenType::Operator => {
                        panic!();
                    }
                    TokenType::Deliminator => todo!(),
                    TokenType::Unkown => todo!(),
                }
            }
        }
    }
}


