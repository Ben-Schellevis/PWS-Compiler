pub mod ast {
    use std::{error::Error, fmt, string, vec};

    use crate::lexer::lexer::{Lexer, Literal as TokenLiteral, Token, TokenType};

    #[derive(Debug, Clone, PartialEq)]
    pub enum VarType {
        Number,
        Text,
        Bool,
        Function,
        Void,
        Any,
    }

    #[derive(Debug, Clone)]
    pub struct Literal {
        pub type_: VarType,
        pub data: String,
    }
    impl Literal {
        fn try_new(tok: TokenLiteral, data: String) -> (Literal, VarType) {
            match tok {
                TokenLiteral::Bool => (
                    Literal {
                        type_: VarType::Bool,
                        data,
                    },
                    VarType::Bool,
                ),
                TokenLiteral::Number => (
                    Literal {
                        type_: VarType::Number,
                        data,
                    },
                    VarType::Number,
                ),
                TokenLiteral::String => (
                    Literal {
                        type_: VarType::Text,
                        data,
                    },
                    VarType::Text,
                ),
            }
        }
    }

    #[derive(Debug, Clone)]
    pub struct Variable {
        pub name: String,
        pub type_: VarType,
        pub arg: bool,
    }

    #[derive(Debug, Clone)]
    pub struct FunctionArgs {
        pub func_name: String,
        pub args: Vec<VarType>,
        pub returns: Vec<VarType>,
    }

    #[derive(Debug, Clone)]
    pub struct Block {
        pub data: Vec<AstNode>,
        pub variables: Vec<Variable>,
        pub funcs: Vec<FunctionArgs>,
        pub parent: Option<usize>,
    }

    impl Block {
        pub fn get_variable<'a>(
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

        pub fn get_func<'a>(
            &'a self,
            name: String,
            parents: &'a Vec<Block>,
        ) -> Option<&'a FunctionArgs> {
            let x = self.funcs.iter().find(|&x| x.func_name == name);
            match x {
                Some(var) => Some(var),
                None => match self.parent {
                    Some(index) => {
                        return parents[index].get_func(name, parents);
                    }
                    None => None,
                },
            }
        }

        pub fn get_vars(&self, parents: &Vec<Block>) -> Vec<String> {
            let mut res = vec![];
            for var in &self.variables {
                let mut name = var.name.clone();
                //name.push_str(format!("-{}", depth).as_str());
                res.push(name);
            }
            for node in &self.data {
                match node {
                    AstNode::Block(inner) => {
                        let nextblock = &parents[*inner];
                        res.extend(nextblock.get_vars(parents));
                    }
                    AstNode::Function(_) => {}

                    AstNode::If(_, inner) => match inner.as_ref() {
                        AstNode::Block(b) => {
                            let nextblock = &parents[*b];
                            res.extend(nextblock.get_vars(parents));
                        }
                        AstNode::Function(_) => todo!(),
                        AstNode::While(_, _)=> {}
                        AstNode::If(_, _) => todo!(),
                        AstNode::Literal(_) => todo!(),
                        AstNode::BinaryOperator(_) => todo!(),
                        AstNode::FunctionOperator(_) => todo!(),
                        AstNode::Identifier(_) => todo!(),
                        AstNode::Unknown => todo!(),
                    },
                    AstNode::Literal(_) => {}
                    AstNode::BinaryOperator(_) => {}
                    AstNode::FunctionOperator(_) => {}
                    AstNode::Identifier(_) => {}
                    AstNode::While(_, _)=> {}
                    AstNode::Unknown => {}
                }
            }
            res
        }

        fn add_variable(&mut self, name: String, type_: VarType) {
            self.variables.push(Variable {
                name,
                type_,
                arg: false,
            });
        }
        fn add_function(&mut self, func_name: String, args: Vec<VarType>, returns: Vec<VarType>) {
            self.funcs.push(FunctionArgs {
                func_name,
                args,
                returns,
            });
        }
    }

    #[derive(Debug, Clone, PartialEq)]
    pub enum Operators {
        Eq,
        Plus,
        Minus,
        Set,
        Divide,
        Mult,
        Greater,
        Less
    }

    impl Operators {
        pub fn new(data: String) -> Operators {
            match data.as_str() {
                "===" => Operators::Eq,
                "+" => Operators::Plus,
                "-" => Operators::Minus,
                "=" => Operators::Set,
                "/" => Operators::Divide,
                "*" => Operators::Mult,
                ">" => Operators::Greater,
                "<" => Operators::Less,
                _ => panic!("not a operator"),
            }
        }
    }

    #[derive(Debug, Clone)]
    pub struct BinaryOperator {
        pub left: Box<AstNode>,
        pub right: Box<AstNode>,
        pub type_: Operators,
    }
    impl BinaryOperator {
        fn validate_type(&self, left: VarType, right: VarType) -> Result<VarType, Errors> {
            match self.type_ {
                Operators::Minus | Operators::Plus | Operators::Divide | Operators::Mult => {
                    if left == VarType::Number && right == VarType::Number {
                        return Ok(VarType::Number);
                    } else {
                        Err(Errors::WrongType(
                            vec![left, right],
                            vec![VarType::Number, VarType::Number],
                        ))
                    }
                }

                Operators::Eq => {
                    if left == right {
                        return Ok(VarType::Bool);
                    } else {
                        Err(Errors::WrongType(
                            vec![left.clone(), right.clone()],
                            vec![left.clone(), left.clone()],
                        ))
                    }
                }

                Operators::Greater | Operators::Less => {
                    if left == VarType::Number && right == VarType::Number {
                        return Ok(VarType::Bool);
                    } else {
                        Err(Errors::WrongType(
                            vec![left, right],
                            vec![VarType::Number, VarType::Number],
                        ))
                    }
                }

                Operators::Set => {
                    if left == right {
                        return Ok(right);
                    } else {
                        Err(Errors::WrongType(
                            vec![left.clone(), right.clone()],
                            vec![left.clone(), left.clone()],
                        ))
                    }
                }
            }
        }

        fn new(
            left: (AstNode, VarType),
            right: (AstNode, VarType),
            op: Operators,
        ) -> Result<(BinaryOperator, VarType), Errors> {
            let left_box = Box::new(left.0);
            let right_box = Box::new(right.0);
            let op = BinaryOperator {
                left: left_box,
                right: right_box,
                type_: op,
            };
            let type_ = op.validate_type(left.1, right.1)?;
            return Ok((op, type_));
        }
    }

    #[derive(Debug, Clone)]
    pub struct FunctionOperator {
        pub args: Vec<AstNode>,
        pub name: String,
    }

    impl FunctionOperator {
        fn new(
            data: Vec<(AstNode, Vec<VarType>)>,
            func: FunctionArgs,
        ) -> Result<(FunctionOperator, Vec<VarType>), Errors> {
            let name = func.func_name;
            let args_check = func.args;
            let (args_received, types): (Vec<AstNode>, Vec<Vec<VarType>>) =
                data.iter().cloned().unzip();
            let received_arg_count: usize = types.iter().map(Vec::len).sum();
            if args_check.len() != received_arg_count {
                return Err(Errors::ArgCount(received_arg_count, args_check.len()));
            }

            let mut index_of_check = 0;
            for (index, _) in args_received.iter().enumerate() {
                let args_at_index = types[index].clone();
                for type_ in args_at_index.clone() {
                    if args_check[index_of_check] != type_ {
                        return Err(Errors::WrongType(args_check.clone(), args_at_index.clone()));
                    }
                    index_of_check += 1;
                }
            }
            Ok((
                FunctionOperator {
                    args: args_received,
                    name,
                },
                func.returns,
            ))
        }
    }

    #[derive(Debug, Clone)]
    pub struct Function {
        pub name: String,
        pub inner_block: usize,
    }

    #[derive(Debug, Clone)]
    pub enum AstNode {
        Block(usize),
        Function(Function),
        If(Box<AstNode>, Box<AstNode>),
        While(Box<AstNode>, Box<AstNode>),
        Literal(Literal),
        BinaryOperator(BinaryOperator),
        FunctionOperator(FunctionOperator),
        Identifier(Variable),
        Unknown,
    }

    #[derive(Debug)]
    struct Precedence {
        add: i32,      // +
        minus: i32,    // -
        multi: i32,    // *
        divide: i32,   // /
        or: i32,       // |
        and: i32,      // &
        less: i32,     // <
        greater: i32,  // >
        not: i32,      // !
        function: i32, //->
        set: i32,      //=
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
                "->" => return self.function,
                "=" => return self.set,
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
        not: 5,
        function: 10,
        set: -5,
    };

    #[derive(Debug)]
    enum Errors {
        TokenExpected(String, String),
        TokenTypeExpected(TokenType, String),
        WrongType(Vec<VarType>, Vec<VarType>),
        ArgCount(usize, usize),
    }

    impl fmt::Display for Errors {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            match self {
                Errors::TokenExpected(expected, recieved) => {
                    write!(f, "Expected {} got {}", expected, recieved)
                }
                Errors::TokenTypeExpected(type_, recieved) => {
                    write!(f, "Expected an {:#?} got {:#?}", type_, recieved)
                }
                Errors::WrongType(expected, recieved) => {
                    write!(f, "{:#?} is not of type {:#?}", recieved, expected)
                }
                Errors::ArgCount(expected, recieved) => {
                    write!(f, "Expected {} arguments got {}", recieved, expected)
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
                    funcs: vec![
                        FunctionArgs {
                            func_name: "return".to_owned(),
                            args: vec![],
                            returns: vec![],
                        },
                        FunctionArgs {
                            func_name: "print".to_owned(),
                            args: vec![VarType::Text],
                            returns: vec![],
                        },
                        FunctionArgs {
                            func_name: "printnumber".to_owned(),
                            args: vec![VarType::Number],
                            returns: vec![],
                        },
                        FunctionArgs {
                            func_name: "printchar".to_owned(),
                            args: vec![VarType::Number],
                            returns: vec![],
                        },
                        FunctionArgs {
                            func_name: "printbool".to_owned(),
                            args:vec![VarType::Bool],
                            returns: vec![],
                        },
                    ],
                }],
            }
        }

        pub fn build(mut self) -> Vec<Block> {
            let done = self.parse_block(0 as usize, None);
            match done {
                Ok(_) => {
                    println!("{:#?}", self.program);
                    self.program
                }
                Err(err) => {
                    panic!("{}", err);
                    //process::exit(1)
                }
            }
        }

        fn parse_type(name: String) -> VarType {
            match name.as_str() {
                "int" => VarType::Number,
                "bool" => VarType::Bool,
                "string" => VarType::Text,
                "void" => VarType::Void,
                _ => panic!("{}", name),
            }
        }

        fn expect_token_type(&mut self, type_: TokenType) -> Result<Token, Errors> {
            let token = self
                .lex
                .peek_next_token()
                .ok_or(Errors::TokenTypeExpected(type_, "Nothing".to_owned()))?;

            if !(token.type_ == type_) {
                return Err(Errors::TokenTypeExpected(
                    type_,
                    format!("{:#?}", token.type_),
                ));
            } else {
                self.lex.next_token();
                return Ok(token);
            }
        }

        fn expect_exact_token<T: ToString>(&mut self, data: T) -> Result<Token, Errors> {
            let data = data.to_string();
            let token = self
                .lex
                .peek_next_token()
                .ok_or(Errors::TokenExpected(data.clone(), "Nothing".to_owned()))?;
            if !(token.data == data) {
                return Err(Errors::TokenExpected(data.clone(), token.data));
            }
            self.lex.next_token();
            Ok(token)
        }

        fn collect_until(&mut self, stop: &str) -> Vec<Token> {
            let mut exp = vec![];
            let mut next_token = self
                .lex
                .next_token()
                .expect(format!("exepected {}", stop).as_str());
            while next_token.data != stop {
                exp.push(next_token);
                next_token = self
                    .lex
                    .next_token()
                    .expect(format!("exepected {}", stop).as_str());
            }
            exp
        }

        fn parse_exp(
            &mut self,
            toks: Vec<Token>,
            scope: usize,
            _top_call: bool,
        ) -> Result<(AstNode, Vec<VarType>), Errors> {
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
                                return Ok((
                                    AstNode::Identifier(Variable {
                                        type_: var.type_.clone(),
                                        name: next_token.data.clone(),
                                        arg: false,
                                    }),
                                    vec![var.type_.clone()],
                                ));
                            }
                        } else if toks.len() == 1 {
                            panic!("{} not defined", next_token.data);
                        }
                    }
                    TokenType::Literal(literal_tok) => {
                        if toks.len() == 1 {
                            let lit = Literal::try_new(literal_tok, next_token.data);
                            return Ok((AstNode::Literal(lit.0), vec![lit.1]));
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
                // all_operators
                //     .iter()
                //     .min_by(|x, y| x.0.cmp(&y.0))
                //     .expect("No operator")
                //     .1
                let mut winner = all_operators[0];
                for op in all_operators {
                    if op.0 <= winner.0 {
                        winner = op;
                    }
                }
                winner.1
            };

            let tok = toks[op].clone();

            //function call
            if tok.data.clone() == "->" {
                //the function name
                let name = toks[op + 1].clone();

                let mut args = vec![];
                let mut index = 0;
                while index < op {
                    if toks[index].data.clone() != "(".to_string() {
                        return Err(Errors::TokenExpected(
                            "(".to_owned(),
                            toks[index].data.clone(),
                        ));
                    }
                    let mut bracket_count = 0;
                    let mut exp = vec![];

                    let mut first_iter = true;
                    while bracket_count > 0 || first_iter {
                        first_iter = false;
                        let token = toks.get(index).ok_or(Errors::TokenExpected(
                            "expresion".to_owned(),
                            "nothing".to_owned(),
                        ))?;

                        let token = token.clone();

                        if token.data == "(" {
                            bracket_count += 1;
                        } else if token.data == ")" {
                            bracket_count -= 1;
                        }
                        exp.push(token);
                        index += 1;
                    }
                    exp.remove(0);
                    exp.pop(); // remove the trailing ")"
                    let arg = self.parse_exp(exp, scope, false)?;
                    args.push(arg);
                }

                let func_var;
                if name.data == "return" {
                    let mut inputs = vec![];
                    for (_, args) in &args {
                        for arg in args {
                            inputs.push(arg.clone())
                        }
                    }
                    func_var = FunctionArgs {
                        func_name: "return".to_owned(),
                        args: inputs.clone(),
                        returns: inputs.clone(),
                    }
                } else {
                    func_var = self.program[scope]
                        .get_func(name.data.clone(), &self.program)
                        .ok_or(Errors::TokenExpected(
                            name.data.clone(),
                            "not valid func".to_owned(),
                        ))?
                        .clone();
                }

                let op_node = FunctionOperator::new(args, func_var)?;
                let node = AstNode::FunctionOperator(op_node.0);
                return Ok((node, op_node.1));
            } else {
                let left =
                    self.parse_exp(toks[least_brackets as usize..op].to_vec(), scope, false)?;
                let right = self.parse_exp(
                    toks[op + 1..toks.len() - least_brackets as usize].to_vec(),
                    scope,
                    false,
                )?;
                if left.1.len() > 1 || right.1.len() > 1 {
                    return Err(Errors::ArgCount(1, left.1.len()));
                }

                let left = (left.0, left.1[0].clone());
                let right = (right.0, right.1[0].clone());

                let op_variant = Operators::new(tok.data.clone());
                let op_node = BinaryOperator::new(left, right, op_variant)?;
                let node = AstNode::BinaryOperator(op_node.0);
                return Ok((node, vec![op_node.1]));
            }
        }

        fn parse_keyword(
            &mut self,
            tok: String,
            scope: usize,
        ) -> Result<(AstNode, Option<(Vec<VarType>, bool)>), Errors> {
            match tok.as_str() {
                "if" => {
                    let mut next_token = self.lex.next_token().expect("exepected do");
                    let mut logical_nodes = vec![];
                    while next_token.data != "do" {
                        logical_nodes.push(next_token);
                        next_token = self.lex.next_token().expect("exepected do");
                    }
                    let logical_exp = self.parse_exp(logical_nodes, scope, false)?;
                    if logical_exp.1.len() == 0 || logical_exp.1[0] != VarType::Bool {
                        return Err(Errors::WrongType(vec![VarType::Bool], logical_exp.1));
                    }
                    let block = self.parse_block(scope, None)?;
                    Ok((
                        AstNode::If(Box::new(logical_exp.0), Box::new(AstNode::Block(block.0))),
                        Some((block.1, block.2)),
                    ))
                },
                "while" => {
                    let mut next_token = self.lex.next_token().expect("exepected do");
                    let mut logical_nodes = vec![];
                    while next_token.data != "do" {
                        logical_nodes.push(next_token);
                        next_token = self.lex.next_token().expect("exepected do");
                    }
                    let logical_exp = self.parse_exp(logical_nodes, scope, false)?;
                    if logical_exp.1.len() == 0 || logical_exp.1[0] != VarType::Bool {
                        return Err(Errors::WrongType(vec![VarType::Bool], logical_exp.1));
                    }
                    let block = self.parse_block(scope, None)?;
                    Ok((
                        AstNode::While(Box::new(logical_exp.0), Box::new(AstNode::Block(block.0))),
                        Some((block.1, block.2)),
                    ))
                }
                "let" => {
                    let ident = self.expect_token_type(TokenType::Identifier)?;

                    self.expect_exact_token(":".to_owned())?;

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

                    let exp = self.collect_until(";");
                    let exp = self.parse_exp(exp, scope, false)?;

                    if exp.1.len() > 1 {
                        return Err(Errors::ArgCount(1, exp.1.len()));
                    }

                    let exp = (exp.0, exp.1[0].clone());

                    let left = AstNode::Identifier(Variable {
                        name: ident.data,
                        type_: type_.clone(),
                        arg: false,
                    });
                    let op_variant = Operators::new("=".to_owned());
                    let op_node = BinaryOperator::new((left, type_), exp, op_variant)?;

                    Ok((AstNode::BinaryOperator(op_node.0), None))
                }
                "func" => {
                    let function_name = self.expect_token_type(TokenType::Identifier)?;

                    self.expect_exact_token("(".to_owned())?;

                    let mut function_args = vec![];
                    let mut token = self.expect_token_type(TokenType::Identifier);
                    while let Ok(ref arg) = token {
                        self.expect_exact_token(":".to_owned())?;
                        let type_token = self.expect_token_type(TokenType::Identifier)?;
                        let type_ = Tree::<'a>::parse_type(type_token.data);
                        function_args.push((arg.data.clone(), type_));

                        token = self.expect_token_type(TokenType::Identifier);
                    }

                    self.expect_exact_token(")".to_owned())?;
                    self.expect_exact_token(":".to_owned())?;

                    let mut returns = vec![];
                    let mut type_token = self.expect_token_type(TokenType::Identifier);
                    while let Ok(ref arg) = type_token {
                        let type_ = Tree::<'a>::parse_type(arg.data.clone());
                        if type_ != VarType::Void {
                            returns.push(type_);
                        }

                        type_token = self.expect_token_type(TokenType::Identifier);
                    }

                    self.program[scope].add_function(
                        function_name.data.clone(),
                        function_args.iter().map(|f| f.1.clone()).collect(),
                        returns.clone(),
                    );

                    let mut funcblock = Block {
                        data: vec![],
                        variables: vec![],
                        funcs: vec![],
                        parent: Some(scope),
                    };
                    for (name, type_) in function_args {
                        let var = Variable {
                            name,
                            type_,
                            arg: true,
                        };
                        funcblock.variables.push(var);
                    }

                    let inner_code = self.parse_block(scope, Some(funcblock))?;
                    if returns != inner_code.1 {
                        return Err(Errors::WrongType(returns.clone(), inner_code.1));
                    }
                    Ok((
                        AstNode::Function(Function {
                            name: function_name.data,
                            inner_block: inner_code.0,
                        }),
                        None,
                    ))
                }

                _ => panic!("Unknown keyword"),
            }
        }

        //after we find a do or at the start of the program
        fn parse_block(
            &mut self,
            scope: usize,
            block: Option<Block>,
        ) -> Result<(usize, Vec<VarType>, bool), Errors> {
            let scope = scope;
            let mut returns = vec![];
            let mut return_check = vec![];
            let mut return_called = false;
            let start = block.unwrap_or(Block {
                data: vec![],
                variables: vec![],
                funcs: vec![],
                parent: Some(scope),
            });
            self.program.push(start);
            let innerscope = self.program.len() - 1;

            loop {
                let token = self.lex.next_token().expect("expected end");
                match token.type_ {
                    TokenType::Keyword => {
                        if token.data == "end" {
                            if return_check != returns
                                && (returns.len() > 0 && return_check.len() > 0)
                            {
                                return Err(Errors::WrongType(return_check, returns));
                            }
                            return Ok((innerscope, returns, return_called));
                        }
                        if token.data == "do" {
                            let new_node = self.parse_block(innerscope, None)?;
                            if returns.len() == 0 {
                                returns = new_node.1;
                            } else if returns != new_node.1 {
                                return Err(Errors::WrongType(returns.clone(), new_node.1));
                            }
                            self.program[innerscope]
                                .data
                                .push(AstNode::Block(new_node.0));
                        } else {
                            let new_node = self.parse_keyword(token.data, innerscope)?;
                            if let Some(n) = new_node.1 {
                                if returns.len() == 0 {
                                    if n.1 {
                                        returns = n.0;
                                    } else if return_check.len() == 0 {
                                        return_check = n.0;
                                    } else if return_check != n.0 {
                                        return Err(Errors::WrongType(returns.clone(), n.0));
                                    }
                                } else if returns != n.0 {
                                    return Err(Errors::WrongType(returns.clone(), n.0));
                                }
                            }
                            self.program[innerscope].data.push(new_node.0);
                        }
                    }
                    TokenType::Identifier => {
                        let mut exp = self.collect_until(";");
                        exp.insert(0, token);
                        let call = self.parse_exp(exp, innerscope, true)?.0;
                        self.program[innerscope].data.push(call);
                    }
                    TokenType::Literal(_) => panic!("did not expect literal did you forget ("),
                    TokenType::Operator => {
                        return Err(Errors::TokenExpected(
                            "A statement".to_owned(),
                            token.data.clone(),
                        ));
                    }
                    TokenType::Deliminator => {
                        let mut exp = self.collect_until(";");
                        exp.insert(0, token);
                        let call = self.parse_exp(exp, innerscope, true)?;
                        if call.1.len() != 0 {
                            if returns.len() == 0 {
                                returns = call.1;
                            } else if returns != call.1 {
                                return Err(Errors::WrongType(returns.clone(), call.1));
                            }
                            return_called = true;
                        }

                        self.program[innerscope].data.push(call.0);
                    }
                    TokenType::Unkown => todo!(),
                }
            }
        }
    }
}
