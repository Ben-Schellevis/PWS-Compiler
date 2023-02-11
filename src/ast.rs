pub mod ast {
    use std::{clone, error::Error, fmt, ops::Index, process, vec};

    use crate::lexer::lexer::{Lexer, Literal as TokenLiteral, Token, TokenType};

    #[derive(Debug, Clone, PartialEq)]
    enum VarType {
        Number,
        Text,
        Bool,
        Function,
        Void,
        Any,
    }

    #[derive(Debug, Clone)]
    struct Literal {
        type_: VarType,
        data: String,
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
    struct Variable {
        name: String,
        type_: VarType,
    }

    #[derive(Debug, Clone)]
    struct FunctionArgs {
        func_name: String,
        args: Vec<VarType>,
        returns: Vec<VarType>
    }

    #[derive(Debug, Clone)]
    struct Block {
        data: Vec<AstNode>,
        variables: Vec<Variable>,
        funcs: Vec<FunctionArgs>,
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

        fn get_func<'a>(
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

        fn add_variable(&mut self, name: String, type_: VarType) {
            self.variables.push(Variable { name, type_ });
        }
        fn add_function(&mut self, func_name: String, args: Vec<VarType>, returns:  Vec<VarType>) {
            self.funcs.push(FunctionArgs { func_name, args, returns});
        }
    }

    #[derive(Debug, Clone)]
    struct BinaryOperator {
        left: Box<AstNode>,
        right: Box<AstNode>,
        type_: String,
    }
    impl BinaryOperator {
        fn validate_type(&self, left: VarType, right: VarType) -> Result<VarType, Errors> {
            match self.type_.as_str() {
                "+" | "-" | "*" | "/" => {
                    if left == VarType::Number && right == VarType::Number {
                        return Ok(VarType::Number);
                    } else {
                        Err(Errors::WrongType(right, self.type_.clone()))
                    }
                }
                ">" | "<" => {
                    if left == VarType::Number && right == VarType::Number {
                        return Ok(VarType::Bool);
                    } else {
                        Err(Errors::WrongType(right, self.type_.clone()))
                    }
                }
                "&" | "|" => {
                    if left == VarType::Bool && right == VarType::Bool {
                        return Ok(VarType::Bool);
                    } else {
                        Err(Errors::WrongType(right, self.type_.clone()))
                    }
                }
                "=" => {
                    if left == right {
                        return Ok(right);
                    } else {
                        Err(Errors::WrongType(right, self.type_.clone()))
                    }
                }
                "===" => {
                    if left == right {
                        return Ok(VarType::Bool);
                    } else {
                        Err(Errors::WrongType(right, self.type_.clone()))
                    }
                }
                _ => Err(Errors::TokenTypeExpected(
                    TokenType::Operator,
                    self.type_.clone(),
                )),
            }
        }

        fn new(
            left: (AstNode, VarType),
            right: (AstNode, VarType),
            op: String,
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
    struct FunctionOperator {
        args: Vec<AstNode>,
        name: String,
    }

    impl FunctionOperator {
        fn new(data: Vec<(AstNode, Vec<VarType>)>, func:FunctionArgs ) -> Result<(FunctionOperator, Vec<VarType>), Errors> {
            let name = func.func_name;
            let args_check = func.args;
            let (args_received, types) : (Vec<AstNode>, Vec<Vec<VarType>>) = data.iter().cloned().unzip();
            let received_arg_count: usize = types.iter().map(Vec::len).sum();
            if args_check.len() != received_arg_count {
                return Err(Errors::ArgCount(args_check.len(), received_arg_count));
            }

            let mut index_of_check = 0;
            for (index, _) in args_received.iter().enumerate() {
                let args_at_index = types[index].clone();
                for type_ in args_at_index {
                    if args_check[index_of_check] != type_ {
                        return Err(Errors::WrongType(args_check[index_of_check].clone(), format!("{:#?}", type_)));
                    }
                    index_of_check += 1;
                }
            }
            Ok((FunctionOperator{
                args: args_received,
                name,
            },func.returns))
        }
    }

    #[derive(Debug, Clone)]
    struct Function {
        name: String,
        inner_block: usize,
    }

    #[derive(Debug, Clone)]
    enum AstNode {
        Block(usize),
        Function(Function),
        If(Box<AstNode>, Box<AstNode>),
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
        set: 9,
    };

    #[derive(Debug)]
    enum Errors {
        TokenExpected(String, String),
        TokenTypeExpected(TokenType, String),
        WrongType(VarType, String),
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
                    write!(f, "{} is not of type {:#?}", recieved, expected)
                }
                Errors::ArgCount(expected, recieved) =>{
                    write!(f, "Expected {} arguments got {}", recieved, expected)
                },
                
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
                    funcs: vec![],
                }],
            }
        }

        pub fn build(&mut self) {
            let done = self.parse_block(0 as usize, None);
            match done {
                Ok(ast) => {
                    println!("{:#?}", self.program[ast]);
                    println!("{:#?}", self.program)
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
                _ => todo!(),
            }
        }

        fn expect_token_type(&mut self, type_: TokenType) -> Result<Token, Errors> {
            let token = self
                .lex
                .peek_next_token()
                .ok_or(Errors::TokenTypeExpected(type_, "Nothing".to_owned()))?;

            if !(token.type_ == type_) {
                return Err(Errors::TokenTypeExpected(type_, token.data));
            } else {
                self.lex.next_token();
                return Ok(token);
            }
        }

        fn expect_exact_token(&mut self, data: String) -> Result<Token, Errors> {
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
                all_operators
                    .iter()
                    .min_by(|x, y| x.0.cmp(&y.0))
                    .expect("No operator")
                    .1
            };

            let tok = toks[op].clone();

            //function call
            if tok.data.clone() == "->" {
                //the function name
                let name = toks[op + 1].clone();

                let func_var = self.program[scope]
                    .get_func(name.data.clone(), &self.program)
                    .ok_or(Errors::TokenExpected(
                        name.data.clone(),
                        "not valid func".to_owned(),
                    ))?.clone();

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
                    let arg = self.parse_exp(exp, scope)?;
                    args.push(arg);
                }

                let op_node = FunctionOperator::new(args, func_var)?;
                let node = AstNode::FunctionOperator(op_node.0);
                return Ok((node, op_node.1));
            } else {
                let left = self.parse_exp(toks[least_brackets as usize..op].to_vec(), scope)?;
                let right = self.parse_exp(
                    toks[op + 1..toks.len() - least_brackets as usize].to_vec(),
                    scope,
                )?;
                if left.1.len() > 1 || right.1.len() > 1 {
                    return Err(Errors::ArgCount(1, left.1.len()));
                }
                
                let left = (left.0, left.1[0].clone());
                let right = (right.0, right.1[0].clone());

                let op_node = BinaryOperator::new(left, right, tok.data.clone())?;
                let node = AstNode::BinaryOperator(op_node.0);
                return Ok((node, vec![op_node.1]));
            }
        }

        fn parse_keyword(&mut self, tok: String, scope: usize) -> Result<AstNode, Errors> {
            match tok.as_str() {
                "if" => {
                    let mut next_token = self.lex.next_token().expect("exepected do");
                    let mut logical_nodes = vec![];
                    while next_token.data != "do" {
                        logical_nodes.push(next_token);
                        next_token = self.lex.next_token().expect("exepected do");
                    }
                    let logical_exp = self.parse_exp(logical_nodes, scope)?.0;
                    let block = self.parse_block(scope, None)?;
                    Ok(AstNode::If(
                        Box::new(logical_exp),
                        Box::new(AstNode::Block(block)),
                    ))
                }
                "let" => {
                    let ident = self.expect_token_type(TokenType::Identifier).unwrap();

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
                    let exp = self.parse_exp(exp, scope)?;

                    if exp.1.len() > 1 {
                        return Err(Errors::ArgCount(1, exp.1.len()));
                    }
                    
                    let exp = (exp.0, exp.1[0].clone());


                    let left = AstNode::Identifier(Variable {
                        name: ident.data,
                        type_: type_.clone(),
                    });
                    let op_node = BinaryOperator::new((left, type_), exp, "=".to_owned())?;

                    Ok(AstNode::BinaryOperator(op_node.0))
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
                        returns.push(type_);

                        type_token = self.expect_token_type(TokenType::Identifier);
                    }




                    self.program[scope].add_function(
                        function_name.data.clone(),
                        function_args.iter().map(|f| f.1.clone()).collect(),
                        returns
                    );

                    let mut funcblock = Block {
                        data: vec![],
                        variables: vec![],
                        funcs: vec![],
                        parent: Some(scope),
                    };
                    for (name, type_) in function_args {
                        let var = Variable { name, type_ };
                        funcblock.variables.push(var);
                    }

                    let inner_code = self.parse_block(scope, Some(funcblock))?;
                    Ok(AstNode::Function(Function {
                        name: function_name.data,
                        inner_block: inner_code,
                    }))
                },
                _ => panic!("Unknown keyword"),
            }
        }

        //after we find a do or at the start of the program
        fn parse_block(&mut self, scope: usize, block: Option<Block>) -> Result<usize, Errors> {
            let scope = scope;
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
                            return Ok(innerscope);
                        }
                        if token.data == "do" {
                            let new_node = self.parse_block(innerscope, None)?;
                            self.program[innerscope].data.push(AstNode::Block(new_node));
                        } else {
                            let new_node = self.parse_keyword(token.data, innerscope)?;
                            self.program[innerscope].data.push(new_node);
                        }
                    }
                    TokenType::Identifier => {
                        let mut exp = self.collect_until(";");
                        exp.insert(0, token);
                        let call = self.parse_exp(exp, innerscope)?.0;
                        self.program[innerscope].data.push(call);
                    }
                    TokenType::Literal(_) => todo!(),
                    TokenType::Operator => {
                        panic!();
                    }
                    TokenType::Deliminator => {
                        let mut exp = self.collect_until(";");
                        exp.insert(0, token);
                        let call = self.parse_exp(exp, innerscope)?.0;
                        self.program[innerscope].data.push(call);
                    },
                    TokenType::Unkown => todo!(),
                }
            }
        }
    }
}



