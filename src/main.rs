#[macro_use]
mod lexer;
mod ast;


use std::fs;

pub use crate::ast::ast::{Tree};

pub use crate::lexer::{lexer::Lexer, lexer::TokenType, lexer::Node, lexer::Token, lexer::Pattern};
fn main() {
    let raw_text = fs::read_to_string("/mnt/c/programming/files/rust/PWS-Compiler/source.txt")
        .expect("Should have been able to read the file") +  "end";

    let mut node = Node::new(TokenType::Unkown, false, None);

    let mut strings = vec![
        pat!("\"", false, "abcdefghijklmnopqrstuvwxyz1234567890", true, "\"", false, type: TokenType::Literal),
        pat!("1234567890", true, type: TokenType::Literal),
        Pattern::Str(("true".to_owned(), TokenType::Literal)),
        Pattern::Str(("false".to_owned(), TokenType::Literal)),


        pat!("abcdefghijklmnopqrstuvwxyz_", true, type: TokenType::Identifier),

        
        pat!("():;", false, type: TokenType::Deliminator),

        Pattern::Str(("=".to_owned(), TokenType::Operator)),
        Pattern::Str(("!".to_owned(), TokenType::Operator)),
        Pattern::Str(("===".to_owned(), TokenType::Operator)),
        Pattern::Str(("call".to_owned(), TokenType::Operator)),
        pat!("+-|><*&", false,  type: TokenType::Operator),
    ];

    strings.extend(str_pat!(
        "let",
        "do",
        "if",
        "end",
        "func",
        type: TokenType::Keyword
    ));

    let lex = Lexer::new(raw_text.chars(), strings, &mut node);

    let mut ast = Tree::new(lex);
    ast.build();
}
