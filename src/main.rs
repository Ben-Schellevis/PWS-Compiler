#[macro_use]
mod lexer;
mod ast;
mod elf;


use std::fs;

pub use crate::ast::ast::Tree;

pub use crate::elf::elf::Elf;
pub use crate::lexer::lexer::{Lexer, TokenType, Node, Token, Pattern, Literal};
fn main() {
    let raw_text = fs::read_to_string("/mnt/c/programming/files/rust/PWS-Compiler/source.txt")
        .expect("Should have been able to read the file") +  "end";

    let mut node = Node::new(TokenType::Unkown, false, None);

    let mut strings = vec![
        pat!("\"", false, "abcdefghijklmnopqrstuvwxyz1234567890!,", true, "\"", false, type: TokenType::Literal(Literal::String)),
        pat!("1234567890", true, type: TokenType::Literal(Literal::Number)),

        Pattern::Str(("true".to_owned(), TokenType::Literal(Literal::Bool))),
        Pattern::Str(("false".to_owned(), TokenType::Literal(Literal::Bool))),


        pat!("abcdefghijklmnopqrstuvwxyz_", true, type: TokenType::Identifier),

        
        pat!("():;", false, type: TokenType::Deliminator),

        Pattern::Str(("===".to_owned(), TokenType::Operator)),
        Pattern::Str(("->".to_owned(), TokenType::Operator)),
        pat!("+-*/><&|=!", false,  type: TokenType::Operator),
    ];

    strings.extend(str_pat!(
        "let",
        "do",
        "if",
        "func",
        "end",
        "while",
        type: TokenType::Keyword
    ));

    strings.push(
        Pattern::Str(("false".to_owned(), TokenType::Literal(Literal::Bool))),
    );

    let lex = Lexer::new(raw_text.chars(), strings, &mut node);

    let ast = Tree::new(lex);
    let x= ast.build();
    let elf = Elf::new(x);
    elf.write();
}





