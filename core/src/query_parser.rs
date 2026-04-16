use std::{io::Error, path::PathBuf};

/* EXPRESSION TYPES */
pub enum Value {
    Int(i64),
    Float(f64),
    Text(String),
    Bool(bool),
    Null,
    // Date and Timestamp
}
pub enum LogicalOperator {
    And,
    Or,
}
pub enum ComparisonOperator {
    Eq,
    NotEq,
    LtEq,
    GtEq,
    Lt,
    Gt,
}
pub type FieldPath = Vec<String>;

// Priority order explanation
/*
 * Logical expressions > comparison expressions > terminal nodes (priority order)
 *
 * Base() variant in Logical/Comparison enums push the lexer down the priority order.
 * Base makes logical/comparison operations optional while keeping them at the correct priority level.
 *
 * EXAMPLE:
 * if lexer doesn't find AND/OR => LogicalExpr::Base(/* */)
 * if lexer doesn't find any binary operator => LogicalExpr::Base(ComparisonExpr::Base(/* */))
 * if lexer finds a Field ("A", "B.B1", etc.) => LogicalExpr::Base(ComparisonExpr::Base(ExprTerminalNode::Field(field_path)))
 */
pub enum LogicalExpr {
    And(Box<LogicalExpr>, Box<LogicalExpr>),
    Or(Box<LogicalExpr>, Box<LogicalExpr>),
    Base(ComparisonExpr), // push lexer to comparison operation
}
pub enum ComparisonExpr {
    Binary(ExprTerminalNode, ComparisonOperator, ExprTerminalNode),
    Base(ExprTerminalNode), // push lexer to the terminal node
}
pub enum ExprTerminalNode {
    Literal(Value),
    Field(FieldPath),
    Grouping(Box<LogicalExpr>), // push lexer to a sub-condition (inside parantheses)
}

/* AST TYPES */
pub enum Keyword {
    Select,
    From,
    Where,
    And,
    Or,
}
pub enum Symbol {
    OpenParan,
    CloseParan,
    Dot,
    Comma,
    DoubleQt,
    SingleQt,
}
pub enum Token {
    Keyword(Keyword),
    Identifier(String),
    Literal(Value),
    LogicalOperator(LogicalOperator),
    ComparisonOperator(ComparisonOperator),
    Symbol(Symbol),
}
pub type SourceFilePath = PathBuf;
pub struct Parser {
    tokens: Vec<Token>,
    position: usize, // determines the starting point of the token consumption (consumption starts from this index)
}
pub struct AST {
    source: SourceFilePath,
    projections: Vec<FieldPath>,
    conditions: Option<LogicalExpr>,
}

/* UTILITY FUNCTIONS */
pub fn is_operator(ch: char) -> bool {
    return ch == '<' || ch == '>' || ch == '!' || ch == '=';
}

/* FUNCTIONS */
pub fn convert_to_tokens(parser: &mut Parser, query: &str) -> Result<(), Error> {
    let bytes = query.as_bytes();
    while parser.position < bytes.len() {
        let ch = bytes[parser.position] as char;

        if ch.is_whitespace() {
            parser.position += 1;
            continue;
        }

        // number loop (literal)
        if ch.is_digit(10) {
            let mut num_str = String::new();
            let mut has_dot = false;
            let mut j = parser.position;

            while j < bytes.len() {
                let x = bytes[j] as char;
                if x.is_digit(10) {
                    num_str.push(x);
                } else if x == '.' && !has_dot {
                    if j + 1 < bytes.len() && (bytes[j + 1] as char).is_digit(10) {
                        has_dot = true;
                        num_str.push(x);
                    } else {
                        break;
                    }
                } else {
                    break;
                }
                j += 1;
            }
            parser.position = j;
            if has_dot {
                let num: f64 = num_str.parse().map_err(|_| {
                    Error::new(std::io::ErrorKind::InvalidData, "Invalid float format")
                })?;
                parser.tokens.push(Token::Literal(Value::Float(num)));
            } else {
                let num: i64 = num_str.parse().map_err(|_| {
                    Error::new(std::io::ErrorKind::InvalidData, "Invalid integer format")
                })?;
                parser.tokens.push(Token::Literal(Value::Int(num)));
            }
        }
        // literal loop
        if ch == '\'' {
            let mut text_str = String::new();
            let mut j = parser.position + 1;

            while j < bytes.len() {
                if (bytes[j] as char) == '\'' {
                    break;
                } else {
                    text_str.push(bytes[j] as char);
                }
                j += 1;
            }
            // j points to '\'' => skip it
            // otherwise the next loop iteration will consider this a starting of another literal
            parser.position = j + 1;
            parser
                .tokens
                .push(Token::Literal(Value::Text(String::from(&text_str))));
        }
        // text loop (identifier / logical operator)
        else if ch.is_alphabetic() {
            let mut text_str = String::new();
            let mut j = parser.position;

            while j < bytes.len() {
                let x = bytes[j] as char;
                if x.is_alphanumeric() || x == '_' {
                    text_str.push(x);
                } else {
                    break;
                }
                j += 1;
            }
            parser.position = j;

            // logical operator
            if text_str == "AND" {
                parser
                    .tokens
                    .push(Token::LogicalOperator(LogicalOperator::And));
            } else if text_str == "OR" {
                parser
                    .tokens
                    .push(Token::LogicalOperator(LogicalOperator::Or));
            }
            // identifier
            else {
                parser.tokens.push(Token::Identifier(text_str));
            }
        }
        // comparison operator loop
        else if ch == '<' || ch == '>' || ch == '!' || ch == '=' {
            let mut move_by = 1;
            if parser.position + 1 < bytes.len() {
                let next_ch = bytes[parser.position + 1] as char;
                if ch == '<' {
                    let mut opr_variant = ComparisonOperator::Lt;
                    if next_ch == '=' {
                        move_by = 2;
                        opr_variant = ComparisonOperator::LtEq;
                    }
                    parser.tokens.push(Token::ComparisonOperator(opr_variant));
                } else if ch == '>' {
                    let mut opr_variant = ComparisonOperator::Gt;
                    if next_ch == '=' {
                        move_by = 2;
                        opr_variant = ComparisonOperator::GtEq;
                    }
                    parser.tokens.push(Token::ComparisonOperator(opr_variant));
                } else if ch == '!' && next_ch == '=' {
                    move_by = 2;
                    parser
                        .tokens
                        .push(Token::ComparisonOperator(ComparisonOperator::NotEq));
                } else if ch == '=' {
                    parser
                        .tokens
                        .push(Token::ComparisonOperator(ComparisonOperator::Eq));
                }
            } else if ch == '=' {
                parser
                    .tokens
                    .push(Token::ComparisonOperator(ComparisonOperator::Eq));
            } else if ch == '<' {
                parser
                    .tokens
                    .push(Token::ComparisonOperator(ComparisonOperator::Lt));
            } else if ch == '>' {
                parser
                    .tokens
                    .push(Token::ComparisonOperator(ComparisonOperator::Gt));
            }
            parser.position += move_by;
        }
        // symbol loop
        else if ch == '.' || ch == '\'' || ch == '\"' || ch == ',' || ch == '(' || ch == ')' {
            let mut symbol_variant = Symbol::Dot;
            if ch == '\'' {
                symbol_variant = Symbol::SingleQt;
            } else if ch == '\"' {
                symbol_variant = Symbol::DoubleQt;
            } else if ch == ',' {
                symbol_variant = Symbol::Comma;
            } else if ch == '(' {
                symbol_variant = Symbol::OpenParan;
            } else if ch == ')' {
                symbol_variant = Symbol::CloseParan;
            }
            parser.position += 1;
            parser.tokens.push(Token::Symbol(symbol_variant));
        } else {
            parser.position += 1;
        }
    }
    Ok(())
}
pub fn parse_projections(parser: &mut Parser) -> Vec<FieldPath> {
    return Vec::new();
}
pub fn parse_source(parser: &mut Parser) -> SourceFilePath {
    return PathBuf::new();
}
pub fn parse_conditions(parser: &mut Parser) -> Option<LogicalExpr> {
    return Some(LogicalExpr::Base(ComparisonExpr::Base(
        ExprTerminalNode::Literal(Value::Null),
    )));
}
pub fn parse_query(query: &str) -> Result<AST, Error> {
    let mut parser = Parser {
        tokens: Vec::new(),
        position: 0,
    };
    convert_to_tokens(&mut parser, query)?;
    let projections = parse_projections(&mut parser);
    let source = parse_source(&mut parser);
    let conditions = parse_conditions(&mut parser);
    let ast = AST {
        source,
        projections,
        conditions,
    };
    return Ok(ast);
}
