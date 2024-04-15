use std::env;
use std::fs;
use std::iter::Peekable;
use std::collections::HashMap;
use std::io;

#[derive(Debug, PartialEq, Clone)]
enum Token {
    Identifier(String),
    Keyword(String),
    String(String),
    Number(i32),
    True,
    False,
    Or,
    And,
    Plus,
    Minus,
    Asterisk,
    Slash,
    Assign,
    Semicolon,
    OpenParen,
    CloseParen,
    OpenBrace,
    CloseBrace,
    LessThan,
    LessThanEqual,
    GreaterThan,
    GreaterThanEqual,
    EqualTo,
    OpenBracket,
    CloseBracket,
}
pub trait CheckChar {
    fn is_letter(&self) -> bool;
    fn is_number(&self) -> bool;
}


impl CheckChar for char{
    fn is_letter(&self) -> bool {
        self.is_alphabetic() || *self == '_'
    }

    fn is_number(&self) -> bool {
        self.is_digit(10)
    }
}
pub trait CheckStr{
    fn is_keyword(&self) -> bool;
}
impl CheckStr for str{
    fn is_keyword(&self) -> bool{
        match self{
            "if" | "else" | "while" | "print" | "println" | "int" | "string" | "bool" | "input_" => true,
            _ => false
        }
    }
}

fn lex(input: &str) -> Vec<Token> {
    let mut tokens = Vec::new();
    let mut chars = input.chars().peekable();

    while let Some(&c) = chars.peek() {
        match c {
            '+' => {
                chars.next();
                tokens.push(Token::Plus);
            }
            '-' => {
                chars.next();
                tokens.push(Token::Minus);
            }
            '*' => {
                chars.next();
                tokens.push(Token::Asterisk);
            }
            '/' => {
                chars.next();
                tokens.push(Token::Slash);
            }
            '=' => {
                if chars.next() == Some('='){
                    tokens.push(Token::EqualTo)
                }else{
                    tokens.push(Token::Assign);
                }
            }
            ';' => {
                chars.next();
                tokens.push(Token::Semicolon);
            }
            '(' => {
                chars.next();
                tokens.push(Token::OpenParen);
            }
            ')' => {
                chars.next();
                tokens.push(Token::CloseParen);
            }
            '{' => {
                chars.next();
                tokens.push(Token::OpenBrace);
            }
            '}' => {
                chars.next();
                tokens.push(Token::CloseBrace);
            }
            '[' => {
                chars.next();
                tokens.push(Token::OpenBracket);
            }
            ']' => {
                chars.next();
                tokens.push(Token::CloseBracket);
            }
            '<' => {
                chars.next();
                if chars.peek() == Some(&'='){
                    chars.next();
                    tokens.push(Token::LessThanEqual);
                }else{
                    tokens.push(Token::LessThan);
                }
            }
            '>' => {
                chars.next();
                if chars.peek() == Some(&'='){
                    chars.next();
                    tokens.push(Token::GreaterThanEqual);
                }else{
                    tokens.push(Token::GreaterThan);
                }   
            }
            '"' => {
                chars.next();
                let mut s = String::new();
                while let Some(&c) = chars.peek(){
                    if c != '"'{
                        s = s + &c.to_string();
                        chars.next();
                    }else{
                        chars.next();
                        break;
                    }
                }
                tokens.push(Token::String(s))
            }
            _ if c.is_letter() => {
                let mut identifier = String::new();
                while let Some(&c) = chars.peek() {
                    if c.is_letter() || c.is_number() {
                        identifier.push(chars.next().unwrap());
                    } else {
                        break;
                    }
                }
                if identifier == "true" {
                    tokens.push(Token::True);
                } else if identifier == "false" {
                    tokens.push(Token::False);
                }else if identifier.is_keyword() {
                    tokens.push(Token::Keyword(identifier));
                } else if identifier == "true" || identifier == "false"{
                } else {
                    tokens.push(Token::Identifier(identifier));
                }
            }
            _ if c.is_number() => {
                let mut number = 0;
                while let Some(&c) = chars.peek() {
                    if c.is_number(){
                        number = number * 10 + (c as u8 - b'0') as i32;
                        chars.next();
                    } else {
                        break;
                    }
                }
                tokens.push(Token::Number(number));
            }
            _ if c == '&' && chars.peek() == Some(&'&') => {
                chars.next();
                chars.next();
                tokens.push(Token::And);
            }
            _ if c == '|' && chars.peek() == Some(&'|') => {
                chars.next();
                chars.next();
                tokens.push(Token::Or);
            }
            _ => {
                chars.next();
            }

        }
    }

    tokens
}
enum Variable{
    String(String),
    Int(i32),
    Bool(bool)
}

#[allow(dead_code)] #[derive(Clone)]
struct Parser<'a> {
    tokens: Peekable<std::vec::IntoIter<Token>>,
    input: &'a str,
    ints: HashMap<String, i32>,
    strings: HashMap<String, String>,
    bools:HashMap<String, bool>
}
impl<'a> Parser<'a> {
    fn new(input: &'a str) -> Self {
        let tokens = lex(input).into_iter().peekable();
        let ints = HashMap::new();
        let strings = HashMap::new();
        let bools = HashMap::new();
        Parser { tokens, input, ints, strings, bools}
    }
    fn flow_end(&mut self) -> i32 {
        let mut count = 0;
        while let Some(t) = self.tokens.peek(){
            match t {
                Token::OpenBrace => {count = count + 1; self.tokens.next(); count = count + self.flow_end();}
                Token::CloseBrace => {count = count + 1; self.tokens.next(); break;}
                _ => {count = count + 1; self.tokens.next();}
            } 
        }
        count
    }

    fn parse(&mut self) {

        while let Some(tok) = self.tokens.peek(){

            match tok{
                Token::Keyword(word) => {
                    match word.as_str(){
                        "int" => {
                            self.tokens.next();
                            let Some(id_tok) = self.tokens.next()else{panic!()};
                            let mut id = String::new();
                            match id_tok{
                                Token::Identifier(x) =>{id = x;}
                                _ =>{panic!()}
                            }
                            self.tokens.next();
                            
                            let value = self.expression();
                            self.ints.insert(id, value);
                            self.tokens.next();
                        }
                        "string" => {
                            self.tokens.next();
                            let Some(id_tok) = self.tokens.next()else{panic!()};
                            let mut id = String::new();
                            match id_tok{
                                Token::Identifier(x) => {id = x;}
                                _ => { panic!() }
                            }
                            self.tokens.next();
                            let value = self.concatination();
                            self.strings.insert(id, value);
                            self.tokens.next();
                        }
                        "bool" => {
                            self.tokens.next();
                            let Some(id_tok) = self.tokens.next()else{panic!()};
                            let mut id = String::new();
                            match id_tok{
                                Token::Identifier(x) => {id = x;}
                                _ => {panic!()}
                            }
                            self.tokens.next();

                            let value = self.boolean_expr();
                            
                            self.bools.insert(id, value);
                            self.tokens.next();

                        }
                        "print" => {
                            self.tokens.next();
                            print!("{}", self.to_print());
                            self.tokens.next();
                        }
                        "println" => {
                            self.tokens.next();
                            println!("{}", self.to_print());
                            self.tokens.next();
                        }
                        "if" => {
                            self.tokens.next();
                            let p = self.boolean_expr();
                            if !p {
                                self.flow_end();
                                if self.tokens.peek() == Some(&Token::Keyword(String::from("else"))){
                                    self.tokens.next();
                                }
                            }

                        }
                        "else" => {
                            self.flow_end();
                        }
                        "while" => {
                            self.tokens.next();
                            let mut copy_tokens = self.tokens.clone();
                            let mut length_of_condition = 0;
                            while let Some(t) = self.tokens.peek(){
                                match t {
                                    Token::OpenBrace => {self.tokens.next(); break;}
                                    _ => {
                                        self.tokens.next();
                                        length_of_condition = length_of_condition + 1;
                                    }
                                }
                            }
                            let condition_vec:Vec<Token> = copy_tokens.clone().take(length_of_condition).collect();
                            let mut condition = condition_vec.into_iter().peekable();
                            let remaining_copied_tokens = copy_tokens.skip(length_of_condition);
                            let length_of_statment = self.flow_end() as usize;
                            let statment_vec:Vec<Token> = remaining_copied_tokens.take(length_of_statment).collect();
                            let mut statement = statment_vec.into_iter().peekable();
                            let afterloop = self.tokens.clone();
                            loop {
                                self.tokens = condition.clone();
                                if !self.boolean_expr(){
                                    break;
                                }
                                self.tokens = statement.clone();
                                self.parse();
                            }
                            self.tokens = afterloop;
                        }
                        _ => {self.tokens.next();}
                    }
                    
                }
                Token::Identifier(n)=>{
                    let id = n.to_string();
                    self.tokens.next();
                    self.tokens.next();
                    
                    let Some(t) = self.tokens.peek() else {panic!()};
                    let mut s = String::new();
                    let mut i = 0;
                    let mut is_id = String::new();
                    let mut typ = "";
                    match t{
                        Token::String(..) => {s = self.concatination(); typ = "int";}
                        Token::Number(..) => {i = self.expression(); typ = "string";}
                        Token::Identifier(n) => {is_id = n.to_string();}
                        _ => {panic!("h1")}
                    }
                    if is_id != String::new(){
                        if self.ints.contains_key(&is_id) {i = self.expression(); typ = "int";}
                        if self.strings.contains_key(&is_id) {s = self.concatination().to_string(); typ = "string";}
                    } 
                    match typ {
                        "int" => {
                            self.ints.insert(id.clone(), i);
                        }
                        "string" => {
                            self.strings.insert(id.clone(), s);
                        }
                        _ => {panic!()}
                    }
                    self.tokens.next();
                }
                _ => {self.tokens.next();}
            }

        }
    }
    fn get_var(&mut self, z: &String) -> Variable{
        
        if self.strings.contains_key(z){
            Variable::String(self.strings.get(z).unwrap().to_string())
        }else if self.ints.contains_key(z){
            Variable::Int(*self.ints.get(z).unwrap())
        }else if self.bools.contains_key(z){
            Variable::Bool(*self.bools.get(z).unwrap())
        } else {
            panic!("invalid id");
        }
    }
    
    fn to_print(&mut self) -> String{
        let mut s2 = self.clone(); 
        self.tokens.next();
        let mut to_print = String::new();
        while let Some(token) = self.tokens.peek(){
            match token {
                Token::Keyword(k) => {
                    to_print = to_print + &match k.as_str(){
                        "input_" => self.string_input(),
                        _ => panic!(),
                    };
                    self.tokens.next();
                }
                Token::Number(n) => {
                    to_print = to_print + &n.to_string();         
                    self.tokens.next();
                }
                Token::String(s) => {
                    to_print = to_print + s;
                    self.tokens.next();
                }
                Token::True => {
                    to_print = to_print + "true";
                    self.tokens.next();
                }
                Token::False => {
                    to_print = to_print + "false";
                    self.tokens.next();
                }
                Token::Identifier(z) =>{
                    let q = s2.get_var(z);
                    match q{
                        Variable::Int(n) => {to_print = to_print + &n.to_string()}
                        Variable::String(s) => {to_print = to_print + &s}
                        Variable::Bool(b) => {to_print = to_print + &b.to_string()}
                        _ => {panic!("ahh")}
                    }
                    self.tokens.next();
                }
                Token::Plus => {
                    self.tokens.next();
                }
                Token::CloseParen => {
                    self.tokens.next();
                    break;
                }
                _ => {panic!("broke")}
            }
        }
        to_print
    }
    
    fn concatination(&mut self) -> String{
        let mut concated = String::from("");
        let mut s2 = self.clone();
        while let Some(token) = self.tokens.peek(){
            match token{
                Token::String(s)=>{
                    concated = concated + s;
                    self.tokens.next();
                }
                Token::Plus =>{
                    self.tokens.next();
                }
                Token::Keyword(k) => {
                    concated = concated + &match k.as_str(){
                        "input_" => self.string_input(),
                        _ => panic!(),
                    };
                    self.tokens.next();
                }
                Token::Identifier(id) =>{
                    match s2.get_var(id){ 
                        Variable::String(n) => {concated =concated + &n}
                        _ => {panic!("error")}
                    }
                    self.tokens.next();
                }
                _ => {break;}
            }
        }
        concated
    }
    fn int_input(&mut self) -> i32{
        self.string_input().trim().parse().expect("invalid input")
    }
    fn string_input(&mut self) -> String {
        let mut input = String::new();
        io::stdin()
        .read_line(&mut input)
        .expect("Failed to read line");
        input.trim_end_matches('\n').to_string()
    }

    fn expression(&mut self) -> i32 {
        let mut result = self.term();

        while let Some(token) = self.tokens.peek() {
            match token {
                Token::Plus => {
                    self.tokens.next();
                    result += self.term();
                }
                Token::Minus => {
                    self.tokens.next();
                    result -= self.term();
                }
                _ => break,
            }
        }

        result
    }

    fn term(&mut self) -> i32 {
        let mut result = self.factor();

        while let Some(token) = self.tokens.peek() {
            match token {
                Token::Asterisk => {
                    self.tokens.next();
                    result *= self.factor();
                }
                Token::Slash => {
                    self.tokens.next();
                    result /= self.factor();
                }
                _ => break,
            }
        }

        result
    }
        fn factor(&mut self) -> i32 {
        match self.tokens.next() {
            Some(Token::Number(n)) => n,
            Some(Token::Identifier(n)) => {
                match self.get_var(&n){
                    Variable::Int(z) => z,
                    _ => 0,
                }
            }
            Some(Token::Keyword(z)) => {
                match z.as_str() {
                    "input_" => self.int_input(),
                    _ => {panic!()}
                }
            }
            Some(Token::OpenParen) => {
                let result = self.expression();
                if let Some(Token::CloseParen) = self.tokens.next() {
                    result
                } else {
                    panic!("Expected closing parenthesis");
                }
            }
            _ => panic!("Expected number or parenthesized expression"),
        }
        }
    fn boolean_expr(&mut self) -> bool {
        self.boolean_or()
    }

    fn boolean_or(&mut self) -> bool {
        let mut result = self.boolean_and();

        while let Some(token) = self.tokens.peek() {
            match token {
                Token::Or => {
                    self.tokens.next();
                    result = result || self.boolean_and();
                }
                _ => break,
            }
        }

        result
    }

    fn boolean_and(&mut self) -> bool {
        let mut result = self.bool_comp();
        
        while let Some(token) = self.tokens.peek() {
            match token {
                Token::And => {
                    self.tokens.next();
                    result = result && self.bool_comp();
                }
                _ => break,
            }
        }

        result
    }
    fn bool_factor(&mut self) -> bool {
        match self.tokens.peek() {
            Some(Token::False)=> {
                self.tokens.next();
                false
            }
            Some(Token::True)=> {
                self.tokens.next();
                true
            }
            Some(Token::OpenParen) => {
                self.tokens.next();
                let result = self.boolean_expr();
                if let Some(Token::CloseParen) = self.tokens.next() {
                    result
                } else {
                    panic!("Expected closing parenthesis");
                }
            }
            Some(Token::Identifier(z)) => {
                *self.bools.get(z).unwrap()
            }
            _ => {
                panic!();
            }
        }
    }
    fn bool_comp(&mut self) -> bool {
        match self.tokens.peek(){
            Some(Token::Number(..)) => self.int_comp(),
            Some(Token::Identifier(n)) => 
                if self.ints.contains_key(n) {
                    self.int_comp()
                } else {
                    self.bool_factor()
                }
            _ => self.bool_factor(),
        }
    }
    fn int_comp(&mut self) -> bool {
        let a = self.expression();
        let op = self.tokens.next().unwrap();
        let b = self.expression();
        match op {
            Token::LessThan => a.lt(&b),
            Token::GreaterThan => a.gt(&b),
            Token::EqualTo => a.eq(&b),
            Token::LessThanEqual => a.le(&b),
            Token::GreaterThanEqual => a.ge(&b),
            _ => {panic!();}
        }
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let file_path = &args[1];
    println!("file:\n{}", file_path);

    let contents = fs::read_to_string(file_path).expect("File not found");
    print!("text:\n{contents}");  

    let tokens = lex(&contents);
    println!("tokens:\n{:?}\noutput: [", tokens);

    let mut parser = Parser::new(&contents);
    parser.parse();
    println!("\n]\nvariable: \n\t{:?}\n\t{:?}\n\t{:?}",parser.ints, parser.strings, parser.bools);

}
