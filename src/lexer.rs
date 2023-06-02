#[derive(Debug)]
pub struct Token {
    ttype: TokenType,

    // TODO(spff): При обработке ошибок было бы классно выводить место в строке, где произошёл инцидент
    // TODO(spff): Вынести мета-информацию о токене в отдельную структуру
    line_num: usize,
    tab_lvl: usize, // Кол-во пробелов перед первым символом строки
}

impl Token {
    fn new(ttype: TokenType, line_num: usize, tab_lvl: usize) -> Self {
        Self {
            ttype,
            line_num,
            tab_lvl,
        }
    }

    pub fn ttype(&self) -> TokenType {
        self.ttype.clone()
    }

    pub fn line_num(&self) -> usize {
        self.line_num
    }

    pub fn tab_lvl(&self) -> usize {
        self.tab_lvl
    }
}

#[derive(Debug, Clone)]
pub enum TokenType {
    Number { value: f64 },
    Str { contents: String },
    Identifier { name: String },

    Equals,
    EqEq,
    Lparen,
    Rparen,
    Comma,

    // Ключевые слова
    If,
    Elif,
    Else,
    While,
    Fn,
    Return,
    Continue,
    Break,

    True,
    False,

    Print,
    Sin,
    Cos,

    // Арифметика
    Plus,
    Minus,
    Mult,
    Div,
    Mod,
    Pow,
    Fact,

    // Сравнения
    Less,
    LessEq,
    Greater,
    GreaterEq,
    Not,
    NotEq,

    // Конец линии
    Endl,
}

pub struct Lexer {
    output: Vec<Token>,

    curr_line: Vec<char>,
    pos: usize,
    line_num: usize,
    tab_lvl: usize,
}

macro_rules! push_token {
    ($lexer: ident, $tok_type: expr) => {
        $lexer
            .output
            .push(Token::new($tok_type, $lexer.line_num, $lexer.tab_lvl))
    };
}

// При форматировании файла выражение ниже почему-то уезжает вправо,
// не пытайтесь поправлять вручную
macro_rules! next {
    ($lexer: ident) => {{
        $lexer.pos += 1;
        let Some(res) = $lexer.curr_line.get($lexer.pos) else {
                                        return Err(err!($lexer, "unexpected line end"));
                                    };
        res
    }};
}

macro_rules! err {
    ($lexer: ident, $src: literal) => {
        format!("line {}: ", $lexer.line_num) + &format!($src)
    };
}

use TokenType::*;
impl Lexer {
    pub fn lex(src: &str) -> Result<Vec<Token>, String> {
        let mut l = Self {
            output: vec![],
            curr_line: vec![],
            pos: 0,
            line_num: 1,
            tab_lvl: 0,
        };

        for (line_num, line) in src.lines().enumerate() {
            l.curr_line = line.chars().collect();
            l.pos = 0;
            l.line_num = line_num + 1;

            let old_len = l.output.len();
            l.lex_line()?;
            let new_len = l.output.len();

            // Не ставим Endl в конец пустых линий
            if new_len != old_len {
                push_token!(l, Endl);
            }
        }

        if l.output.is_empty() {
            return Err(format!("source file contains no code"));
        }

        Ok(l.output)
    }

    fn lex_line(&mut self) -> Result<(), String> {
        while let Some(t) = self.curr_line.get(self.pos) {
            if !t.is_whitespace() {
                break;
            }
            self.pos += 1;
        }
        self.tab_lvl = self.pos;

        while let Some(c) = self.curr_line.get(self.pos) {
            match c {
                _ if c.is_whitespace() => {
                    self.pos += 1;
                    continue;
                }
                _ if c.is_numeric() => self.number()?,
                _ if c.is_alphabetic() => self.identifier(),
                '\'' => self.string()?,
                '=' => {
                    let n = next!(self);
                    match n {
                        '=' => push_token!(self, EqEq),
                        _ => push_token!(self, Equals),
                    }
                }
                '(' => push_token!(self, Lparen),
                ')' => push_token!(self, Rparen),
                ',' => push_token!(self, Comma),
                '+' => push_token!(self, Plus),
                '-' => push_token!(self, Minus),
                '*' => push_token!(self, Mult),
                '/' => push_token!(self, Div),
                '%' => push_token!(self, Mod),
                '^' => push_token!(self, Pow),
                '!' => push_token!(self, Fact),
                '<' => {
                    let n = next!(self);
                    match n {
                        '=' => push_token!(self, LessEq),
                        _ => push_token!(self, Less),
                    }
                }
                '>' => {
                    let n = next!(self);
                    match n {
                        '=' => push_token!(self, GreaterEq),
                        _ => push_token!(self, Greater),
                    }
                }
                '~' => {
                    let n = next!(self);
                    match n {
                        '=' => push_token!(self, NotEq),
                        _ => push_token!(self, Not),
                    }
                }
                s => return Err(err!(self, "unexpected symbol {s}")),
            }

            self.pos += 1;
        }

        Ok(())
    }

    fn number(&mut self) -> Result<(), String> {
        let mut num_str = String::new();

        while let Some(c) = self.curr_line.get(self.pos) {
            if !c.is_numeric() && !(*c == '.') {
                self.pos -= 1;
                break;
            }
            num_str.push(*c);

            self.pos += 1;
        }

        let Ok(value) = num_str.parse::<f64>() else {
            return Err(err!(self, "invalid number: '{num_str}'"))
        };

        push_token!(self, Number { value });

        Ok(())
    }

    fn identifier(&mut self) {
        let mut name = String::new();

        while let Some(c) = self.curr_line.get(self.pos) {
            if !c.is_alphanumeric() && !(*c == '_') {
                self.pos -= 1;
                break;
            }
            name.push(*c);

            self.pos += 1;
        }

        if let Some(keyword) = Lexer::as_keyword(&name) {
            push_token!(self, keyword);
            return;
        }

        push_token!(self, Identifier { name });
    }

    fn as_keyword(src: &str) -> Option<TokenType> {
        let keyword = match src {
            "if" => If,
            "elif" => Elif,
            "else" => Else,
            "while" => While,
            "fn" => Fn,
            "return" => Return,
            "continue" => Continue,
            "break" => Break,

            "true" => True,
            "false" => False,

            "print" => Print,
            "sin" => Sin,
            "cos" => Cos,
            _ => return None,
        };

        Some(keyword)
    }

    fn string(&mut self) -> Result<(), String> {
        self.pos += 1;
        let mut contents = String::new();

        while let Some(c) = self.curr_line.get(self.pos) {
            let mut c = *c; // Рофл из-за `.get()`
            match c {
                '\'' => break,
                '\\' => {
                    let esc = next!(self);

                    match esc {
                        'n' => c = '\n',
                        other => {
                            return Err(err!(
                                self,
                                "unsupported escape sequence: \\{other}"
                            ))
                        }
                    }
                }
                _ => (),
            }
            contents.push(c);

            self.pos += 1;
        }

        push_token!(self, Str { contents });
        Ok(())
    }
}
