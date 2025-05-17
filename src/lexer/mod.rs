//! A lexical analyzer (lexer/tokenizer) for Nix expression language.
//!
//! Ported from official Nix grammar written in flex grammar, this should be fully compatible with
//! the official Nix lexer i.e. it should be able to tokenize anything the official Nix lexer can
//! tokenize. But produced `Token`s will be different although minimal. Some of the differences
//! include (but not limited to)
//!
//! 1. URI literals are feature-based opt-in, as they'll eventually be deprecated.
//! 2. Trailing slashes are allowed in **unambiguous** search paths and paths.
//! 3. Whitespaces and comments are entirely preserved.
//! 4. Paths are never turned into string even as fragments, unlike the original lexer which turns
//!    them into strings. They're yielded as separate `PathFragment` tokens.
//! 5. Primitives such as `true`, `false` and `null` are keywords instead of builtins unlike in the
//!    original lexer.
//!
//! These differences arise as a result of deliberate goal of this lexer, to make it infallible and
//! preserve the entire source **concretely**. This is ideal for constructing CSTs for use of error
//! reporting and formatting in IDEs and text editors. Even invalid tokens will be preserved.
//!
//! The lexer should work with a parser to maintain full compatibility with the official Nix
//! language. Validating numeral literals before parsing and reporting trailing slashes and invalid
//! characters should be done by the parser.

#[cfg(test)]
mod tests;

use core::ops::Range;
use core::str::Chars;

const EOF_CHAR: char = '\0';

/// Matches if the character is of pattern [a-zA-Z0-9\+\-\.]
#[inline]
fn is_uri_char_before_colon(c: char) -> bool {
    c.is_ascii_alphabetic() || c.is_ascii_digit() || matches!(c, '+' | '-' | '.')
}

/// Matches if the character is of pattern [a-zA-Z0-9\%\/\?\:\@\&\=\+\$\,\-\_\.\!\~\*\']
#[rustfmt::skip]
#[inline]
fn is_uri_char_after_colon(c: char) -> bool {
    c.is_ascii_alphabetic() 
        || c.is_ascii_digit() 
        || matches!(c, '%' | '/' | '?' | ':' | '@' | '&' | '=' | '+' | '$' | ',' | '-' | '_' | '.' | '!' | '~' | '*' | '\'')
}

/// Matches if the character is of pattern [a-zA-Z0-9\.\_\-\+]
#[inline]
fn is_path_char(c: char) -> bool {
    c.is_ascii_alphabetic() || c.is_ascii_digit() || matches!(c, '.' | '_' | '-' | '+')
}

/// Matches if the character is of pattern [a-zA-Z\_]
#[inline]
fn is_identifier_start(c: char) -> bool {
    c.is_ascii_alphabetic() || c == '_'
}

/// Matches if the character is of pattern [a-zA-Z0-9\_\'\-]
#[inline]
fn is_identifier_continue(c: char) -> bool {
    c.is_ascii_alphabetic() || c.is_ascii_digit() || matches!(c, '_' | '\'' | '-')
}

#[derive(Debug, PartialEq)]
pub enum TokenKind {
    Unknown,
    Whitespace,
    InlineComment,
    BlockComment,

    Plus,
    Minus,
    Asterisk,
    Slash,
    Greater,
    Less,
    Equal,
    Not,
    Dot,
    At,
    Colon,
    Semicolon,
    Comma,
    Question,
    GreaterEqual,
    LessEqual,
    EqualEqual,
    NotEqual,
    And,
    Or,
    Imply,
    Update,
    Concat,
    #[cfg(feature = "pipe-operators")]
    PipeFrom,
    #[cfg(feature = "pipe-operators")]
    PipeInto,
    OpenParen,
    CloseParen,
    OpenBracket,
    CloseBracket,
    OpenBrace,
    CloseBrace,
    OpenQuote,
    CloseQuote,
    OpenIndentQuote,
    CloseIndentQuote,
    DollarCurly,
    Ellipsis,

    If,
    Then,
    Else,
    Assert,
    With,
    Let,
    In,
    Rec,
    Inherit,
    OrKeyword,
    True,
    False,
    Null,

    #[cfg(feature = "uri-literals")]
    Uri,
    SearchPath,
    HomePathStart,
    PathStart,
    PathFragment,
    PathEnd,
    Identifier,
    Int,
    Float,
    StringContent,
    IndentedStringContent,
}

impl TokenKind {
    pub fn parse_keyword(keyword: &str) -> Option<Self> {
        match keyword {
            "if" => Some(Self::If),
            "then" => Some(Self::Then),
            "else" => Some(Self::Else),
            "assert" => Some(Self::Assert),
            "with" => Some(Self::With),
            "let" => Some(Self::Let),
            "in" => Some(Self::In),
            "rec" => Some(Self::Rec),
            "inherit" => Some(Self::Inherit),
            "or" => Some(Self::OrKeyword),
            "true" => Some(Self::True),
            "false" => Some(Self::False),
            "null" => Some(Self::Null),
            _ => None,
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Range<usize>,
}

impl Token {
    pub fn new(kind: TokenKind, span: Range<usize>) -> Self {
        Self { kind, span }
    }
}

/// Manages a character iterator and it's current byte index (offset).
/// Provides methods to advance the iterator and update the offset.
#[derive(Clone)]
pub(crate) struct Cursor<'a> {
    chars: Chars<'a>,
    pub(crate) offset: usize,
}

impl<'a> Cursor<'a> {
    pub fn new(chars: Chars<'a>) -> Self {
        Self { chars, offset: 0 }
    }

    /// Checks if the character iterator is empty.
    fn is_eof(&self) -> bool {
        self.chars.as_str().is_empty()
    }

    /// Peeks at the next character from the iterator without advancing it.
    /// Returns NULL character in case the iterator ends.
    fn first(&self) -> char {
        self.chars.clone().next().unwrap_or(EOF_CHAR)
    }

    /// Peeks at the **next next** character from the iterator without advancing it.
    /// Returns NULL character in case the iterator ends.
    fn second(&self) -> char {
        let mut iter = self.chars.clone();
        iter.next();
        iter.next().unwrap_or(EOF_CHAR)
    }

    /// Peeks at the **next next next** character from the iterator without advancing it.
    /// Returns NULL character in case the iterator ends.
    fn third(&self) -> char {
        let mut iter = self.chars.clone();
        iter.next();
        iter.next();
        iter.next().unwrap_or(EOF_CHAR)
    }

    /// Consumes the next character from the iterator and advances the offset.
    /// Returns NULL character in case the iterator ends.
    fn bump(&mut self) -> char {
        if let Some(c) = self.chars.next() {
            self.offset += c.len_utf8();
            c
        } else {
            EOF_CHAR
        }
    }

    /// Consumes from the iterator as long as `precicate` holds and the end of the
    /// iterator is not reached yet.
    fn eat_while(&mut self, predicate: impl Fn(char) -> bool) {
        loop {
            let c = self.first();
            if predicate(c) && !self.is_eof() {
                self.bump();
            } else {
                break;
            }
        }
    }

    /// Consumes from the iterator as long as `predicate` holds separated by provided `delimiter`.
    /// Returns whether there's a trailing delimiter or not.
    fn eat_while_delimited(&mut self, predicate: impl Fn(char) -> bool, delimiter: char) -> bool {
        loop {
            self.eat_while(&predicate);
            if self.first() == delimiter {
                self.bump();
                if predicate(self.first()) {
                    continue;
                }
                break true;
            }
            break false;
        }
    }
}

#[derive(Clone, Copy, PartialEq)]
pub enum LexerState {
    Default,
    String,
    IndentedString,
    InPath,
}

pub struct Lexer<'a> {
    source: &'a str,
    token_start: usize,
    cursor: Cursor<'a>,
    state_stack: Vec<LexerState>,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            source,
            token_start: 0,
            cursor: Cursor::new(source.chars()),
            state_stack: Vec::new(),
        }
    }

    fn spanned(&self, token_kind: TokenKind) -> Option<Token> {
        Some(Token::new(token_kind, self.token_start..self.cursor.offset))
    }

    fn block_comment(&mut self) -> TokenKind {
        let _terminated = loop {
            let current_char = self.cursor.bump();
            let next_char = self.cursor.first();

            // In case the comment isn't terminated.
            if current_char == EOF_CHAR && self.cursor.is_eof() {
                break false;
            }

            if current_char == '*' && next_char == '/' {
                self.cursor.bump();
                break true;
            }
        };

        TokenKind::BlockComment
    }

    fn is_exponent_ahead(&self) -> bool {
        if !matches!(self.cursor.first(), 'e' | 'E') {
            return false;
        }

        match self.cursor.second() {
            '-' | '+' => self.cursor.third().is_ascii_digit(),
            '0'..='9' => true,
            _ => false,
        }
    }

    /// This method should be invoked only when at least one digit and the decimal separator has
    /// been consumed already.
    fn float(&mut self) -> TokenKind {
        self.cursor.eat_while(|c| c.is_ascii_digit());

        if self.is_exponent_ahead() {
            self.cursor.bump();
            if matches!(self.cursor.first(), '-' | '+') {
                self.cursor.bump();
            }
            self.cursor.eat_while(|c| c.is_ascii_digit());
        }

        TokenKind::Float
    }

    fn number(&mut self) -> TokenKind {
        self.cursor.eat_while(|c| c.is_ascii_digit());

        if self.cursor.first() == '.' {
            self.cursor.bump();
            return self.float();
        }

        TokenKind::Int
    }

    fn lex_default(&mut self) -> TokenKind {
        let current_char = self.cursor.bump();
        let next_char = self.cursor.first();
        let next_next_char = self.cursor.second();

        // Try to tokenize a URI literal if it starts with an ASCII alphabet.
        #[cfg(feature = "uri-literals")]
        if current_char.is_ascii_alphabetic() {
            let mut cursor = self.cursor.clone();
            cursor.eat_while(is_uri_char_before_colon);

            if cursor.first() == ':' && is_uri_char_after_colon(cursor.second()) {
                cursor.bump();
                cursor.eat_while(is_uri_char_after_colon);

                self.cursor = cursor;
                return TokenKind::Uri;
            }
        }

        // Try to tokenize a SearchPath if it starts with a < followed by a PATH_CHAR.
        if current_char == '<' && is_path_char(next_char) {
            let mut cursor = self.cursor.clone();
            cursor.eat_while_delimited(is_path_char, '/');

            if cursor.first() == '>' {
                cursor.bump();

                self.cursor = cursor;
                return TokenKind::SearchPath;
            }
        }

        // Try to tokenize a PathStart if it starts with PATH_CHAR pattern.
        if is_path_char(current_char) {
            let mut cursor = self.cursor.clone();
            cursor.eat_while(is_path_char);

            if cursor.first() == '/' && is_path_char(cursor.second()) {
                cursor.bump();
                cursor.eat_while_delimited(is_path_char, '/');

                self.cursor = cursor;
                self.state_stack.push(LexerState::InPath);
                return TokenKind::PathStart;
            }

            if cursor.first() == '/' && cursor.second() == '$' && cursor.third() == '{' {
                cursor.bump();
                self.cursor = cursor;
                self.state_stack.push(LexerState::InPath);
                return TokenKind::PathStart;
            }
        }

        // Handle potential paths starting with / followed by PATH_CHAR or ${.
        let expecting = is_path_char(next_char) || (next_char == '$' && next_next_char == '{');
        if current_char == '/' && expecting {
            self.cursor.eat_while_delimited(is_path_char, '/');

            self.state_stack.push(LexerState::InPath);
            return TokenKind::PathStart;
        }

        // Handle home paths (paths starting with "~/").
        if current_char == '~' && next_char == '/' {
            self.cursor.bump();
            self.cursor.eat_while_delimited(is_path_char, '/');
            self.state_stack.push(LexerState::InPath);
            return TokenKind::HomePathStart;
        }

        // Try to tokenize identifier and keywords.
        if is_identifier_start(current_char) {
            self.cursor.eat_while(is_identifier_continue);
            let id = &self.source[self.token_start..self.cursor.offset];
            return TokenKind::parse_keyword(id).unwrap_or(TokenKind::Identifier);
        }

        match current_char {
            c if c.is_ascii_whitespace() => {
                self.cursor.eat_while(|c| c.is_ascii_whitespace());
                TokenKind::Whitespace
            }
            '#' => {
                self.cursor.eat_while(|c| c != '\n');
                TokenKind::InlineComment
            }
            '/' if next_char == '*' => {
                self.cursor.bump();
                self.block_comment()
            }
            '$' if next_char == '{' => {
                self.cursor.bump();
                self.state_stack.push(LexerState::Default);
                TokenKind::DollarCurly
            }
            '{' => {
                // This only pushes the state just to balance } out.
                self.state_stack.push(LexerState::Default);
                TokenKind::OpenBrace
            }
            '}' => {
                self.state_stack.pop();
                TokenKind::CloseBrace
            }
            '0'..='9' => self.number(),
            '.' if next_char.is_ascii_digit() => self.float(),
            '"' => {
                self.state_stack.push(LexerState::String);
                TokenKind::OpenQuote
            }
            '\'' if next_char == '\'' => {
                self.cursor.bump();
                self.state_stack.push(LexerState::IndentedString);
                TokenKind::OpenIndentQuote
            }
            '+' if next_char == '+' => {
                self.cursor.bump();
                TokenKind::Concat
            }
            '+' => TokenKind::Plus,
            '-' if next_char == '>' => {
                self.cursor.bump();
                TokenKind::Imply
            }
            '-' => TokenKind::Minus,
            '*' => TokenKind::Asterisk,
            '/' if next_char == '/' => {
                self.cursor.bump();
                TokenKind::Update
            }
            '/' => TokenKind::Slash,
            '>' if next_char == '=' => {
                self.cursor.bump();
                TokenKind::GreaterEqual
            }
            '>' => TokenKind::Greater,
            #[cfg(feature = "pipe-operators")]
            '<' if next_char == '|' => {
                self.cursor.bump();
                TokenKind::PipeFrom
            }
            '<' if next_char == '=' => {
                self.cursor.bump();
                TokenKind::LessEqual
            }
            '<' => TokenKind::Less,
            '=' if next_char == '=' => {
                self.cursor.bump();
                TokenKind::EqualEqual
            }
            '=' => TokenKind::Equal,
            '!' => TokenKind::Not,
            '.' if next_char == '.' && next_next_char == '.' => {
                self.cursor.bump();
                self.cursor.bump();
                TokenKind::Ellipsis
            }
            '.' => TokenKind::Dot,
            '@' => TokenKind::At,
            '?' => TokenKind::Question,
            ',' => TokenKind::Comma,
            ':' => TokenKind::Colon,
            ';' => TokenKind::Semicolon,
            '&' if next_char == '&' => {
                self.cursor.bump();
                TokenKind::And
            }
            '|' if next_char == '|' => {
                self.cursor.bump();
                TokenKind::Or
            }
            #[cfg(feature = "pipe-operators")]
            '|' if next_char == '>' => {
                self.cursor.bump();
                TokenKind::PipeInto
            }
            '(' => TokenKind::OpenParen,
            ')' => TokenKind::CloseParen,
            '[' => TokenKind::OpenBracket,
            ']' => TokenKind::CloseBracket,
            _ => TokenKind::Unknown,
        }
    }

    fn lex_in_path(&mut self) -> TokenKind {
        match self.cursor.first() {
            '$' if self.cursor.second() == '{' => {
                self.cursor.bump();
                self.cursor.bump();
                self.state_stack.push(LexerState::Default);
                TokenKind::DollarCurly
            }
            '/' => {
                self.cursor.bump();
                if is_path_char(self.cursor.first()) {
                    self.cursor.eat_while_delimited(is_path_char, '/');
                }
                TokenKind::PathFragment
            }
            c if is_path_char(c) => {
                self.cursor.eat_while_delimited(is_path_char, '/');
                TokenKind::PathFragment
            }
            _ => {
                self.state_stack.pop();
                TokenKind::PathEnd
            }
        }
    }

    fn lex_string(&mut self) -> TokenKind {
        match self.cursor.first() {
            '$' if self.cursor.second() == '{' => {
                self.cursor.bump();
                self.cursor.bump();

                self.state_stack.push(LexerState::Default);
                TokenKind::DollarCurly
            }
            '"' => {
                self.cursor.bump();
                self.state_stack.pop();
                TokenKind::CloseQuote
            }
            _ => {
                loop {
                    let first_char = self.cursor.first();
                    let second_char = self.cursor.second();
                    match first_char {
                        // TODO: How to combine these expressions into one arm?
                        '"' => break,
                        '$' if second_char == '{' => break,
                        // String escapes, e.g. $$ and \$.
                        '\\' => {
                            self.cursor.bump();
                            self.cursor.bump();
                        }
                        '$' if second_char == '$' => {
                            self.cursor.bump();
                            self.cursor.bump();
                        }
                        _ => {
                            self.cursor.bump();
                        }
                    }
                }

                TokenKind::StringContent
            }
        }
    }

    /// This method should always follow a `OpenIdentQuote` token. In the original Nix grammar,
    /// leading whitespace and newlines (but not tabs) are removed and all the strings are dedented
    /// with the smallest indentation prefix from each line. This doesn't do that, all whitespaces
    /// are preserved.
    fn lex_indented_string(&mut self) -> TokenKind {
        match self.cursor.first() {
            '$' if self.cursor.second() == '{' => {
                self.cursor.bump();
                self.cursor.bump();

                self.state_stack.push(LexerState::Default);
                TokenKind::DollarCurly
            }
            '\'' if self.cursor.second() == '\'' => {
                self.cursor.bump();
                self.cursor.bump();

                self.state_stack.pop();
                TokenKind::CloseIndentQuote
            }
            _ => {
                // TODO: Nix's original grammar will parse out escapes into separate tokens,
                // probably to save time finding them later to unescape them. Explore whether we
                // should do that as well or not.
                loop {
                    let first_char = self.cursor.first();
                    let second_char = self.cursor.second();

                    match first_char {
                        '$' if second_char == '{' => break,
                        // String escapes, e.g. $$, ''$, ''' and ''\.
                        '$' if second_char == '$' => {
                            self.cursor.bump();
                            self.cursor.bump();
                        }
                        '\'' if second_char == '\'' => {
                            if !matches!(self.cursor.third(), '$' | '\\' | '\'') {
                                break;
                            } else {
                                self.cursor.bump();
                                self.cursor.bump();
                                self.cursor.bump();
                            }
                        }
                        _ => {
                            self.cursor.bump();
                        }
                    }
                }

                TokenKind::IndentedStringContent
            }
        }
    }

    fn handle_unexpected_eof(&mut self, current_state: LexerState) -> Option<Token> {
        // Empty out state_stack whenever an EOF is encountered unexpectedly, certain states like
        // `InPath` might yield ending tokens first.
        self.state_stack = Vec::new();

        if current_state == LexerState::InPath {
            return self.spanned(TokenKind::PathEnd);
        }
        return None;
    }

    fn next_token(&mut self) -> Option<Token> {
        self.token_start = self.cursor.offset;
        let current_state = *self.state_stack.last().unwrap_or(&LexerState::Default);

        if self.cursor.first() == EOF_CHAR && self.cursor.is_eof() {
            if self.state_stack.is_empty() {
                return None;
            }
            return self.handle_unexpected_eof(current_state);
        }

        let token_kind = match current_state {
            LexerState::Default => self.lex_default(),
            LexerState::String => self.lex_string(),
            LexerState::IndentedString => self.lex_indented_string(),
            LexerState::InPath => self.lex_in_path(),
        };
        self.spanned(token_kind)
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.next_token()
    }
}

/// Creates an iterator that produces `Token`s from provided `source` string.
///
/// This is the main entry point for using this lexer.
///
/// # Example
///
/// ```rust
/// use manito::lexer::{lex, Token};
///
/// let source = "let x = 1 + 2; in x";
/// let tokens: Vec<Token> = lex(source).collect();
/// assert!(!tokens.is_empty());
/// ```
pub fn lex(source: &str) -> impl Iterator<Item = Token> {
    Lexer::new(source)
}
