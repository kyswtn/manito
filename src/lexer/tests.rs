//! Tests are organized in two different categories.
//! 1. Snapshot tests - using `expect_test` and cover basic cases
//! 2. Property tests - using `proptest` and are based on RegExps from the original grammar file.
//!    This provides confidence for compatibility with the original lexer.

use super::*;
use expect_test::{Expect, expect};
use proptest::prelude::*;

fn lex_and_check(input: &str, whitespace: bool, e: Expect) {
    let s: String = lex(input)
        .filter(|token| {
            if !whitespace {
                token.kind != TokenKind::Whitespace
            } else {
                true
            }
        })
        .map(|token| {
            let kind = token.kind;
            let text = &input[token.span];
            format!("{kind:?} {text:?}\n")
        })
        .collect();
    e.assert_eq(&s);
}

#[test]
fn whitespace() {
    let input = "   ";
    lex_and_check(
        input,
        true,
        expect![[r#"
        Whitespace "   "
    "#]],
    );
}

#[test]
fn comment() {
    let input = r#"
        # inline comment one
        # inline comment two
        /* simple block comment */
        /**
          doc comment
        */
        /*
         * not terminated
        "#;
    lex_and_check(
        input,
        false,
        expect![[r##"
            InlineComment "# inline comment one"
            InlineComment "# inline comment two"
            BlockComment "/* simple block comment */"
            BlockComment "/**\n          doc comment\n        */"
            BlockComment "/*\n         * not terminated\n        "
        "##]],
    );
}

#[test]
fn uri_literals() {
    let input = r#"
        x:x
        https://rust-lang.org
        postgresql://username:password@localhost:5432/db?connection_timeout=1s
        "#;
    lex_and_check(
        input,
        false,
        expect![[r#"
        Uri "x:x"
        Uri "https://rust-lang.org"
        Uri "postgresql://username:password@localhost:5432/db?connection_timeout=1s"
    "#]],
    );
}

#[test]
fn paths() {
    let search_paths = "<foo> <foo/bar> <foo/bar/baz/>";
    let regular_paths = "foo/bar foo/${bar}/baz";
    let absolute_paths = "/foo /foo/${bar} /${foo}/bar/baz/";
    let home_paths = "~/foo ~/${foo}/bar ~/foo/${bar}/baz/";
    let input = [search_paths, regular_paths, absolute_paths, home_paths].join("\n");
    lex_and_check(
        &input,
        false,
        expect![[r#"
            SearchPath "<foo>"
            SearchPath "<foo/bar>"
            SearchPath "<foo/bar/baz/>"
            PathStart "foo/bar"
            PathEnd ""
            PathStart "foo/"
            DollarCurly "${"
            Identifier "bar"
            CloseBrace "}"
            PathFragment "/baz"
            PathEnd ""
            PathStart "/foo"
            PathEnd ""
            PathStart "/foo/"
            DollarCurly "${"
            Identifier "bar"
            CloseBrace "}"
            PathEnd ""
            PathStart "/"
            DollarCurly "${"
            Identifier "foo"
            CloseBrace "}"
            PathFragment "/bar/baz/"
            PathEnd ""
            HomePathStart "~/foo"
            PathEnd ""
            HomePathStart "~/"
            DollarCurly "${"
            Identifier "foo"
            CloseBrace "}"
            PathFragment "/bar"
            PathEnd ""
            HomePathStart "~/foo/"
            DollarCurly "${"
            Identifier "bar"
            CloseBrace "}"
            PathFragment "/baz/"
            PathEnd ""
        "#]],
    );
}

#[test]
fn numbers() {
    let valid = "0 12 3. 4.56 .78 9.e1 .0E12 3.e45 6.E+7 8.E-9";
    let invalid = ".e01 2E3 4e+-5";
    let s = [valid, invalid].join("\n");
    lex_and_check(
        &s,
        false,
        expect![[r#"
            Int "0"
            Int "12"
            Float "3."
            Float "4.56"
            Float ".78"
            Float "9.e1"
            Float ".0E12"
            Float "3.e45"
            Float "6.E+7"
            Float "8.E-9"
            Dot "."
            Identifier "e01"
            Int "2"
            Identifier "E3"
            Int "4"
            Identifier "e"
            Plus "+"
            Minus "-"
            Int "5"
        "#]],
    );
}

#[test]
fn strings() {
    let input = r#"
        "drink this water of the spring"
        "rest ${"here"} a while ..."
        "we have a $${} long way yet to go \${}"
        "and i can't go \n without \"you\""
        "#;
    lex_and_check(
        input,
        false,
        expect![[r#"
        OpenQuote "\""
        StringContent "drink this water of the spring"
        CloseQuote "\""
        OpenQuote "\""
        StringContent "rest "
        DollarCurly "${"
        OpenQuote "\""
        StringContent "here"
        CloseQuote "\""
        CloseBrace "}"
        StringContent " a while ..."
        CloseQuote "\""
        OpenQuote "\""
        StringContent "we have a $${} long way yet to go \\${}"
        CloseQuote "\""
        OpenQuote "\""
        StringContent "and i can't go \\n without \\\"you\\\""
        CloseQuote "\""
    "#]],
    );
}

#[test]
fn indented_strings() {
    let input = r#"
        '' drink this water of the spring ''
        '' rest ${'' here ''} a while ... ''
        '' we have a $${} long way yet to go ''${} ''
        '' and i can't go ''\n without '''you''' ''
        "#;
    lex_and_check(
        input,
        false,
        expect![[r#"
        OpenIndentQuote "''"
        IndentedStringContent " drink this water of the spring "
        CloseIndentQuote "''"
        OpenIndentQuote "''"
        IndentedStringContent " rest "
        DollarCurly "${"
        OpenIndentQuote "''"
        IndentedStringContent " here "
        CloseIndentQuote "''"
        CloseBrace "}"
        IndentedStringContent " a while ... "
        CloseIndentQuote "''"
        OpenIndentQuote "''"
        IndentedStringContent " we have a $${} long way yet to go ''${} "
        CloseIndentQuote "''"
        OpenIndentQuote "''"
        IndentedStringContent " and i can't go ''\\n without '''you''' "
        CloseIndentQuote "''"
    "#]],
    );
}

#[test]
fn keywords_and_identifiers() {
    let keywords = "if then else assert with let in inherit rec or true false null";
    let identifiers = "pen pine'apple ap_ple pen0";
    let input = [keywords, identifiers].join("\n");
    lex_and_check(
        &input,
        false,
        expect![[r#"
            If "if"
            Then "then"
            Else "else"
            Assert "assert"
            With "with"
            Let "let"
            In "in"
            Inherit "inherit"
            Rec "rec"
            OrKeyword "or"
            True "true"
            False "false"
            Null "null"
            Identifier "pen"
            Identifier "pine'apple"
            Identifier "ap_ple"
            Identifier "pen0"
        "#]],
    );
}

fn lex_to_kinds(input: &str, whitespace: bool) -> Vec<TokenKind> {
    lex(input)
        .map(|token| token.kind)
        .filter(|kind| {
            if !whitespace {
                *kind != TokenKind::Whitespace
            } else {
                true
            }
        })
        .collect()
}

const WHITESPACE: &str = r"[ \t\r\n]+";
const INLINE_COMMENT: &str = r"\#[^\r\n]*";
const BLOCK_COMMENT: &str = r"\/\*([^*]|\*+[^*/])*\*+\/";
const URI: &str = r"[a-zA-Z][a-zA-Z0-9\+\-\.]*\:[a-zA-Z0-9\%\/\?\:\@\&\=\+\$\,\-\_\.\!\~\*\']+";
const _PATH_CHAR: &str = r"[a-zA-Z0-9\.\_\-\+]";
const SPATH: &str = r"<[a-zA-Z0-9\.\_\-\+]+(\/[a-zA-Z0-9\.\_\-\+]+)*>";
const PATH: &str = r"[a-zA-Z0-9\.\_\-\+]*(\/[a-zA-Z0-9\.\_\-\+]+)+\/?";
const HPATH: &str = r"\~(\/[a-zA-Z0-9\.\_\-\+]+)+\/?";
const INT: &str = r"[0-9]+";
const FLOAT: &str = r"(([1-9][0-9]*\.[0-9]*)|(0?\.[0-9]+))([Ee][+-]?[0-9]+)?";
const ID: &str = r"[a-zA-Z\_][a-zA-Z0-9\_\'\-]*";

// TODO: Write custom strategies for generating interpolated expressions.

proptest! {
    #[test]
    fn whitespace_prop(s in WHITESPACE) {
        let tokens = lex_to_kinds(&s, true);
        prop_assert_eq!(tokens, vec![TokenKind::Whitespace]);
    }

    #[test]
    fn comment_prop(i in INLINE_COMMENT, b in BLOCK_COMMENT) {
        let s = [i, b].join("\n");
        let tokens = lex_to_kinds(&s, false);
        prop_assert_eq!(
            tokens,
            vec![
                TokenKind::InlineComment,
                TokenKind::BlockComment,
            ]);
    }

    #[test]
    fn literals_prop(
        uri in URI,
        search_path in SPATH,
        path in PATH,
        hpath in HPATH,
        int in INT,
        float in FLOAT,
        ident in ID,
    ) {
        let ident_no_token = if TokenKind::parse_keyword(&ident).is_some() {
            format!("{ident}'") // Make sure a keyword doesn't turn up in RNG.
        } else {
            ident
        };
        let s = [uri, search_path, path, hpath, int, float, ident_no_token].join("\n");
        let tokens = lex_to_kinds(&s, false);
        prop_assert_eq!(
            tokens,
            vec![
                TokenKind::Uri,
                TokenKind::SearchPath,
                TokenKind::PathStart,
                TokenKind::PathEnd,
                TokenKind::HomePathStart,
                TokenKind::PathEnd,
                TokenKind::Int,
                TokenKind::Float,
                TokenKind::Identifier,
            ]);
    }
}
