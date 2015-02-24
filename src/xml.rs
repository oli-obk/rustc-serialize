#![allow(dead_code)]

use std::iter::IntoIterator;

/*

pub struct XmlDecoder {}

pub enum XmlError {}

pub type DecodeResult<T> = Result<T, XmlError>

impl Decoder for XmlDecoder {
    type Error = XmlError;

    // Primitive types:
    fn read_nil(&mut self) -> DecodeResult<()> { unimplemented!() }
    fn read_usize(&mut self) -> DecodeResult<usize> { unimplemented!() }
    fn read_u64(&mut self) -> DecodeResult<u64> { unimplemented!() }
    fn read_u32(&mut self) -> DecodeResult<u32> { unimplemented!() }
    fn read_u16(&mut self) -> DecodeResult<u16> { unimplemented!() }
    fn read_u8(&mut self) -> DecodeResult<u8> { unimplemented!() }
    fn read_isize(&mut self) -> DecodeResult<isize> { unimplemented!() }
    fn read_i64(&mut self) -> DecodeResult<i64> { unimplemented!() }
    fn read_i32(&mut self) -> DecodeResult<i32> { unimplemented!() }
    fn read_i16(&mut self) -> DecodeResult<i16> { unimplemented!() }
    fn read_i8(&mut self) -> DecodeResult<i8> { unimplemented!() }
    fn read_bool(&mut self) -> DecodeResult<bool> { unimplemented!() }
    fn read_f64(&mut self) -> DecodeResult<f64> { unimplemented!() }
    fn read_f32(&mut self) -> DecodeResult<f32> { unimplemented!() }
    fn read_char(&mut self) -> DecodeResult<char> { unimplemented!() }
    fn read_str(&mut self) -> DecodeResult<String> { unimplemented!() }

    // Compound types:
    fn read_enum<T, F>(&mut self, name: &str, f: F) -> DecodeResult<T>
        where F: FnOnce(&mut Self) -> DecodeResult<T> { unimplemented!() }

    fn read_enum_variant<T, F>(&mut self, names: &[&str], f: F)
                               -> DecodeResult<T>
        where F: FnMut(&mut Self, usize) -> DecodeResult<T> { unimplemented!() }
    fn read_enum_variant_arg<T, F>(&mut self, a_idx: usize, f: F)
                                   -> DecodeResult<T>
        where F: FnOnce(&mut Self) -> DecodeResult<T> { unimplemented!() }

    fn read_enum_struct_variant<T, F>(&mut self, names: &[&str], f: F)
                                      -> DecodeResult<T>
        where F: FnMut(&mut Self, usize) -> DecodeResult<T> { unimplemented!() }
    fn read_enum_struct_variant_field<T, F>(&mut self,
                                            &f_name: &str,
                                            f_idx: usize,
                                            f: F)
                                            -> DecodeResult<T>
        where F: FnOnce(&mut Self) -> DecodeResult<T> { unimplemented!() }

    fn read_struct<T, F>(&mut self, s_name: &str, len: usize, f: F)
                         -> DecodeResult<T>
        where F: FnOnce(&mut Self) -> DecodeResult<T> { unimplemented!() }
    fn read_struct_field<T, F>(&mut self,
                               f_name: &str,
                               f_idx: usize,
                               f: F)
                               -> DecodeResult<T>
        where F: FnOnce(&mut Self) -> DecodeResult<T> { unimplemented!() }

    fn read_tuple<T, F>(&mut self, len: usize, f: F) -> DecodeResult<T>
        where F: FnOnce(&mut Self) -> DecodeResult<T> { unimplemented!() }
    fn read_tuple_arg<T, F>(&mut self, a_idx: usize, f: F)
                            -> DecodeResult<T>
        where F: FnOnce(&mut Self) -> DecodeResult<T> { unimplemented!() }

    fn read_tuple_struct<T, F>(&mut self, s_name: &str, len: usize, f: F)
                               -> DecodeResult<T>
        where F: FnOnce(&mut Self) -> DecodeResult<T> { unimplemented!() }
    fn read_tuple_struct_arg<T, F>(&mut self, a_idx: usize, f: F)
                                   -> DecodeResult<T>
        where F: FnOnce(&mut Self) -> DecodeResult<T> { unimplemented!() }

    // Specialized types:
    fn read_option<T, F>(&mut self, f: F) -> DecodeResult<T>
        where F: FnMut(&mut Self, bool) -> DecodeResult<T> { unimplemented!() }

    fn read_seq<T, F>(&mut self, f: F) -> DecodeResult<T>
        where F: FnOnce(&mut Self, usize) -> DecodeResult<T> { unimplemented!() }
    fn read_seq_elt<T, F>(&mut self, idx: usize, f: F) -> DecodeResult<T>
        where F: FnOnce(&mut Self) -> DecodeResult<T> { unimplemented!() }

    fn read_map<T, F>(&mut self, f: F) -> DecodeResult<T>
        where F: FnOnce(&mut Self, usize) -> DecodeResult<T> { unimplemented!() }
    fn read_map_elt_key<T, F>(&mut self, idx: usize, f: F)
                              -> DecodeResult<T>
        where F: FnOnce(&mut Self) -> DecodeResult<T> { unimplemented!() }
    fn read_map_elt_val<T, F>(&mut self, idx: usize, f: F)
                              -> DecodeResult<T>
        where F: FnOnce(&mut Self) -> DecodeResult<T> { unimplemented!() }

    // Failure
    fn error(&mut self, err: &str) -> Self::Error { unimplemented!() }
}
*/

enum Content {
    Text(String),
    Elements(Vec<Element>),
    Nothing,
}

struct Attribute {
    name: String,
    value: String,
}

struct Element {
    attributes: Vec<Attribute>,
    content: Content,
    name : String,
}

struct Stack {
    v : Vec<Content>
}
impl Stack {
    fn new() -> Stack {
        Stack {
            v: vec![],
        }
    }
}

struct Parser<T> {
    stack: Stack,
    rdr: T,
    line: usize,
    col: usize,
    state: ParserState,
}

#[derive(Debug, Eq, PartialEq)]
enum ParserError {
    MultipleRoots,
    BogusGt, // >
    BogusLt, // <
    EOF,
    Unexpected(char),
    BadNameChar,
}
type ParserResult<T> = Result<T, ParserError>;

use self::ParserError::*;

macro_rules! expect {
    ($a:expr, $b:pat) => {
        match try!($a) {
            $b => {},
            _ => return Err(Unexpected($a)),
        }
    }
}

impl<T> Parser<T> where T: Iterator<Item = char> {
    /// Creates the Xml parser.
    pub fn new<U>(rdr: U) -> Parser<T> where U: IntoIterator<Item = char, IntoIter = T> {
        Parser {
            rdr: rdr.into_iter(),
            line: 1,
            col: 0,
            stack: Stack::new(),
            state: FindTag,
        }
    }

    fn parse(&mut self) -> ParserResult<Element> {
        self.parse_element()
    }

    fn is_bad_name_char(c : char) {
        r###"!"#$%&'()*+,/;<=>?@[\]^`{|}~"###.contains_char(c)
    }

    fn parse_element(&mut self) -> ParserResult<Element> {
        expect!(self.parse_char(), '<');
        let mut name = String::new();
        loop {
            match self.parse_char() {
                c if is_bad_name_char(c) => return Err(BadNameChar),
                c if c.is_whitespace() => break,
                '>' => return self.parse_element_body(name, vec![]),
                c => name.push(c),
            }
        }
        self.parse_element_attributes(name)
    }

    fn parse_element_attributes(&mut self, name: String) -> ParserResult<Element> {
        let mut v = vec![];
        loop {
            loop {
                match self.parse_char() {
                    c if c.is_whitespace() => continue,
                    '>' => return self.parse_element_body(name, v),

                }
            }
        }
    }

    fn parse_char(&mut self) -> ParserResult<char> {
        let ch = try!(rdr.next().ok_or(EOF));
        match ch {
            '\n' => {
                self.line += 1;
                self.col = 1;
            },
            _ => self.col += 1,
        }
        ch
    }
}

#[cfg(test)]
mod tests {
    extern crate test;
    use super::Parser;
    use super::ParserError::*;

    #[test]
    fn test_decode_option_none() {
        let txt = "<root><elem></elem></root>";
        let mut p = Parser::new(txt.chars());
    }

    #[test]
    fn test_two_roots() {
        let txt = "<root></root><root></root>";
        let mut p = Parser::new(txt.chars());
        assert!(p.parse() == Err(MultipleRoots));
    }
}
