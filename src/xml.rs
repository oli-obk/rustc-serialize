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

#[derive(Debug, PartialEq, Eq)]
enum Content {
    Text(String),
    Elements(Vec<Element>),
    Nothing,
}

#[derive(Debug, PartialEq, Eq)]
struct Attribute {
    name: String,
    value: String,
}

#[derive(Debug, Eq, PartialEq)]
struct Element {
    attributes: Vec<Attribute>,
    content: Content,
    name : String,
}

#[derive(Debug)]
struct Parser<T> {
    rdr: T,
    line: usize,
    col: usize,
}

#[derive(Debug, Eq, PartialEq)]
struct ParserError {
    line: usize,
    col: usize,
    kind: ParserErrorKind,
}

#[derive(Debug, Eq, PartialEq)]
enum ParserErrorKind {
    MultipleRoots,
    BogusGt, // >
    BogusLt, // <
    EOF,
    Unexpected(char),
    BadNameChar,
    EmptyAttributeName,
    EmptyElementName,
    MixedTextAndElements,
}

type ParserResult<T> = Result<T, ParserError>;
type ParserKindResult<T> = Result<T, ParserErrorKind>;

use self::ParserErrorKind::*;

macro_rules! expect {
    ($a:expr, $b:expr) => {{
        let ch = try!($a);
        if ch != $b {
            return Err(Unexpected(ch));
        }
    }}
}

fn is_bad_name_char(c : char) -> bool {
    r###"!"#$%&'()*+,/;<=>?@[\]^`{|}~"###.contains_char(c)
}

impl<T> Parser<T> where T: Iterator<Item = char> {
    /// Creates the Xml parser.
    pub fn new<U>(rdr: U) -> Parser<T> where U: IntoIterator<Item = char, IntoIter = T> {
        Parser {
            rdr: rdr.into_iter(),
            line: 1,
            col: 0,
        }
    }

    fn parse(&mut self) -> ParserResult<Element> {
        match self.parse_element() {
            Ok(el) => {
                if self.parse_char_skip_ws() == Err(EOF) {
                    Ok(el)
                } else {
                    Err(ParserError {
                        kind: MultipleRoots,
                        line: self.line,
                        col: self.col,
                    })
                }
            },
            Err(err) => Err(ParserError {
                kind: err,
                line: self.line,
                col: self.col,
            }),
        }
    }

    fn parse_element_name(&mut self, first_char: char) -> ParserKindResult<Element> {
        if is_bad_name_char(first_char) { return Err(BadNameChar); }
        if first_char.is_whitespace() { return Err(EmptyElementName); }

        let mut name = format!("{}", first_char);
        loop {
            match try!(self.parse_char()) {
                '>' => return self.parse_element_body(name, vec![]),
                c if is_bad_name_char(c) => return Err(BadNameChar),
                c if c.is_whitespace() => break,
                c => name.push(c),
            }
        }
        self.parse_element_attributes(name)
    }

    fn parse_element(&mut self) -> ParserKindResult<Element> {
        expect!(self.parse_char(), '<');
        let ch = try!(self.parse_char());
        self.parse_element_name(ch)
    }

    fn parse_element_body(&mut self,
                          name: String,
                          attributes: Vec<Attribute>)
                          -> ParserKindResult<Element> {
        let mut text = String::new();
        loop {
            match try!(self.parse_char()) {
                '<' => match try!(self.parse_char()) {
                    '/' => return self.parse_element_close(name, attributes, Content::Nothing),
                    c => {
                        let inner = try!(self.parse_element_name(c));
                        return self.parse_element_content_elements(name, attributes, inner);
                    }
                },
                c => {
                    text.push(c);
                    if c.is_whitespace() { continue; }
                    return self.parse_element_content_text(name, attributes, text);
                }
            }
        }
    }

    fn parse_element_close(&mut self,
                           name: String,
                           attributes: Vec<Attribute>,
                           content: Content)
                           -> ParserKindResult<Element> {
        for c in name.chars() {
            expect!(self.parse_char(), c);
        }
        expect!(self.parse_char(), '>');
        Ok(Element {
            name: name,
            attributes: attributes,
            content: content,
        })
    }

    fn parse_element_content_elements(&mut self,
                                      name: String,
                                      attributes: Vec<Attribute>,
                                      first_element: Element)
                                      -> ParserKindResult<Element> {
        let mut v = vec![first_element];
        loop {
            match try!(self.parse_char_skip_ws()) {
                '<' => match try!(self.parse_char()) {
                    '/' => return self.parse_element_close(name, attributes, Content::Elements(v)),
                    c => v.push(try!(self.parse_element_name(c))),
                },
                _ => return Err(MixedTextAndElements),
            }
        }
    }

    fn parse_element_content_text(&mut self,
                                  name: String,
                                  attributes: Vec<Attribute>,
                                  mut text: String)
                                  -> ParserKindResult<Element> {
        loop {
            match try!(self.parse_char()) {
                '<' => match try!(self.parse_char()) {
                    '/' => return self.parse_element_close(name, attributes, Content::Text(text)),
                    _ => return Err(MixedTextAndElements),
                },
                c => text.push(c),
            }
        }
    }

    fn parse_element_attributes(&mut self, name: String) -> ParserKindResult<Element> {
        let mut v = vec![];
        loop {
            let mut attr_name = String::new();
            match try!(self.parse_char_skip_ws()) {
                '>' => return self.parse_element_body(name, v),
                '=' => return Err(EmptyAttributeName),
                c => attr_name.push(c),
            }
            loop {
                match try!(self.parse_char()) {
                    c if c.is_whitespace() => {
                        expect!(self.parse_char_skip_ws(), '=');
                        break;
                    },
                    '=' => break,
                    c => attr_name.push(c),
                }
            }
            expect!(self.parse_char_skip_ws(), '"');
            let mut attr_value = String::new();
            loop {
                match try!(self.parse_char()) {
                    '"' => break,
                    c => attr_value.push(c),
                }
            }
            v.push(Attribute {
                name: attr_name,
                value: attr_value,
            });
        }
    }
    fn parse_char_skip_ws(&mut self) -> ParserKindResult<char> {
        loop {
            match try!(self.parse_char()) {
                c if c.is_whitespace() => continue,
                c => return Ok(c),
            }
        }
    }

    fn parse_char(&mut self) -> ParserKindResult<char> {
        let ch = try!(self.rdr.next().ok_or(EOF));
        match ch {
            '\n' => {
                self.line += 1;
                self.col = 1;
            },
            _ => self.col += 1,
        }
        Ok(ch)
    }
}

#[cfg(test)]
mod tests {
    extern crate test;
    use super::{Parser, Element, Content, ParserError, Attribute};
    use super::ParserErrorKind::*;

    #[test]
    fn test_parse_root_elem() {
        let txt = "<root><elem></elem></root>";
        let mut p = Parser::new(txt.chars());
        let structure = Element {
            name: "root".to_string(),
            attributes: vec![],
            content: Content::Elements(vec![Element{
                name: "elem".to_string(),
                attributes: vec![],
                content: Content::Nothing,
            }])
        };
        assert_eq!(p.parse(), Ok(structure));
    }

    #[test]
    fn test_parse_attributes() {
        let txt = r#"<root><elem bla="blub hi cake><></baa>"></elem></root>"#;
        let mut p = Parser::new(txt.chars());
        let structure = Element {
            name: "root".to_string(),
            attributes: vec![],
            content: Content::Elements(vec![Element{
                name: "elem".to_string(),
                attributes: vec![
                    Attribute {
                        name: "bla".to_string(),
                        value: "blub hi cake><></baa>".to_string(),
                    }
                ],
                content: Content::Nothing,
            }])
        };
        assert_eq!(p.parse(), Ok(structure));
    }

    #[test]
    fn test_parse_text() {
        let txt = r#"<root>   i am groot!  </root>"#;
        let mut p = Parser::new(txt.chars());
        let structure = Element {
            name: "root".to_string(),
            attributes: vec![],
            content: Content::Text("   i am groot!  ".to_string()),
        };
        assert_eq!(p.parse(), Ok(structure));
    }

    #[test]
    fn test_parse_nothing() {
        let txt = r#"<root>     </root>"#;
        let mut p = Parser::new(txt.chars());
        let structure = Element {
            name: "root".to_string(),
            attributes: vec![],
            content: Content::Nothing,
        };
        assert_eq!(p.parse(), Ok(structure));
    }

    #[test]
    fn test_parse_two_roots() {
        let txt = "<root></root><root></root>";
        let mut p = Parser::new(txt.chars());
        assert_eq!(p.parse(), Err(ParserError {
            kind: MultipleRoots,
            col: 14,
            line: 1,
        }));
    }
}
