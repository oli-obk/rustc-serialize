#![allow(dead_code)]

use std::iter::IntoIterator;
use std::num::Int;
use std::{fmt, error};

#[derive(Copy, Debug)]
pub enum EncoderError {
    FmtError(fmt::Error),
    BadHashmapKey,
    BadStructName,
    BadStructFieldName,
    BadEnumName,
}

impl error::FromError<fmt::Error> for EncoderError {
    fn from_error(err: fmt::Error) -> EncoderError { EncoderError::FmtError(err) }
}

pub type EncodeResult<T> = Result<T, EncoderError>;

enum EncodingFormat {
    Compact,
    Pretty {
        curr_indent: u32,
        indent: u32
    }
}

/// A structure for implementing serialization to JSON.
pub struct Encoder<'a> {
    writer: &'a mut (fmt::Write+'a),
    format : EncodingFormat,
    is_emitting_map_key: bool,
}

macro_rules! emit {
    ($enc:ident,$e:expr) => {{
        try!(write!($enc.writer, "{}", $e));
        Ok(())
    }}
}

macro_rules! try_emit {
    ($enc:ident,$e:expr) => {
        try!(write!($enc.writer, "{}", $e))
    }
}

impl<'a> Encoder<'a> {
    /// Creates a new encoder whose output will be written in human-readable
    /// Xml to the specified writer
    pub fn new_pretty(writer: &'a mut fmt::Write) -> Encoder<'a> {
        Encoder {
            writer: writer,
            format: EncodingFormat::Pretty {
                curr_indent: 0,
                indent: 2,
            },
            is_emitting_map_key: false,
        }
    }

    /// Creates a new encoder whose output will be written in compact
    /// Xml to the specified writer
    pub fn new(writer: &'a mut fmt::Write) -> Encoder<'a> {
        Encoder {
            writer: writer,
            format: EncodingFormat::Compact,
            is_emitting_map_key: false,
        }
    }

    /// Set the number of spaces to indent for each level.
    /// This is safe to set during encoding.
    pub fn set_indent(&mut self, new_indent: u32) -> Result<(), ()> {
        if let EncodingFormat::Pretty{ref mut curr_indent, ref mut indent} = self.format {
            // self.indent very well could be 0 so we need to use checked division.
            let level = curr_indent.checked_div(*indent).unwrap_or(0);
            *indent = new_indent;
            *curr_indent = level * *indent;
            Ok(())
        } else {
            Err(())
        }
    }
}

fn escape_char(writer: &mut fmt::Write, ch: char) -> EncodeResult<()> {
    let enc = match ch {
        '>' => "&gt;",
        '<' => "&lt;",
        '&' => "&amp;",
        '\'' => "&apos;",
        '"' => "&quot;",
        _ => unreachable!(),
    };
    try!(write!(writer, "{}", enc));
    Ok(())
}

fn escape_str(writer: &mut fmt::Write, s: &str) -> EncodeResult<()> {
    for c in s.chars() {
        try!(escape_char(writer, c));
    }
    Ok(())
}

fn spaces(wr: &mut fmt::Write, n: u32) -> EncodeResult<()> {
    let mut n = n as usize;
    const BUF: &'static str = "                ";

    while n >= BUF.len() {
        try!(wr.write_str(BUF));
        n -= BUF.len();
    }

    if n > 0 {
        try!(wr.write_str(&BUF[..n]));
    }
    Ok(())
}

/// Shortcut function to encode a `T` into a Xml `String`
pub fn encode<T: ::Encodable>(object: &T) -> EncodeResult<String> {
    let mut s = String::new();
    {
        let mut encoder = Encoder::new(&mut s);
        try!(object.encode(&mut encoder));
    }
    Ok(s)
}

/// Shortcut function to encode a `T` into a pretty Xml `String`
pub fn encode_pretty<T: ::Encodable>(object: &T) -> EncodeResult<String> {
    let mut s = String::new();
    {
        let mut encoder = Encoder::new_pretty(&mut s);
        try!(object.encode(&mut encoder));
    }
    Ok(s)
}

impl<'a> ::Encoder for Encoder<'a> {
    type Error = EncoderError;

    fn emit_nil(&mut self) -> EncodeResult<()> {
        if self.is_emitting_map_key { return Err(EncoderError::BadHashmapKey); }
        try!(write!(self.writer, "null"));
        Ok(())
    }

    fn emit_usize(&mut self, v: usize) -> EncodeResult<()> { emit!(self, v) }
    fn emit_u64(&mut self, v: u64) -> EncodeResult<()> { emit!(self, v) }
    fn emit_u32(&mut self, v: u32) -> EncodeResult<()> { emit!(self, v) }
    fn emit_u16(&mut self, v: u16) -> EncodeResult<()> { emit!(self, v) }
    fn emit_u8(&mut self, v: u8) -> EncodeResult<()> { emit!(self, v) }

    fn emit_isize(&mut self, v: isize) -> EncodeResult<()> { emit!(self, v) }
    fn emit_i64(&mut self, v: i64) -> EncodeResult<()> { emit!(self, v) }
    fn emit_i32(&mut self, v: i32) -> EncodeResult<()> { emit!(self, v) }
    fn emit_i16(&mut self, v: i16) -> EncodeResult<()> { emit!(self, v) }
    fn emit_i8(&mut self, v: i8) -> EncodeResult<()> { emit!(self, v) }

    fn emit_bool(&mut self, v: bool) -> EncodeResult<()> {
        if self.is_emitting_map_key { return Err(EncoderError::BadHashmapKey); }
        if v {
            emit!(self, "true")
        } else {
            emit!(self, "false")
        }
    }

    fn emit_f64(&mut self, v: f64) -> EncodeResult<()> {
        emit!(self, v)
    }
    fn emit_f32(&mut self, v: f32) -> EncodeResult<()> {
        self.emit_f64(v as f64)
    }

    fn emit_char(&mut self, v: char) -> EncodeResult<()> {
        escape_char(self.writer, v)
    }
    fn emit_str(&mut self, v: &str) -> EncodeResult<()> {
        escape_str(self.writer, v)
    }

    fn emit_enum<F>(&mut self, _name: &str, f: F) -> EncodeResult<()> where
        F: FnOnce(&mut Encoder<'a>) -> EncodeResult<()>,
    {
        f(self)
    }

    fn emit_enum_variant<F>(&mut self,
                            name: &str,
                            _id: usize,
                            cnt: usize,
                            f: F)
                            -> EncodeResult<()> where
        F: FnOnce(&mut Encoder<'a>) -> EncodeResult<()>,
    {
        // Bunny => <Bunny/>
        // Kangaroo(34,"William") => <Kangaroo><field>34</field><field>William</field></Kangaroo>}
        try_emit!(self, "<");
        if name.chars().any(|ch| is_bad_name_char(ch)) {
            return Err(EncoderError::BadEnumName);
        }
        try!(escape_str(self.writer, name));
        if cnt == 0 {
            emit!(self, "/>")
        } else {
            if self.is_emitting_map_key { return Err(EncoderError::BadHashmapKey); }
            if let EncodingFormat::Pretty{ref mut curr_indent, indent} = self.format {
                try_emit!(self, ">\n");
                *curr_indent += indent;
                try!(spaces(self.writer, *curr_indent));
            } else {
                try_emit!(self, ">");
            }
            try!(f(self));
            if let EncodingFormat::Pretty{ref mut curr_indent, indent} = self.format {
                *curr_indent -= indent;
                try!(write!(self.writer, "\n"));
                try!(spaces(self.writer, *curr_indent));
            }
            try_emit!(self, "</");
            try!(escape_str(self.writer, name));
            emit!(self, ">")
        }
    }

    fn emit_enum_variant_arg<F>(&mut self, idx: usize, f: F) -> EncodeResult<()> where
        F: FnOnce(&mut Encoder<'a>) -> EncodeResult<()>,
    {
        self.emit_struct_field("field", idx, f)
    }

    fn emit_enum_struct_variant<F>(&mut self,
                                   name: &str,
                                   id: usize,
                                   cnt: usize,
                                   f: F) -> EncodeResult<()> where
        F: FnOnce(&mut Encoder<'a>) -> EncodeResult<()>,
    {
        if self.is_emitting_map_key { return Err(EncoderError::BadHashmapKey); }
        self.emit_enum_variant(name, id, cnt, f)
    }

    fn emit_enum_struct_variant_field<F>(&mut self,
                                         _: &str,
                                         idx: usize,
                                         f: F) -> EncodeResult<()> where
        F: FnOnce(&mut Encoder<'a>) -> EncodeResult<()>,
    {
        if self.is_emitting_map_key { return Err(EncoderError::BadHashmapKey); }
        self.emit_enum_variant_arg(idx, f)
    }


    fn emit_struct<F>(&mut self, name: &str, len: usize, f: F) -> EncodeResult<()> where
        F: FnOnce(&mut Encoder<'a>) -> EncodeResult<()>,
    {
        if self.is_emitting_map_key { return Err(EncoderError::BadHashmapKey); }
        try_emit!(self, "<");
        if name.chars().any(|ch| is_bad_name_char(ch)) {
            return Err(EncoderError::BadStructName);
        }
        try_emit!(self, name);
        if len == 0 {
            emit!(self, "/>")
        } else {
            try_emit!(self, ">");
            if let EncodingFormat::Pretty{ref mut curr_indent, indent} = self.format {
                *curr_indent += indent;
            }
            try!(f(self));
            if let EncodingFormat::Pretty{ref mut curr_indent, indent} = self.format {
                *curr_indent -= indent;
                try_emit!(self, "\n");
                try!(spaces(self.writer, *curr_indent));
            }
            try_emit!(self, "</");
            try_emit!(self, name);
            emit!(self, ">")
        }
    }

    fn emit_struct_field<F>(&mut self, name: &str, _: usize, f: F) -> EncodeResult<()> where
        F: FnOnce(&mut Encoder<'a>) -> EncodeResult<()>,
    {
        if self.is_emitting_map_key { return Err(EncoderError::BadHashmapKey); }
        if name.chars().any(|ch| is_bad_name_char(ch)) {
            return Err(EncoderError::BadStructFieldName);
        }
        try_emit!(self, "<");
        try_emit!(self, name);
        try_emit!(self, ">");
        try!(f(self));
        try_emit!(self, "</");
        try_emit!(self, name);
        try_emit!(self, ">");
        if let EncodingFormat::Pretty{ref curr_indent, ..} = self.format {
            try_emit!(self, "\n");
            try!(spaces(self.writer, *curr_indent));
        }
        Ok(())
    }

    fn emit_tuple<F>(&mut self, len: usize, f: F) -> EncodeResult<()> where
        F: FnOnce(&mut Encoder<'a>) -> EncodeResult<()>,
    {
        if self.is_emitting_map_key { return Err(EncoderError::BadHashmapKey); }
        self.emit_seq(len, f)
    }
    fn emit_tuple_arg<F>(&mut self, idx: usize, f: F) -> EncodeResult<()> where
        F: FnOnce(&mut Encoder<'a>) -> EncodeResult<()>,
    {
        if self.is_emitting_map_key { return Err(EncoderError::BadHashmapKey); }
        self.emit_seq_elt(idx, f)
    }

    fn emit_tuple_struct<F>(&mut self, _: &str, len: usize, f: F) -> EncodeResult<()> where
        F: FnOnce(&mut Encoder<'a>) -> EncodeResult<()>,
    {
        if self.is_emitting_map_key { return Err(EncoderError::BadHashmapKey); }
        self.emit_seq(len, f)
    }
    fn emit_tuple_struct_arg<F>(&mut self, idx: usize, f: F) -> EncodeResult<()> where
        F: FnOnce(&mut Encoder<'a>) -> EncodeResult<()>,
    {
        if self.is_emitting_map_key { return Err(EncoderError::BadHashmapKey); }
        self.emit_seq_elt(idx, f)
    }

    fn emit_option<F>(&mut self, f: F) -> EncodeResult<()> where
        F: FnOnce(&mut Encoder<'a>) -> EncodeResult<()>,
    {
        if self.is_emitting_map_key { return Err(EncoderError::BadHashmapKey); }
        f(self)
    }
    fn emit_option_none(&mut self) -> EncodeResult<()> {
        if self.is_emitting_map_key { return Err(EncoderError::BadHashmapKey); }
        self.emit_nil()
    }
    fn emit_option_some<F>(&mut self, f: F) -> EncodeResult<()> where
        F: FnOnce(&mut Encoder<'a>) -> EncodeResult<()>,
    {
        if self.is_emitting_map_key { return Err(EncoderError::BadHashmapKey); }
        f(self)
    }

    fn emit_seq<F>(&mut self, len: usize, f: F) -> EncodeResult<()> where
        F: FnOnce(&mut Encoder<'a>) -> EncodeResult<()>,
    {
        if self.is_emitting_map_key { return Err(EncoderError::BadHashmapKey); }
        if len > 0 {
            try!(f(self));
        }
        Ok(())
    }

    fn emit_seq_elt<F>(&mut self, _: usize, f: F) -> EncodeResult<()> where
        F: FnOnce(&mut Encoder<'a>) -> EncodeResult<()>,
    {
        if self.is_emitting_map_key { return Err(EncoderError::BadHashmapKey); }
        if let EncodingFormat::Pretty{ref mut curr_indent, ..} = self.format {
            try!(write!(self.writer, "\n"));
            try!(spaces(self.writer, *curr_indent));
        }
        f(self)
    }

    fn emit_map<F>(&mut self, len: usize, f: F) -> EncodeResult<()> where
        F: FnOnce(&mut Encoder<'a>) -> EncodeResult<()>,
    {
        if self.is_emitting_map_key { return Err(EncoderError::BadHashmapKey); }
        if len > 0 {
            try!(f(self));
        }
        Ok(())
    }

    fn emit_map_elt_key<F>(&mut self, _: usize, f: F) -> EncodeResult<()> where
        F: FnOnce(&mut Encoder<'a>) -> EncodeResult<()>,
    {
        if self.is_emitting_map_key { return Err(EncoderError::BadHashmapKey); }
        if let EncodingFormat::Pretty{curr_indent, ..} = self.format {
            try!(write!(self.writer, "\n"));
            try!(spaces(self.writer, curr_indent));
        }
        self.is_emitting_map_key = true;
        try!(f(self));
        self.is_emitting_map_key = false;
        Ok(())
    }

    fn emit_map_elt_val<F>(&mut self, _idx: usize, f: F) -> EncodeResult<()> where
        F: FnOnce(&mut Encoder<'a>) -> EncodeResult<()>,
    {
        if self.is_emitting_map_key { return Err(EncoderError::BadHashmapKey); }
        f(self)
    }
}


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
    use super::{Parser, Element, Content, ParserError, Attribute, encode, encode_pretty};
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

    #[test]
    fn test_write_object() {
        #[derive(RustcEncodable)]
        struct Dummy;
        assert_eq!(encode(&Dummy).unwrap(), "<Dummy/>");
        assert_eq!(encode_pretty(&Dummy).unwrap(), "<Dummy/>");

        #[derive(RustcEncodable)]
        struct Simple {
            a: bool,
        }

        assert_eq!(
            encode(&Simple{a: true}).unwrap(),
            "<Simple><a>true</a></Simple>"
        );
        assert_eq!(
            encode_pretty(&Simple{a: true}).unwrap(),
            "<Simple>\n  \
                <a>true</a>\n\
            </Simple>"
        );

        #[derive(RustcEncodable)]
        struct Simple2 {
            c: String,
        }

        #[derive(RustcEncodable)]
        struct Complex {
            b: Vec<Simple2>,
        }

        let complex_obj = Complex {
            b: vec![
                Simple2 { c: "&uiaebla<>hello\x0c\r".to_string() },
                Simple2 { c: "".to_string() },
            ],
        };

        assert_eq!(
            encode(&complex_obj).unwrap(),
            "<Complex>\
                <b>\
                    <Simple2><c>&uiaebla<>hello\x0c\r</c></Simple2>\
                    <Simple2><c></c></Simple2>\
                </b>
            </Complex>"
        );

        assert_eq!(
            encode_pretty(&complex_obj).unwrap(),
            "<Complex>\
                <b>\n  \
                    <Simple2><c>&uiaebla<>hello\x0c\r</c></Simple2>\n  \
                    <Simple2><c></c></Simple2>\n  \
                </b>\n  \
            </Complex>"
        );
    }
/*
    #[test]
    fn test_write_enum() {
        let animal = Dog;
        assert_eq!(
            format!("{}", super::as_json(&animal)),
            "\"Dog\""
        );
        assert_eq!(
            format!("{}", super::as_pretty_json(&animal)),
            "\"Dog\""
        );

        let animal = Frog("Henry".to_string(), 349);
        assert_eq!(
            format!("{}", super::as_json(&animal)),
            "{\"variant\":\"Frog\",\"fields\":[\"Henry\",349]}"
        );
        assert_eq!(
            format!("{}", super::as_pretty_json(&animal)),
            "{\n  \
               \"variant\": \"Frog\",\n  \
               \"fields\": [\n    \
                 \"Henry\",\n    \
                 349\n  \
               ]\n\
             }"
        );
    }
    */
}
