#![allow(dead_code, unused_variables)]

use std::num::{Int, ParseIntError, ParseFloatError};
use std::{fmt, error};
use std::str::FromStr;
use std::iter::Peekable;

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
    is_emitting_raw_field: bool,
}

macro_rules! emit_lit {
    ($enc:ident,$e:expr,$txt:expr) => {{
        if $enc.is_emitting_raw_field {
            try!(write!($enc.writer, "<{1}>{0}</{1}>", $e, $txt));
        } else {
            try!(write!($enc.writer, "{}", $e));
        }
        Ok(())
    }}
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
            is_emitting_raw_field: true,
        }
    }

    /// Creates a new encoder whose output will be written in compact
    /// Xml to the specified writer
    pub fn new(writer: &'a mut fmt::Write) -> Encoder<'a> {
        Encoder {
            writer: writer,
            format: EncodingFormat::Compact,
            is_emitting_map_key: false,
            is_emitting_raw_field: true,
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
        c => return Ok(try!(write!(writer, "{}", c))),
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
        try!(write!(self.writer, "<null/>"));
        Ok(())
    }

    fn emit_usize(&mut self, v: usize) -> EncodeResult<()> { emit_lit!(self, v, "int") }
    fn emit_u64(&mut self, v: u64) -> EncodeResult<()> { emit_lit!(self, v, "int") }
    fn emit_u32(&mut self, v: u32) -> EncodeResult<()> { emit_lit!(self, v, "int") }
    fn emit_u16(&mut self, v: u16) -> EncodeResult<()> { emit_lit!(self, v, "int") }
    fn emit_u8(&mut self, v: u8) -> EncodeResult<()> { emit_lit!(self, v, "int") }

    fn emit_isize(&mut self, v: isize) -> EncodeResult<()> { emit_lit!(self, v, "int") }
    fn emit_i64(&mut self, v: i64) -> EncodeResult<()> { emit_lit!(self, v, "int") }
    fn emit_i32(&mut self, v: i32) -> EncodeResult<()> { emit_lit!(self, v, "int") }
    fn emit_i16(&mut self, v: i16) -> EncodeResult<()> { emit_lit!(self, v, "int") }
    fn emit_i8(&mut self, v: i8) -> EncodeResult<()> { emit_lit!(self, v, "int") }

    fn emit_bool(&mut self, v: bool) -> EncodeResult<()> {
        if self.is_emitting_map_key { return Err(EncoderError::BadHashmapKey); }
        if v {
            emit!(self, "<true/>")
        } else {
            emit!(self, "<false/>")
        }
    }

    fn emit_f64(&mut self, v: f64) -> EncodeResult<()> {
        emit_lit!(self, v, "float")
    }
    fn emit_f32(&mut self, v: f32) -> EncodeResult<()> {
        self.emit_f64(v as f64)
    }

    fn emit_char(&mut self, v: char) -> EncodeResult<()> {
        if self.is_emitting_raw_field { try_emit!(self, "<char>"); }
        try!(escape_char(self.writer, v));
        if self.is_emitting_raw_field { try_emit!(self, "</char>"); }
        Ok(())
    }
    fn emit_str(&mut self, v: &str) -> EncodeResult<()> {
        if self.is_emitting_raw_field { try_emit!(self, "<string>"); }
        try!(escape_str(self.writer, v));
        if self.is_emitting_raw_field { try_emit!(self, "</string>"); }
        Ok(())
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
            try_emit!(self, ">");
            if let EncodingFormat::Pretty{ref mut curr_indent, indent} = self.format {
                *curr_indent += indent;
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

    fn emit_enum_variant_arg<F>(&mut self, _: usize, f: F) -> EncodeResult<()> where
        F: FnOnce(&mut Encoder<'a>) -> EncodeResult<()>,
    {
        if let EncodingFormat::Pretty{ref curr_indent, ..} = self.format {
            try!(write!(self.writer, "\n"));
            try!(spaces(self.writer, *curr_indent));
        }
        self.is_emitting_raw_field = true;
        try!(f(self));
        self.is_emitting_raw_field = false;
        Ok(())
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
                                         name: &str,
                                         idx: usize,
                                         f: F) -> EncodeResult<()> where
        F: FnOnce(&mut Encoder<'a>) -> EncodeResult<()>,
    {
        if self.is_emitting_map_key { return Err(EncoderError::BadHashmapKey); }
        self.emit_struct_field(name, idx, f)
    }


    fn emit_struct<F>(&mut self, name: &str, len: usize, f: F) -> EncodeResult<()> where
        F: FnOnce(&mut Encoder<'a>) -> EncodeResult<()>,
    {
        if self.is_emitting_map_key { return Err(EncoderError::BadHashmapKey); }
        self.is_emitting_raw_field = false;
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
        if let EncodingFormat::Pretty{ref curr_indent, ..} = self.format {
            try_emit!(self, "\n");
            try!(spaces(self.writer, *curr_indent));
        }
        try_emit!(self, "<");
        try_emit!(self, name);
        try_emit!(self, ">");
        try!(f(self));
        try_emit!(self, "</");
        try_emit!(self, name);
        try_emit!(self, ">");
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
            if let EncodingFormat::Pretty{ref mut curr_indent, ref indent} = self.format {
                *curr_indent += *indent;
            }
            try!(f(self));
            if let EncodingFormat::Pretty{ref mut curr_indent, ref indent} = self.format {
                *curr_indent -= *indent;
                try!(write!(self.writer, "\n"));
                try!(spaces(self.writer, *curr_indent));
            }
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
        self.is_emitting_raw_field = true;
        try!(f(self));
        self.is_emitting_raw_field = false;
        Ok(())
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
        self.is_emitting_raw_field = true;
        try!(f(self));
        self.is_emitting_raw_field = false;
        Ok(())
    }
}

macro_rules! expect {
    ($e:expr, $f:expr) => {{
        let e = try!($e);
        if e != $f {
            try!(Err(ExpectedChar(e, $f)));
        }
    }}
}

macro_rules! expect_str {
    ($sel:ident, $f:expr) => {{
        for c in $f.chars() {
            expect!($sel.bump(), c);
        }
    }}
}

pub fn decode<T>(rdr: &str) -> DecodeResult<T> where T: ::Decodable {
    let mut decoder = Decoder::new(rdr.chars());
    match ::Decodable::decode(&mut decoder) {
        Ok(val) => Ok(val),
        Err(err) => panic!("{:?}: {}", err, decoder.dump()),
    }
}

pub struct Decoder<I : Iterator<Item = char>> {
    rdr: Peekable<I>,
    is_reading_raw_field: bool,
}

impl<I> Decoder<I> where I: Iterator<Item = char> {

    fn new(rdr: I) -> Decoder<I> {
        Decoder {
            rdr: rdr.peekable(),
            is_reading_raw_field: true,
        }
    }

    fn dump(self) -> String {
        self.rdr.collect::<String>()
    }

    fn read_tag(&mut self, name: &str) -> DecodeResult<String> {
        try!(self.expect_open_tag(name));
        let content = try!(self.read_until_exclusive('<'));
        try!(self.expect_close_tag(name));
        Ok(content)
    }

    fn expect_empty_tag(&mut self, name: &str) -> DecodeResult<()> {
        expect!(self.bump_skip_ws(), '<');

        for ch in name.chars() {
            expect!(self.bump(), ch);
        }

        match try!(self.bump()) {
            '/' => {
	            expect!(self.bump(), '>');
	            Ok(())
            },
            '>' => self.expect_close_tag(name),
            c => Err(ExpectedChars(c, "/>"))
        }
    }

    fn expect_open_tag(&mut self, name: &str) -> DecodeResult<()> {
        expect!(self.bump_skip_ws(), '<');

        for ch in name.chars() {
            expect!(self.bump(), ch);
        }

        expect!(self.bump(), '>');
        Ok(())
    }

    fn expect_close_tag(&mut self, name: &str) -> DecodeResult<()> {
        expect!(self.bump_skip_ws(), '<');

        expect!(self.bump(), '/');

        for ch in name.chars() {
            expect!(self.bump(), ch);
        }

        expect!(self.bump(), '>');
        Ok(())
    }

    fn bump_skip_ws(&mut self) -> DecodeResult<char> {
        for c in &mut self.rdr {
            if !c.is_whitespace() {
                return Ok(c);
            }
        }
        Err(EOF)
    }

    fn read_until_exclusive(&mut self, ch: char) -> DecodeResult<String> {
        let mut s = String::new();
        while let Some(&c) = self.rdr.peek() {
            if ch == c { return Ok(s); }
            s.push(c);
            if let None = self.rdr.next() {
                break;
            }
        }
        Err(EOF)
    }

    fn bump_until_inclusive(&mut self, ch: char) -> DecodeResult<()> {
        match self.rdr.find(|&c| c == ch) {
            Some(_) => Ok(()),
            None => Err(EOF),
        }
    }
    fn bump_until_exclusive(&mut self, ch: char) -> DecodeResult<()> {
        while let Some(&c) = self.rdr.peek() {
            if ch == c { return Ok(()); }
            if let None = self.rdr.next() {
                break;
            }
        }
        Err(EOF)
    }

    fn bump(&mut self) -> DecodeResult<char> {
        self.rdr.next().ok_or(EOF)
    }
}

#[derive(Debug, PartialEq)]
pub enum DecoderError {
    ExpectedChar(char, char),
    ExpectedChars(char, &'static str),
    EOF,
    ParseInt(ParseIntError),
    ParseFloat(ParseFloatError),
    ExpectedBool,
    UnmatchedAmpersand,
    StringInsteadChar,
    NoChar,
    BadEncodedChar,
}
use self::DecoderError::*;

impl error::FromError<ParseIntError> for DecoderError {
    fn from_error(err: ParseIntError) -> DecoderError {
        DecoderError::ParseInt(err)
    }
}
impl error::FromError<ParseFloatError> for DecoderError {
    fn from_error(err: ParseFloatError) -> DecoderError {
        DecoderError::ParseFloat(err)
    }
}

pub type DecodeResult<T> = Result<T, DecoderError>;

macro_rules! read_str {
    ($sel: ident, $name: expr) => {{
        let content = if $sel.is_reading_raw_field {
            $sel.read_tag($name)
        } else {
            $sel.read_until_exclusive('<')
        };
        try!(content)
    }}
}

macro_rules! read_primitive {
    ($ident:ident, $ty:ty, $name:expr) => {
        fn $ident(&mut self) -> DecodeResult<$ty> {
            let content = read_str!(self, $name);
            let content = content.as_slice();
            Ok(try!(FromStr::from_str(content)))
        }
    }
}

fn hex2int(s: &str) -> DecodeResult<u32> {
    if s.len() > 8 || s.len() == 0 {
        return Err(BadEncodedChar);
    }
    let mut res = 0;
    for c in s.chars() {
        res *= 16;
        let c = c.to_digit(16);
        let c = try!(c.ok_or(BadEncodedChar));
        res += c;
    }
    Ok(res)
}

fn decode_xml_char(s: &str) -> DecodeResult<char> {
    use std::char::from_u32;
    Ok(match s {
        "amp" => '&',
        "quot" => '"',
        "apos" => '\'',
        "lt" => '<',
        "gt" => '>',
        "" => return Err(NoChar),
        s if s.len() < 2 => return Err(BadEncodedChar),
        s if s.char_at(0) == '#' => {
            let s = &s[1..];
            let num = if s.char_at(0) == 'x' {
                try!(hex2int(&s[1..]))
            } else {
                try!(FromStr::from_str(s))
            };
            try!(from_u32(num).ok_or(BadEncodedChar))
        }
        _ => return Err(BadEncodedChar),
    })
}

impl<I> ::Decoder for Decoder<I> where I: Iterator<Item = char> {
    type Error = DecoderError;

    // Primitive types:
    fn read_nil(&mut self) -> DecodeResult<()> {
        self.expect_empty_tag("null")
    }

    read_primitive! { read_usize, usize, "int" }
    read_primitive! { read_u8, u8, "int" }
    read_primitive! { read_u16, u16, "int" }
    read_primitive! { read_u32, u32, "int" }
    read_primitive! { read_u64, u64, "int" }
    read_primitive! { read_isize, isize, "int" }
    read_primitive! { read_i8, i8, "int" }
    read_primitive! { read_i16, i16, "int" }
    read_primitive! { read_i32, i32, "int" }
    read_primitive! { read_i64, i64, "int" }
    read_primitive! { read_f64, f64, "float" }
    read_primitive! { read_f32, f32, "float" }

    fn read_bool(&mut self) -> DecodeResult<bool> {
        expect!(self.bump_skip_ws(), '<');
        match try!(self.bump()) {
            't' => { expect_str!(self, "rue/>"); Ok(true) },
            'f' => { expect_str!(self, "alse/>"); Ok(false) },
            _ => Err(ExpectedBool),
        }
    }
    fn read_char(&mut self) -> DecodeResult<char> {
        match try!(self.read_str()).as_slice() {
            "" => Err(NoChar),
            c if c.len() == 1 => Ok(c.char_at(0)),
            _ => Err(StringInsteadChar),
        }
    }
    fn read_str(&mut self) -> DecodeResult<String> {
        let content = read_str!(self, "string");
        let mut res = String::new();
        let mut it = content.chars();
        loop {
            let c = match it.next() {
                Some(c) => c,
                None => return Ok(res),
            };
            if c != '&' {
                res.push(c);
                continue;
            }
            let mut encoded = String::new();
            loop {
                let c = match it.next() {
                    Some(c) => c,
                    None => return Err(UnmatchedAmpersand),
                };
                if c != ';' {
                    encoded.push(c);
                    continue;
                }
                break;
            }
            res.push(try!(decode_xml_char(encoded.as_slice())));
        }
    }

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
                                            f_name: &str,
                                            f_idx: usize,
                                            f: F)
                                            -> DecodeResult<T>
        where F: FnOnce(&mut Self) -> DecodeResult<T> { unimplemented!() }

    fn read_struct<T, F>(&mut self, s_name: &str, len: usize, f: F)
                         -> DecodeResult<T>
        where F: FnOnce(&mut Self) -> DecodeResult<T> {
        if len == 0 {
            try!(self.expect_empty_tag(s_name));
            f(self)
        } else {
	        try!(self.expect_open_tag(s_name));
	        let res = try!(f(self));
	        try!(self.expect_close_tag(s_name));
	        Ok(res)
        }
    }

    fn read_struct_field<T, F>(&mut self,
                               f_name: &str,
                               _: usize,
                               f: F)
                               -> DecodeResult<T>
        where F: FnOnce(&mut Self) -> DecodeResult<T> {
        try!(self.expect_open_tag(f_name));
        self.is_reading_raw_field = false;
        let res = try!(f(self));
        try!(self.expect_close_tag(f_name));
        Ok(res)
    }

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
    fn error(&mut self, err: &str) -> DecoderError { unimplemented!() }
}

fn is_bad_name_char(c : char) -> bool {
    r###"!"#$%&'()*+,/;<=>?@[\]^`{|}~"###.contains_char(c)
}

#[cfg(test)]
mod tests {
    extern crate test;
    use super::{encode, encode_pretty, decode};

    #[test]
    fn test_read_object() {
        #[derive(RustcDecodable, Debug, PartialEq)]
        struct Dummy;
        let dec = decode("<Dummy/>");
        assert_eq!(dec, Ok(Dummy));
        let dec = decode("<Dummy></Dummy>");
        assert_eq!(dec, Ok(Dummy));
    }

    #[test]
    fn test_read_string() {
        #[derive(RustcDecodable, Debug, PartialEq)]
        struct Dummy {
            a: String,
        };
        let dec = decode("<Dummy><a>bla</a></Dummy>");
        assert_eq!(dec, Ok(Dummy{a: "bla".to_string()}));
    }

    #[test]
    fn test_read_encoded_string() {
        #[derive(RustcDecodable, Debug, PartialEq)]
        struct Dummy {
            a: String,
        };
        let dec = decode("<Dummy><a>bla&amp;blub</a></Dummy>");
        assert_eq!(dec, Ok(Dummy{a: "bla&blub".to_string()}));
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
            "<Simple><a><true/></a></Simple>"
        );
        assert_eq!(
            encode_pretty(&Simple{a: true}).unwrap(),
            "<Simple>\n  \
                <a><true/></a>\n\
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
                    <Simple2><c>&amp;uiaebla&lt;&gt;hello\x0c\r</c></Simple2>\
                    <Simple2><c></c></Simple2>\
                </b>\
            </Complex>"
        );

        assert_eq!(
            encode_pretty(&complex_obj).unwrap(),
            "<Complex>\n  \
                <b>\n    \
                    <Simple2>\n      \
                        <c>&amp;uiaebla&lt;&gt;hello\x0c\r</c>\n    \
                    </Simple2>\n    \
                    <Simple2>\n      \
                        <c></c>\n    \
                    </Simple2>\n  \
                </b>\n\
            </Complex>"
        );
    }

    #[test]
    fn test_write_enum() {
        #[derive(RustcEncodable)]
        enum Animal {
            Dog,
            Frog(String, i32),
        }

        assert_eq!(encode(&Animal::Dog).unwrap(), "<Dog/>");
        assert_eq!(encode_pretty(&Animal::Dog).unwrap(), "<Dog/>");

        let animal = Animal::Frog("Henry".to_string(), 349);
        assert_eq!(encode(&animal).unwrap(), "<Frog><string>Henry</string><int>349</int></Frog>");
        assert_eq!(encode_pretty(&animal).unwrap(), "\
        <Frog>\n  \
            <string>Henry</string>\n  \
            <int>349</int>\n\
        </Frog>");
    }
}
