use thiserror::Error;

pub mod ascii_char {
    #[derive(Copy, Clone, PartialEq)]
    enum AsciiCharCategory {
        Alphabetic,
        Digit,
        Symbol,
        InlineWhitespace,
        LineFeedWhitespace,
        Invalid,
    }

    fn ascii_char_category(ch: u8) -> AsciiCharCategory {
        match ch {
            0x09 | 0xC | b' ' => AsciiCharCategory::InlineWhitespace,
            0xA | 0xD => AsciiCharCategory::LineFeedWhitespace,
            b'A'..=b'Z' | b'a'..=b'z' => AsciiCharCategory::Alphabetic,
            b'0'..=b'9' => AsciiCharCategory::Digit,
            0x0..=0x8 | 0xB | 0xE..=0x1F | 0x7F..=0xFF => AsciiCharCategory::Invalid,
            _ => AsciiCharCategory::Symbol,
        }
    }

    pub fn is_invalid_char(ch: u8) -> bool {
        ascii_char_category(ch) == AsciiCharCategory::Invalid
    }

    pub fn is_whitespace_char(ch: u8) -> bool {
        let category = ascii_char_category(ch);
        category == AsciiCharCategory::InlineWhitespace
            || category == AsciiCharCategory::LineFeedWhitespace
    }

    pub fn is_alphanumeric_char(ch: u8) -> bool {
        let category = ascii_char_category(ch);
        category == AsciiCharCategory::Alphabetic || category == AsciiCharCategory::Digit
    }

    pub fn is_numeric_char(ch: u8) -> bool {
        let category = ascii_char_category(ch);
        category == AsciiCharCategory::Digit
    }

    pub fn is_id_start(ch: u8) -> bool {
        match ch {
            b'A'..=b'Z' | b'a'..=b'z' | b'_' => true,
            _ => false,
        }
    }

    pub fn is_id_continue(ch: u8) -> bool {
        match ch {
            b'A'..=b'Z' | b'a'..=b'z' | b'0'..=b'9' | b'_' => true,
            _ => false,
        }
    }

    pub fn is_punct_char(ch: u8) -> bool {
        let category = ascii_char_category(ch);
        category == AsciiCharCategory::Symbol
    }

    pub fn is_octal_digit(ch: u8) -> bool {
        match ch {
            b'0'..=b'7' => true,
            _ => false,
        }
    }

    pub fn is_hex_digit(ch: u8) -> bool {
        match ch {
            b'0'..=b'9' | b'A'..=b'F' | b'a'..=b'f' => true,
            _ => false,
        }
    }
}

pub mod ascii_str {
    use std::fmt::{self, Debug};
    use thiserror::Error;
    #[derive(PartialEq)]
    pub struct AsciiStr(pub [u8]);

    #[derive(Error, Debug)]
    #[error("not 7-bit ascii string")]
    pub struct NotAsciiStrError;

    pub fn from_bytes(bytes: &[u8]) -> Result<&AsciiStr, NotAsciiStrError> {
        for &byte in bytes {
            if byte >= 0x80 {
                return Err(NotAsciiStrError);
            }
        }
        unsafe { Ok(std::mem::transmute(bytes)) }
    }

    impl<'x> Debug for &'x AsciiStr {
        fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
            let str = std::str::from_utf8(&self.0).map_err(|_| fmt::Error)?;
            write!(fmt, "{:?}", str).map_err(|_| fmt::Error)?;
            Ok(())
        }
    }
}

use bitflags::bitflags;

#[allow(non_upper_case_globals)]
bitflags! {
    #[allow(non_upper_case_globals)]
    pub struct LexModeSet : u8 {
        #[allow(non_upper_case_globals)]
        const Nothing = 0;
        #[allow(non_upper_case_globals)]
        const Comment = 0x1;
        #[allow(non_upper_case_globals)]
        const Limbo = 0x2;
        #[allow(non_upper_case_globals)]
        const ModuleName = 0x4;
        #[allow(non_upper_case_globals)]
        const StrLiteral = 0x8;
        #[allow(non_upper_case_globals)]
        const PascalText = 0x10;
        #[allow(non_upper_case_globals)]
        const TeXText = 0x20;
        #[allow(non_upper_case_globals)]
        const DefinitionText = 0x40;
    }
}

impl LexModeSet {
    // workaround for https://github.com/bitflags/bitflags/issues/180
    const fn const_or(self, other: LexModeSet) -> Self {
        LexModeSet::from_bits_truncate(self.bits() | other.bits())
    }
    const fn contains_mode(&self, mode: LexMode) -> bool {
        (self.bits & mode.0) != 0
    }
}

#[derive(Copy, Clone, PartialEq, Eq)]
pub struct LexMode(u8);

use std::fmt;

impl fmt::Debug for LexMode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mode_text = match *self {
            LexMode::Limbo => "Limbo",
            LexMode::TeXText => "TeXText",
            LexMode::ModuleName => "ModuleName",
            LexMode::PascalText => "PascalText",
            LexMode::Comment => "Comment",
            LexMode::StrLiteral => "StrLiteral",
            LexMode::DefinitionText => "DefinitionText",
            _ => unreachable!(),
        };
        write!(f, "{}", mode_text).map_err(|_| fmt::Error)?;
        Ok(())
    }
}

#[allow(non_upper_case_globals)]
impl LexMode {
    pub const Limbo: LexMode = LexMode(LexModeSet::Limbo.bits);
    pub const TeXText: LexMode = LexMode(LexModeSet::TeXText.bits);
    pub const ModuleName: LexMode = LexMode(LexModeSet::ModuleName.bits);
    pub const PascalText: LexMode = LexMode(LexModeSet::PascalText.bits);
    pub const Comment: LexMode = LexMode(LexModeSet::Comment.bits);
    pub const StrLiteral: LexMode = LexMode(LexModeSet::StrLiteral.bits);
    pub const DefinitionText: LexMode = LexMode(LexModeSet::DefinitionText.bits);
}

pub mod control_code {
    use super::token::BoxedTokenList;
    use super::LexModeSet;

    #[derive(Copy, Clone)]
    pub enum SpecialHandling {
        None,
        GroupTitle,
        ModuleName,
        OctalConst,
        HexConst,
        ControlTextUpToAtGT,
    }

    #[derive(Copy, Clone, PartialEq, Debug)]
    pub enum ControlCodeKind {
        EscapedAt,
        DefineModule,
        DefineStarredModule,
        DefineMacro,
        DefineFormat,
        DefineProgram,
        ModuleName,
        OctalConst,
        HexConst,
        StringPoolChecksum,
        MetaCommentBegin,
        MetaCommentEnd,
        ProgramAdjacent,
        ForceIndex,
        ForceIndexMono,
        ForceIndexStyle9,
        ForceHBox,
        ForceVerbatim,
        ForceEOL,
        UnderlineFlag,
        NoUnderlineFlag,
        FormatThinSpace,
        FormatLineBreak,
        FormatSuggestLineBreak,
        FormatLineBreakLarge,
        FormatNoLineBreak,
        FormatInvisibleSemicolon,
        HiddenEndOfModuleName,
    }

    #[derive(Debug, PartialEq)]
    pub struct ControlCode<'x> {
        pub kind: ControlCodeKind,
        pub param: Option<BoxedTokenList<'x>>,
    }

    pub struct ControlCodeInfoRecord {
        pub selector: &'static [u8],
        pub kind: ControlCodeKind,
        pub special_handling: SpecialHandling,
        pub terminating_modes: LexModeSet,
        pub appliable_modes: LexModeSet,
    }

    pub const CONTROL_CODE_DATA: &'static [ControlCodeInfoRecord] = &[
        ControlCodeInfoRecord {
            selector: b"@",
            kind: ControlCodeKind::EscapedAt,
            special_handling: SpecialHandling::None,
            terminating_modes: LexModeSet::Nothing,
            appliable_modes: LexModeSet::Comment
                .const_or(LexModeSet::Limbo)
                .const_or(LexModeSet::ModuleName)
                .const_or(LexModeSet::PascalText)
                .const_or(LexModeSet::StrLiteral)
                .const_or(LexModeSet::TeXText),
        },
        ControlCodeInfoRecord {
            selector: b" \t\r\n",
            kind: ControlCodeKind::DefineModule,
            special_handling: SpecialHandling::None,
            terminating_modes: LexModeSet::Limbo
                .const_or(LexModeSet::PascalText)
                .const_or(LexModeSet::TeXText),
            appliable_modes: LexModeSet::Limbo
                .const_or(LexModeSet::PascalText)
                .const_or(LexModeSet::TeXText),
        },
        ControlCodeInfoRecord {
            selector: b"*",
            kind: ControlCodeKind::DefineStarredModule,
            special_handling: SpecialHandling::GroupTitle,
            terminating_modes: LexModeSet::Limbo
                .const_or(LexModeSet::PascalText)
                .const_or(LexModeSet::TeXText),
            appliable_modes: LexModeSet::Limbo
                .const_or(LexModeSet::PascalText)
                .const_or(LexModeSet::TeXText),
        },
        ControlCodeInfoRecord {
            selector: b"dD",
            kind: ControlCodeKind::DefineMacro,
            special_handling: SpecialHandling::None,
            terminating_modes: LexModeSet::PascalText.const_or(LexModeSet::TeXText),
            appliable_modes: LexModeSet::PascalText.const_or(LexModeSet::TeXText),
        },
        ControlCodeInfoRecord {
            selector: b"fF",
            kind: ControlCodeKind::DefineFormat,
            special_handling: SpecialHandling::None,
            terminating_modes: LexModeSet::PascalText.const_or(LexModeSet::TeXText),
            appliable_modes: LexModeSet::PascalText.const_or(LexModeSet::TeXText),
        },
        ControlCodeInfoRecord {
            selector: b"pP",
            kind: ControlCodeKind::DefineProgram,
            special_handling: SpecialHandling::None,
            terminating_modes: LexModeSet::PascalText.const_or(LexModeSet::TeXText),
            appliable_modes: LexModeSet::PascalText.const_or(LexModeSet::TeXText),
        },
        ControlCodeInfoRecord {
            selector: b"<",
            kind: ControlCodeKind::ModuleName,
            special_handling: SpecialHandling::ModuleName,
            terminating_modes: LexModeSet::TeXText,
            appliable_modes: LexModeSet::PascalText.const_or(LexModeSet::TeXText),
        },
        ControlCodeInfoRecord {
            selector: b"\'",
            kind: ControlCodeKind::OctalConst,
            special_handling: SpecialHandling::OctalConst,
            terminating_modes: LexModeSet::Nothing,
            appliable_modes: LexModeSet::PascalText.const_or(LexModeSet::TeXText),
        },
        ControlCodeInfoRecord {
            selector: b"\"",
            kind: ControlCodeKind::HexConst,
            special_handling: SpecialHandling::HexConst,
            terminating_modes: LexModeSet::Nothing,
            appliable_modes: LexModeSet::PascalText.const_or(LexModeSet::TeXText),
        },
        ControlCodeInfoRecord {
            selector: b"$",
            kind: ControlCodeKind::StringPoolChecksum,
            special_handling: SpecialHandling::None,
            terminating_modes: LexModeSet::Nothing,
            appliable_modes: LexModeSet::PascalText,
        },
        ControlCodeInfoRecord {
            selector: b"{",
            kind: ControlCodeKind::MetaCommentBegin,
            special_handling: SpecialHandling::None,
            terminating_modes: LexModeSet::Nothing,
            appliable_modes: LexModeSet::PascalText,
        },
        ControlCodeInfoRecord {
            selector: b"}",
            kind: ControlCodeKind::MetaCommentEnd,
            special_handling: SpecialHandling::None,
            terminating_modes: LexModeSet::Nothing,
            appliable_modes: LexModeSet::PascalText,
        },
        ControlCodeInfoRecord {
            selector: b"&",
            kind: ControlCodeKind::ProgramAdjacent,
            special_handling: SpecialHandling::None,
            terminating_modes: LexModeSet::Nothing,
            appliable_modes: LexModeSet::PascalText,
        },
        ControlCodeInfoRecord {
            selector: b"^",
            kind: ControlCodeKind::ForceIndex,
            special_handling: SpecialHandling::ControlTextUpToAtGT,
            terminating_modes: LexModeSet::Nothing,
            appliable_modes: LexModeSet::PascalText.const_or(LexModeSet::TeXText),
        },
        ControlCodeInfoRecord {
            selector: b".",
            kind: ControlCodeKind::ForceIndexMono,
            special_handling: SpecialHandling::ControlTextUpToAtGT,
            terminating_modes: LexModeSet::Nothing,
            appliable_modes: LexModeSet::PascalText.const_or(LexModeSet::TeXText),
        },
        ControlCodeInfoRecord {
            selector: b":",
            kind: ControlCodeKind::ForceIndexStyle9,
            special_handling: SpecialHandling::ControlTextUpToAtGT,
            terminating_modes: LexModeSet::Nothing,
            appliable_modes: LexModeSet::PascalText.const_or(LexModeSet::TeXText),
        },
        ControlCodeInfoRecord {
            selector: b"t",
            kind: ControlCodeKind::ForceHBox,
            special_handling: SpecialHandling::ControlTextUpToAtGT,
            terminating_modes: LexModeSet::Nothing,
            appliable_modes: LexModeSet::PascalText,
        },
        ControlCodeInfoRecord {
            selector: b"=",
            kind: ControlCodeKind::ForceVerbatim,
            special_handling: SpecialHandling::ControlTextUpToAtGT,
            terminating_modes: LexModeSet::Nothing,
            appliable_modes: LexModeSet::PascalText,
        },
        ControlCodeInfoRecord {
            selector: b"\\",
            kind: ControlCodeKind::ForceEOL,
            special_handling: SpecialHandling::None,
            terminating_modes: LexModeSet::Nothing,
            appliable_modes: LexModeSet::PascalText,
        },
        ControlCodeInfoRecord {
            selector: b"!",
            kind: ControlCodeKind::UnderlineFlag,
            special_handling: SpecialHandling::None,
            terminating_modes: LexModeSet::Nothing,
            appliable_modes: LexModeSet::PascalText.const_or(LexModeSet::TeXText),
        },
        ControlCodeInfoRecord {
            selector: b"?",
            kind: ControlCodeKind::NoUnderlineFlag,
            special_handling: SpecialHandling::None,
            terminating_modes: LexModeSet::Nothing,
            appliable_modes: LexModeSet::PascalText.const_or(LexModeSet::TeXText),
        },
        ControlCodeInfoRecord {
            selector: b",",
            kind: ControlCodeKind::FormatThinSpace,
            special_handling: SpecialHandling::None,
            terminating_modes: LexModeSet::Nothing,
            appliable_modes: LexModeSet::PascalText,
        },
        ControlCodeInfoRecord {
            selector: b"/",
            kind: ControlCodeKind::FormatLineBreak,
            special_handling: SpecialHandling::None,
            terminating_modes: LexModeSet::Nothing,
            appliable_modes: LexModeSet::PascalText,
        },
        ControlCodeInfoRecord {
            selector: b"|",
            kind: ControlCodeKind::FormatSuggestLineBreak,
            special_handling: SpecialHandling::None,
            terminating_modes: LexModeSet::Nothing,
            appliable_modes: LexModeSet::PascalText,
        },
        ControlCodeInfoRecord {
            selector: b"#",
            kind: ControlCodeKind::FormatLineBreakLarge,
            special_handling: SpecialHandling::None,
            terminating_modes: LexModeSet::Nothing,
            appliable_modes: LexModeSet::PascalText,
        },
        ControlCodeInfoRecord {
            selector: b"+",
            kind: ControlCodeKind::FormatNoLineBreak,
            special_handling: SpecialHandling::None,
            terminating_modes: LexModeSet::Nothing,
            appliable_modes: LexModeSet::PascalText,
        },
        ControlCodeInfoRecord {
            selector: b";",
            kind: ControlCodeKind::FormatInvisibleSemicolon,
            special_handling: SpecialHandling::None,
            terminating_modes: LexModeSet::Nothing,
            appliable_modes: LexModeSet::PascalText,
        },
    ];
    pub fn get_control_code_info_record_for_selector(
        selector: u8,
    ) -> Option<&'static ControlCodeInfoRecord> {
        use once_cell::sync::Lazy;
        static CONTROL_CODE_TABLE: Lazy<[Option<&'static ControlCodeInfoRecord>; 256]> =
            Lazy::new(|| {
                let mut table = [None; 256];
                for item in CONTROL_CODE_DATA.iter() {
                    for &ch in item.selector.iter() {
                        assert!(table[ch as usize].is_none());
                        table[ch as usize] = Some(item);
                    }
                }
                table
            });
        CONTROL_CODE_TABLE[selector as usize]
    }
}

pub mod operator {
    #[derive(Clone, PartialEq, Debug)]
    pub enum Operator {
        Plus,
        Subtract,
        Multiply,
        Divide,
        Modulus,
        Equal,
        NotEqual,
        GreaterThan,
        LessThan,
        GreaterEq,
        LessEq,
        In,
        And,
        AndThen,
        Or,
        OrElse,
        Not,
        BitAnd,
        BitOr,
        BitNot,
        BitShl,
        BitShr,
        Assign,
    }
}

pub mod punctuation {
    use super::operator::Operator;

    #[derive(Clone, PartialEq, Debug)]
    pub enum Punctuation {
        Op(Operator),
        LParen,
        RParen,
        RangeUntil,
        WithType,
        EndOfStatement,
        EndOfLastStatement,
    }

    pub struct PunctuationInfo {
        pub literal: &'static [u8],
        pub kind: Punctuation,
    }

    pub const PUNCTUATION_TABLE: &[PunctuationInfo] = &[
        PunctuationInfo {
            literal: b"..",
            kind: Punctuation::RangeUntil,
        },
        PunctuationInfo {
            literal: b":",
            kind: Punctuation::WithType,
        },
        PunctuationInfo {
            literal: b"(",
            kind: Punctuation::LParen,
        },
        PunctuationInfo {
            literal: b")",
            kind: Punctuation::RParen,
        },
        PunctuationInfo {
            literal: b";",
            kind: Punctuation::EndOfStatement,
        },
        PunctuationInfo {
            literal: b".",
            kind: Punctuation::EndOfLastStatement,
        },
        PunctuationInfo {
            literal: b"=",
            kind: Punctuation::Op(Operator::Equal),
        },
    ];
}

pub mod literal {
    use super::ascii_str::AsciiStr;
    use super::token::BoxedTokenList;

    #[derive(Debug, PartialEq)]
    pub enum Literal<'x> {
        IntegerU32(u32),
        StringLiteral(&'x AsciiStr),
        PreprocessedStringLiteral(BoxedTokenList<'x>),
    }
}

#[derive(Error, Debug)]
pub enum LexError {
    #[error("Unexpected EOF reached before proper finish")]
    UnexpectedEOF,
    #[error("Not 7-bit ascii byte occurred")]
    Not7BitAscii(#[from] ascii_str::NotAsciiStrError),
    #[error("Invalid control code")]
    InvalidControlCodeChar { control_code: u8, pos: usize },
    #[error("Control code used where it's not usable")]
    ControlCodeInNonApplicableMode,
    #[error("Integer literal overflow")]
    IntegerLiteralOverflow,
    #[error("Control text not properly finished with @>")]
    ControlTextNotProperlyFinished,
    #[error("Group title not properly finished with .")]
    GroupTitleNotProperlyFinished,
    #[error("Comment not properly finished with }}")]
    CommentNotProperlyFinished,
    #[error("String literal not properly finished with \'")]
    StringLiteralNotProperlyFinished,
    #[error("String literal not properly finished with \"")]
    PreprocessedStringLiteralNotProperlyFinished,
    #[error("Any lex error!")]
    AnyLexError,
}

pub enum LexControlFlow<'x> {
    Continue(&'x [u8], usize),
    Finish,
    StartNewModule(LexMode, &'x [u8], usize),
    StartNewDefinition(LexMode, &'x [u8], usize),
    StartNewProgramText(LexMode, &'x [u8], usize),
}

pub mod program_text {
    use super::punctuation::Punctuation;
    pub type U8SlicePair<'x> = (&'x [u8], &'x [u8]);

    pub fn parse_maybe_whitespace(l: &[u8]) -> U8SlicePair<'_> {
        use super::ascii_char::is_whitespace_char;
        let pos = l
            .iter()
            .copied()
            .take_while(|&ch| is_whitespace_char(ch))
            .count();
        l.split_at(pos)
    }

    pub fn parse_identifier(l: &[u8]) -> Option<U8SlicePair<'_>> {
        use super::ascii_char::{is_id_continue, is_id_start};
        let pos = l
            .iter()
            .copied()
            .enumerate()
            .take_while(|&(n, ch)| {
                if n == 0 {
                    is_id_start(ch)
                } else {
                    is_id_continue(ch)
                }
            })
            .count();

        if pos == 0 {
            return None;
        }
        Some(l.split_at(pos))
    }

    pub fn parse_punct(l: &[u8]) -> Option<(Punctuation, usize, &[u8])> {
        use super::punctuation::PUNCTUATION_TABLE;
        for table_item in PUNCTUATION_TABLE {
            if l.starts_with(table_item.literal) {
                let literal_len = table_item.literal.len();
                return Some((table_item.kind.clone(), literal_len, &l[literal_len..]));
            }
        }
        None
    }
}

pub mod token {
    use super::ascii_char::{is_hex_digit, is_octal_digit};
    use super::ascii_str::{self, AsciiStr};
    use super::control_code::ControlCode;
    use super::literal::Literal;
    use super::punctuation::Punctuation;
    use super::{LexControlFlow, LexError, LexMode};

    #[derive(Debug, PartialEq)]
    pub enum Token<'x> {
        CtrlCode(ControlCode<'x>),
        WS,
        EOL,
        EOF,
        TextFragment(&'x AsciiStr),
        Ident(&'x AsciiStr),
        Punct(Punctuation),
        Literal(Literal<'x>),
        Comment(BoxedTokenList<'x>),
        Text(BoxedTokenList<'x>),
    }

    pub type TokenList<'x> = Vec<Token<'x>>;
    pub type BoxedTokenList<'x> = Box<Vec<Token<'x>>>;

    fn continue_or_finish(l: &[u8], pos: usize) -> LexControlFlow<'_> {
        if l.is_empty() {
            LexControlFlow::Finish
        } else {
            LexControlFlow::Continue(l, pos)
        }
    }

    fn switch_mode<'x>(
        control_code: &ControlCode<'x>,
        l: &'x [u8],
        pos: usize,
    ) -> LexControlFlow<'x> {
        use super::control_code::ControlCodeKind;
        match control_code.kind {
            ControlCodeKind::DefineModule => {
                LexControlFlow::StartNewModule(LexMode::TeXText, l, pos)
            }
            ControlCodeKind::DefineStarredModule => {
                LexControlFlow::StartNewModule(LexMode::TeXText, l, pos)
            }
            ControlCodeKind::DefineMacro => {
                LexControlFlow::StartNewDefinition(LexMode::DefinitionText, l, pos)
            }
            ControlCodeKind::DefineFormat => {
                LexControlFlow::StartNewDefinition(LexMode::DefinitionText, l, pos)
            }
            ControlCodeKind::DefineProgram => {
                LexControlFlow::StartNewProgramText(LexMode::PascalText, l, pos)
            }
            ControlCodeKind::ModuleName => {
                LexControlFlow::StartNewProgramText(LexMode::PascalText, l, pos)
            }
            _ => unreachable! {},
        }
    }

    pub const CONTROL_CODE_PREFIX: u8 = b'@';

    pub const LINE_FEED: u8 = b'\n';
    pub const CARRIAGE_RETURN: u8 = b'\r';

    pub const SIMPLE_ESCAPED_ATAIL: &'static [u8] = b"@@";
    pub const END_OF_CONTROL_TEXT: &'static [u8] = b"@>";

    pub fn lex_u32_literal_with_radix(l: &[u8], radix: usize) -> Result<Literal, LexError> {
        use std::str::from_utf8;
        if let Ok(v) = u32::from_str_radix(from_utf8(l).unwrap(), radix as u32) {
            Ok(Literal::IntegerU32(v))
        } else {
            Err(LexError::IntegerLiteralOverflow)
        }
    }

    pub fn lex_numeric_literal(l: &[u8]) -> Result<(Literal, usize, &[u8]), LexError> {
        use super::ascii_char::is_numeric_char;
        let count = l
            .iter()
            .copied()
            .take_while(|&ch| is_numeric_char(ch))
            .count();
        let (numeric, rest) = l.split_at(count);
        let literal = lex_u32_literal_with_radix(numeric, 10)?;
        Ok((literal, numeric.len(), rest))
    }

    fn lex_control_code_rest<'x>(
        l: &'x [u8],
        mode: LexMode,
        pos: usize,
    ) -> Result<(ControlCode<'x>, &'x [u8], usize, bool), LexError> {
        use super::control_code::get_control_code_info_record_for_selector;
        use super::control_code::SpecialHandling;
        let selector = l.get(0).copied().ok_or_else(|| LexError::UnexpectedEOF)?;

        let control_code_info =
            get_control_code_info_record_for_selector(selector).ok_or_else(|| {
                LexError::InvalidControlCodeChar {
                    control_code: selector,
                    pos: pos,
                }
            })?;

        if !control_code_info.appliable_modes.contains_mode(mode) {
            return Err(LexError::ControlCodeInNonApplicableMode);
        }

        let is_terminator = control_code_info.terminating_modes.contains_mode(mode);

        let rest = &l[1..];
        let (control_code, rest, pos) = match control_code_info.special_handling {
            SpecialHandling::None => {
                let control_code = ControlCode {
                    kind: control_code_info.kind,
                    param: None,
                };
                (control_code, rest, pos + 1)
            }
            SpecialHandling::GroupTitle => {
                let group_title_end = memchr::memchr2(b'.', b'\n', rest).unwrap_or(rest.len());
                if !rest[group_title_end..].starts_with(b".") {
                    return Err(LexError::GroupTitleNotProperlyFinished);
                }
                let control_text_end = group_title_end + 1;
                let group_title_text = ascii_str::from_bytes(&rest[..group_title_end])?;
                let control_code = ControlCode {
                    kind: control_code_info.kind,
                    param: Some(Box::new(vec![Token::TextFragment(group_title_text)])),
                };
                (
                    control_code,
                    &rest[control_text_end..],
                    pos + 1 + control_text_end,
                )
            }
            SpecialHandling::ModuleName => {
                let mut mode = LexMode::ModuleName;
                let mut data = rest;
                let mut pos = pos + 1;
                let mut tokens = vec![];
                'module_name_loop: loop {
                    use super::control_code::ControlCodeKind;
                    let (token, control_flow) = lex_token(data, mode, pos)?;
                    match control_flow {
                        LexControlFlow::Continue(rest_data, new_pos) => {
                            pos = new_pos;
                            data = rest_data;
                            match token {
                                Token::CtrlCode(ControlCode {
                                    kind: ControlCodeKind::HiddenEndOfModuleName,
                                    ..
                                }) => {
                                    break 'module_name_loop;
                                }
                                _ => {
                                    tokens.push(token);
                                }
                            }
                        }
                        LexControlFlow::Finish => {
                            return Err(LexError::UnexpectedEOF);
                        }
                        _ => {
                            // can't start new module | definition | program text here
                            return Err(LexError::ControlCodeInNonApplicableMode);
                        }
                    }
                }
                let control_code = ControlCode {
                    kind: control_code_info.kind,
                    param: Some(Box::new(tokens)),
                };
                (control_code, data, pos)
            }
            SpecialHandling::OctalConst => {
                let octal_digit_count = rest
                    .iter()
                    .copied()
                    .take_while(|&ch| is_octal_digit(ch))
                    .count();
                let octal_digits = &rest[..octal_digit_count];
                let literal = lex_u32_literal_with_radix(octal_digits, 8)?;
                let control_code = ControlCode {
                    kind: control_code_info.kind,
                    param: Some(Box::new(vec![Token::Literal(literal)])),
                };
                (
                    control_code,
                    &rest[octal_digit_count..],
                    pos + 1 + octal_digit_count,
                )
            }
            SpecialHandling::HexConst => {
                let hex_digit_count = rest
                    .iter()
                    .copied()
                    .take_while(|&ch| is_hex_digit(ch))
                    .count();
                let hex_digits = &rest[..hex_digit_count];
                let literal = lex_u32_literal_with_radix(hex_digits, 8)?;
                let control_code = ControlCode {
                    kind: control_code_info.kind,
                    param: Some(Box::new(vec![Token::Literal(literal)])),
                };
                (
                    control_code,
                    &rest[hex_digit_count..],
                    pos + 1 + hex_digit_count,
                )
            }
            SpecialHandling::ControlTextUpToAtGT => {
                let control_text_len =
                    memchr::memchr3(CONTROL_CODE_PREFIX, LINE_FEED, CARRIAGE_RETURN, rest)
                        .unwrap_or(rest.len());
                if !rest[control_text_len..].starts_with(END_OF_CONTROL_TEXT) {
                    return Err(LexError::ControlTextNotProperlyFinished);
                }
                let control_code = ControlCode {
                    kind: control_code_info.kind,
                    param: Some(Box::new(vec![Token::TextFragment(ascii_str::from_bytes(
                        &rest[..control_text_len],
                    )?)])),
                };
                (
                    control_code,
                    &rest[control_text_len + END_OF_CONTROL_TEXT.len()..],
                    pos + 1 + control_text_len + END_OF_CONTROL_TEXT.len(),
                )
            }
        };
        Ok((control_code, rest, pos, is_terminator))
    }

    pub fn lex_comment_rest<'x>(
        l: &'x [u8],
        pos: usize,
    ) -> Result<(Token<'x>, LexControlFlow<'x>), LexError> {
        // fixme: properly parse commment
        let text_end = memchr::memchr2(b'}', b'\n', l).unwrap_or(l.len());
        if !l[text_end..].starts_with(b"}") {
            return Err(LexError::CommentNotProperlyFinished);
        }
        let comment_end = text_end + 1;
        let mut tokens = vec![];
        tokens.push(Token::TextFragment(ascii_str::from_bytes(&l[..text_end])?));
        let token = Token::Comment(Box::new(tokens));
        Ok((
            token,
            continue_or_finish(&l[comment_end..], pos + comment_end),
        ))
    }

    pub fn lex_string_literal_rest<'x>(
        l: &'x [u8],
        pos: usize,
    ) -> Result<(Token<'x>, LexControlFlow<'x>), LexError> {
        // fixme: properly parse string literal
        let text_end = memchr::memchr2(b'\'', b'\n', l).unwrap_or(l.len());
        if !l[text_end..].starts_with(b"\'") {
            return Err(LexError::StringLiteralNotProperlyFinished);
        }
        let comment_end = text_end + 1;
        let literal_text = ascii_str::from_bytes(&l[..text_end])?;
        let token = Token::Literal(Literal::StringLiteral(literal_text));
        Ok((
            token,
            continue_or_finish(&l[comment_end..], pos + comment_end),
        ))
    }

    pub fn lex_preprocessed_string_literal_rest<'x>(
        l: &'x [u8],
        pos: usize,
    ) -> Result<(Token<'x>, LexControlFlow<'x>), LexError> {
        // fixme: properly parse string literal
        let text_end = memchr::memchr2(b'\"', b'\n', l).unwrap_or(l.len());
        if !l[text_end..].starts_with(b"\"") {
            return Err(LexError::PreprocessedStringLiteralNotProperlyFinished);
        }
        let comment_end = text_end + 1;
        let mut tokens = vec![];
        tokens.push(Token::TextFragment(ascii_str::from_bytes(&l[..text_end])?));
        let token = Token::Literal(Literal::PreprocessedStringLiteral(Box::new(tokens)));
        Ok((
            token,
            continue_or_finish(&l[comment_end..], pos + comment_end),
        ))
    }

    pub fn lex_token<'x>(
        l: &'x [u8],
        mode: LexMode,
        pos: usize,
    ) -> Result<(Token<'x>, LexControlFlow<'x>), LexError> {
        let first_ch = if let Some(&ch) = l.get(0) {
            ch
        } else {
            match mode {
                LexMode::Limbo | LexMode::TeXText => {
                    let l = ascii_str::from_bytes(l)?;
                    return Ok((Token::TextFragment(l), LexControlFlow::Finish));
                }
                _ => {
                    return Err(LexError::UnexpectedEOF);
                }
            }
        };
        match mode {
            LexMode::Limbo | LexMode::TeXText | LexMode::PascalText
                if first_ch == CONTROL_CODE_PREFIX =>
            {
                let rest = &l[1..];
                let (control_code, rest, pos, is_terminator) =
                    lex_control_code_rest(rest, mode, pos + 1)?;
                if !is_terminator {
                    return Ok((Token::CtrlCode(control_code), continue_or_finish(rest, pos)));
                } else {
                    let new_mode = switch_mode(&control_code, rest, pos);
                    return Ok((Token::CtrlCode(control_code), new_mode));
                }
            }
            LexMode::ModuleName if first_ch == CONTROL_CODE_PREFIX => {
                use super::control_code::ControlCodeKind;
                if l.starts_with(SIMPLE_ESCAPED_ATAIL) {
                    let control_code = ControlCode {
                        kind: ControlCodeKind::EscapedAt,
                        param: None,
                    };
                    return Ok((
                        Token::CtrlCode(control_code),
                        continue_or_finish(&l[2..], pos + 2),
                    ));
                } else if l.starts_with(END_OF_CONTROL_TEXT) {
                    let control_code = ControlCode {
                        kind: ControlCodeKind::HiddenEndOfModuleName,
                        param: None,
                    };
                    return Ok((
                        Token::CtrlCode(control_code),
                        continue_or_finish(&l[2..], pos + 2),
                    ));
                } else {
                    return Err(LexError::ControlCodeInNonApplicableMode);
                }
            }
            LexMode::Limbo | LexMode::TeXText => {
                use memchr::memchr;
                debug_assert!(first_ch != CONTROL_CODE_PREFIX);
                let text_len = memchr(CONTROL_CODE_PREFIX, l).unwrap_or_else(|| l.len());
                let (text, rest) = l.split_at(text_len);
                let text = ascii_str::from_bytes(text)?;
                return Ok((
                    Token::TextFragment(text),
                    continue_or_finish(rest, pos + text_len),
                ));
            }
            LexMode::ModuleName => {
                use memchr::memchr;
                debug_assert!(first_ch != CONTROL_CODE_PREFIX);
                let text_len = memchr(CONTROL_CODE_PREFIX, l).unwrap_or_else(|| l.len());
                let (text, rest) = l.split_at(text_len);
                let text = ascii_str::from_bytes(text)?;
                return Ok((
                    Token::TextFragment(text),
                    continue_or_finish(rest, pos + text_len),
                ));
            }
            LexMode::PascalText => {
                use super::ascii_char;
                use super::program_text;

                debug_assert!(first_ch != CONTROL_CODE_PREFIX);
                if ascii_char::is_whitespace_char(first_ch) {
                    let (ws, rest) = program_text::parse_maybe_whitespace(l);
                    return Ok((Token::WS, continue_or_finish(rest, pos + ws.len())));
                } else if ascii_char::is_id_start(first_ch) {
                    let (id, rest) = program_text::parse_identifier(l).expect("");
                    return Ok((
                        Token::Ident(ascii_str::from_bytes(id)?),
                        continue_or_finish(rest, pos + id.len()),
                    ));
                } else if first_ch == b'{' {
                    let rest = &l[1..];
                    return lex_comment_rest(rest, pos + 1);
                } else if first_ch == b'\'' {
                    let rest = &l[1..];
                    return lex_string_literal_rest(rest, pos + 1);
                } else if first_ch == b'\"' {
                    let rest = &l[1..];
                    return lex_preprocessed_string_literal_rest(rest, pos + 1);
                } else if ascii_char::is_punct_char(first_ch) {
                    let (punct, punct_len, rest) = program_text::parse_punct(l).expect("");
                    return Ok((
                        Token::Punct(punct),
                        continue_or_finish(rest, pos + punct_len),
                    ));
                } else if ascii_char::is_numeric_char(first_ch) {
                    let (numeric, numeric_len, rest) = lex_numeric_literal(l)?;
                    return Ok((
                        Token::Literal(numeric),
                        continue_or_finish(rest, pos + numeric_len),
                    ));
                } else {
                    unimplemented!("{:?}", first_ch);
                }
            }
            _ => unimplemented!(),
        }
    }
}
