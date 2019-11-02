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

    #[allow(dead_code)]
    pub fn is_invalid_char(ch: u8) -> bool {
        ascii_char_category(ch) == AsciiCharCategory::Invalid
    }

    pub fn is_inline_whitespace_char(ch: u8) -> bool {
        let category = ascii_char_category(ch);
        category == AsciiCharCategory::InlineWhitespace
    }

    pub fn is_whitespace_char(ch: u8) -> bool {
        let category = ascii_char_category(ch);
        category == AsciiCharCategory::InlineWhitespace
            || category == AsciiCharCategory::LineFeedWhitespace
    }

    #[allow(dead_code)]
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
    #[repr(transparent)]
    #[derive(PartialEq)]
    pub struct AsciiStr(pub [u8]);

    impl AsciiStr {
        pub fn try_split_ending_substr(&self, bytes: &Self) -> (&Self, Option<&Self>) {
            if (self.0).ends_with(&bytes.0) {
                let pos = self.0.len() - bytes.0.len();
                unsafe { std::mem::transmute((&(self.0)[..pos], Some(&(self.0)[pos..]))) }
            } else {
                (self, None)
            }
        }
    }

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

bitflags! {
    pub struct LexModeSet : u8 {
        const NOTHING = 0;
        const COMMENT = 0x1;
        const LIMBO = 0x2;
        const MODULE_NAME = 0x4;
        const STRING_LITERAL = 0x8;
        const PASCAL_TEXT = 0x10;
        const TEX_TEXT = 0x20;
        const DEFINITION_TEXT = 0x40;
        const INLINE_PASCAL_TEXT = 0x80;
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
            LexMode::LIMBO => "Limbo",
            LexMode::TEX_TEXT => "TeXText",
            LexMode::MODULE_NAME => "ModuleName",
            LexMode::PASCAL_TEXT => "PascalText",
            LexMode::COMMENT => "Comment",
            LexMode::STRING_LITERAL => "StrLiteral",
            LexMode::DEFINITION_TEXT => "DefinitionText",
            LexMode::INLINE_PASCAL_TEXT => "InlinePascalText",
            _ => unreachable!(),
        };
        write!(f, "{}", mode_text).map_err(|_| fmt::Error)?;
        Ok(())
    }
}

impl LexMode {
    pub const LIMBO: LexMode = LexMode(LexModeSet::LIMBO.bits);
    pub const TEX_TEXT: LexMode = LexMode(LexModeSet::TEX_TEXT.bits);
    pub const MODULE_NAME: LexMode = LexMode(LexModeSet::MODULE_NAME.bits);
    pub const PASCAL_TEXT: LexMode = LexMode(LexModeSet::PASCAL_TEXT.bits);
    pub const COMMENT: LexMode = LexMode(LexModeSet::COMMENT.bits);
    pub const STRING_LITERAL: LexMode = LexMode(LexModeSet::STRING_LITERAL.bits);
    pub const DEFINITION_TEXT: LexMode = LexMode(LexModeSet::DEFINITION_TEXT.bits);
    pub const INLINE_PASCAL_TEXT: LexMode = LexMode(LexModeSet::INLINE_PASCAL_TEXT.bits);
}

pub mod control_code {
    use super::token::BoxedTokenList;
    use super::LexModeSet;

    #[derive(Copy, Clone)]
    pub enum SpecialHandling {
        None,
        GroupTitle,
        ModuleName,
        MacroDefinition,
        FormatDefinition,
        OctalConst,
        HexConst,
        ControlTextUpToAtGT,
        WarnAndIgnore, // occurred in xetex.web:9057
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
        Ignored,
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
            terminating_modes: LexModeSet::NOTHING,
            appliable_modes: LexModeSet::COMMENT
                .const_or(LexModeSet::LIMBO)
                .const_or(LexModeSet::MODULE_NAME)
                .const_or(LexModeSet::PASCAL_TEXT)
                .const_or(LexModeSet::STRING_LITERAL)
                .const_or(LexModeSet::TEX_TEXT)
                .const_or(LexModeSet::DEFINITION_TEXT)
                .const_or(LexModeSet::INLINE_PASCAL_TEXT),
        },
        ControlCodeInfoRecord {
            selector: b" \t\r\n",
            kind: ControlCodeKind::DefineModule,
            special_handling: SpecialHandling::None,
            terminating_modes: LexModeSet::LIMBO
                .const_or(LexModeSet::PASCAL_TEXT)
                .const_or(LexModeSet::TEX_TEXT)
                .const_or(LexModeSet::DEFINITION_TEXT)
                .const_or(LexModeSet::INLINE_PASCAL_TEXT),
            appliable_modes: LexModeSet::LIMBO
                .const_or(LexModeSet::PASCAL_TEXT)
                .const_or(LexModeSet::TEX_TEXT)
                .const_or(LexModeSet::DEFINITION_TEXT)
                .const_or(LexModeSet::INLINE_PASCAL_TEXT),
        },
        ControlCodeInfoRecord {
            selector: b"*",
            kind: ControlCodeKind::DefineStarredModule,
            special_handling: SpecialHandling::GroupTitle,
            terminating_modes: LexModeSet::LIMBO
                .const_or(LexModeSet::PASCAL_TEXT)
                .const_or(LexModeSet::TEX_TEXT)
                .const_or(LexModeSet::DEFINITION_TEXT),
            appliable_modes: LexModeSet::LIMBO
                .const_or(LexModeSet::PASCAL_TEXT)
                .const_or(LexModeSet::TEX_TEXT)
                .const_or(LexModeSet::DEFINITION_TEXT),
        },
        ControlCodeInfoRecord {
            selector: b"dD",
            kind: ControlCodeKind::DefineMacro,
            special_handling: SpecialHandling::MacroDefinition,
            terminating_modes: LexModeSet::PASCAL_TEXT
                .const_or(LexModeSet::TEX_TEXT)
                .const_or(LexModeSet::DEFINITION_TEXT),
            appliable_modes: LexModeSet::PASCAL_TEXT
                .const_or(LexModeSet::TEX_TEXT)
                .const_or(LexModeSet::DEFINITION_TEXT),
        },
        ControlCodeInfoRecord {
            selector: b"fF",
            kind: ControlCodeKind::DefineFormat,
            special_handling: SpecialHandling::FormatDefinition,
            terminating_modes: LexModeSet::PASCAL_TEXT
                .const_or(LexModeSet::TEX_TEXT)
                .const_or(LexModeSet::DEFINITION_TEXT),
            appliable_modes: LexModeSet::PASCAL_TEXT
                .const_or(LexModeSet::TEX_TEXT)
                .const_or(LexModeSet::DEFINITION_TEXT),
        },
        ControlCodeInfoRecord {
            selector: b"pP",
            kind: ControlCodeKind::DefineProgram,
            special_handling: SpecialHandling::None,
            terminating_modes: LexModeSet::PASCAL_TEXT
                .const_or(LexModeSet::TEX_TEXT)
                .const_or(LexModeSet::DEFINITION_TEXT),
            appliable_modes: LexModeSet::PASCAL_TEXT
                .const_or(LexModeSet::TEX_TEXT)
                .const_or(LexModeSet::DEFINITION_TEXT),
        },
        ControlCodeInfoRecord {
            selector: b"<",
            kind: ControlCodeKind::ModuleName,
            special_handling: SpecialHandling::ModuleName,
            terminating_modes: LexModeSet::TEX_TEXT.const_or(LexModeSet::DEFINITION_TEXT),
            appliable_modes: LexModeSet::PASCAL_TEXT
                .const_or(LexModeSet::DEFINITION_TEXT)
                .const_or(LexModeSet::TEX_TEXT),
        },
        ControlCodeInfoRecord {
            selector: b"\'",
            kind: ControlCodeKind::OctalConst,
            special_handling: SpecialHandling::OctalConst,
            terminating_modes: LexModeSet::NOTHING,
            appliable_modes: LexModeSet::PASCAL_TEXT
                .const_or(LexModeSet::DEFINITION_TEXT)
                .const_or(LexModeSet::TEX_TEXT)
                .const_or(LexModeSet::INLINE_PASCAL_TEXT),
        },
        ControlCodeInfoRecord {
            selector: b"\"",
            kind: ControlCodeKind::HexConst,
            special_handling: SpecialHandling::HexConst,
            terminating_modes: LexModeSet::NOTHING,
            appliable_modes: LexModeSet::PASCAL_TEXT
                .const_or(LexModeSet::DEFINITION_TEXT)
                .const_or(LexModeSet::TEX_TEXT)
                .const_or(LexModeSet::INLINE_PASCAL_TEXT)
                .const_or(LexModeSet::COMMENT /*xetex.web:8641*/),
        },
        ControlCodeInfoRecord {
            selector: b"$",
            kind: ControlCodeKind::StringPoolChecksum,
            special_handling: SpecialHandling::None,
            terminating_modes: LexModeSet::NOTHING,
            appliable_modes: LexModeSet::PASCAL_TEXT
                .const_or(LexModeSet::DEFINITION_TEXT)
                .const_or(LexModeSet::INLINE_PASCAL_TEXT),
        },
        ControlCodeInfoRecord {
            selector: b"{",
            kind: ControlCodeKind::MetaCommentBegin,
            special_handling: SpecialHandling::None,
            terminating_modes: LexModeSet::NOTHING,
            appliable_modes: LexModeSet::PASCAL_TEXT
                .const_or(LexModeSet::DEFINITION_TEXT)
                .const_or(LexModeSet::INLINE_PASCAL_TEXT),
        },
        ControlCodeInfoRecord {
            selector: b"}",
            kind: ControlCodeKind::MetaCommentEnd,
            special_handling: SpecialHandling::None,
            terminating_modes: LexModeSet::NOTHING,
            appliable_modes: LexModeSet::PASCAL_TEXT
                .const_or(LexModeSet::DEFINITION_TEXT)
                .const_or(LexModeSet::INLINE_PASCAL_TEXT),
        },
        ControlCodeInfoRecord {
            selector: b"&",
            kind: ControlCodeKind::ProgramAdjacent,
            special_handling: SpecialHandling::None,
            terminating_modes: LexModeSet::NOTHING,
            appliable_modes: LexModeSet::PASCAL_TEXT
                .const_or(LexModeSet::DEFINITION_TEXT)
                .const_or(LexModeSet::INLINE_PASCAL_TEXT),
        },
        ControlCodeInfoRecord {
            selector: b"^",
            kind: ControlCodeKind::ForceIndex,
            special_handling: SpecialHandling::ControlTextUpToAtGT,
            terminating_modes: LexModeSet::NOTHING,
            appliable_modes: LexModeSet::PASCAL_TEXT
                .const_or(LexModeSet::DEFINITION_TEXT)
                .const_or(LexModeSet::TEX_TEXT)
                .const_or(LexModeSet::INLINE_PASCAL_TEXT),
        },
        ControlCodeInfoRecord {
            selector: b".",
            kind: ControlCodeKind::ForceIndexMono,
            special_handling: SpecialHandling::ControlTextUpToAtGT,
            terminating_modes: LexModeSet::NOTHING,
            appliable_modes: LexModeSet::PASCAL_TEXT
                .const_or(LexModeSet::DEFINITION_TEXT)
                .const_or(LexModeSet::TEX_TEXT)
                .const_or(LexModeSet::INLINE_PASCAL_TEXT),
        },
        ControlCodeInfoRecord {
            selector: b":",
            kind: ControlCodeKind::ForceIndexStyle9,
            special_handling: SpecialHandling::ControlTextUpToAtGT,
            terminating_modes: LexModeSet::NOTHING,
            appliable_modes: LexModeSet::PASCAL_TEXT
                .const_or(LexModeSet::DEFINITION_TEXT)
                .const_or(LexModeSet::TEX_TEXT)
                .const_or(LexModeSet::INLINE_PASCAL_TEXT),
        },
        ControlCodeInfoRecord {
            selector: b"t",
            kind: ControlCodeKind::ForceHBox,
            special_handling: SpecialHandling::ControlTextUpToAtGT,
            terminating_modes: LexModeSet::NOTHING,
            appliable_modes: LexModeSet::PASCAL_TEXT
                .const_or(LexModeSet::DEFINITION_TEXT)
                .const_or(LexModeSet::INLINE_PASCAL_TEXT),
        },
        ControlCodeInfoRecord {
            selector: b"=",
            kind: ControlCodeKind::ForceVerbatim,
            special_handling: SpecialHandling::ControlTextUpToAtGT,
            terminating_modes: LexModeSet::NOTHING,
            appliable_modes: LexModeSet::PASCAL_TEXT
                .const_or(LexModeSet::DEFINITION_TEXT)
                .const_or(LexModeSet::INLINE_PASCAL_TEXT),
        },
        ControlCodeInfoRecord {
            selector: b"\\",
            kind: ControlCodeKind::ForceEOL,
            special_handling: SpecialHandling::None,
            terminating_modes: LexModeSet::NOTHING,
            appliable_modes: LexModeSet::PASCAL_TEXT
                .const_or(LexModeSet::DEFINITION_TEXT)
                .const_or(LexModeSet::INLINE_PASCAL_TEXT),
        },
        ControlCodeInfoRecord {
            selector: b"!",
            kind: ControlCodeKind::UnderlineFlag,
            special_handling: SpecialHandling::None,
            terminating_modes: LexModeSet::NOTHING,
            appliable_modes: LexModeSet::PASCAL_TEXT
                .const_or(LexModeSet::DEFINITION_TEXT)
                .const_or(LexModeSet::TEX_TEXT)
                .const_or(LexModeSet::INLINE_PASCAL_TEXT),
        },
        ControlCodeInfoRecord {
            selector: b"?",
            kind: ControlCodeKind::NoUnderlineFlag,
            special_handling: SpecialHandling::None,
            terminating_modes: LexModeSet::NOTHING,
            appliable_modes: LexModeSet::PASCAL_TEXT
                .const_or(LexModeSet::DEFINITION_TEXT)
                .const_or(LexModeSet::TEX_TEXT)
                .const_or(LexModeSet::INLINE_PASCAL_TEXT),
        },
        ControlCodeInfoRecord {
            selector: b",",
            kind: ControlCodeKind::FormatThinSpace,
            special_handling: SpecialHandling::None,
            terminating_modes: LexModeSet::NOTHING,
            appliable_modes: LexModeSet::PASCAL_TEXT
                .const_or(LexModeSet::DEFINITION_TEXT)
                .const_or(LexModeSet::INLINE_PASCAL_TEXT),
        },
        ControlCodeInfoRecord {
            selector: b"/",
            kind: ControlCodeKind::FormatLineBreak,
            special_handling: SpecialHandling::None,
            terminating_modes: LexModeSet::NOTHING,
            appliable_modes: LexModeSet::PASCAL_TEXT
                .const_or(LexModeSet::DEFINITION_TEXT)
                .const_or(LexModeSet::INLINE_PASCAL_TEXT),
        },
        ControlCodeInfoRecord {
            selector: b"|",
            kind: ControlCodeKind::FormatSuggestLineBreak,
            special_handling: SpecialHandling::None,
            terminating_modes: LexModeSet::NOTHING,
            appliable_modes: LexModeSet::PASCAL_TEXT
                .const_or(LexModeSet::DEFINITION_TEXT)
                .const_or(LexModeSet::INLINE_PASCAL_TEXT),
        },
        ControlCodeInfoRecord {
            selector: b"#",
            kind: ControlCodeKind::FormatLineBreakLarge,
            special_handling: SpecialHandling::None,
            terminating_modes: LexModeSet::NOTHING,
            appliable_modes: LexModeSet::PASCAL_TEXT
                .const_or(LexModeSet::DEFINITION_TEXT)
                .const_or(LexModeSet::INLINE_PASCAL_TEXT),
        },
        ControlCodeInfoRecord {
            selector: b"+",
            kind: ControlCodeKind::FormatNoLineBreak,
            special_handling: SpecialHandling::None,
            terminating_modes: LexModeSet::NOTHING,
            appliable_modes: LexModeSet::PASCAL_TEXT
                .const_or(LexModeSet::DEFINITION_TEXT)
                .const_or(LexModeSet::INLINE_PASCAL_TEXT),
        },
        ControlCodeInfoRecord {
            selector: b";",
            kind: ControlCodeKind::FormatInvisibleSemicolon,
            special_handling: SpecialHandling::None,
            terminating_modes: LexModeSet::NOTHING,
            appliable_modes: LexModeSet::PASCAL_TEXT
                .const_or(LexModeSet::DEFINITION_TEXT)
                .const_or(LexModeSet::INLINE_PASCAL_TEXT),
        },
        ControlCodeInfoRecord {
            selector: b"z",
            kind: ControlCodeKind::Ignored,
            special_handling: SpecialHandling::WarnAndIgnore,
            terminating_modes: LexModeSet::NOTHING,
            appliable_modes: LexModeSet::PASCAL_TEXT,
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
        Dereference,
        Equal,
        NotEqual,
        GreaterThan,
        LessThan,
        GreaterEq,
        LessEq,
        Multiply,
        Divide,
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
        LBracket,
        RBracket,
        RangeUntil,
        WithType,
        ArgumentSeparator,
        EndOfStatement,
        EndOfLastStatement,
        DefineAs,
        Dollar,
        Backslash, /*xetex.web:24446*/
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
            literal: b":=",
            kind: Punctuation::Op(Operator::Assign),
        },
        PunctuationInfo {
            literal: b"<>",
            kind: Punctuation::Op(Operator::NotEqual),
        },
        PunctuationInfo {
            literal: b"==",
            kind: Punctuation::DefineAs,
        },
        PunctuationInfo {
            literal: b">=",
            kind: Punctuation::Op(Operator::GreaterEq),
        },
        PunctuationInfo {
            literal: b"<=",
            kind: Punctuation::Op(Operator::LessEq),
        },
        PunctuationInfo {
            literal: b">",
            kind: Punctuation::Op(Operator::GreaterThan),
        },
        PunctuationInfo {
            literal: b"<",
            kind: Punctuation::Op(Operator::LessThan),
        },
        PunctuationInfo {
            literal: b":",
            kind: Punctuation::WithType,
        },
        PunctuationInfo {
            literal: b"^",
            kind: Punctuation::Op(Operator::Dereference),
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
            literal: b"[",
            kind: Punctuation::LBracket,
        },
        PunctuationInfo {
            literal: b"]",
            kind: Punctuation::RBracket,
        },
        PunctuationInfo {
            literal: b",",
            kind: Punctuation::ArgumentSeparator,
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
            literal: b"$",
            kind: Punctuation::Dollar,
        },
        PunctuationInfo {
            literal: b"=",
            kind: Punctuation::Op(Operator::Equal),
        },
        PunctuationInfo {
            literal: b"+",
            kind: Punctuation::Op(Operator::Plus),
        },
        PunctuationInfo {
            literal: b"-",
            kind: Punctuation::Op(Operator::Subtract),
        },
        PunctuationInfo {
            literal: b"*",
            kind: Punctuation::Op(Operator::Multiply),
        },
        PunctuationInfo {
            literal: b"/",
            kind: Punctuation::Op(Operator::Divide),
        },
        PunctuationInfo {
            literal: b"\\",
            kind: Punctuation::Backslash,
        },
    ];
}

pub mod literal {
    use super::ascii_str::AsciiStr;
    use super::token::BoxedTokenList;

    #[derive(Debug, PartialEq)]
    pub enum Literal<'x> {
        IntegerU32(u32),
        RealF64(f64),
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
    #[error("Unrecognized symbol starting with '{0}'")]
    UnrecognizedPunctuation(char),
    #[error("Control code used where it's not usable")]
    ControlCodeInNonApplicableMode,
    #[error("Control code character '{0}' used where it's not usable")]
    ControlCodeCharInNonApplicableMode(char),
    #[error("Integer literal overflow: {0} with radix {1}")]
    IntegerLiteralOverflow(String, u32),
    #[error("Float literal lex error: {0}")]
    FloatLiteralLexError(String),
    #[error("Numeric literal not properly finished")]
    NumericLiteralNotProperlyFinished,
    #[error("Control text not properly finished with @>")]
    ControlTextNotProperlyFinished,
    #[error("Group title not properly finished with .")]
    GroupTitleNotProperlyFinished,
    #[error("Inline program fragment not properly finished")]
    InlineProgFragmentNotProperlyFinished,
    #[error("Comment not properly finished with }}")]
    CommentNotProperlyFinished,
    #[error("Comment nesting too deep")]
    CommentNestingTooDeep,
    #[error("String literal not properly finished with \'")]
    StringLiteralNotProperlyFinished,
    #[error("String literal not properly finished with \"")]
    PreprocessedStringLiteralNotProperlyFinished,
    #[error("Any lex error!")]
    AnyLexError,
}

pub enum LexControlFlowNewItem {
    Module,
    Definition,
    ProgramText,
}

pub enum LexControlFlow<'x> {
    Continue(&'x [u8], usize),
    Finish(&'x [u8], usize),
    StartNew(LexControlFlowNewItem, LexMode, &'x [u8], usize),
    ModuleNameInlineProgAbort(&'x [u8], usize),
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
    use super::ascii_char::{is_hex_digit, is_inline_whitespace_char, is_octal_digit};
    use super::ascii_str::{self, AsciiStr};
    use super::control_code::ControlCode;
    use super::literal::Literal;
    use super::punctuation::Punctuation;
    use super::{LexControlFlow, LexControlFlowNewItem, LexError, LexMode};

    #[derive(Debug, PartialEq)]
    pub enum Token<'x> {
        CtrlCode(ControlCode<'x>),
        WS,
        MacroParamMark,
        Ident(&'x AsciiStr),
        Punct(Punctuation),
        Literal(Literal<'x>),
        Comment(BoxedTokenList<'x>),

        InlineProgramFragment(BoxedTokenList<'x>),
        TextFragment(&'x AsciiStr),

        ModuleNameInlineProgAbort,
    }

    pub type TokenList<'x> = Vec<Token<'x>>;
    pub type BoxedTokenList<'x> = Box<Vec<Token<'x>>>;

    fn continue_or_finish(l: &[u8], pos: usize) -> LexControlFlow<'_> {
        if l.is_empty() {
            LexControlFlow::Finish(l, pos)
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
            ControlCodeKind::DefineModule | ControlCodeKind::DefineStarredModule => {
                LexControlFlow::StartNew(LexControlFlowNewItem::Module, LexMode::TEX_TEXT, l, pos)
            }
            ControlCodeKind::DefineMacro | ControlCodeKind::DefineFormat => {
                LexControlFlow::StartNew(
                    LexControlFlowNewItem::Definition,
                    LexMode::DEFINITION_TEXT,
                    l,
                    pos,
                )
            }
            ControlCodeKind::DefineProgram | ControlCodeKind::ModuleName => {
                LexControlFlow::StartNew(
                    LexControlFlowNewItem::ProgramText,
                    LexMode::PASCAL_TEXT,
                    l,
                    pos,
                )
            }
            _ => unreachable! {},
        }
    }

    pub const CONTROL_CODE_PREFIX: u8 = b'@';
    pub const INLINE_PROGRAM_FRAGMENT: u8 = b'|';
    pub const ESCAPE_CHARACTER: u8 = b'\\';
    pub const START_OF_COMMENT: u8 = b'{';
    pub const END_OF_COMMENT: u8 = b'}';

    pub const LINE_FEED: u8 = b'\n';
    pub const CARRIAGE_RETURN: u8 = b'\r';

    pub const SIMPLE_ESCAPED_ATAIL: &'static [u8] = b"@@";
    pub const END_OF_CONTROL_TEXT: &'static [u8] = b"@>";

    pub const START_OF_MACRO_DEFINITION: &'static [u8] = b"@d";
    pub const START_OF_FORMAT_DEFINITION: &'static [u8] = b"@f";

    pub const MODULE_NAME_INLINE_PROGFRAG_ABORT: &'static [u8] = b"...@>";

    pub fn lex_u32_literal_with_radix(l: &[u8], radix: usize) -> Result<Literal, LexError> {
        use std::str::from_utf8;
        let str = from_utf8(l).unwrap();
        if let Ok(v) = u32::from_str_radix(str, radix as u32) {
            Ok(Literal::IntegerU32(v))
        } else {
            Err(LexError::IntegerLiteralOverflow(
                str.to_owned(),
                radix as u32,
            ))
        }
    }

    pub fn lex_f64_literal(l: &[u8]) -> Result<Literal, LexError> {
        use std::str::{from_utf8, FromStr};
        let str = from_utf8(l).unwrap();
        if let Ok(v) = f64::from_str(str) {
            Ok(Literal::RealF64(v))
        } else {
            Err(LexError::FloatLiteralLexError(str.to_owned()))
        }
    }

    pub fn lex_numeric_literal(l: &[u8]) -> Result<(Literal, usize, &[u8]), LexError> {
        use super::ascii_char::is_numeric_char;
        let count_int = l
            .iter()
            .copied()
            .take_while(|&ch| is_numeric_char(ch))
            .count();
        let has_dot = count_int > 0 && l[count_int..].starts_with(b".");
        let count_fraction = if has_dot {
            l[count_int + 1..]
                .iter()
                .copied()
                .take_while(|&ch| is_numeric_char(ch))
                .count()
        } else {
            0
        };
        if has_dot && count_fraction > 0 {
            let (numeric, rest) = l.split_at(count_int + 1 + count_fraction);
            let literal = lex_f64_literal(numeric)?;
            Ok((literal, numeric.len(), rest))
        } else if count_int > 0 {
            let (numeric, rest) = l.split_at(count_int);
            let literal = lex_u32_literal_with_radix(numeric, 10)?;
            Ok((literal, numeric.len(), rest))
        } else {
            Err(LexError::NumericLiteralNotProperlyFinished)
        }
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
            return Err(LexError::ControlCodeCharInNonApplicableMode(
                selector as char,
            ));
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
                let group_title_start = rest
                    .iter()
                    .take_while(|&&ch| is_inline_whitespace_char(ch))
                    .count();
                let group_title_end = memchr::memchr2(b'.', b'\n', rest).unwrap_or(rest.len());

                let control_text_end;
                if !rest[group_title_end..].starts_with(b".") {
                    eprintln!(
                        "WARN: module group title not finished with dot character, continuing."
                    );
                    control_text_end = group_title_end;
                //return Err(LexError::GroupTitleNotProperlyFinished);
                } else {
                    control_text_end = group_title_end + 1;
                }
                let group_title_text =
                    ascii_str::from_bytes(&rest[group_title_start..group_title_end])?;
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
                let mode = LexMode::MODULE_NAME;
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
                        LexControlFlow::Finish(..) => {
                            return Err(LexError::UnexpectedEOF);
                        }
                        LexControlFlow::StartNew(..) => {
                            return Err(LexError::ControlCodeInNonApplicableMode);
                        }
                        LexControlFlow::ModuleNameInlineProgAbort(..) => {
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
            SpecialHandling::FormatDefinition | SpecialHandling::MacroDefinition => {
                let mode = LexMode::DEFINITION_TEXT;
                let mut data = rest;
                let mut pos = pos + 1;
                let mut tokens = vec![];
                'definition_loop: loop {
                    if data.starts_with(START_OF_MACRO_DEFINITION)
                        || data.starts_with(START_OF_FORMAT_DEFINITION)
                    {
                        break 'definition_loop;
                    }
                    let (token, control_flow) = lex_token(data, mode, pos)?;
                    match control_flow {
                        LexControlFlow::Continue(rest_data, new_pos) => {
                            pos = new_pos;
                            data = rest_data;
                            tokens.push(token);
                        }
                        LexControlFlow::Finish(rest_data, new_pos) => {
                            pos = new_pos;
                            data = rest_data;
                            tokens.push(token);
                            break 'definition_loop;
                        }
                        LexControlFlow::StartNew(..) => {
                            break 'definition_loop;
                        }
                        LexControlFlow::ModuleNameInlineProgAbort(..) => {
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
                let literal = lex_u32_literal_with_radix(hex_digits, 16)?;
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
            SpecialHandling::WarnAndIgnore => {
                use super::control_code::ControlCodeKind;
                eprintln!(
                    "WARN: %{} occurred in the web file, ignoring.",
                    selector as char
                );
                let control_code = ControlCode {
                    kind: ControlCodeKind::Ignored,
                    param: None,
                };
                (control_code, rest, pos + 1)
            }
        };
        Ok((control_code, rest, pos, is_terminator))
    }

    pub fn lex_comment_rest<'x>(
        l: &'x [u8],
        pos: usize,
    ) -> Result<(Token<'x>, LexControlFlow<'x>), LexError> {
        let mode = LexMode::COMMENT;
        let mut data = l;
        let mut pos = pos;
        let mut tokens = vec![];

        let mut level = 1usize;
        'comment_loop: loop {
            if data.starts_with(b"\\") {
                if data.len() >= 2 {
                    let escaped_fragment = Token::TextFragment(ascii_str::from_bytes(&l[..2])?);
                    tokens.push(escaped_fragment);
                    data = &data[2..];
                    pos += 2;
                } else {
                    return Err(LexError::CommentNotProperlyFinished);
                }
            } else if data.starts_with(b"{") {
                let fragment = Token::TextFragment(ascii_str::from_bytes(&l[..1])?);
                tokens.push(fragment);
                level = level
                    .checked_add(1)
                    .ok_or(LexError::CommentNestingTooDeep)?;
                data = &data[1..];
                pos += 1;
            } else if data.starts_with(b"}") {
                level -= 1;
                if level != 0 {
                    let fragment = Token::TextFragment(ascii_str::from_bytes(&l[..1])?);
                    tokens.push(fragment);
                }
                data = &data[1..];
                pos += 1;
                if level == 0 {
                    break 'comment_loop;
                }
            } else {
                let (token, control_flow) = lex_token(data, mode, pos)?;
                match control_flow {
                    LexControlFlow::Continue(rest_data, new_pos) => {
                        pos = new_pos;
                        data = rest_data;
                        tokens.push(token);
                    }
                    LexControlFlow::Finish(..) => {
                        return Err(LexError::UnexpectedEOF);
                    }
                    LexControlFlow::StartNew(..) => {
                        return Err(LexError::ControlCodeInNonApplicableMode);
                    }
                    LexControlFlow::ModuleNameInlineProgAbort(..) => {
                        return Err(LexError::ControlCodeInNonApplicableMode);
                    }
                }
            }
        }
        let token = Token::Comment(Box::new(tokens));
        Ok((token, continue_or_finish(data, pos)))
    }

    fn lex_string_literal_rest<'x>(
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

    fn lex_preprocessed_string_literal_rest<'x>(
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

    fn lex_inline_prog_rest<'x>(
        l: &'x [u8],
        parent_mode: LexMode,
        pos: usize,
    ) -> Result<(Token<'x>, LexControlFlow<'x>), LexError> {
        let mode = LexMode::INLINE_PASCAL_TEXT;
        let mut data = l;
        let mut pos = pos;
        let mut tokens = vec![];
        'inline_prog_loop: loop {
            if data.starts_with(b"|") {
                data = &data[1..];
                pos = pos + 1;
                break 'inline_prog_loop;
            } else {
                let (token, control_flow) = lex_token(data, mode, pos)?;
                match control_flow {
                    LexControlFlow::Continue(rest_data, new_pos) => {
                        pos = new_pos;
                        data = rest_data;
                        tokens.push(token);
                    }
                    LexControlFlow::ModuleNameInlineProgAbort(rest_data, new_pos)
                        if parent_mode == LexMode::MODULE_NAME =>
                    {
                        pos = new_pos;
                        data = rest_data;
                        tokens.push(token);
                        break 'inline_prog_loop;
                    }
                    _ => {
                        return Err(LexError::InlineProgFragmentNotProperlyFinished);
                    }
                }
            }
        }
        let token = Token::InlineProgramFragment(Box::new(tokens));
        Ok((token, continue_or_finish(data, pos)))
    }

    pub fn lex_token<'x>(
        l: &'x [u8],
        mode: LexMode,
        pos: usize,
    ) -> Result<(Token<'x>, LexControlFlow<'x>), LexError> {
        let (l_is_empty, first_ch) = match l.get(0) {
            Some(&ch) => (false, ch),
            None => (true, 0),
        };
        match mode {
            LexMode::LIMBO | LexMode::TEX_TEXT if l_is_empty => {
                let empty = ascii_str::from_bytes(l)?;
                return Ok((Token::TextFragment(empty), LexControlFlow::Finish(l, pos)));
            }
            LexMode::DEFINITION_TEXT | LexMode::PASCAL_TEXT if l_is_empty => {
                return Ok((Token::WS, LexControlFlow::Finish(l, pos)));
            }
            _ if l_is_empty => {
                return Err(LexError::UnexpectedEOF);
            }
            LexMode::LIMBO
            | LexMode::TEX_TEXT
            | LexMode::PASCAL_TEXT
            | LexMode::INLINE_PASCAL_TEXT
            | LexMode::DEFINITION_TEXT
            | LexMode::COMMENT
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
            LexMode::MODULE_NAME if first_ch == CONTROL_CODE_PREFIX => {
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
            LexMode::LIMBO | LexMode::TEX_TEXT | LexMode::MODULE_NAME | LexMode::COMMENT
                if first_ch == INLINE_PROGRAM_FRAGMENT =>
            {
                let rest = &l[1..];
                return lex_inline_prog_rest(rest, mode, pos + 1);
            }
            LexMode::LIMBO | LexMode::TEX_TEXT | LexMode::MODULE_NAME | LexMode::COMMENT => {
                use memchr::{memchr, memchr2};
                debug_assert_ne!(first_ch, CONTROL_CODE_PREFIX);
                debug_assert_ne!(first_ch, INLINE_PROGRAM_FRAGMENT);
                let text_len = if mode == LexMode::LIMBO {
                    memchr(CONTROL_CODE_PREFIX, l)
                } else if mode != LexMode::COMMENT {
                    memchr2(CONTROL_CODE_PREFIX, INLINE_PROGRAM_FRAGMENT, l)
                } else {
                    let count = l
                        .iter()
                        .take_while(|&&ch| {
                            ch != CONTROL_CODE_PREFIX
                                && ch != INLINE_PROGRAM_FRAGMENT
                                && ch != ESCAPE_CHARACTER
                                && ch != START_OF_COMMENT
                                && ch != END_OF_COMMENT
                        })
                        .count();
                    Some(count)
                }
                .unwrap_or_else(|| l.len());
                let (text, rest) = l.split_at(text_len);
                let text = ascii_str::from_bytes(text)?;
                return Ok((
                    Token::TextFragment(text),
                    continue_or_finish(rest, pos + text_len),
                ));
            }
            LexMode::PASCAL_TEXT | LexMode::DEFINITION_TEXT | LexMode::INLINE_PASCAL_TEXT => {
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
                } else if first_ch == b'#' {
                    let rest = &l[1..];
                    return Ok((Token::MacroParamMark, continue_or_finish(rest, pos + 1)));
                } else if mode == LexMode::INLINE_PASCAL_TEXT
                    && first_ch == b'.'
                    && l.starts_with(MODULE_NAME_INLINE_PROGFRAG_ABORT)
                {
                    return Ok((
                        Token::ModuleNameInlineProgAbort,
                        LexControlFlow::ModuleNameInlineProgAbort(l, pos),
                    ));
                } else if ascii_char::is_punct_char(first_ch) {
                    let (punct, punct_len, rest) = program_text::parse_punct(l)
                        .ok_or_else(|| LexError::UnrecognizedPunctuation(first_ch as char))?;
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

pub struct LexerRawBuf<'x> {
    mode: LexMode,
    data: &'x [u8],
    pos: usize,
}

#[derive(Default)]
pub struct LexerLimboBuf<'x> {
    pub(crate) limbo_tokens: token::TokenList<'x>,
}

pub struct LexerModuleBuf<'x> {
    pub(crate) module_type: token::Token<'x>,
    pub(crate) text_in_tex: token::TokenList<'x>,
    pub(crate) definitions: token::TokenList<'x>,
    pub(crate) code_in_pascal: token::TokenList<'x>,
}

#[derive(Clone, Copy)]
enum LexerInternalState {
    LimboDirty,
    LimboFilledModuleDirty,
    LimboFilledEOF,
    LimboTakenModuleDirty,
    ModuleFilledNextModuleDirty,
    ModuleFilledEOF,
    EOF,
}

pub struct WEBLexer<'x> {
    raw_buf: LexerRawBuf<'x>,
    state: LexerInternalState,
    limbo_buf: Option<LexerLimboBuf<'x>>,
    module_buf: Option<LexerModuleBuf<'x>>,
    next_module_buf: Option<LexerModuleBuf<'x>>,
}

impl<'x> WEBLexer<'x> {
    pub fn new(data: &'x [u8]) -> Self {
        let raw_buf = LexerRawBuf {
            mode: LexMode::LIMBO,
            data,
            pos: 0,
        };
        let limbo_buf = Some(Default::default());
        let state = LexerInternalState::LimboDirty;
        let module_buf = None;
        let next_module_buf = None;
        WEBLexer {
            raw_buf,
            state,
            limbo_buf,
            module_buf,
            next_module_buf,
        }
    }

    fn refill(&mut self) -> Result<(), LexError> {
        let mut output_module;
        match self.state {
            LexerInternalState::LimboDirty => {
                output_module = None;
            }
            LexerInternalState::LimboTakenModuleDirty
            | LexerInternalState::LimboFilledModuleDirty => {
                output_module = Some(self.module_buf.as_mut().unwrap());
            }
            LexerInternalState::ModuleFilledNextModuleDirty => {
                output_module = Some(self.next_module_buf.as_mut().unwrap());
            }
            LexerInternalState::LimboFilledEOF
            | LexerInternalState::ModuleFilledEOF
            | LexerInternalState::EOF => {
                return Ok(());
            }
        }
        let mut pending_token = None;
        'outer: loop {
            let output_tokenlist;
            if let Some(module) = &mut output_module {
                output_tokenlist = match self.raw_buf.mode {
                    LexMode::TEX_TEXT => &mut module.text_in_tex,
                    LexMode::DEFINITION_TEXT => &mut module.definitions,
                    LexMode::PASCAL_TEXT => &mut module.code_in_pascal,
                    _ => unreachable!(),
                };
            } else {
                assert!(self.raw_buf.mode == LexMode::LIMBO);
                output_tokenlist = &mut self.limbo_buf.as_mut().unwrap().limbo_tokens;
            }

            if let Some(token) = pending_token.take() {
                output_tokenlist.push(token);
            }

            'inner: loop {
                let (token, control_flow) =
                    token::lex_token(self.raw_buf.data, self.raw_buf.mode, self.raw_buf.pos)?;
                match control_flow {
                    LexControlFlow::Continue(rest_data, new_pos) => {
                        output_tokenlist.push(token);
                        self.raw_buf.pos = new_pos;
                        self.raw_buf.data = rest_data;
                        continue 'inner;
                    }
                    LexControlFlow::Finish(rest_data, new_pos) => {
                        output_tokenlist.push(token);
                        self.raw_buf.pos = new_pos;
                        self.raw_buf.data = rest_data;
                        self.state = match self.state {
                            LexerInternalState::LimboDirty => LexerInternalState::LimboFilledEOF,
                            LexerInternalState::LimboTakenModuleDirty => {
                                LexerInternalState::ModuleFilledEOF
                            }
                            LexerInternalState::LimboFilledModuleDirty
                            | LexerInternalState::ModuleFilledNextModuleDirty
                            | LexerInternalState::LimboFilledEOF
                            | LexerInternalState::ModuleFilledEOF
                            | LexerInternalState::EOF => unreachable!(),
                        };
                        break 'outer;
                    }
                    LexControlFlow::StartNew(
                        LexControlFlowNewItem::Module,
                        new_mode,
                        rest_data,
                        new_pos,
                    ) => {
                        self.raw_buf.mode = new_mode;
                        self.raw_buf.pos = new_pos;
                        self.raw_buf.data = rest_data;
                        let new_module = LexerModuleBuf {
                            module_type: token,
                            text_in_tex: Default::default(),
                            definitions: Default::default(),
                            code_in_pascal: Default::default(),
                        };
                        self.state = match self.state {
                            LexerInternalState::LimboDirty => {
                                assert!(self.module_buf.is_none());
                                self.module_buf = Some(new_module);
                                LexerInternalState::LimboFilledModuleDirty
                            }
                            LexerInternalState::LimboTakenModuleDirty => {
                                assert!(self.next_module_buf.is_none());
                                self.next_module_buf = Some(new_module);
                                LexerInternalState::ModuleFilledNextModuleDirty
                            }
                            LexerInternalState::LimboFilledModuleDirty
                            | LexerInternalState::ModuleFilledNextModuleDirty
                            | LexerInternalState::LimboFilledEOF
                            | LexerInternalState::ModuleFilledEOF
                            | LexerInternalState::EOF => unreachable!(),
                        };
                        break 'outer;
                    }
                    LexControlFlow::StartNew(
                        LexControlFlowNewItem::Definition,
                        new_mode,
                        rest_data,
                        new_pos,
                    )
                    | LexControlFlow::StartNew(
                        LexControlFlowNewItem::ProgramText,
                        new_mode,
                        rest_data,
                        new_pos,
                    ) => {
                        assert!(pending_token.is_none());
                        pending_token = Some(token);
                        self.raw_buf.mode = new_mode;
                        self.raw_buf.pos = new_pos;
                        self.raw_buf.data = rest_data;
                        continue 'outer;
                    }
                    LexControlFlow::ModuleNameInlineProgAbort(..) => {
                        unreachable!();
                    }
                }
            }
        }
        Ok(())
    }

    pub fn lex_limbo(&mut self) -> Result<Option<LexerLimboBuf<'x>>, LexError> {
        self.refill()?;
        let result;
        self.state = match self.state {
            LexerInternalState::LimboDirty | LexerInternalState::LimboTakenModuleDirty => unreachable!(),
            LexerInternalState::LimboFilledModuleDirty => {
                result = self.limbo_buf.take();
                LexerInternalState::LimboTakenModuleDirty
            }
            LexerInternalState::LimboFilledEOF => {
                result = self.limbo_buf.take();
                LexerInternalState::EOF
            }
            LexerInternalState::ModuleFilledNextModuleDirty
            | LexerInternalState::ModuleFilledEOF
            | LexerInternalState::EOF => {
                result = None;
                self.state
            }
        };
        Ok(result)
    }

    pub fn lex_module(&mut self) -> Result<Option<LexerModuleBuf<'x>>, LexError> {
        self.refill()?;
        let result;
        self.state = match self.state {
            LexerInternalState::LimboDirty | LexerInternalState::LimboTakenModuleDirty => unreachable!(),
            LexerInternalState::LimboFilledModuleDirty | LexerInternalState::LimboFilledEOF => {
                // must be called in the wrong order.
                unreachable!();
            }
            LexerInternalState::ModuleFilledNextModuleDirty => {
                use std::mem::swap;
                result = self.module_buf.take();
                swap(&mut self.module_buf, &mut self.next_module_buf);
                LexerInternalState::LimboTakenModuleDirty
            }
            LexerInternalState::ModuleFilledEOF => {
                result = self.module_buf.take();
                LexerInternalState::EOF
            }
            LexerInternalState::EOF => {
                result = None;
                self.state
            }
        };
        Ok(result)
    }
}
