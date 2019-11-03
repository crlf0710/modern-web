use super::lexer::token::{BoxedTokenList, Token};
use easy_ext::ext;
use once_cell::sync::Lazy;
use std::collections::BTreeMap;
use std::rc::Rc;

pub struct GrammarDesc {
    production: FragPatternId,
    segment: GrammarSeg,
}

enum GrammarSeg {
    Fixed(Token<'static>),
    Pat(FragPatternId, FragComponentRole),

    Seq(Vec<GrammarSeg>),
    Any(Vec<GrammarSeg>),
    Opt(Box<GrammarSeg>),
    Rep(Box<GrammarSeg>),
}

#[repr(usize)]
enum NamedFragPatternId {
    Program,
    ProgramHeading,
    ProgramBlock,
    ProgramParameterList,

    Block,

    LabelDeclarationPart,
    ConstantDeclarationPart,
    TypeDeclarationPart,
    ProcedureAndFunctionDeclarationPart,
    StatementPart,

    CompoundStatement,
    StatementSequence,
    Statement,

    Label,

    SimpleStatement,
    StructuredStatement,

    ConditionalStatement,
    RepetitiveStatement,
    WithStatement,

    IfStatement,
    CaseStatement,

    Identifier,
}

#[ext(GrammarRegistryExt)]
impl BTreeMap<FragPatternId, GrammarDesc> {
    fn register_grammar(mut self, id: NamedFragPatternId, segment: GrammarSeg) -> Self {
        use std::collections::btree_map::Entry;
        let id = id.into();
        let entry = self.entry(id);
        match entry {
            Entry::Occupied(..) => {
                eprintln!("WARN: Duplicating pattern id found!");
                entry.and_modify(|old| old.segment = segment);
            }
            _ => {
                entry.or_insert(GrammarDesc {
                    production: id,
                    segment,
                });
            }
        }
        self
    }
}

pub static GRAMMARS: Lazy<BTreeMap<FragPatternId, GrammarDesc>> = Lazy::new(|| {
    use super::lexer::ascii_str;
    use super::lexer::punctuation::Punctuation;
    use GrammarSeg as Seg;
    use NamedFragComponentRole as Role;
    use NamedFragPatternId as Id;
    macro_rules! seq {
        ($($e:expr),*) => {seq!($($e,)*)};
        ($($e:expr,)*) => {Seg::Seq(vec![$($e,)*])};
    }
    macro_rules! any {
        ($($e:expr),*) => {any!($($e,)*)};
        ($($e:expr,)*) => {Seg::Any(vec![$($e,)*])};
    }
    macro_rules! opt_seq {
        ($($e:expr),*) => {opt_seq!($($e,)*)};
        ($($e:expr,)*) => {Seg::Opt(Box::new(Seg::Seq(vec![$($e,)*])))};
    }
    macro_rules! opt_rep_seq {
        ($($e:expr),*) => {opt_rep_seq!($($e,)*)};
        ($($e:expr,)*) => {Seg::Opt(Box::new(Seg::Rep(Box::new(Seg::Seq(vec![$($e,)*])))))};
    }
    macro_rules! pat {
        ($id:expr, $role:expr) => {
            Seg::Pat($id.into(), $role.into())
        };
    }
    macro_rules! kw {
        ($v:expr) => {
            ascii_str::from_bytes($v).unwrap()
        };
    }

    const Tok_Semicolon: Token<'_> = Token::Punct(Punctuation::EndOfStatement);
    const Tok_Dot: Token<'_> = Token::Punct(Punctuation::DotOrEndOfProgram);
    const Tok_LParen: Token<'_> = Token::Punct(Punctuation::LParen);
    const Tok_RParen: Token<'_> = Token::Punct(Punctuation::RParen);
    let Tok_Kw_Program: Token<'_> = Token::IdentOrKw(kw!(b"program"));
    let Tok_Kw_Begin: Token<'_> = Token::IdentOrKw(kw!(b"begin"));
    let Tok_Kw_End: Token<'_> = Token::IdentOrKw(kw!(b"end"));

    BTreeMap::new()
        .register_grammar(
            Id::Program,
            // program = program-heading `;' program-block `.' .
            seq![
                pat!(Id::ProgramHeading, Role::Heading),
                Seg::Fixed(Tok_Semicolon),
                pat!(Id::ProgramBlock, Role::Inner),
                Seg::Fixed(Tok_Dot),
            ],
        )
        .register_grammar(
            Id::ProgramHeading,
            // program-heading = `program' identifier [ `(' program-parameter-list `)' ] .
            seq![
                Seg::Fixed(Tok_Kw_Program),
                pat!(Id::Identifier, Role::Name),
                Seg::Fixed(Tok_LParen),
                pat!(Id::ProgramParameterList, Role::Arguments),
                Seg::Fixed(Tok_RParen),
            ],
        )
        .register_grammar(
            Id::ProgramBlock,
            // program-block = block .
            pat!(Id::Block, Role::Inner),
        )
        .register_grammar(
            Id::Block,
            // block = label-declaration-part constant-definition-part type-definition-part variable-declaration-part
            // procedure-and-function-declaration-part statement-part .
            seq![
                pat!(Id::LabelDeclarationPart, Role::Definitions),
                pat!(Id::ConstantDeclarationPart, Role::Definitions),
                pat!(Id::TypeDeclarationPart, Role::Definitions),
                pat!(Id::ProcedureAndFunctionDeclarationPart, Role::Definitions),
                pat!(Id::StatementPart, Role::Inner),
            ],
        )
        .register_grammar(
            Id::StatementPart,
            // statement-part = compound-statement .
            pat!(Id::CompoundStatement, Role::Inner),
        )
        .register_grammar(
            Id::CompoundStatement,
            // compound-statement = `begin' statement-sequence `end' .
            seq![
                Seg::Fixed(Tok_Kw_Begin),
                pat!(Id::StatementSequence, Role::Inner),
                Seg::Fixed(Tok_Kw_End),
            ],
        )
        .register_grammar(
            Id::StatementSequence,
            // statement-sequence = statement { `;' statement } .
            seq![
                pat!(Id::Statement, Role::Item),
                opt_rep_seq![Seg::Fixed(Tok_Semicolon), pat!(Id::Statement, Role::Item),],
            ],
        )
        .register_grammar(
            Id::Statement,
            // statement = [ label ` :' ] ( simple-statement | structured-statement ) .
            seq![
                opt_seq![pat!(Id::Label, Role::Label),],
                any![
                    pat!(Id::SimpleStatement, Role::Inner),
                    pat!(Id::StructuredStatement, Role::Inner),
                ]
            ],
        )
        .register_grammar(
            Id::StructuredStatement,
            // structured-statement = compound-statement | conditional-statement
            // | repetitive-statement | with-statement .
            any![
                pat!(Id::CompoundStatement, Role::Inner),
                pat!(Id::ConditionalStatement, Role::Inner),
                pat!(Id::RepetitiveStatement, Role::Inner),
                pat!(Id::WithStatement, Role::Inner),
            ],
        )
        .register_grammar(
            Id::ConditionalStatement,
            // conditional-statement = if-statement | case-statement .
            any![
                pat!(Id::IfStatement, Role::Inner),
                pat!(Id::CaseStatement, Role::Inner),
            ],
        )
});

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct FragPatternId(usize);

impl From<NamedFragPatternId> for FragPatternId {
    fn from(v: NamedFragPatternId) -> Self {
        FragPatternId(v as usize)
    }
}

/*
impl FragPatternId {
    /// program = program-heading `;' program-block `.' .
    pub const PROGRAM: FragPatternId = FragPatternId(0);

    /// program-heading = `program' identifier [ `(' program-parameter-list `)' ] .
    pub const PROGRAM_HEADING: FragPatternId = FragPatternId(1);

    // program-block = block .
    pub const PROGRAM_BLOCK: FragPatternId = FragPatternId(2);

    /// program-parameter-list = identifier-list .
    pub const PROGRAM_PARAMETER_LIST: FragPatternId = FragPatternId(3);

    /// identifier = letter { letter | digit } .
    pub const IDENTIFIER: FragPatternId = FragPatternId(256);

    /// identifier-list = identifier { `,' identifier } .
    pub const IDENTIFIER_LIST: FragPatternId = FragPatternId(257);

    pub const PROGRAM_HEADING__TYPE_DEFINITION_PART: FragPatternId = FragPatternId(1);
    pub const STATEMENT_PART__DOT: FragPatternId = FragPatternId(2);

    pub const TYPE_DEFINITION_PART: FragPatternId = FragPatternId(17);
    pub const STATEMENT_PART: FragPatternId = FragPatternId(18);

    pub const STATEMENT_SEQUENCE: FragPatternId = FragPatternId(32);

    pub const DOT: FragPatternId = FragPatternId(256);
}
*/

enum Fragment<'x> {
    Primitive {
        id: FragPatternId,
        leading: Option<BoxedTokenList<'x>>,
        tokens: BoxedTokenList<'x>,
        trailing: Option<BoxedTokenList<'x>>,
    },
    Compound {
        id: FragPatternId,
        components: Vec<FragComponent<'x>>,
    },
}

pub struct FragComponentRole(usize);

enum NamedFragComponentRole {
    LeadingDesc,
    GapDesc,
    TrailingDesc,

    Inner,
    Heading,
    Definitions,
    Name,
    Arguments,
    Label,
    Item,
}

impl From<NamedFragComponentRole> for FragComponentRole {
    fn from(v: NamedFragComponentRole) -> Self {
        FragComponentRole(v as usize)
    }
}

enum FragComponent<'x> {
    Inline {
        role: FragComponentRole,
        inner: Rc<Fragment<'x>>,
    },
    Remote {
        role: FragComponentRole,
        code_module_id: usize,
        frag_pattern_id: FragPatternId,
    },
}

mod fragpattern_procedure;
