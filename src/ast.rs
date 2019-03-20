use std::fmt::{self, Debug, Display};

pub enum Expr {
    Num(f64),
    Bool(bool),
    Ident(String),
    Str(String),
    Seq(Vec<Box<Expr>>),
    UnOp(Opcode, Box<Expr>),
    BinOp(Box<Expr>, Opcode, Box<Expr>),
    Tern(Box<Expr>, Box<Expr>, Box<Expr>),
    New(String, Vec<Box<Expr>>),
    Access(Box<Expr>, Box<Expr>),
    Call(Box<Expr>, Vec<Box<Expr>>),
    Decl(Declaration, Box<Expr>),
    Block(Vec<Box<Expr>>),
    Empty,
    If {
        predicate: Box<Expr>,
        consequent: Box<Expr>,
        alternative: Box<Expr>,
    },
}

pub enum Opcode {
    Yield(bool),
    Asn(Assign),
    Or,
    And,
    BitOr,
    BitXor,
    BitAnd,
    Eq { strict: bool, negated: bool },
    Lt,
    Gt,
    Lte,
    Gte,
    In,
    InstanceOf,
    Shl,
    Shr,
    Ushr,
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    Exp,
    Not,
    BitNot,
    Pos,
    Neg,
    Inc { post: bool },
    Dec { post: bool },
    TypeOf,
    Void,
    Delete,
    Await,
}

pub enum Assign {
    Basic,
    Add,
    Sub,
    Exp,
    Mul,
    Div,
    Rem,
    Shl,
    Shr,
    Ushr,
    And,
    Xor,
    Or,
}

pub enum Declaration {
    Var,
    Let,
    Const,
}

impl Debug for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expr::Num(n) => write!(f, "Num({})", n),
            Expr::Bool(b) => write!(f, "Bool({})", b),
            Expr::Ident(s) => write!(f, "Ident({})", s),
            Expr::Str(s) => write!(f, "Str({})", s),
            Expr::Seq(vals) => write!(f, "Seq{:?}", vals),
            Expr::UnOp(o, e) => write!(f, "UnOp({} {:?})", o, *e),
            Expr::BinOp(e0, o, e1) => write!(f, "BinOp({:?} {} {:?})", *e0, o, *e1),
            Expr::Tern(p, c, a) => write!(f, "Ternary({:?} ? {:?} : {:?})", *p, *c, *a),
            Expr::New(name, args) if args.is_empty() => write!(f, "New[ {} ]", name),
            Expr::New(name, args) => write!(f, "New[ {}({:?}) ]", name, args),
            Expr::Access(obj, key) => write!(f, "Access( {:?}[{:?}] )", *obj, *key),
            Expr::Call(func, args) => write!(f, "Call[ {:?}({:?}) ]", *func, *args),
            Expr::Decl(kind, decl) => write!(f, "Declare[{}, {:?}]", kind, *decl),
            Expr::Block(vals) => write!(f, "Block{{{:?}}}", vals),
            Expr::Empty => write!(f, "Empty"),
            Expr::If {
                predicate,
                consequent,
                alternative,
            } => write!(
                f,
                "If({:?}){{{:?}}}Else{{{:?}}}",
                *predicate, *consequent, *alternative
            ),
        }
    }
}

impl Display for Opcode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Opcode::Yield(false) => write!(f, "yield"),
            Opcode::Yield(true) => write!(f, "yield*"),
            Opcode::Asn(a) => write!(f, "{}", a),
            Opcode::Or => write!(f, "||"),
            Opcode::And => write!(f, "&&"),
            Opcode::BitOr => write!(f, "|"),
            Opcode::BitXor => write!(f, "^"),
            Opcode::BitAnd => write!(f, "&"),
            Opcode::Eq {
                strict: true,
                negated: true,
            } => write!(f, "!=="),
            Opcode::Eq { strict: true, .. } => write!(f, "==="),
            Opcode::Eq { negated: true, .. } => write!(f, "!="),
            Opcode::Eq { .. } => write!(f, "=="),
            Opcode::Lt => write!(f, "<"),
            Opcode::Gt => write!(f, ">"),
            Opcode::Lte => write!(f, "<="),
            Opcode::Gte => write!(f, ">="),
            Opcode::In => write!(f, "in"),
            Opcode::InstanceOf => write!(f, "instanceof"),
            Opcode::Shl => write!(f, "<<"),
            Opcode::Shr => write!(f, ">>"),
            Opcode::Ushr => write!(f, ">>>"),
            Opcode::Add => write!(f, "+"),
            Opcode::Sub => write!(f, "-"),
            Opcode::Mul => write!(f, "*"),
            Opcode::Div => write!(f, "/"),
            Opcode::Rem => write!(f, "%"),
            Opcode::Exp => write!(f, "**"),
            Opcode::Not => write!(f, "!"),
            Opcode::BitNot => write!(f, "~"),
            Opcode::Pos => write!(f, "+"),
            Opcode::Neg => write!(f, "-"),
            Opcode::Inc { .. } => write!(f, "++"),
            Opcode::Dec { .. } => write!(f, "--"),
            Opcode::TypeOf => write!(f, "typeof"),
            Opcode::Void => write!(f, "void"),
            Opcode::Delete => write!(f, "delete"),
            Opcode::Await => write!(f, "await"),
        }
    }
}

impl Debug for Opcode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self)
    }
}

impl Display for Assign {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let prefix = match self {
            Assign::Basic => "",
            Assign::Add => "+",
            Assign::Sub => "-",
            Assign::Exp => "**",
            Assign::Mul => "*",
            Assign::Div => "/",
            Assign::Rem => "%",
            Assign::Shl => "<<",
            Assign::Shr => ">>",
            Assign::Ushr => ">>>",
            Assign::And => "&",
            Assign::Xor => "^",
            Assign::Or => "|",
        };

        write!(f, "{}=", prefix)
    }
}

impl Debug for Assign {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self)
    }
}

impl Display for Declaration {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let name = match self {
            Declaration::Var => "var",
            Declaration::Let => "let",
            Declaration::Const => "const",
        };

        write!(f, "{}", name)
    }
}

impl Debug for Declaration {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self)
    }
}
