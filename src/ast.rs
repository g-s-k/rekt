use std::fmt::{self, Debug, Display};

type Script = Vec<Box<Statement>>;
type Args = Vec<Box<Expr>>;

pub enum Statement {
    Empty,
    Plain(Box<Expr>),
    Return(Option<Box<Expr>>),
    Break(Option<String>),
    Continue(Option<String>),
    Throw(Box<Expr>),
    Decl {
        kind: Declaration,
        lhs: String,
        rhs: Option<Box<Expr>>,
    },
    Block(Vec<Box<Statement>>),
    If {
        pred: Box<Expr>,
        consq: Box<Statement>,
        alt: Option<Box<Statement>>,
    },
    TryCatch {
        try_body: Script,
        catch: Option<(String, Script)>,
        finally_body: Option<Script>,
    },
    Switch {
        value: Box<Expr>,
        body: Vec<(Case, Script)>,
    },
    For {
        init: Box<Statement>,
        term: Box<Expr>,
        incr: Box<Expr>,
        body: Box<Statement>,
    },
    ForIn {
        decl: Declaration,
        var: String,
        obj: Box<Expr>,
        body: Box<Statement>,
    },
    ForOf {
        decl: Declaration,
        var: String,
        obj: Box<Expr>,
        body: Box<Statement>,
    },
    While {
        cond: Box<Expr>,
        body: Box<Statement>,
    },
    DoWhile {
        cond: Box<Expr>,
        body: Box<Statement>,
    },
    Label(String, Box<Statement>),
}

pub enum Expr {
    Num(f64),
    Bool(bool),
    Ident(String),
    Str(String),
    Seq(Vec<Box<Expr>>),
    UnOp(Opcode, Box<Expr>),
    BinOp(Box<Expr>, Opcode, Box<Expr>),
    Tern(Box<Expr>, Box<Expr>, Box<Expr>),
    New(String, Args),
    Access(Box<Expr>, Box<Expr>),
    Call(Box<Expr>, Args),
    Defun {
        name: Option<String>,
        params: Vec<String>,
        body: Script,
        generator: bool,
    },
    Array(Vec<Box<Expr>>),
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

pub enum Case {
    Case(Box<Expr>),
    Default,
}

impl Debug for Statement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Statement::Empty => write!(f, "Empty"),
            Statement::Plain(e) => write!(f, "{:?}", *e),
            Statement::Decl {
                kind,
                lhs,
                rhs: Some(r),
            } => write!(f, "Declare[{} {} = {:?}]", kind, lhs, *r),
            Statement::Decl { kind, lhs, .. } => write!(f, "Declare[{} {}]", kind, lhs),
            Statement::Block(vals) => write!(f, "Block{{{:?}}}", vals),
            Statement::If {
                pred,
                consq,
                alt: Some(alt),
            } => write!(f, "If({:?}){{{:?}}}Else{{{:?}}}", *pred, *consq, *alt),
            Statement::If { pred, consq, .. } => write!(f, "If({:?}){{{:?}}}", *pred, *consq),
            Statement::Return(Some(e)) => write!(f, "Return({:?})", *e),
            Statement::Return(_) => write!(f, "Return"),
            Statement::Break(Some(l)) => write!(f, "Break({})", l),
            Statement::Break(_) => write!(f, "Break"),
            Statement::Continue(Some(l)) => write!(f, "Continue({})", l),
            Statement::Continue(_) => write!(f, "Continue"),
            Statement::Throw(e) => write!(f, "Throw({:?})", *e),
            Statement::TryCatch {
                try_body,
                catch: Some((e, c)),
                finally_body: Some(fi),
            } => write!(
                f,
                "Try{{{:?}}}Catch({}){{{:?}}}Finally{{{:?}}}",
                *try_body, e, *c, *fi
            ),
            Statement::TryCatch {
                try_body,
                catch: Some((e, c)),
                ..
            } => write!(f, "Try{{{:?}}}Catch({}){{{:?}}}", *try_body, e, *c),
            Statement::TryCatch {
                try_body,
                finally_body: Some(fi),
                ..
            } => write!(f, "Try{{{:?}}}Finally{{{:?}}}", *try_body, *fi),
            Statement::TryCatch { .. } => unreachable!(),
            Statement::Switch { value, body } => write!(f, "Switch({:?}){{{:?}}}", *value, body),
            Statement::For {
                init,
                term,
                incr,
                body,
            } => write!(f, "For({:?};{:?};{:?}){{{:?}}}", *init, *term, *incr, *body),
            Statement::ForIn { var, obj, body, .. } => {
                write!(f, "For[{}]In[{:?}]{{{:?}}}", var, *obj, body)
            }
            Statement::ForOf { var, obj, body, .. } => {
                write!(f, "For[{}]Of[{:?}]{{{:?}}}", var, *obj, body)
            }
            Statement::While { cond, body } => write!(f, "While({:?}){{{:?}}}", *cond, *body),
            Statement::DoWhile { cond, body } => write!(f, "Do{{{:?}}}While({:?})", *body, *cond),
            Statement::Label(l, e) => write!(f, "Label[{}]{{{:?}}}", l, *e),
        }
    }
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
            Expr::Defun {
                name: Some(name),
                params,
                body,
                generator,
            } => write!(
                f,
                "Function{}[{}]({:?}){{{:?}}}",
                if *generator { "*" } else { "" },
                name,
                params,
                body
            ),
            Expr::Defun {
                params,
                body,
                generator,
                ..
            } => write!(
                f,
                "Function{}({:?}){{{:?}}}",
                if *generator { "*" } else { "" },
                params,
                body
            ),
            Expr::Array(v) => write!(f, "Array[{:?}]", v),
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

impl Debug for Case {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Case::Case(e) => write!(f, "Case[{:?}]", *e),
            Case::Default => write!(f, "Default"),
        }
    }
}
