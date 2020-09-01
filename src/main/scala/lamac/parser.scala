package lamac

import fastparse.JavaWhitespace._
import fastparse._

// Étant donné que fastparse simplifie la chose, on transforme directement le texte en AST
// La syntaxe [_: P] est due aux implicites utilisées par fastparse
object parser {

  import ast.Expression._
  import ast.Statement._
  import ast._

  def parseProgram(input: String): Parsed[AST] = fastparse.parse(input, program(_))

  def program[_: P]: P[AST] = P(newLine.rep ~ line.rep(min = 1, sep = newLine.rep) ~ newLine.rep)

  def line[_: P]: P[Line] = P(statement | (expression ~ ";"))

  def statement[_: P]: P[Statement] =
    P(ret | ifStmt | whileStmt | forStmt | funDef | (declaration ~ ";") | (assignment ~ ";"))

  def declaration[_: P]: P[Declaration] = P((nameSegment ~ ":" ~ name) map {
    case (name, ty) => Declaration(name, ty)
  })

  def assignment[_: P]: P[Assignment] = P((name ~ "=" ~ expression) map {
    case (name, expr) => Assignment(name, expr)
  })

  def block[_: P]: P[Block] = P(("{" ~ program ~ "}") map Block)

  def newLine[_: P]: P[Unit] = "\n" | "\r\n"

  def ifStmt[_: P]: P[IfStmt] = P(("if" ~ expression ~ block ~ ("else" ~ block).?) map {
    case (expr, ifBlock, elseBlock) => IfStmt(expr, ifBlock, elseBlock)
  })

  def whileStmt[_: P]: P[WhileStmt] = P(("while" ~ expression ~ block) map {
    case (expr, block) => WhileStmt(expr, block)
  })

  def forStmt[_: P]: P[ForStmt] =
    P(("for" ~ assignment.? ~ ";" ~ expression.? ~ ";" ~ assignment.? ~ block) map {
      case (init, cond, end, block) => ForStmt(init, cond, end, block)
    })

  def funCall[_: P]: P[FunCall] = P((name ~ "(" ~ expression.rep(sep = ",") ~ ")") map {
    case (identifier, args) => FunCall(identifier, args)
  })

  def funDef[_: P]: P[Statement] = P(("fn" ~ name ~ "(" ~ varDecl.rep(sep = ",") ~ ")" ~ (":" ~ name).? ~ block) map {
    case (identifier, args, retTy, code) => FunDef(identifier, args, retTy, code)
  })

  def ret[_: P]: P[Return] = P(("return" ~ expression ~ ";") map Return)

  def varDecl[_: P]: P[Declaration] = P((nameSegment ~ ":" ~ name) map {
    case (name, ty) => Declaration(name, ty)
  })

  def name[_: P]: P[Name] = P(nameSegment.rep(min = 1, sep = ".") map Name)

  def nameSegment[_: P]: P[NameSegment] = P((CharIn("_a-zA-Z") ~ CharIn("_a-zA-Z0-9").rep).! map NameSegment)

  def literal[_: P]: P[Expression] = P(intLit | strLit | boolLit)

  def intLit[_: P]: P[IntConstant] = P(("-".!.? ~ CharIn("0-9") ~ CharIn("0-9").rep).! map {
    str => IntConstant(str.toInt)
  })

  def strLit[_: P]: P[StrConstant] = P(("\"" ~~ CharsWhile(_ != '"', 0).! ~~ "\"") map StrConstant)

  def boolLit[_: P]: P[BoolConstant] = P(("true" | "false").! ~ (!nameSegment) map { x=>
    println(x)
    x match {
    case "true" => BoolConstant(true)
    case "false" => BoolConstant(false)
    }
  })

  // From lesser to higher priority
  def expression[_: P]: P[Expression] = P(
    "(" ~/ expression ~ ")"
      | exprChain(comparison, ("&&" | "||").! map {
      case "&&" => Operator.And
      case "||" => Operator.Or
    })
  )

  def comparison[_: P]: P[Expression] = exprChain(addition, ("==" | "!=" | ">=" | "<=" | ">" | "<").! map {
    case "==" => Operator.Eq
    case "!=" => Operator.NEq
    case ">=" => Operator.GE
    case "<=" => Operator.LE
    case ">" => Operator.GT
    case "<" => Operator.LT
  })

  def addition[_: P]: P[Expression] = exprChain(multiplication, ("+" | "-").! map {
    case "+" => Operator.Plus
    case "-" => Operator.Minus
  })

  def multiplication[_: P]: P[Expression] = exprChain(simpleExpr, ("*" | "/" | "%").! map {
    case "*" => Operator.Times
    case "/" => Operator.Div
    case "%" => Operator.Mod
  })

  def simpleExpr[_: P]: P[Expression] = P(funCall | literal | name)

  // Trouvé sur https://github.com/lihaoyi/fastparse/blob/master/pythonparse/src/pythonparse/Expressions.scala
  // Très malin.
  def exprChain[_: P](higherPrecedence: => P[Expression], op: => P[Operator]): P[Expression] =
    P(higherPrecedence ~ (op ~ higherPrecedence).rep) map {
      case (lhs, rest) =>
        rest.foldLeft(lhs) {
          case (lhs, (op, rhs)) =>
            OpExpression(op, lhs, rhs)
        }
    }
}

object ast {
  type AST = Seq[Line]

  // L'AST lui-même.
  sealed trait Line

  import Expression._

  sealed trait Statement extends Line

  object Statement { // Juste pour les indenter ensemble, c'est plus clair
    case class Declaration(name: NameSegment, ty: Name) extends Statement

    case class Assignment(name: Name, expr: Expression) extends Statement

    case class Return(expression: Expression) extends Statement

    case class Block(lines: Seq[Line]) extends Statement

    case class IfStmt(condition: Expression, ifBlock: Block, elseBlock: Option[Block]) extends Statement

    case class WhileStmt(condition: Expression, block: Block) extends Statement

    case class ForStmt(init: Option[Assignment], condition: Option[Expression], end: Option[Assignment], block: Block) extends Statement

    case class FunDef(name: Name, params: Seq[Declaration], retTy: Option[Name], code: Block) extends Statement

  }

  case class NameSegment(name: String)

  sealed trait Expression extends Line

  object Expression {

    /// Is a seq of segments for module scoping (unimplemented) and dot syntax for function calls
    case class Name(segments: Seq[NameSegment]) extends Expression {
      override def toString: String = segments.map(_.name).mkString(".")
    }

    def name(segments: String*): Name = Name(segments map NameSegment)

    case class FunCall(name: Name, args: Seq[Expression]) extends Expression

    case class OpExpression(operator: Operator, lhs: Expression, rhs: Expression) extends Expression

    case class IntConstant(value: Int) extends Expression

    case class StrConstant(value: String) extends Expression {
      override def toString: String = s""""$value""""
    }

    case class BoolConstant(value: Boolean) extends Expression

  }

  // Les opérateurs pour OpExpression
  sealed trait Operator {
    override def toString: String = this match {
      case Operator.Plus => "+"
      case Operator.Minus => "-"
      case Operator.Times => "*"
      case Operator.Div => "/"
      case Operator.GT => ">"
      case Operator.GE => ">="
      case Operator.LT => "<"
      case Operator.LE => "<="
      case Operator.Eq => "=="
      case Operator.NEq => "!="
      case Operator.And => "&&"
      case Operator.Or => "||"
      case Operator.Mod => "%"
    }
  }

  object Operator {

    case object Plus extends Operator

    case object Minus extends Operator

    case object Times extends Operator

    case object Div extends Operator

    case object GT extends Operator

    case object GE extends Operator

    case object LT extends Operator

    case object LE extends Operator

    case object Eq extends Operator

    case object NEq extends Operator

    case object And extends Operator

    case object Or extends Operator

    case object Mod extends Operator

  }

}
