package lamac


import lamac.RuntimeValue.Callable._
import lamac.RuntimeValue._
import lamac.ast.Expression._
import lamac.ast.Statement._
import lamac.ast._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.io.StdIn

case class InterpreterException(msg: String) extends Exception

object interpreter {

  type Scope = mutable.HashMap[Name, RuntimeValue]

  val globalValues: Scope = mutable.HashMap()

  Builtins.initBuiltins(globalValues)

  def eval(ast: AST): Unit = evalScope(ast, List())

  /// We return a RuntimeValue if there was a return statement in the scope.
  /// If the scope is in a function, it will then be able to use it.
  def evalScope(ast: AST, outerScope: List[Scope]): Option[RuntimeValue] = {
    // All the values reachable from the current scope. Used as a stack so we can look up names first locally, then enclosing
    // scopes, and finally in the global scope
    // The first element is the current scope.
    val reachable = mutable.HashMap[Name, RuntimeValue]() :: outerScope
    for (line <- ast) {
      line match {
        case stmt: Statement =>
          evalStatement(stmt, reachable) match {
            case Some(retVal) => return Some(retVal)
            case None => ()
          }
        case expr: Expression => evalExpression(expr, reachable)
      }
    }
    None
  }

  def evalStatement(statement: Statement, reachable: List[Scope]): Option[RuntimeValue] = {
    statement match {
      case Declaration(nameSegment, ty) => reachable.head(Name(Seq(nameSegment))) = UninitVal(ty)
        None
      case Assignment(name, expr) => evalAssignment(name, expr, reachable)
      case Block(lines) => evalScope(lines, reachable)
      case IfStmt(condition, ifBlock, elseBlock) =>
        if (evalCondition(condition, reachable)) {
          evalScope(ifBlock.lines, reachable)
        }
        else {
          elseBlock.flatMap(block => evalScope(block.lines, reachable))
        }
      case WhileStmt(condition, block) =>
        while (evalCondition(condition, reachable)) {
          evalScope(block.lines, reachable) match {
            case Some(retVal) => return Some(retVal)
            case None => ()
          }
        }
        None
      case ForStmt(init, condition, end, block) =>
        init.foreach(evalStatement(_, reachable))
        evalStatement(
          WhileStmt(
            condition.getOrElse(BoolConstant(true)),
            Block(block.lines ++ end.map(Seq(_)).getOrElse(Seq()))
          ),
          reachable
        )
      case FunDef(name, params, retTy, code) =>
        reachable.head(name) = FunVal(params, retTy, code)
        None
      case Return(expression) => Some(evalExpression(expression, reachable))
    }
  }

  def evalAssignment(name: Name, expr: Expression, reachable: List[Scope]): Option[RuntimeValue] = {
    valueLookup(name, reachable) match {
      case Some((scope, _)) =>
        val newVal = evalExpression(expr, reachable)
        scope(name) = newVal
      case None => throw InterpreterException(s"Tried to assign to undeclared variable $name")
    }
    None
  }

  def evalCondition(condition: Expression, reachable: List[Scope]): Boolean = {
    evalExpression(condition, reachable) match {
      case BoolVal(true) => true
      case BoolVal(false) => false
      case other => throw InterpreterException(s"Value $condition = $other is not a boolean")
    }
  }

  def evalExpression(expression: Expression, reachable: List[Scope]): RuntimeValue = {
    expression match {
      case name: Name => valueLookup(name, reachable) match {
        case None => throw InterpreterException(s"Name $name is not reachable (reachable scopes: $reachable")
        case Some((_, value)) => value
      }
      case FunCall(name, args) => evalFunCall(name, args, reachable)
      case OpExpression(operator, lhs, rhs) => evalOpExpression(operator, lhs, rhs, reachable)
      case IntConstant(int) => IntVal(int)
      case StrConstant(str) => StrVal(str)
      case BoolConstant(bool) => BoolVal(bool)
    }
  }

  def evalFunCall(name: Name, args: Seq[Expression], reachable: List[Scope]): RuntimeValue = {
    val function = valueLookup(name, reachable) match {
      case Some((_, value)) => value match {
        case c: Callable => c
        case _ => throw InterpreterException(s"$name is not a function but you tried calling it")
      }
      case None => throw InterpreterException(s"Function $name is not reachable (reachable scopes: $reachable")
    }

    if (args.length != params(function).length) {
      throw InterpreterException(s"Function $name must be called with ${params(function).length} arguments")
    }

    val argValues = args map (evalExpression(_, reachable))

    function match {
      case FunVal(_, _, code) =>
        // Arguments are in a separate scope because it's easier
        val argScope = mutable.HashMap[Name, RuntimeValue]()
        for ((name, arg) <- params(function) zip argValues) {
          argScope.put(Name(Seq(name)), arg)
        }
        evalScope(code.lines, List(argScope)) match {
          case Some(retVal) => retVal
          case None => VoidVal
        }
      case BuiltinVal(_, _, code) => code(argValues)
    }
  }

  def evalOpExpression(operator: Operator, lhs: Expression, rhs: Expression, reachable: List[Scope]): RuntimeValue = {
    import lamac.ast.Operator._
    val leftValue = evalExpression(lhs, reachable)
    val rightValue = evalExpression(rhs, reachable)
    (operator, leftValue, rightValue) match {
      case (Plus, IntVal(l), IntVal(r)) => IntVal(l + r)
      case (Minus, IntVal(l), IntVal(r)) => IntVal(l - r)
      case (Times, IntVal(l), IntVal(r)) => IntVal(l * r)
      case (Div, IntVal(l), IntVal(r)) => IntVal(l / r)
      case (Mod, IntVal(l), IntVal(r)) => IntVal(l % r)
      case (GT, IntVal(l), IntVal(r)) => BoolVal(l > r)
      case (GE, IntVal(l), IntVal(r)) => BoolVal(l >= r)
      case (LT, IntVal(l), IntVal(r)) => BoolVal(l < r)
      case (LE, IntVal(l), IntVal(r)) => BoolVal(l <= r)
      case (Eq, UninitVal(_), UninitVal(_)) => BoolVal(true)
      case (NEq, UninitVal(_), UninitVal(_)) => BoolVal(false)
      case (Eq, l, r) => BoolVal(l == r)
      case (NEq, l, r) => BoolVal(l != r)
      case (And, BoolVal(l), BoolVal(r)) => BoolVal(l && r)
      case (Or, BoolVal(l), BoolVal(r)) => BoolVal(l || r)
      case _ => throw InterpreterException(s"Unhandled operation $leftValue $operator $rightValue")
    }
  }

  // We return both the scope and a "cached" RuntimeValue for immediate use
  def valueLookup(name: Name, reachable: List[Scope]): Option[(Scope, RuntimeValue)] = {
    for (scope <- reachable) {
      scope.get(name) match {
        case Some(value) => return Some((scope, value))
        case None => ()
      }
    }
    globalValues.get(name).map(value => (globalValues, value))
  }
}

sealed trait RuntimeValue {
  override def toString: String = {
    this match {
      case VoidVal => "void"
      case UninitVal(ty) => s"uninit of type $ty"
      case IntVal(value) => String.valueOf(value)
      case StrVal(value) => value
      case BoolVal(value) => String.valueOf(value)
      case ArrayVal(values) => values.map(x => x.toString).mkString(start = "[", sep = ", ", end = "]")
      case _ => super.toString
    }
  }
}

object RuntimeValue {

  case object VoidVal extends RuntimeValue

  case class UninitVal(ty: Name) extends RuntimeValue

  // This design works because we only have builtin types
  case class ArrayVal(values: ArrayBuffer[RuntimeValue]) extends RuntimeValue

  case class IntVal(value: Int) extends RuntimeValue

  case class StrVal(value: String) extends RuntimeValue

  case class BoolVal(value: Boolean) extends RuntimeValue

  case class TyVal(name: Name) extends RuntimeValue

  sealed trait Callable extends RuntimeValue

  object Callable {

    case class FunVal(params: Seq[Declaration], retTy: Option[Name], code: Block) extends Callable

    case class BuiltinVal(params: Seq[Declaration], retTy: Option[Name], code: Seq[RuntimeValue] => RuntimeValue) extends Callable

    import ast.Expression.name

    def builtin(params: Seq[(String, String)], retTy: Option[String], code: Seq[RuntimeValue] => RuntimeValue): BuiltinVal =
      BuiltinVal(
        params map { case (ident, ty) => Declaration(NameSegment(ident), name(ty)) },
        retTy map (name(_)),
        code
      )

    def params(c: Callable): Seq[NameSegment] = {
      val params = c match {
        case FunVal(params, _, _) => params
        case BuiltinVal(params, _, _) => params
      }
      params map (_.name)
    }
  }

}

object Builtins {

  def initBuiltins(scope: interpreter.Scope): Option[RuntimeValue] = {
    scope.put(name("uninit"),
      UninitVal(name("Any"))
    )
    scope.put(name("read"),
      builtin(Seq(), Some("String"),
        _ => {
          StrVal(StdIn.readLine())
        }
      )
    )
    scope.put(name("readInt"),
      builtin(Seq(), Some("Int"),
        _ => {
          try {
            IntVal(StdIn.readInt())
          } catch {
            case _: Throwable => UninitVal(name("Any"))
          }
        }
      )
    )
    scope.put(name("print"),
      builtin(Seq(("val", "any")), None,
        args => {
          print(args.head)
          VoidVal
        }
      )
    )
    scope.put(name("println"),
      builtin(Seq(("val", "any")), None,
        args => {
          println(args.head)
          VoidVal
        }
      )
    )
    scope.put(name("a_new"),
      builtin(Seq(), Some("Array"),
        {
          case Seq() => ArrayVal(ArrayBuffer())
          case _ => throw InterpreterException(s"a_new takes 0 argument")
        }
      )
    )
    scope.put(name("a_len"),
      builtin(Seq(("a", "Array")), Some("Int"),
        {
          case Seq(ArrayVal(array)) => IntVal(array.length)
          case _ => throw InterpreterException(s"a_len takes an array arg")
        }
      )
    )
    scope.put(name("a_get"),
      builtin(Seq(("a", "Array"), ("idx", "Int")), Some("Any"),
        {
          case Seq(ArrayVal(array), IntVal(idx)) =>
            if (array isDefinedAt idx) {
              array(idx)
            } else {
              throw InterpreterException(s"Out of bounds read: array ${ArrayVal(array)} at index $idx")
            }
          case _ => throw InterpreterException(s"a_get takes an array and an int as args")
        }
      )
    )
    scope.put(name("a_set"),
      builtin(Seq(("a", "Array"), ("idx", "Int"), ("elt", "Any")), None,
        {
          case Seq(ArrayVal(array), IntVal(idx), elt) =>
            if (array isDefinedAt idx) {
              array(idx) = elt
              VoidVal
            } else {
              throw InterpreterException(s"Out of bounds write: array ${ArrayVal(array)} at index $idx")
            }
          case _ => throw InterpreterException(s"a_set takes an array, an int and an object as args")
        }
      )
    )
    scope.put(name("a_push"),
      builtin(Seq(("a", "Array"), ("elt", "Any")), None,
        {
          case Seq(ArrayVal(array), elt) =>
            array.append(elt)
            VoidVal
          case _ => throw InterpreterException(s"a_push takes an array and an object as args")
        }
      )
    )
    scope.put(name("a_rem"),
      builtin(Seq(("a", "Array"), ("idx", "Int")), None,
        {
          case Seq(ArrayVal(array), IntVal(idx)) =>
            if (array isDefinedAt idx) {
              array.remove(idx)
              VoidVal
            } else {
              throw InterpreterException(s"Out of bounds remove: array ${ArrayVal(array)} at index $idx")
            }
          case _ => throw InterpreterException(s"a_rem takes an array and an int as args")
        }
      )
    )
  }
}
