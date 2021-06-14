package Compiler
import Lexer.*

trait AST {
  def evaluate(): Any
}

case class EmptyNode() extends AST {
  override def evaluate(): EmptyNode = EmptyNode()
}

case class NilNode() extends AST {
  override def evaluate(): NilNode = NilNode()
}

case class ProgramNode(program: List[AST]) extends AST {
  def evaluate(): (Int, Any) = {
    try {
      val results = program.map(_.evaluate())
      return (0, results)
    } catch {
      case e: Exception => {
        return (1, e)
      }
    }
  }
}

case class FunctionNode(body: AST) extends AST {
  def evaluate(): Any = {
    body.evaluate()
  }
}

case class IntNode(value: Int) extends AST {
  def evaluate(): Int = value
}

case class FloatNode(value: Float) extends AST {
  def evaluate(): Float = value
}

case class PrefixAddNode(children: List[AST]) extends AST {
  def evaluate(): Int | Float = {
    val result = children.foldLeft(0.0f)((x, y) => {
      y.evaluate() match
        case z: Int => x + z
        case z: Float => x + z
    })

    // If its possible for it to have been an Int, we should cast it to Int
    result.isValidInt match
      case true => result.toInt
      case false => result
  }
}

case class PrefixMultiplyNode(children: List[AST]) extends AST {
  def evaluate(): Int | Float = {
    val result = children.foldLeft(1.0f)((x, y) => {
      y.evaluate() match
        case z: Int => x * z
        case z: Float => x * z
    })

    // If its possible for it to have been an Int, we should cast it to Int
    result.isValidInt match
      case true => result.toInt
      case false => result
  }
}

case class OrNode(children: List[AST]) extends AST {
  // Note: Strange return values due to mixing AST and Scala types
  def evaluate(): AST | Int | Float = {
    val maybeNotNil = children.find(_.evaluate() != NilNode())
    val result = maybeNotNil match
      case Some(value) => value.evaluate()
      case None => NilNode()

    result match
      case x: (AST | Int | Float) => x
      case _ => NilNode()
  }
}

case class AndNode(children: List[AST]) extends AST {
  def evaluate(): (AST | Int | Float)  = {
    val maybeNil = children.find(_.evaluate() == NilNode)
    var result = maybeNil match
      case Some(_) => NilNode()
      case None => children.last.evaluate()

    result match
      case x: (AST | Int | Float) => x
      case _ => NilNode()
  }
}

object Parser {
  def buildProgram(tokens: List[Token]): AST = {
    val expressions = buildExpressionList(tokens, List()).filter(_ != EmptyNode())
    ProgramNode(expressions)
  }

  private def buildExpressionList(tokens: List[Token], expressions: List[AST]): List[AST] = {
    tokens match
      case x :: xs => {
        val (remainder, expression) = buildExpression(xs)
        buildExpressionList(remainder, expression :: expressions)
      }
      case Nil => expressions.reverse
  }

  private def buildExpression(tokens: List[Token]): (List[Token], AST) = {
    tokens match
      case x :: xs => {
        x match
          case _: (OpenParens | CloseParens) => buildExpression(xs)
          case _: Add => {
            val (remainder, children) = buildFunctionParams(xs, List())
            (remainder,  PrefixAddNode(children.reverse))
          }
          case _: Multiply => {
            val (remainder, children) = buildFunctionParams(xs, List())
            (remainder,  PrefixMultiplyNode(children.reverse))
          }
          case _: Or => {
            val (remainder, children) = buildFunctionParams(xs, List())
            (remainder, OrNode(children.reverse))
          }
          case _: And => {
            val (remainder, children) = buildFunctionParams(xs, List())
            (remainder, AndNode(children.reverse))
          }
      }
      case Nil => (Nil, EmptyNode())
  }

  def buildFunctionParams(tokens: List[Token], children: List[AST]): (List[Token], List[AST]) = {
    tokens match
      case x :: xs => {
        x match
          case _: OpenParens => {
            val (remainder, expression) = buildExpression(tokens)
            remainder match {
              case Nil => buildFunctionParams(remainder, children)
              case z :: zs => buildFunctionParams(remainder, expression :: children)
            }
          }
          case y: IntegerType => buildFunctionParams(xs, IntNode(y.value) :: children)
          case y: FloatType => buildFunctionParams(xs, FloatNode(y.value) :: children)
          case _: NilType => buildFunctionParams(xs, NilNode() :: children)
          case _: CloseParens => (xs, children)
      }
      case Nil => (Nil, children)
  }
}