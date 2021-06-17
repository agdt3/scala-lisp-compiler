package com.compiler
import com.compiler.Token
import com.compiler.Token.*
import com.compiler.AST

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
          case (OpenParens | CloseParens) => buildExpression(xs)
          case Add => {
            val (remainder, children) = buildFunctionParams(xs, List())
            (remainder,  PrefixAddNode(children.reverse))
          }
          case Multiply => {
            val (remainder, children) = buildFunctionParams(xs, List())
            (remainder,  PrefixMultiplyNode(children.reverse))
          }
          case Or => {
            val (remainder, children) = buildFunctionParams(xs, List())
            (remainder, OrNode(children.reverse))
          }
          case And => {
            val (remainder, children) = buildFunctionParams(xs, List())
            (remainder, AndNode(children.reverse))
          }
          case _ => throw new Exception("Unsupported expression token")
      }
      case Nil => (Nil, EmptyNode())
  }

  private def buildFunctionParams(tokens: List[Token], children: List[AST]): (List[Token], List[AST]) = {
    tokens match
      case x :: xs => {
        x match
          case OpenParens => {
            val (remainder, expression) = buildExpression(tokens)
            remainder match {
              case Nil => buildFunctionParams(remainder, children)
              case z :: zs => buildFunctionParams(remainder, expression :: children)
            }
          }
          case y: IntegerType => buildFunctionParams(xs, IntNode(y.value) :: children)
          case y: FloatType => buildFunctionParams(xs, FloatNode(y.value) :: children)
          case NilType => buildFunctionParams(xs, NilNode() :: children)
          case CloseParens => (xs, children)
          case _ => throw new Exception("Unsupported function param token")
      }
      case Nil => (Nil, children)
  }
}