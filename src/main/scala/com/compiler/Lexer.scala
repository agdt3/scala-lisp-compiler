package com.compiler
import com.compiler.Token
import com.compiler.Token.*

object Lexer {
  def tokenizeString(text: String): List[Token] = {
    _tokenizeString(text.toList, List()).filter(_ != Space).reverse
  }

  private def _tokenizeString(expression: List[Char], tokens: List[Token]): List[Token] = {
    expression match
      case x :: xs => {
        val (token, rest) = getToken(x, xs)
        _tokenizeString(rest, token :: tokens)
      }
      case Nil => tokens
  }

  private def getToken(x: Char, xs: List[Char]): (Token, List[Char]) = {
    x match
      case '(' => (OpenParens, xs)
      case ')' => (CloseParens, xs)
      case '+' => (Add, xs)
      case '-' => (Subtract, xs)
      case '*' => (Multiply, xs)
      case '/' => (Divide, xs)
      case y if 0 to 9 contains y.asDigit => matchNumber(s"${y}", xs)
      case y if y.isLetter => matchWord(x :: xs, List())
      case '.' => (Decimal, xs)
      case ' ' => (Space, xs)
      case _ => (Unknown(x), xs)
  }

  private def matchNumber(x: String, xs: List[Char]): (Token, List[Char]) = {
    xs match
      case y :: ys => {
        y match
          case y if 0 to 9 contains y.asDigit => matchNumber(s"${x}${y}", ys)
          case '.' => matchNumber(s"${x}${y}", ys)
          case _ => (resolveNumberString(x), xs)
      }
      case Nil => (resolveNumberString(x), xs)
  }

  private def resolveNumberString(x: String): IntegerType | FloatType = {
    try {
      IntegerType(x.toInt)
    } catch {
      case _: Exception => {
        FloatType(x.toFloat)
      }
    }
  }

  private def matchWord(chars: List[Char], word: List[Char]): (Token, List[Char]) = {
    chars match
      case x :: xs => {
        x match
          case y if y.isLetter => matchWord(xs, y :: word)
          case ' ' | _ => (matchWordType(word.reverse.mkString), chars)
      }
      case Nil => (matchWordType(word.reverse.mkString), chars)
  }

  private def matchWordType(word: String): Token = {
    word match
      case "nil" => NilType
      case "or" => Or
      case "and" => And
      case "not" => Not
      case "print" => PrintFn
      case _ => VarName(word)
  }
}
