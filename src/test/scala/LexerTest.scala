import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.*
import org.scalatest.prop.TableDrivenPropertyChecks.*
import com.compiler.Lexer
import com.compiler.Token
import com.compiler.Token.*

class LexerTest extends AnyFlatSpec with Matchers {
  val tokenTable = Table(
    ("character", "expectedToken"),
    ("(", OpenParens),
    (")", CloseParens),
    ("1", IntegerType(1)),
    ("1.0", FloatType(1.0)),
    ("+", Add),
    ("-", Subtract),
    ("*", Multiply),
    ("/", Divide),
    (".", Decimal),
    ("nil", NilType),
    ("or", Or),
    ("and", And),
    ("not", Not),
    ("print", PrintFn),
  )
  "A Lexer" should "map all the characters correctly" in {
    forAll (tokenTable) { (character: String, expectedToken: Token) =>
      val resultToken = Lexer.tokenizeString(character)
      resultToken shouldBe List(expectedToken)
    }
  }

  it should "parse integers, floats and simple operations correctly" in {
    val test = "(+ 2 3.4)"
    val expectedTokens = List(OpenParens, Add, IntegerType(2), FloatType(3.4), CloseParens)
    val createdTokens = Lexer.tokenizeString(test)
    createdTokens shouldBe expectedTokens
  }

  it should "allow for nested expressions" in {
    val test2 = "(/ (+ 2 3.5) 2)"
    val expectedTokens = List(
      OpenParens, Divide, OpenParens, Add, IntegerType(2), FloatType(3.5), CloseParens, IntegerType(2), CloseParens)
    val createdTokens = Lexer.tokenizeString(test2)
    createdTokens shouldBe expectedTokens
  }
}