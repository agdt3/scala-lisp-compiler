import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.*
import org.scalatest.prop.TableDrivenPropertyChecks
import com.compiler.{Lexer, Parser, PrefixAddNode, ProgramNode, IntNode, FloatNode, Token}

class ParserTest extends AnyFlatSpec with Matchers {
  "A Parser" should "produce a valid program if the input is valid" in {
    val tokens = Lexer.tokenizeString("(+ 2 3.4)")
    val program = Parser.buildProgram(tokens)
    val resultProgram = ProgramNode(List(PrefixAddNode(List(IntNode(2), FloatNode(3.4)))))
    program shouldBe resultProgram
  }
}