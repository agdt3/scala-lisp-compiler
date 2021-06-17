import com.compiler.{Lexer, Parser}

@main def hello: Unit =
  val test = "(+ 2 3.4)"
  val test2 = "(/ (+ 2 3.5) 2)"
  val test3 = "((+ 2 3.4)(* 4 1.5))"
  val test5 = "(+ (+ 2 3.4)(* 4 1.5))"
  val test6 = "(or nil nil nil (+ 1.5 3))"
  val test7 = "(and 3 2.5 nil 23)"
  val tokenList = Lexer.tokenizeString(test5)
  println(tokenList)
  val ast = Parser.buildProgram(tokenList)
  println(ast)
  val e = ast.evaluate()
  println(e)

