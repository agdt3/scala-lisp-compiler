import Compiler.{Add, Multiply, FloatType, IntegerType, Lexer, Parser}

@main def hello: Unit = {
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
  /*
  val test4 = "(+ 2 (* 4 1.5))"
  val tokenList2 = Lexer.tokenizeString(test4)
  println(tokenList2)
  val ast2 = Parser.buildProgram(tokenList2)
  println(ast2)
  val e2 = ast2.evaluate()
  println(e2)
  */
  /*
  val test2 = "(/ (+ 2 3.5) 2)"
  val test3 = "((+ 2 3.4)(* 4 1.5))"
  println(test)
  println(test2)
  val symbolList = Lexer.tokenizeString(test)
  val symbolList2 = Lexer.tokenizeString(test2)
  val symbolList3 = Lexer.tokenizeString(test3)
  println(symbolList)
  println(symbolList2)
  println(symbolList3)

   */
  /*
  val tokens1 = List(Add('+'), IntegerType(3), FloatType(2.5));
  val r = Parser.test(tokens1)
  println(r)
  val e = r.evaluate()
  println(e)
  val tokens2 = List(Multiply('*'), IntegerType(3), FloatType(2.5));
  val r2 = Parser.test(tokens2)
  println(r2)
  val e2 = r2.evaluate()
  println(e2)
   */
  /*
  val ast = Parser.test(symbolList3);
  println(ast);
  val e = ast.evaluate();
  println(e);
   */
}