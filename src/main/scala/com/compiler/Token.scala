package com.compiler

enum Token:
  case IntegerType(value: Int)
  case FloatType(value: Float)
  case NilType
  case OpenParens
  case CloseParens
  case Add
  case Subtract
  case Multiply
  case Divide
  case Space
  case Decimal
  case And
  case Or
  case Not
  case VarName(value: String)
  case PrintFn
  case Unknown(value: Char)