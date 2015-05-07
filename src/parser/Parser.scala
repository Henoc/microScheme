package parser

import scala.util.parsing.combinator._
import scala.util.parsing.input.Positional

sealed trait Form extends Positional
case class Lst(elems : List[Form]) extends Form {
  override def toString = "Lst(" + elems.mkString(", ") + ")"
}
sealed trait Atom extends Form
case class Sym(text : String) extends Atom
case class Str(text : String) extends Atom
case class Num(num : Int) extends Atom

sealed trait Func extends Form
case class Syntax(oriEval : (Lst,Env) => Form) extends Func
case class Macro() extends Func
case class Closure(lambdaExpr : Form,var closedEnv : Env) extends Func
case class Primitive(priFn : (Primitive,List[Form]) => Form) extends Func   // priFnの引数:Primitive はエラー時の行報告用であり、あまり必要はない


/**
 *
 * program := { form }
 * form := [ readMacro ] ( list | atom )
 * list := "(" { form } ")"
 * atom := SYMBOL | STRING | NUMBER
 *
 *
 * Created by heno on 2015/05/05.
 */
object BasicParser extends JavaTokenParsers with RegexParsers {
  def SYMBOL : Parser[Sym] = positioned(ident ^^ {case e => Sym(e)})
  def STRING : Parser[Str] = positioned(stringLiteral ^^ {case e => Str(e.substring(1,e.length - 1))})
  def NUMBER : Parser[Num] = positioned(decimalNumber ^^ {case e => Num(e.toInt)})
  def form : Parser[Form] = opt(readMacro) ~ (list | atom) ^^ {
    case Some(mac) ~ e => Lst(mac :: e :: Nil)
    case None ~ e => e
  }
  def atom : Parser[Atom] = SYMBOL | STRING | NUMBER
  def list : Parser[Lst] = "(" ~> rep(form) <~ ")" ^^ {
    case lst => Lst(lst)
  }
  def readMacro : Parser[Sym] = """'""".r ^^ {
    case _  => Sym("quote")
  }
  def program : Parser[List[Form]] = rep(form)
}
