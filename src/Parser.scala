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

sealed trait Func extends Atom
sealed trait Syntax extends Func
sealed trait Primitive extends Func
sealed trait Closure extends Func
sealed trait Macro extends Func

case class If(cond : Form, thenForm : Form, elseForm : Form) extends Syntax
case class Quote(form : Form) extends Syntax
case class Def(name : Sym, value : Form) extends Syntax
case class DefMacro(name : Sym, value : Form) extends Syntax



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
  def STRING : Parser[Str] = positioned(stringLiteral ^^ {case e => Str(e)})
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
