package parser

/**
 * Created by heno on 2015/05/06.
 */

object Eval {
  def eval(form:Form,env:Env):(Form,Env) = form match {
    case lst @ Lst(elems) => eval(elems.head,env) match{
      case (fn : Func, env) => fn match {
          // シンタックス形式が持つ、専用の評価関数に入れる
        case Syntax(oriEval) => oriEval(lst,env)
          // シンタックス形式以外は関数適用する (引数はあらかじめ評価する)
        case _ => (fnApply(fn, elems.tail.map(eval(_,env)._1)),env)
      }
      case (others, _) => throw new EvalException("評価されたリストの先頭要素が関数値ではありません",others)
    }
    case atom : Atom => atom match {
      case sym : Sym => (env.getOrElse(sym,throw new EvalException("シンボルに関連付けられたアトムがありません",form)),env)
        // 自己評価フォーム
      case _ => (atom,env)
    }
  }

  /**
   * 関数適用
   * args は fn に適用する引数のリスト
   * @param fn
   * @param args
   * @return
   */
  def fnApply(fn : Func, args : List[Form]) = fn match{
    case p @ Primitive(priFn) => priFn(p,args)
    case _ => throw new EvalException("プリミティブ関数以外の関数適用可能型は未実装です",fn)
  }

  /**
   * evalで使用する例外
   * @param mes
   */
  class EvalException(mes : String = null) extends Exception(mes) {
    def this(mes : String, line : Int, column : Int) = {
      this(mes + " at line: " + line + ", column: " + column)
    }
    def this(mes : String, form : Form) = this(mes, form.pos.line, form.pos.column)
  }

  /**
   * 初期の環境
   * 関数などが入っている
   */
  val initialEnv : Env =
    Map(
      Sym("true") -> Sym("true"),
      Sym("false") -> Sym("false"),
      Sym("nil") -> Sym("nil"),
      Sym("print") -> Primitive{
        case (_,arg :: Nil) => {
          arg match {
            case Str(text) => println(text)
            case Num(num) => println(num)
            case Sym(text) => println(text)
            case Lst(elems) => println("(なんかのリスト)")     // TODO: リスト表示
          }
          Sym("nil")
        }
        case (primitive,_) => throw new EvalException("引数の数が違います",primitive)
      },
      Sym("if") -> Syntax{
        case (Lst(syntax :: cond :: thenSt :: elseSt :: Nil),env) => {
          eval(cond,env)._1 match{
            case Sym("true") => eval(thenSt,env)
            case Sym("false") => eval(elseSt,env)
            case _ => throw new EvalException("真偽シンボル以外が条件式より返されました",cond)
          }
        }
        case (others,_) => throw new EvalException("おかしなif文",others)
      },
      Sym("def") -> Syntax{
        // 束縛される値は評価してから
        case (Lst(syntax :: symbols :: values :: Nil),env) => {
          (symbols,values) match{
            // シンボル が1つだけのとき
            case (s @ Sym(_), v : Form) => {
              val nv = eval(v,env)._1
              val newEnv = env + (s -> nv)
              (Sym("nil"),newEnv)
            }
            // シンボル がリストになっているとき
            // 値もリストである必要がある
            case (Lst(s : List[Sym]),Lst(v)) if s.length == v.length => {
              val newEnv = env ++ s.zip(v.map(eval(_,env)._1))
              (Sym("nil"),newEnv)
            }
            case _ => throw new EvalException("変数定義において、シンボルの数と値の数が一致しないか、シンボルではありません",syntax)
          }
        }
        case (others,_) => throw new EvalException("おかしなdef文",others)
      },
      Sym("quote") -> Syntax{
        case (Lst(syntax :: quoted :: Nil), env) => (quoted,env)
        case (others,_) => throw new EvalException("おかしなquote文",others)
      }
    )
}