package parser

/**
 * Created by heno on 2015/05/06.
 */

import scala.collection.mutable.{Map => MuMap}

object Eval {
  /**
   * formを評価する
   * localEnvは局所環境、通常は空にする
   * グローバル環境はグローバル変数(globalEnv)にあり、localEnvに対応がない場合、そちらを探す
   * @param form
   * @param localEnv
   * @return
   */
  def eval(form:Form,localEnv:Env):Form = form match {
    case lst @ Lst(elems) => eval(elems.head,localEnv) match{
      case fn : Func => fn match {
          // シンタックス形式が持つ、専用の評価関数に入れる
        case Syntax(oriEval) => oriEval(lst,localEnv)
          // シンタックス形式以外は関数適用する (引数はあらかじめ評価する)
        case _ => fnApply(fn, elems.tail.map(eval(_,localEnv)))
      }
      case others => throw new EvalException("評価されたリストの先頭要素が関数値ではありません",others)
    }
    case atom : Atom => atom match {
      case sym : Sym => {
        lookup(sym,localEnv) match {
          case Some(x) => x
          case None => throw new EvalException ("シンボルに関連付けられたアトムがありません", form)
        }
      }
        // 自己評価フォーム
      case _ => atom
    }
  }

  /**
   * シンボルに対応づいた値を探す
   * 局所環境を探し、無ければグローバル変数のglobalEnv上を探す
   * evalの補助関数
   */
  def lookup(sym : Sym,localEnv : Env) : Option[Form] =
    Option(localEnv.getOrElse(sym,globalEnv.getOrElse(sym,null)))

  /**
   * 関数適用
   * args は fn に適用する引数のリスト
   * args は 評価済みであることに注意
   * @param fn
   * @param args
   * @return
   */
  def fnApply(fn : Func, args : List[Form]) = fn match{
    case p @ Primitive(priFn) => priFn(p,args)
    case c @ Closure(lambdaExpr,closedEnv) => lambdaExpr match{
      case Lst(Sym("lambda") :: Lst(params : List[Sym]) :: expr :: Nil) => {
        // クロージャの持つ環境に引数対応付けを追加(いいはず…)
        closedEnv ++= params.zip(args)
        eval(expr,closedEnv)
      }
      case _ => throw new EvalException("クロージャの中身が不正です",c)
    }
    case _ => throw new EvalException("プリミティブ関数以外の関数適用可能型は未実装です",fn)
  }

  /**
   * リスト以外のフォームまたはリストをリストにする
   * @return
   */
  def lstCover : Form => Lst = {
    case Lst(form) => Lst(form)
    case others => Lst(others :: Nil)
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
   * グローバル環境(可変マップ)
   * 関数などが入っている
   */
  val globalEnv : Env =
    MuMap(
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
          eval(cond,env) match{
            case Sym("true") => eval(thenSt,env)
            case Sym("false") => eval(elseSt,env)
            case _ => throw new EvalException("真偽シンボル以外が条件式より返されました",cond)
          }
        }
        case (others,_) => throw new EvalException("おかしなif文",others)
      },
      // グローバル環境にシンボルと値の対を追加
      Sym("def") -> Syntax{
        // 束縛される値は評価してから
        case (Lst(syntax :: symbols :: values :: Nil),env) => {
          (symbols,values) match{
            // シンボル が1つだけのとき
            case (s @ Sym(_), v : Form) => {
              val nv = eval(v,env)
              globalEnv += s -> nv
              Sym("nil")
            }
            // シンボル がリストになっているとき
            // 値もリストである必要がある
            case (Lst(s : List[Sym]),Lst(v)) if s.length == v.length => {
              globalEnv ++= s.zip(v.map(eval(_,env)))
              Sym("nil")
            }
            case _ => throw new EvalException("変数定義において、シンボルの数と値の数が一致しないか、シンボルではありません",syntax)
          }
        }
        case (others,_) => throw new EvalException("おかしなdef文",others)
      },
      Sym("quote") -> Syntax{
        case (Lst(syntax :: quoted :: Nil), env) => quoted
        case (others,_) => throw new EvalException("おかしなquote文",others)
      },
      Sym("lambda") -> Syntax{
        case ( Lst(syntax :: symbols :: expr :: Nil),env) => {
          // 仮引数が1つだけでリストになっていない場合、リストにしておく
          val symbolLst : Lst = symbols match{
            case Lst(elems) => Lst(elems)
            case _ => Lst(symbols :: Nil)
          }
          // ラムダ式がある場所の局所環境をコピーしてクロージャオブジェクトに閉じ込める
          Closure(Lst(syntax :: symbolLst :: expr :: Nil),env.clone())
        }
        case (others,_) => throw new EvalException("おかしなlambda文",others)
      }
    )
}