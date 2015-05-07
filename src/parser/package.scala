/**
 * パッケージオブジェクト
 * 第一級ではない、エイリアス定義などに使用
 * Created by heno on 2015/05/06.
 */
package object parser {
  import scala.collection.mutable.{Map => MuMap}
  type Env = MuMap[Sym,Form]
}
