import java.util.Scanner
import scala.util.control.Breaks.{break, breakable}

/**
 * Created by heno on 2015/05/05.
 */
object Main {
  val scan = new Scanner(System.in)
  val str = new StringBuilder

  def main(args: Array[String]) {
    breakable{
      while(scan.hasNext){
        val line = scan.nextLine() + '\n'
        if(line.charAt(0) != ':') str append line
        else line.charAt(1) match{
          case 'e' => {
            try{
              val program = parse(str.toString()).get
//              for(stmt <- stmtLst) env = Evaluator.eval(stmt,env)
              println("parsed:\n" + program.foldRight("")((e,s) => e.toString + "\n" + s))
//              println("ans: " + env.ret)
            }
            catch{
              case e => println(e)
            }
            str.clear()
          }
          case 'q' => {
            break()
          }
          case 's' => {
//            println(env)
          }
        }
      }
    }
  }

  def parse(expression : String) = BasicParser.parseAll(BasicParser.program, expression)
}
