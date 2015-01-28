package fiddle

import scala.annotation.ClassfileAnnotation

object Shared{
  val prelude =
    """
      |import scalatags.JsDom.all._
      |import org.scalajs.dom
      |import fiddle.Page
      |import Page.{red, green, blue, println}
      |import scalajs.js
    """.stripMargin

  val default = """
    |import scalajs.js
    |object ScalaJSExample extends js.JSApp{
    |  def main() = {
    |    println("Looks like there was an error loading the default Gist!")
    |    println("Loading an empty application so you can get started")
    |  }
    |}
  """.stripMargin

  val gistId = "9443f8e0ecc68d1058ad"

  //val url = "http://localhost:8080"
  val defaultPort = 28080
  val url = "http://localhost:" + defaultPort

}

class Web extends ClassfileAnnotation

@Web
trait Api{
  def compile(txt: String): (String, Option[String])
  def fastOpt(txt: String): (String, Option[String])
  def fullOpt(txt: String): (String, Option[String])
  def export(compiled: String, source: String): String
  def `import`(compiled: String, source: String): String
  def completeStuff(txt: String, flag: String, offset: Int): List[(String, String)]
  def sparkSQLEval(txt: String): (String, Option[String])
  def streaming(txt: String) : (String, Option[String])
}