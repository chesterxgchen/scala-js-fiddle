package fiddle

import org.scalajs.dom

import scala.collection.mutable
import scala.scalajs.js.Dynamic.{literal => lit}
import scala.util.Try


case class Point(x: Double, y: Double){
  def +(p: Point) = Point(x + p.x, y + p.y)
  def -(p: Point) = Point(x - p.x, y - p.y)
  def *(d: Double) = Point(x * d, y * d)
  def /(d: Double) = Point(x / d, y / d)
  def length = Math.sqrt(x * x + y * y)
}


case class Operator(name: String, pos: Point){
  val width = 20
  val height= 20
  val toLinks = mutable.ListBuffer.empty[Operator]

  def link(op: Operator) :Unit = {
    toLinks += op
  }

}

object EvalCommands {
  type Command = String
  val Say    : Command      = "Say"
  val RunDSL : Command      = "RunDSL"
  val Elevation : Command   = "Elevation"
  val RunWorkflow : Command = "Workflow"

  val commands = Seq(Say, RunDSL, RunWorkflow)
}

object FlowCommand {
  type FlowCmd = String
  val Show : FlowCmd = "Show"
  val Run  : FlowCmd = "Run"
}


object Canvas {
  def getElem[T](id: String) = dom.document.getElementById(id).asInstanceOf[T]
  def canvas = getElem[dom.HTMLCanvasElement]("canvas")
  def renderer = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
}

trait Drawer  {
  val renderer = Canvas.renderer
  def draw()
}

case class OperatorDrawer(operator: Operator) extends Drawer {

  def draw() = {
    renderer.strokeStyle = "white"
    renderer.lineWidth = 3
    renderer.strokeRect(operator.pos.x, operator.pos.y, operator.width, operator.height)

    renderer.fillStyle = "white"
    renderer.fillText(operator.name, operator.pos.x - 1, operator.pos.y + 35)
  }

  def drawLinks() = {
    operator.toLinks.foreach(this.drawLink)
  }

  private def drawLink(op: Operator) = {
    // Let's translate the drawing to this operator position
    // renderer.translate( pos.x, pos.y)

    // Set the stroke pattern to red
    renderer.strokeStyle = "#FF0000"

    // Set the fill pattern to grey
    renderer.fillStyle = "grey"
    // Reset the path
    renderer.beginPath()

    // Let's move to our starting point
    renderer.moveTo( operator.pos.x + operator.width, operator.pos.y)

    // Let's draw our triangle lines
    renderer.lineTo( op.pos.x , op.pos.y + op.height)
    renderer.stroke()
    renderer.fill()

    // Close the path
    renderer.closePath()
  }
}


class Alpine(authToken: String, host:String, port: Int) {
  val rootUrl = s"http://$host:$port"
  val baseApiUrl = s"${rootUrl}/alpinedatalabs/api"
  val jsonApiUrl = s"${rootUrl}/alpinedatalabs/api/v1/json"

  //async mode
  val baseWorkflowUrl = s"${jsonApiUrl}/workflows"
  val baseProcessUrl = s"${jsonApiUrl}/processes"

  def getRunUrl(workflowId: String) =
    s"$baseWorkflowUrl/$workflowId/run?token=$authToken&showProgress=true"

  def getQueryUrl(processId: String) =
    s"$baseProcessUrl/$processId/query?token=$authToken"

}

object Alpine {
  def apply(authToken: String, host:String, port: Int) = new Alpine(authToken, host, port)
}




case class Flow (flowId: String , name: String, `type`: String) {
  val ops = mutable.ListBuffer.empty[Operator]
  def addOperator(op: Operator) {
    ops += op
  }

  def runDsl() {
    val workflowId = "65"
    val alpine: Alpine = Alpine("6e730702ad550600b2bee4ce986860411eb5751f", "localhost", 9090)

    val url = alpine.getRunUrl(workflowId)
    val dsl = s"Workflow Run $url"
    //Post[Api](_.evalDSL(dsl))
  }

  def runFlow() {
    val cmd = """RunDSL get data from SparkSQL at "local[4]" using features "name","age" from table "people" limit 10"""
    // Post[Api](_.evalDSL(cmd))
  }
}


case class FlowDrawer(flow: Flow) extends Drawer {

  def show() {
    flow.ops.foreach(OperatorDrawer(_).draw())
    flow.ops.foreach(OperatorDrawer(_).drawLinks())
  }
  def draw() = show()
}

object Workflow {


  import fiddle.Canvas._
  var startPoint = Point(dom.innerWidth.toInt/2 -500 , dom.innerHeight.toInt/2 -50 )

  def draw() : Option[Unit] = {
    Try {
      val f = Flow("16", "test", "cdh4")
      showFlow(f)
    }.toOption
  }

  def showFlow(f: Flow) {

    val flowDrawer = FlowDrawer(f)
    renderer.clearRect(0, 0, canvas.width, canvas.height)
    val op1 = Operator("data", startPoint)
    val op2 = Operator("row filter", Point(startPoint.x + 100, startPoint.y - 50))
    op1.link(op2)

    f.addOperator(op1)
    f.addOperator(op2)

    flowDrawer.draw()

    var current = op2
    for (i <- 0 until 5) {
      val op = Operator(s"row filter_$i", Point(current.pos.x + 10, current.pos.y - 30))
      current.link(op)
      f.addOperator(op)
      current = op
    }

    flowDrawer.draw()
  }


  def main() = draw()

}