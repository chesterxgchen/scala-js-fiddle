package fiddle
import acyclic.file
import scala.scalajs.js
import scala.scalajs.js.Dynamic.{literal => lit, _}
import org.scalajs.dom
import scala.concurrent.Future
import scala.scalajs.concurrent.JSExecutionContext.Implicits.runNow
import scala.async.Async.{async, await}
import scalatags.JsDom.all._
import scala.scalajs.js.annotation.JSExport
import org.scalajs.dom.extensions.{AjaxException, Ajax}
import Page._
import JsVal.jsVal2jsAny
import Client.RedLogger
import scala.Some
import autowire.Request
import upickle.Implicits._

@JSExport
object Checker{
  /**
   * Deadline by which the user code must complete execution.
   */
  private[this] var endTime = 0.0
  /**
   * Switch to flip to once you have run out of time to make
   * `check` fail every single time, ensuring you get thrown out
   * of the user code
   */
  private[this] var dead = false
  /**
   * Used to avoid doing an expensive `currentTimeMillis` check on every call,
   * and instead doing one every N calls.
   */
  private[this] var count = 0
  @JSExport
  def check(): Unit = {
    count += 1
    if (count % 1000 == 0 && js.Date.now() > endTime || dead) {
      dead = true
      Client.clearTimeouts()
      js.eval("""throw new Error("Time's Up! Your code took too long to run.")""")
    }
  }

  @JSExport
  def reset(max: Int) = {
    count = 0
    dead = false
    endTime = math.max(js.Date.now() + max, endTime)
  }

  def scheduleResets() = {
    dom.setInterval(() => Checker.reset(1000), 100)
  }
}

object Post extends autowire.Client[Web]{

  override def callRequest(req: Request): Future[String] = {
    val url = "/api/" + req.path.mkString("/")
    logln("Calling " + url)
    dom.extensions.Ajax.post(
      url = Shared.url + url,
      data = upickle.write(req.args)
    ).map(_.responseText)
  }
}


object StreamingAjax extends autowire.Client[Web]{

  override def callRequest(req: Request): Future[String] = {
    val url = "/api/" + req.path.mkString("/")
    logln("Calling " + url)
    dom.extensions.Ajax.get(
      url = Shared.url + url,
      data = upickle.write(req.args)
    ).map(_.responseText)
  }
}

class Client(){

  Client.scheduleResets()
  val command = Channel[Future[(String, Option[String])]]()

  def exec(s: String) = {
    Client.clear()
    Client.scheduleResets()

    Checker.reset(1000)
    try{
      js.eval(s)
      js.eval("ScalaJSExample().main()")

    }catch{case e: Throwable =>
      Client.logError(e.getStackTraceString)
      Client.logError(e.toString())
    }
  }
  val instrument = "c"

  val compilationLoop = task*async{
    val future = await(command())
    await(compile(future)).foreach(exec)

    while(true){
      val future = await(command())

      val compiled = await(compile(future))
      compiled.foreach(exec)
    }
  }

  val editor: Editor = new Editor(Seq(
    ("Compile", "Enter", () => command.update(Post[Api](_.fastOpt(editor.code)))),
    ("FullOptimize", "Shift-Enter", () => command.update(Post[Api](_.fullOpt(editor.code)))),
    ("Save", "S", save _),
    ("Complete", "Space", () => editor.complete()),
    ("FastOptimizeJavascript", "J", () => showJavascript(Post[Api](_.compile(editor.code)))),
    ("FullOptimizedJavascript", "Shift-J", () => showJavascript(Post[Api](_.fullOpt(editor.code)))),
    ("Export", "E", export _) ,
    ("evalDSL", "Y", () => showResults(evalDSL(editor.code))),
    //("streaming", "Z", () => showResults(Post[Api](_.streaming(editor.code))) )
    ("streaming", "Z", () => showResults(streamingSet()) )

  ), complete, RedLogger)

  logln("- ", blue("Cmd/Ctrl-Enter"), " to compile & execute, ", blue("Cmd/Ctrl-Space"), " for autocomplete.")
  logln("- Go to ", a(href:=fiddle.Shared.url, fiddle.Shared.url), " to find out more.")

  def compile(res: Future[(String, Option[String])]): Future[Option[String]] = {
    res.map { case (logspam, result) =>
      logln(logspam)
      result match{
        case Some(c) =>
          log(green("Success"))
          logln()
        case None =>
          log(red("Failure"))
          logln()
      }
      result
    }.recover { case e: Exception =>
      Client.logError(e.getStackTraceString)
      Client.logError(e.toString)
      None
    }
  }

  def showJavascript(compiled: Future[(String, Option[String])]) = {
    compiled.collect{ case (logspam, Some(code)) =>
      Client.clear()
      Page.output.innerHTML = Page.highlight(code, "ace/mode/javascript")
    }
  }

  def complete() = async {
    log("Completing... ")

    val code = editor.sess.getValue().asInstanceOf[String]

    val intOffset = editor.column + code.split("\n")
                                        .take(editor.row)
                                        .map(_.length + 1)
                                        .sum

    val flag = if(code.take(intOffset).endsWith(".")) "member" else "scope"


    val res = await(Post[Api](_.completeStuff(code, flag, intOffset)))
    log("Done")
    logln()
    res
  }

  def export(): Unit = task*async {
    logln("Exporting...")
    await(compile(Post[Api](_.fullOpt(editor.code)))).foreach{ code =>
      Util.Form.post("/export",
        "source" -> editor.code,
        "compiled" -> code
      )
    }
  }

  def save(): Unit = task*async{
    await(compile(Post[Api](_.fullOpt(editor.code))))
    val data = JsVal.obj(
      "description" -> "Scala.jsFiddle gist",
      "public" -> true,
      "files" -> JsVal.obj(
        "Main.scala" -> JsVal.obj(
          "content" -> editor.code
        )
      )
    ).toString()

    val res = await(Ajax.post("https://api.github.com/gists", data = data))
    val result = JsVal.parse(res.responseText)
    Util.Form.get("/gist/" + result("id").asString)
  }

  def streamingSet():  Future[(String, Option[String])] = {
    Client.clear()

/*
    js.eval(" var source = new EventSource(\"//api/fiddle/Api/streaming\")\n " +
            " source.onopen = function(){\n $('.bar').css('width', '0%');\n    }\n   " +
            " source.onmessage = function(message){\n      var n = message.data;\n     " +
            " console.log(\"message '\", n, \"'\");\n      if (n.toString().indexOf(\"Finish\") >=0 )  {\n        source.close();\n      }\n      if(!isNaN(n)){\n        $('.bar').css('width', n+'%');\n      } \n    }")
*/

    //above js.eval doesn't seem to work. put the whole page here for now.

    Page.output.innerHTML = """<!DOCTYPE html>
                              <html lang="en">
                                <head>
                                  <meta charset="utf-8" />
                                  <title>Event source example</title>
                                  <meta name="viewport" content="width=device-width, initial-scale=1.0" />
                                  <meta name="description" content="" />
                                  <meta name="author" content="Timothy Perrett" />
                                  <link href="css/bootstrap.min.css" rel="stylesheet" />
                                  <style type="text/css">
                                    /*<![CDATA[*/
                                    .container {
                                      margin-top: 40px;
                                      text-align: center;
                                    }
                                    /*]]>*/
                                  </style>
                                </head>
                                <body>
                              
                                  <div class="container">
                                    <h1>Event stream Example</h1>
                                    <p>Use this document as a way to quick start any new project.<br>
                                     All you get is this message and a barebones HTML document.</p>
                              
                                   <div class="span2">&nbsp;</div>
                              
                                   <div style="margin-bottom: 9px;" class="span8 progress progress-success progress-striped">
                                      <div style="width: 0%" class="bar"></div>
                                    </div>
                              
                                  </div> <!-- /container -->
                              
                                  <script type="text/javascript" src="js/jquery.min.js"></script>
                                  <script type="text/javascript">
                                  //<![CDATA[
                                  var source = new EventSource("/sse/streaming")
                                  source.onopen = function(){
                                    $('.bar').css('width', '0%');
                                  }
                                  source.onmessage = function(message){
                                    var n = message.data;
                                    console.log("message '", n, "'");
                                    if (n.toString().indexOf("Finish") >=0 )  {
                                      source.close();
                                    }
                                    if(!isNaN(n)){
                                      $('.bar').css('width', n+'%');
                                    }
                                  }
                                  //]]>
                                  </script>
                                </body>
                              </html>"""

    StreamingAjax[Api](_.streaming("streaming"))
  }


  def showResults(compiled: Future[(String, Option[String])]) = {
    compiled.collect{ case (logspam, Some(code)) =>
      Client.clear()
      Page.output.innerHTML = Page.highlight(code, "ace/mode/javascript")
    }
  }

  def evalDSL(text: String): Future[(String, Option[String])] = {

    def eval(dsl: String): Future[(String, Option[String])] = {

      import EvalCommands._
      val results = commands.filter(dsl.startsWith).map { cmd =>
        val src = dsl.drop(dsl.indexOf(cmd) + cmd.length).trim

        //route based on command
        cmd match {
          case Say => Future ((src, Some(src)))
          case RunDSL => {
            Post[Api](_.sparkSQLEval(src))
          }
          case RunWorkflow => {
            import FlowCommand._
            val cmd = src
            cmd match {
              case Show =>
                logln("Drawing Workflow")
                Client.clear()
                Workflow.draw()
                Future( (src, None))
              case Run =>
               // Post[Api](_.evalDSL(dsl))
                //todo: not completed.
               Future( (src, None))
            }
          }
        }
      }

      if (results.isEmpty) Future("", None) else results.head
    }

    eval(text)
  }
}


@JSExport
object Client{
  implicit val RedLogger = new Logger(logError)

  dom.onerror = ({(event: dom.Event, source: js.String, fileno: js.Number, columnNumber: js.Number) =>
    dom.console.log("dom.onerror")
    Client.logError(event.toString())
  }: js.Function4[dom.Event, js.String, js.Number, js.Number, Unit]).asInstanceOf[dom.ErrorEventHandler]


  @JSExport
  def logError(s: String): Unit = {
    logln(red(s))
  }
  @JSExport
  def clearTimeouts() = {
    for(i <- -100000 until 100000){
      dom.clearInterval(i)
      dom.clearTimeout(i)
    }
    Client.scheduleResets()
  }
  def clear() = {
    clearTimeouts()
    Page.clear()
  }

  @JSExport
  def gistMain(args: js.Array[String]): Unit = task*async{
    dom.console.log("gistMain")
    Editor.initEditor
    val (gistId, fileName) = args.toSeq match{
      case Nil => (fiddle.Shared.gistId, Some("LandingPage.scala"))
      case Seq(g) => (g, None)
      case Seq(g, f) => (g, Some(f))
    }

    val src = await(load(gistId, fileName))
    val client = new Client()
    client.editor.sess.setValue(src)

    client.command.update(Post[Api](_.fullOpt(src)))
  }

  @JSExport
  def importMain(): Unit = {
    clear()
    val client = new Client()
  }

  def load(gistId: String, file: Option[String]): Future[String] = {
    val gistUrl = "https://gist.github.com/" + gistId
    logln(
      "Loading ",
      file.fold(span)(s => span(
        a(href := gistUrl + "#file-" + s.toLowerCase.replace('.', '-'))(s),
        " from "
      )),
      a(href := gistUrl)(gistUrl),
      "..."
    )
    Ajax.get("https://api.github.com/gists/" + gistId).map{ res =>
      val result = JsVal.parse(res.responseText)
      val mainFile = result("files").get(file.getOrElse(""))
      val firstFile = result("files").values(0)
      mainFile.getOrElse(firstFile)("content").asString
    }.recover{case e => fiddle.Shared.default}

  }
  def scheduleResets() = {
    dom.setInterval(() => Checker.reset(1000), 100)
  }
}
