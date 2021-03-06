package fiddle

import scala.scalajs.js
import js.annotation.JSExport
import org.scalajs.dom
import scalatags.JsDom.all._

/**
 * API for things that belong to the page, and are useful to both the fiddle
 * client, user code as well as exported read-only pages.
 */
@JSExport
object Page{
  val fiddlePrelude = Shared.prelude
  val fiddleUrl = Shared.url
  val fiddleGistId = Shared.gistId

  def red = span(color:="#ffaaaa")
  def blue = span(color:="#aaaaff")
  def green = span(color:="#aaffaa")


  def sandbox = Util.getElem[dom.HTMLDivElement]("sandbox")
  def canvas = Util.getElem[dom.HTMLCanvasElement]("canvas")
  def renderer = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
  def output = Util.getElem[dom.HTMLDivElement]("output")
  def logspam = Util.getElem[dom.HTMLPreElement]("logspam")
  def source = Util.getElem[dom.HTMLDivElement]("source")

  def println(ss: Modifier*) = {
    print(div(ss: _*))
  }

  def print(ss: Modifier*) = {
    ss.foreach(_.applyTo(output))
    output.scrollTop = output.scrollHeight - output.clientHeight
  }

  def clear() = {
    output.innerHTML = ""
    canvas.height = sandbox.clientHeight
    canvas.width = sandbox.clientWidth
  }

  def scroll(px: Int) = {
    dom.console.log("Scrolling", px)
    output.scrollTop = output.scrollTop + px
  }

  def logln(s: Modifier*): Unit = {
    log(div(s:_*))
  }

  def log(s: Modifier*): Unit = {
    s.foreach(_.applyTo(logspam))
    logspam.scrollTop = 1000000000
  }

  val compiled = Util.getElem[dom.HTMLDivElement]("compiled").textContent

  @JSExport
  def exportMain(): Unit = {
    dom.console.log("exportMain")
    clear()
    val editor = Util.getElem[dom.HTMLDivElement]("editor")
    js.Dynamic.global.require("ace")
    editor.innerHTML = highlight(source.textContent, "ace/mode/scala")
    if(logspam.textContent == "") {
      logln("- Code snippet exported from ", a(href := fiddle.Shared.url, fiddle.Shared.url))
      logln("- ", blue("Ctrl/Cmd-S"), " and select ", blue("Web Page, Complete"), " to save for offline use")
      logln("- Click ", a(id := "editLink", href := "javascript:", "here"), " to edit a copy online")
    }
    dom.document
       .getElementById("editLink")
       .asInstanceOf[dom.HTMLAnchorElement]
       .onclick = { (e: dom.MouseEvent) =>
      Util.Form.post(fiddle.Shared.url + "/import",
        "source" -> source.textContent,
        "compiled" -> compiled
      )
    }
  }

  /**
   * Uses the Ace editor to generate a syntax-highlighted
   * HTML from the given blob of code.
   */
  def highlight(source: String, m: String): String = {
    val highlighter = js.Dynamic.global.require("ace/ext/static_highlight")
    val mode = js.Dynamic.global.require(m).Mode
    val theme = js.Dynamic.global.require("ace/theme/twilight")
    val dom =  js.Dynamic.global.require("ace/lib/dom")
    val highlighted = highlighter.render(
      source,
      js.Dynamic.newInstance(mode)(),
      theme,
      1,
      true
    )
    dom.importCssString(highlighted.css, "ace_highlight")
    highlighted.html.toString

  }

}
