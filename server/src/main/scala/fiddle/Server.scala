package fiddle
import acyclic.file
import akka.io.Tcp
import spray.http.CacheDirectives.`no-cache`
import spray.http._
import spray.http.HttpHeaders._
import spray.httpx.encoding.Gzip
import spray.routing.directives.CachingDirectives._
import akka.actor.{ActorLogging, Actor, Props, ActorSystem}
import spray.routing.directives.CacheKeyer
import spray.util._
import scala.collection.mutable
import spray.client.pipelining._

import spray.http.HttpRequest
import scala.Some
import spray.http.HttpResponse
import spray.routing._
import upickle._
import upickle.Implicits._
import scala.scalajs.tools.classpath.PartialIRClasspath
import scala.annotation.{ClassfileAnnotation, StaticAnnotation, Annotation}
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.Try

object Server extends SimpleRoutingApp with Api{
  implicit val system = ActorSystem()
  import system.dispatcher
  val clientFiles = Seq("/client-fastopt.js")

  val `text/event-stream` =  MediaType.custom("text/event-stream")
  MediaTypes.register(`text/event-stream`)

  def respondAsEventStream =
    respondWithHeader(`Cache-Control`(`no-cache`)) &
      respondWithHeader(`Connection`("Keep-Alive")) &
      respondWithMediaType(`text/event-stream`)


  def main(args: Array[String]): Unit = {
    implicit val Default: CacheKeyer = CacheKeyer {
      case RequestContext(HttpRequest(_, uri, _, entity, _), _, _) => (uri, entity)
    }

    val simpleCache = routeCache(maxCapacity = 1000)
    println("Power On Self Test")
    val res = Compiler.compile(fiddle.Shared.default.getBytes, println)
    val optimized = res.get |> Compiler.fastOpt |> Compiler.fullOpt |> Compiler.export
    assert(optimized.contains("Looks like"))
    println("Power On Self Test complete: " + optimized.length + " bytes")


    startServer("0.0.0.0", port = Shared.defaultPort) {
      cache(simpleCache) {
        encodeResponse(Gzip) {
          get {
            pathSingleSlash {
              complete{
                HttpEntity(
                  MediaTypes.`text/html`,
                  Static.page(
                    s"Client().gistMain([])",
                    clientFiles,
                    "Loading gist..."
                  )
                )
              }
            } ~
            path("gist" / Segments){ i =>
              complete{
                HttpEntity(
                  MediaTypes.`text/html`,
                  Static.page(
                    s"Client().gistMain(${write(i)})",
                    clientFiles,
                    "Loading gist..."
                  )
                )
              }
            } ~ path("/api/fiddle/Api/streaming") {
              respondAsEventStream {
                sendSSE
              }
            } ~
            getFromResourceDirectory("")
          } ~
          post {
            path("api" / Segments){ s =>
              extract(_.request.entity.asString) { e =>
                complete {
                  autowire.Macros.route[Web](Server)(
                    autowire.Request(s, upickle.read[Map[String, String]](e))
                  )
                }
              }
            }
          }
        }
      }
    }
  }
  def compile(txt: String) = compileStuff(txt, _ |> Compiler.export)
  def fastOpt(txt: String) = compileStuff(txt, _ |> Compiler.fastOpt |> Compiler.export)
  def fullOpt(txt: String) = compileStuff(txt, _ |> Compiler.fastOpt |> Compiler.fullOpt |> Compiler.export)
  def export(compiled: String, source: String) = {
    renderCode(compiled, Nil, source, "Page().exportMain(); ScalaJSExample().main();", analytics = false)
  }
  def `import`(compiled: String, source: String) = {
    renderCode(compiled, clientFiles, source, "Client().importMain(); ScalaJSExample().main();", analytics = true)
  }
  def renderCode(compiled: String, srcFiles: Seq[String], source: String, bootFunc: String, analytics: Boolean) = {
    Static.page(bootFunc, srcFiles, source, compiled, analytics)
  }

  def completeStuff(txt: String, flag: String, offset: Int): List[(String, String)] = {
    Await.result(Compiler.autocomplete(txt, flag, offset), 100.seconds)
  }

  def compileStuff(code: String, processor: PartialIRClasspath => String) = {

    val output = mutable.Buffer.empty[String]

    val res = Compiler.compile(
      code.getBytes,
      output.append(_)
    )

    (output.mkString, res.map(processor))
  }

  def sparkSQLEval(dslStr: String): (String, Option[String]) = {
    val result = Try {
      import scala.sys.process._
      val returnVal = s"java -jar /Users/chester/Downloads/AlpineDSL-assembly-1.0.jar $dslStr".!!
      returnVal
    }.toOption

    (dslStr, result)
  }


  def streaming(text: String): (String, Option[String]) = {
    respondAsEventStream {
      sendSSE
    }
    (text, None)
  }


  def in[U](duration: FiniteDuration)(body: => U): Unit =
    actorSystem.scheduler.scheduleOnce(duration)(body)

  // simple case class whose instances we use as send confirmation message for streaming chunks
  case class Ok(remaining: Int)

  def sendSSE(ctx: RequestContext): Unit =
    actorRefFactory.actorOf {
      Props {
        new Actor with ActorLogging {
          // we use the successful sending of a chunk as trigger for scheduling the next chunk
          val responseStart = HttpResponse(entity = HttpEntity(`text/event-stream`, "data: start\n\n"))
          log.info(" start chunk response  with 10 iterations")
          ctx.responder ! ChunkedResponseStart(responseStart).withAck(Ok(10))

          def receive = {
            case Ok(0) =>
              log.info(" going to stop it " )
              ctx.responder ! MessageChunk("data: Finished.\n\n")
              ctx.responder ! ChunkedMessageEnd
              context.stop(self)
            case Ok(remaining) =>
              log.info(" got ok remaining " + remaining)
              in(Duration(500, MILLISECONDS)) {
                val nextChunk = MessageChunk("data: " + (10 - remaining)*10 + "\n\n")
                ctx.responder ! nextChunk.withAck(Ok(remaining - 1))
              }

            case ev: Tcp.ConnectionClosed =>
              log.warning("Stopping response streaming due to {}", ev)
          }
        }
      }
    }


}
