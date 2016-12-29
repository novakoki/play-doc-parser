/**
  * Created by shaoziqi on 2016/12/29.
  */
import org.scalatest._
import playdoc._
import scala.util._
import scala.meta._

class RouteParserSpec extends FunSuite with Matchers {
  val methods = List("GET", "POST", "PUT", "DELETE")

  test("comment") {
    RouteParser.parse("# Index Page") match {
      case Success(result) => result should be (None)
    }
  }

  test("static route") {
    RouteParser.parse("GET /index controllers.IndexController.index") match {
      case Success(Route(method, path, action)) => {
        methods should contain (method)
        path should contain (Path(2, "index", None))
        action match {
          case ActionCall(controller, method, params) => {
            controller match {
              case Term.Name(c) => c should be ("IndexController")
            }
            method match {
              case Term.Name(m) => m should be ("index")
            }
            params should be (None)
          }
        }
      }
    }
  }
}
