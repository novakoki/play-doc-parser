/**
  * Created by shaoziqi on 2016/12/28.
  */
package playdoc

import scala.collection.immutable.Seq
import scala.meta._
import scala.util._

case class Route (
  method: String,
  path: Seq[Path],
  action: ActionCall
)

case class Path (
  `type`: Int,
  name: String,
  regex: Option[String]
) {
  def getType =
    `type` match {
      case 0 => "static"
      case 1 => "default"
      case 2 => "wildcard"
      case 3 => "custom"
    }
}

case class ActionCall (
  controller: Term.Name,
  method: Term.Name,
  params: Option[Seq[Term.Arg]]
)

object RouteParser {
  def parse (rawRoute: String) = {
    val comment = """^#(.+)$""".r
    val route = """^(\S+)\s+(\S+)\s+(.+)$""".r
    rawRoute match {
      case comment(c) => Success(None)
      case route(method, rawPath, rawAction) =>
        for {
          path <- parsePath(rawPath)
          action <- parseAction(rawAction)
        } yield {
          Route(method, path, action)
        }
      case _ => Failure
    }
  }

  private def parsePath (rawPath: String) = {
    val static = """^(\S+)$""".r
    val default = """^:(\S+)$""".r
    val wildcard = """^*(\S+)$""".r
    val custom = """^\$(\S+)<(.+)>$""".r

    Try {
      rawPath.split('/').map { item =>
        item match {
          case default(name) => Path(1, name, None)
          case wildcard(name) => Path(2, name, None)
          case custom(name, regex) => Path(3, name, Some(regex))
          case static(name) => Path(0, name, None)
          case _ => Path(0, "", None)
        }
      }.filter(_.name.nonEmpty).toList
    }
  }

  private def parseAction (rawAction: String) = {
    Try {
      rawAction.parse[Stat].get match {
        case q"$controllers.$controller.$method" => ActionCall(controller, method, None)
        case q"$controllers.$controller.$method(..$params)" => {
          params match {
            case Nil => ActionCall(controller, method, None)
            case head::tail => ActionCall(controller, method, Some(params))
          }
        }
      }
    }
  }
}
