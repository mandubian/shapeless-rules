package controllers

import play.api._
import play.api.mvc._
import play.api.data.mapping._
import play.api.libs.json._
import play.api.libs.functional.syntax._
import play.api.libs.functional._

import shapeless._
import ops.hlist._
import poly._

import com.mandubian.shapelessrules._

import play.api.data.mapping.json.Rules
import Rules._

case class FooBar(foo: String, bar: Int, foo2: Long)

object Application extends Controller {

  def index = Action { request =>
    Ok(views.html.index("Your new application is ready."))
  }

  /* Rules Zipping */
  val ruleZip = Rule.gen[JsValue, FooBar].hlisted compose hzip(
    (notEmpty:Rule[String, String]) :: (min(5):Rule[Int, Int]) :: (max(10L):Rule[Long,Long]) :: HNil
  )

  def hzipper = Action(parse.json) { request =>
    ruleZip.validate(request.body) map { foobar =>
      Ok(foobar.toString)
    } recoverTotal { errors =>
      BadRequest(errors.toString)
    }

  }

  /* Rules Folding */
  val ruleFold = from[JsValue] { __ =>
    (__  \ "foo1").read[String] ::
    (__  \ "foo2").read[String] ::
    (__  \ "foo3").read[String] ::
    (__  \ "foo4").read[String] ::
    (__  \ "foo5").read[String] ::
    (__  \ "foo6").read[String] ::
    (__  \ "foo7").read[String] ::
    (__  \ "foo8").read[String] ::
    (__  \ "foo9").read[String] ::
    (__  \ "foo10").read[Int] ::
    (__  \ "foo11").read[Int] ::
    (__  \ "foo12").read[Int] ::
    (__  \ "foo13").read[Int] ::
    (__  \ "foo14").read[Int] ::
    (__  \ "foo15").read[Int] ::
    (__  \ "foo16").read[Int] ::
    (__  \ "foo17").read[Int] ::
    (__  \ "foo18").read[Int] ::
    (__  \ "foo19").read[Int] ::
    (__  \ "foo20").read[Boolean] ::
    (__  \ "foo21").read[Boolean] ::
    (__  \ "foo22").read[Boolean] ::
    (__  \ "foo23").read[Boolean] ::
    (__  \ "foo25").read[Boolean] ::
    (__  \ "foo26").read[Boolean] ::
    (__  \ "foo27").read[Boolean] ::
    (__  \ "foo28").read[Boolean] ::
    (__  \ "foo29").read[Boolean] ::
    (__  \ "foo30").read[Float] ::
    (__  \ "foo31").read[Float] ::
    (__  \ "foo32").read[Float] ::
    (__  \ "foo33").read[Float] ::
    (__  \ "foo34").read[Float] ::
    (__  \ "foo35").read[Float] ::
    (__  \ "foo36").read[Float] ::
    (__  \ "foo37").read[Float] ::
    (__  \ "foo38").read[Float] ::
    (__  \ "foo39").read[Float] ::
    (__  \ "foo40").read[List[Long]] ::
    (__  \ "foo41").read[List[Long]] ::
    (__  \ "foo42").read[List[Long]] ::
    (__  \ "foo43").read[List[Long]] ::
    (__  \ "foo44").read[List[Long]] ::
    (__  \ "foo45").read[List[Long]] ::
    (__  \ "foo46").read[List[Long]] ::
    (__  \ "foo47").read[List[Long]] ::
    (__  \ "foo48").read[List[Long]] ::
    (__  \ "foo49").read[List[Long]] ::
    (__  \ "foo50").read[JsNull.type] ::
    HNil
  }

  def hfolder = Action(parse.json) { request =>
    ruleFold.validate(request.body) map { hl =>
      Ok(hl.toString)
    } recoverTotal { errors =>
      BadRequest(errors.toString)
    }
  }

  val ruleFold2 = from[JsValue] { __ =>
    (__  \ "foo1").read[String] ::
    4 ::
    true ::
    (__  \ "foo2").read[String] ::
    List(1,2,3,4) ::
    HNil
  }

  def hfolder2 = Action(parse.json) { request =>
    ruleFold2.validate(request.body) map { hl =>
      Ok(hl.toString)
    } recoverTotal { errors =>
      BadRequest(errors.toString)
    }
  }

}
