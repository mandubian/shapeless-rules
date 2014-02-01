package com.mandubian
package shapelessrules

import play.api.data.mapping._
import play.api.libs.json.JsValue
import play.api.libs.functional.syntax._
import play.api.libs.functional._

import shapeless._
import ops.hlist._
import poly._

trait BinaryTCConstraint[L <: HList, TC[_,_]]

object BinaryTCConstraint {
  type **->**[TC[_,_]] = {
    type λ[L <: HList] = BinaryTCConstraint[L, TC]
  }

  implicit def hnilBinaryTC[TC[_,_]] = new BinaryTCConstraint[HNil, TC] {}

  implicit def hlistBinaryTC1[H1, H2, T <: HList, TC[_,_]](implicit utct : BinaryTCConstraint[T, TC]) =
    new BinaryTCConstraint[TC[H1,H2] :: T, TC] {}
}

trait BinaryTCConstraintLeftFixed[L <: HList, I, TC[_,_]]

object BinaryTCConstraintLeftFixed {
  type +*->+*[I, TC[_,_]] = {
    type λ[L <: HList] = BinaryTCConstraintLeftFixed[L, I, TC]
  }

  implicit def hnilBinaryTCLeftFixed[I, TC[_,_]] = new BinaryTCConstraintLeftFixed[HNil, I, TC] {}

  implicit def hlistBinaryTCLeftFixed1[H, T <: HList, I, TC[_,_]](implicit utct : BinaryTCConstraintLeftFixed[T, I, TC]) =
    new BinaryTCConstraintLeftFixed[TC[I,H] :: T, I, TC] {}
}

trait BinaryTCConstraintEquals[L <: HList, TC[_,_]]

object BinaryTCConstraintEquals {
  type *=->*=[TC[_,_]] = {
    type λ[L <: HList] = BinaryTCConstraintEquals[L, TC]
  }

  implicit def hnilBinaryTCEquals[TC[_,_]] = new BinaryTCConstraintEquals[HNil, TC] {}

  implicit def hlistBinaryTCEquals1[H, T <: HList, TC[_,_]](implicit utct : BinaryTCConstraintEquals[T, TC]) =
    new BinaryTCConstraintEquals[TC[H,H] :: T, TC] {}
}



trait HZip {
  object applicativeValidationFolder extends Poly2 {

    implicit def caseApplicative[A, B <: HList](implicit
      app: Applicative[VV]
    ) = at[Rule[A,A], (Int, Rule[B,B])] {
      case (ra, (idx, rb)) =>
      (
        idx+1,
        Rule[A::B, A::B] {
          case (a:A @unchecked) :: (b:B @unchecked) =>
            app apply (
              app map (
                rb.validate(b),
                (bb:B) => (_:A) :: bb
              ),
              ra.validate(a).fail.map {
                _.map {
                  case (p, errs) => Path \ idx -> errs
                }
              }
            )
        } //.repath(_ => Path \ idx)
      )
    }

    implicit def caseApplicative2[A](implicit
      app: Applicative[VV]
    ) = at[Rule[A,A], (Int, HNil)] {
      case (ra, (idx, rb)) =>
      (
        idx+1,
        Rule[A::HNil, A::HNil] {
          case (a:A @unchecked) => 
            app map (ra.validate(a).fail.map {
              _.map {
                case (p, errs) => Path \ idx -> errs
              }
            }, (_:A) :: HNil)
        }
      )
    }

  }

  //val r: (Int, Rule[Int :: HNil, Int :: HNil]) = applicativeValidationFolder(Rules.min(5), (0, HNil:HNil))
  /*implicitly[Case2.Aux[
    applicativeValidationFolder.type,
    Rule[Int, Int],
    (Int, Rule[HNil, HNil]),
    (Int, Rule[Int :: HNil, Int :: HNil])
  ]]*/

  

  RightFolder.hnilRightFolder[
    (Int, Rule[HNil, HNil]),
    applicativeValidationFolder.type
  ]

  trait RuleApplicativeZipper {
    def apply[L <: HList, M <: HList](l: L)(implicit
      all: BinaryTCConstraintEquals.*=->*=[Rule]#λ[L],
      folder: RightFolder.Aux[L, (Int, Rule[HNil, HNil]), applicativeValidationFolder.type, (Int, Rule[M, M])]
    ): Rule[M, M] = l.foldRight((0:Int, Rule.zero[HNil]:Rule[HNil, HNil]))(applicativeValidationFolder)(folder)._2
  }

  def hzip[L <: HList] = new RuleApplicativeZipper {}

}

trait HFold {

  object applicativeRuleFolder extends Poly2 {

    implicit def caseApplicative[I, A, B <: HList](implicit
      app: Applicative[({type λ[O]=Rule[I, O]})#λ]
    ) = at[Rule[I,A], Rule[I,B]] { (ra, rb) =>
      app apply (
        app map (rb, (bb:B) => (_:A) :: bb),
        ra
      )
    }

    implicit def caseApplicative2[I, A](implicit
      app: Applicative[({type λ[O]=Rule[I, O]})#λ]
    ) = at[Rule[I,A], HNil] { (ra, rb) =>
      app map (ra, (_:A) :: HNil)
    }

  }

  trait RuleApplicativeFolder[I] {
    def apply[L <: HList, M <: HList](l: L)(implicit
      app: Applicative[({type λ[O]=Rule[I, O]})#λ],
      all: BinaryTCConstraintLeftFixed.+*->+*[I, Rule]#λ[L],
      folder: RightFolder.Aux[L, Rule[I, HNil], applicativeRuleFolder.type, Rule[I, M]]
    ): Rule[I, M] = l.foldRight(app.pure(HNil:HNil))(applicativeRuleFolder)(folder)
  }

  def hfold[I] = new RuleApplicativeFolder[I] {}

}


// A LOT OF DRAGONS AFTER THAT

//implicitly[RightFolder.Aux[Rule[Int, Int], (Int, Rule[HNil, HNil]), applicativeValidationFolder.type, Rule[Int :: HNil, Int :: HNil]]]

  /*RightFolder.hlistRightFolder[
    Rule[Int, Int],
    HNil,
    (Int, Rule[HNil, HNil]),
    applicativeValidationFolder.type,
    (Int, Rule[Int :: HNil, Int :: HNil])
  ]*/

  /*def doit = {
    //type RuleId[H] = Rule[H, H]

    val bc = implicitly[BinaryTCConstraint[Rule[String,String] :: Rule[Int,Int] :: HNil, Rule]]

    /*val r /*: Rule[String :: Int :: HNil, String :: Int :: HNil]*/ = {
      import play.api.data.mapping.json.Rules._
      notEmpty :: min(5) :: HNil
    }

    val s = sequence(r)*/

    val rule = {
      import play.api.data.mapping.json.Rules._
      val s: Rule[String, String] :: Rule[Int, Int] :: HNil = notEmpty :: min(5) :: HNil
      Rule.gen[JsValue, FooBar].hlisted compose sequence(s)
    }

  }*/

  // implicit def t[O] = implicitly[Applicative[({ type λ[I] = Rule[I, O] })#λ]]

  // implicitly[Applicative[({ type λ[O] = Rule[Int, O] })#λ]]

  // trait R[A, B]
  // def test[L <: HList](l: L)(implicit all: **->**[R]#λ[L]) = true

  // // compiles
  // implicitly[BinaryTCConstraint[R[Int,Int] :: HNil, R]]

  // implicitly[**->**[R]#λ[R[Int,Int] :: HNil]]

  // // does not compile
  // val p: R[Int, Int] :: HNil = new R[Int, Int] {} :: HNil
  // test(p)

  /*def r = RightFolder.hlistRightFolder[
      Rule[Int, Int],
      HNil,
      Rule[HNil, HNil],
      applicativeFolder.type,
      Rule[Int :: HNil, Int :: HNil]
  ]*/


  //val a : Int = applicativeFolder(min(5), Rule.zero[HNil])
  //implicitly[Case2[applicativeFolder.type, Rule[Int, Int], Rule[Int :: HNil, Int :: HNil]]]

  /*def r4 = RightFolder.hlistRightFolder[
    Rule[HNil,HNil], HNil, Rule[HNil,HNil], applicativeFolder.type, Rule[HNil,HNil]
  ]
  def r3 = RightFolder.hlistRightFolder[
    Rule[Int, Int],
    HNil,
    Rule[HNil, HNil],
    applicativeFolder.type,
    Rule[HNil, HNil]
  ]*/
  //implicit def R[O] = Rule.applicativeRule[O]



  /*type *->* = {
    type λ[L <: HList] = BinaryTCConstraint[L, RuleId]
  }

    implicit def hnilBinaryTC = new BinaryTCConstraint[HNil, RuleId] {}
  implicit def hlistBinaryTC1[H, T <: HList](implicit utct : BinaryTCConstraint[T, RuleId]) =
    new BinaryTCConstraint[RuleId[H] :: T, RuleId] {}
  */
  //def acceptHListRule[L <: HList : (*->*)#λ](l : L) = l
  //acceptHListRule(r)

  //acceptOption(Option(1) :: Option(true) :: HNil)
  //rule.hlisted compose r




  /*object polyrule extends Poly2 {
    implicit def default[H, L <: HList](
      implicit applicative: Applicative[({ type λ[O] = VA[H :: L, O] })#λ]
    ) = at[Rule[H, H], Rule[L, L]]{ (rh, rl) =>
      Rule[H::L, H::L] {
        case (h:H) :: (l:L) => applicative apply (
          applicative map (
            rh.validate(h),
            (h : H) => (l : L) => h :: l
          ),
          rl.validate(l)
        )
      }
    }
  }

  def rt[H, L](rh: Rule[H, H], rl: Rule[L, L])(
    implicit applicative: Applicative[({ type λ[O] = VA[H :: L, O] })#λ]
  ) = {
    Rule[H::L, H::L] {
      case (h:H) :: (l:L) => applicative apply (
        applicative map (
          rh.validate(h),
          (h : H) => (l : L) => h :: l
        ),
        rl.validate(l)
      )
    }
  }

  implicit def toHListRule[H, L <: HList, LL <: HList](rules : Rule[H, H] :: L)(
    implicit
      //folder: RightFolder[L, Rule[LL, LL], polyrule.type],
      gen: L => Rule[LL, LL],
      //ca: Case2[Rule[H, H], Rule[HH::LL, HH::LL], Rule[H::HH::LL, H::HH::LL]],
      //hcons: IsHCons.Aux[L, Rule[H2, H2], L2],
      applicative: Applicative[({ type λ[O] = VA[H :: L, O] })#λ]
  ): Rule[H::LL, H::LL] = rules match {
    case rh :: rl => rt(rh, gen(rl))
  }

  implicit def toHListRule[H](rules : Rule[H, H] :: HNil)(
    implicit applicative: Applicative[({ type λ[O] = VA[H :: HNil, O] })#λ]
  ) = rules match {
    case rh :: HNil => Rule[H::HNil, H::HNil] {
      case (h:H) :: HNil =>
        applicative map (
          rh.validate(h),
          (h : H) => h :: HNil
        )
    }
  }*/

  //def acceptHListRule[L <: HList : *->*[({ type λ[H] = Rule[H, H]})#λ]#λ](l : L) = l

  /*Rule[I, H :: L](rules : Rule[I, H] :: Rule[I, L])(
    implicit applicative: Applicative[({ type λ[A] = Validation[E, A] })#λ]
  ) = Rule {
    case h :: l => app.apply(
      applicative.map(
        rules.head.validate(h),
        (h : H) => (l : L) => h :: l
      ),
      rules.tail
    )
  }*/