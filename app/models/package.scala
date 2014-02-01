package com.mandubian

import play.api.data.mapping._

import shapeless._
import ops.hlist._
import poly._

package object shapelessrules extends HZip with HFold {

  type VV[A] = Validation[(Path, Seq[ValidationError]), A]

  implicit class toHListRule[I, O](val rule: Rule[I, O]) extends AnyVal {
    def hlisted[P <: HList](implicit gen: Generic.Aux[O, P]): Rule[I, P] =
      rule compose Rule.fromMapping[O, P] { o => Success(gen.to(o)) }
  }

}