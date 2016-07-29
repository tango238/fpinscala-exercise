package com.github.tango238.fpinscala.monoids

import org.specs2.mutable.Specification

class MonoidSpec extends Specification {

  "Monoid" should {

    "intAddition" in {
      val m = Monoid.intAddition
      val (x, y, z) = (1, 2, 3)

      m.op(m.op(x, y), z) must_== 6
      m.op(m.zero, x) must_== 1
    }

  }
}
