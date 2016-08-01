package com.github.tango238.fpinscala.monoids

import org.specs2.mutable.Specification

class MonoidSpec extends Specification {

  "Monoid" should {

    "intAddition" in {
      val m = Monoid.intAddition
      val (x, y, z) = (1, 2, 3)

      m.op(m.op(x, y), z) must_== m.op(x, m.op(y, z))
      m.op(m.zero, x) must_== x
      m.op(x, m.zero) must_== x
    }

    "intMultiplication" in {
      val m = Monoid.intMultiplication
      val (x, y, z) = (2, 3, 4)

      m.op(m.op(x, y), z) must_== m.op(x, m.op(y, z))
      m.op(m.zero, x) must_== x
      m.op(x, m.zero) must_== x
    }

    "booleanOr" in {
      val m = Monoid.booleanOr
      val (x, y, z) = (true, false, true)

      m.op(m.op(x, y), z) must_== m.op(x, m.op(y, z))
      m.op(m.zero, x) must_== x
      m.op(x, m.zero) must_== x
    }

    "booleanAnd" in {
      val m = Monoid.booleanAnd
      val (x, y, z) = (true, false, true)

      m.op(m.op(x, y), z) must_== m.op(x, m.op(y, z))
      m.op(m.zero, x) must_== x
      m.op(x, m.zero) must_== x
    }

    "listMonoid" in {
      val m = Monoid.listMonoid[Int]
      val (x, y, z) = (List(1), List(2), List(3))

      m.op(m.op(x, y), z) must_== m.op(x, m.op(y, z))
      m.op(m.zero, x) must_== x
      m.op(x, m.zero) must_== x
    }

    "optionMonoid" in {
      val m = Monoid.optionMonoid[String]
      val (x, y, z) = (Some("a"), Some("b"), Some("c"))

      m.op(m.op(x, y), z) must_== m.op(x, m.op(y, z))
      m.op(m.zero, x) must_== x
      m.op(x, m.zero) must_== x
    }
  }
}
