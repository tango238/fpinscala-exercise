package com.github.tango238.fpinscala.errorhandling

import org.specs2.mutable.Specification

class OptionSpec extends Specification {

  "OptionSpec" should {
    "map" in {
      Some(1).map(_ * 2) must_== Some(2)
    }
    "flatMap" in {
      Some(1).flatMap(a => Some(a + 1)) must_== Some(2)
    }
    "getOrElse" in {
      Some("a").getOrElse("b") must_== "a"
      None.getOrElse("b") must_== "b"
    }
    "orElse" in {
      Some("a").orElse(None) must_== Some("a")
      None.orElse(None) must_== None
    }
    "filter" in {
      Some(10).filter(a => a > 0) must_== Some(10)
      Some(10).filter(a => a > 10) must_== None
    }

    "variance" in {
      // m = 2.5, (2.25 + 0.25 + 0.25 + 2.25) / 4 = 1.25
      Option.variance(Seq(1.0,2.0,3.0,4.0)) must_== Some(1.25)
    }

    "map2" in {
      Option.map2(Some(1), Some(2))((a,b) => a + b) must_== Some(3)
      Option.map2(None, Some(2))((a,b) => b) must_== None
      Option.map2(Some(1), None)((a,b) => a) must_== None
    }

    "map2_2" in {
      Option.map2_2(Some(1), Some(2))((a, b) => a + b) must_== Some(3)
      Option.map2_2(None, Some(2))((a, b) => b) must_== None
      Option.map2_2(Some(1), None)((a, b) => a) must_== None
    }

    "sequence" in {
      Option.sequence(List(Some(1),Some(2),Some(3))) must_== Some(List(1,2,3))
      Option.sequence(List(Some(1),None,Some(3))) must_== None
    }
  }
}
