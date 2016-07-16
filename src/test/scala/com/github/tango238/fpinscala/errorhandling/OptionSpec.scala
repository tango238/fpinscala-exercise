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
  }
}
