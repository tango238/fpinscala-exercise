package com.github.tango238.fpinscala.errorhandling

import org.specs2.mutable.Specification

class OptionSpec extends Specification {

  "OptionSpec" should {
    "map" in {
      Some(1).map(_ * 2) must_== Some(2)
    }
    "flatMap" in {
      ok
    }
    "getOrElse" in {
      Some("a").getOrElse("b") must_== "a"
      None.getOrElse("b") must_== "b"
    }
    "orElse" in {
      ok
    }
    "filter" in {
      ok
    }
  }
}
