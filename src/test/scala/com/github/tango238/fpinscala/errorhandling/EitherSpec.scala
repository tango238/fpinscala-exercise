package com.github.tango238.fpinscala.errorhandling

import org.specs2.mutable.Specification

class EitherSpec extends Specification {

  "EitherTest" should {
    "map" in {
      Right(1).map(a => a + 1) must_== Right(2)
      Left("Error").map(a => a) must_== Left("Error")
    }

    "flatMap" in {
      Right(1).flatMap(a => Right(a + 1)) must_== Right(2)
    }

    "orElse" in {
      ok
    }

    "map2" in {
      ok
    }

  }
}
