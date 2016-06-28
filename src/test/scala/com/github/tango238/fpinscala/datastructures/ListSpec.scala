package com.github.tango238.fpinscala.datastructures

import org.specs2._

class ListSpec extends Specification { def is = s2"""

 This is a specification to check the 'Hello world' string

 The 'Hello world' string should
   1 + 2 must be 3                              $e1
  """

  def e1 = List.sum(List(1, 2)) must_== 3

}
