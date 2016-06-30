package com.github.tango238.fpinscala.datastructures

import org.specs2._

class ListSpec extends Specification { def is = s2"""

 The 'List' should
   1 + 2 = 3                              $e1
   2 * 3 = 6                              $e2
   2 * 3 * 4   = 24                       $e3
   2 * 3 * 4 * 0 = 0                      $e4
   (2 + 3) * 4 = 20                       $e5
   (2 + 3 + 0) * 4 = 20                   $e6
   [1, 2, 3] drops 1 = [2, 3]             $e7
   [1] drops 3 = Nil                      $e8
   [-1, -2, 4] dropWhile 'x < 0' = [4]    $e9
  """

  def e1 = List.sum(List(1, 2)) must_== 3
  def e2 = List.product(List(2, 3)) must_== 6
  def e3 = List.product(List(2, 3, 4)) must_== 24
  def e4 = List.product(List(2, 3, 4, 0)) must_== 0
  def e5 = List.product(List(List.sum(List(2, 3)), 4)) must_== 20
  def e6 = List.product(List(List.sum(List(2, 3, 0)), 4)) must_== 20
  def e7 = List.drop(List(1, 2, 3), 1) must_== List(2, 3)
  def e8 = List.drop(List(1), 3) must_== Nil
  def minus(i: Int) = i < 0
  def e9 = List.dropWhile(List(-1, -2, 4), minus) must_== List(4)
}
