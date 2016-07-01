package com.github.tango238.fpinscala.datastructures

import org.specs2._

class ListSpec extends Specification { def is = s2"""

 The 'List' should
   1 + 2 = 3                              $sum1
   2 * 3 = 6                              $product1
   2 * 3 * 4   = 24                       $product2
   2 * 3 * 4 * 0 = 0                      $product3
   (2 + 3) * 4 = 20                       $product4
   (2 + 3 + 0) * 4 = 20                   $product5
   [1, 2, 3] drops 1 = [2, 3]             $drop1
   [1] drops 3 = Nil                      $drop2
   [-1, -2, 4] dropWhile 'x < 0' = [4]    $dropWhile1
   tail of [1, 2, 3]  = [2,3]             $tail1
   tail of []  = []                       $tail2
  """

  def sum1 = List.sum(List(1, 2)) must_== 3
  def product1 = List.product(List(2, 3)) must_== 6
  def product2 = List.product(List(2, 3, 4)) must_== 24
  def product3 = List.product(List(2, 3, 4, 0)) must_== 0
  def product4 = List.product(List(List.sum(List(2, 3)), 4)) must_== 20
  def product5 = List.product(List(List.sum(List(2, 3, 0)), 4)) must_== 20
  def drop1 = List.drop(List(1, 2, 3), 1) must_== List(2, 3)
  def drop2 = List.drop(List(1), 3) must_== Nil
  def minus(i: Int) = i < 0
  def dropWhile1 = List.dropWhile(List(-1, -2, 4), minus) must_== List(4)
  def tail1 = List.tail(List(1, 2, 3)) must_== List(2, 3)
  def tail2 = List.tail(List()) must_== List()
}
