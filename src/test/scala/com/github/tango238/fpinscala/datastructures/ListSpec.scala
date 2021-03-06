package com.github.tango238.fpinscala.datastructures

import org.specs2._

class ListSpec extends Specification { def is = s2"""

 The 'List' should
   1 + 2 = 3                                 $sum1
   2 * 3 = 6                                 $product1
   2 * 3 * 4   = 24                          $product2
   2 * 3 * 4 * 0 = 0                         $product3
   (2 + 3) * 4 = 20                          $product4
   (2 + 3 + 0) * 4 = 20                      $product5
   [1, 2, 3] drops 1 = [2, 3]                $drop1
   [1] drops 3 = Nil                         $drop2
   tail of [1, 2, 3]  = [2,3]                $tail1
   tail of []  = []                          $tail2
   setHead  [1, 2, 3] with 9 = [9, 2, 3]     $setHead1
   setHead  [] with 9 = []                   $setHead2
   append [3,4] to [1,2] = [1,2,3,4]         $append1
   [-1, -2, 4] dropWhile 'x < 0' = [4]       $dropWhile1
   [-1, -2, 4] dropWhile 'x < 0' = [4]       $dropWhile2
   The length of [1, 2, 3] = 3               $length1
   The length of [] = 0                      $length2
   [1,2,3](_ + _) = 6                        $foldLeft1
   Reverse of [1, 2, 3] = [3, 2, 1]          $reverse1
   addOne: [1,2,3] => [2,3,4]                $addOne1
   addOne: [2,5,7] => [3,6,8]                $addOne2
   stringify: [1,2,3] => ["1.0"."2.0"."3.0"] $stringify1
   map: [1,2,3]((a) => (a,a)) => [(1,1),(2,2),(3,3)] $map1
   filter: [1,2,3](_ > 1) => [2,3]           $filter1
   flatMap: [1,2,3]([_,_]) => [1,1,2,2,3,3]  $flatMap1
   compose: [1,2,3][4,5,6] => [5,7,9]        $compose1
   zipWith: [1,2,3][4,5,6](_*_) => [4,10,18] $zipWith1
  """

  def sum1 = List.sum(List(1, 2)) must_== 3
  def product1 = List.product(List(2, 3)) must_== 6
  def product2 = List.product(List(2, 3, 4)) must_== 24
  def product3 = List.product(List(2, 3, 4, 0)) must_== 0
  def product4 = List.product(List(List.sum(List(2, 3)), 4)) must_== 20
  def product5 = List.product(List(List.sum(List(2, 3, 0)), 4)) must_== 20
  def drop1 = List.drop(List(1, 2, 3), 1) must_== List(2, 3)
  def drop2 = List.drop(List(1), 3) must_== Nil
  def dropWhile1 = List.dropWhile(List(-1, -2, 4), (i: Int) => i < 0) must_== List(4)
  def dropWhile2 = List.dropWhile2(List(-1, -2, 4))(i => i < 0) must_== List(4)
  def tail1 = List.tail(List(1, 2, 3)) must_== List(2, 3)
  def tail2 = List.tail(List()) must_== List()
  def setHead1 = List.setHead(List(1, 2, 3), 9) must_== List(9, 2, 3)
  def setHead2 = List.setHead(List(), 9) must_== List()
  def append1 = List.append(List(1,2), List(3,4)) must_== List(1,2,3,4)
  def length1 = List.length(List(1,2,3)) must_== 3
  def length2 = List.length(List()) must_== 0
  def foldLeft1 = List.foldLeft(List(1,2,3), 0)(_ + _) must_== 6
  def reverse1 = List.reverse(List(1,2,3)) must_== List(3,2,1)
  def addOne1 = List.addOne(List(1,2,3)) must_== List(2,3,4)
  def addOne2 = List.addOne(List(2,5,7)) must_== List(3,6,8)
  def stringify1 = List.stringify(List(1,2,3)) must_== List("1.0","2.0","3.0")
  def map1 = List.map(List(1,2,3))((a) => (a,a)) must_== List((1,1),(2,2),(3,3))
  def filter1 = List.filter(List(1,2,3))(_ > 1) must_== List(2,3)
  def flatMap1 = List.flatMap(List(1,2,3))(i => List(i,i)) must_== List(1,1,2,2,3,3)
  def compose1 = List.compose(List(1,2,3), List(4,5,6)) must_== List(5,7,9)
  def zipWith1 = List.zipWith(List(1,2,3), List(4,5,6))(_ * _) must_== List(4,10,18)
}
