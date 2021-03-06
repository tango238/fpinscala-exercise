package com.github.tango238.fpinscala.datastructures

import scala.annotation.tailrec

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  /*
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }
  */
  def sum(ints: List[Int]): Int = foldLeft(ints, 0)(_ + _)

  /*
  def product(ds: List[Double]): Double = ds match {
    case Nil => 1
    case Cons(0.0, _) => 0
    case Cons(x, xs) => x * product(xs)
  }
  */
  def product(ds: List[Double]): Double = foldLeft(ds, 1.0)(_ * _)

  // Exercise 3.2 Listの最初の要素を削除する
  def tail[A](as: List[A]): List[A] = as match {
    case Cons(x, xs) => xs
    case Nil => Nil
  }

  // Exercise 3.3 Listの最初の要素を別の値と置き換える
  def setHead[A](as: List[A], r: A): List[A] = as match {
    case Cons(x, xs) => Cons(r, xs)
    case Nil => Nil
  }

  // Exercise 3.4 リストの先頭から n 個の要素を削除する
  def drop[A](l: List[A], n: Int): List[A] =
    if (n <= 0) l
    else
      l match {
        case Cons(_, xs) => drop(xs, n - 1)
        case Nil => Nil
      }

  // Exercise 3.5 述語とマッチする場合に限り、Listからその要素までの要素を削除する
  def dropWhile[A](as: List[A], f: A => Boolean): List[A] = as match {
    case Cons(x, xs) => if (f(x)) dropWhile(xs, f) else as
    case Nil => Nil
  }

  def dropWhile2[A](as: List[A])(f: A => Boolean): List[A] = as match {
    case Cons(x, xs) => if (f(x)) dropWhile2(xs)(f) else as
    case Nil => Nil
  }

  /*
  def append[A](as: List[A], l: List[A]): List[A] = as match {
    case Nil => l
    case Cons(x, xs) => Cons(x, append(xs, l))
  }
  */

  /**
    * 1) foldRight(Cons(1, Cons(2, Cons(3, Nil))), 0)( _ + _ )
    * 2) 1 + foldRight(Cons(2, Cons(3, Nil)), 0)
    * 3) 1 + (2 + foldRight(Cons(3, Nil), 0))
    * 4) 1 + (2 + (3 + 0))
    * 5) 6
    */
  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  // Exercise 3.9 リストの長さを計算する
  def length[A](as: List[A]): Int = as match {
    case Cons(x, xs) => foldLeft(xs, 1)((b, a) => 1 + b)
    case Nil => 0
  }

  /**
    * Exercise 3.10
    *
    * 1) foldLeft(Cons(1, Cons(2, Cons(3, Nil))), 0)( _ + _ )
    * 2) foldLeft(Cons(2, Cons(3, Nil)))(0 + 1)
    * 3) foldLeft(Cons(2, Cons(3, Nil)))(0 + 1)
    * 4) foldLeft(Cons(3, Nil))(1 + 2)
    * 5) Nil (3 + 3)
    * 6) 6
    */
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    case Nil => z
  }

  // Exercise 3.12 要素が逆に並んだリストを返す
  def reverse[A](as: List[A]): List[A] = foldLeft(as, List[A]())((b, a) => Cons(a, b))

  // TODO: Exercise 3.13
  // 難問: `foldRight` をベースとして `foldLeft` を記述することは可能か。またその逆はどうか。

  // Exercise 3.14
  def append[A](l: List[A], r: List[A]): List[A] = {
    foldRight(l, r)(Cons(_, _))
  }

  // TODO: Exercise 3.15
  // 難問: 複数のリストからなるリストを1つのリストとして連結する関数を記述せよ


  // Exercise 3.16
  // 整数型リストの各要素に1を足すを変換する
  def addOne(l: List[Int]): List[Int] = {
    foldRight(l, List[Int]())((a, b) => Cons(a + 1, b))
  }

  // Exercise 3.17
  def stringify(as: List[Double]): List[String] = {
    foldRight(as, List[String]())((a, b) => Cons(a.toString, b))
  }

  // Exercise 3.18
  def map[A, B](as: List[A])(f: A => B): List[B] = {
    foldRight(as, List[B]())((a, b) => Cons(f(a), b))
  }

  // Exercise 3.19
  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    foldRight(as, List[A]())((a, b) => if (f(a)) Cons(a, b) else b)
  }

  // Exercise 3.20
  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = as match {
    case Nil => Nil
    case Cons(x, xs) => List.append(f(x), flatMap(xs)(f))
  }

  // Exercise 3.22
  def compose(a1: List[Int], a2: List[Int]): List[Int] = (a1, a2) match {
    case (Cons(x1, xs1), Cons(x2, xs2)) => Cons(x1 + x2, compose(xs1, xs2))
    case (Nil, _) => Nil
  }

  // Exercise 3.23
  def zipWith[A](a1: List[A], a2: List[A])(f: (A, A) => A): List[A] = (a1, a2) match {
    case (Cons(x1, xs1), Cons(x2, xs2)) => Cons(f(x1, x2), zipWith(xs1, xs2)(f))
    case (Nil, _) => Nil
  }

  // TODO: Exercise 3.24
  // 難問: List に別のListがサブシーケンスとして含まれているかどうかを調べる `hasSubsequence` を実装せよ
  // List(1,2,3,4)には List(1,2)、List(2,3)、List(4)などがサブシーケンスとして含まれている
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = ???

}