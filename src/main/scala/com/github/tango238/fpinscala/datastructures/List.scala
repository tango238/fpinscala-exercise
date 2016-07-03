package com.github.tango238.fpinscala.datastructures

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

  def append[A](as: List[A], l: List[A]): List[A] = as match {
    case Nil => l
    case Cons(x, xs) => Cons(x, append(xs, l))
  }

  def foldRight[A,B](as :List[A], z:B)(f: (A,B) => B): B = as match {
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
  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = as match {
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
      case Nil => z
  }

}