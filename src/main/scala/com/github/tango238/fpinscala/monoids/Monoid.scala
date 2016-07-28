package com.github.tango238.fpinscala.monoids

trait Monoid[A] {
  // op(op(x, y), z) == op(x, (y, z)) を満たす
  def op(a1: A, a2: A)
  // op(x, zero) == x および op(zero, x) == x を満たす
  def zero: A
}

object Monoid {

  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String) = a1 + a2
    val zero = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]) = a1 ++ a2
    val zero = Nil
  }

  // Exercise 10.1
  val intAddition: Monoid[Int] = ???
  val intMultiplication: Monoid[Int] = ???
  val booleanOr: Monoid[Boolean] = ???
  val booleanAnd: Monoid[Boolean] = ???

  // Exercise 10.2
  // Option型の値を結合するためのMonoidインスタンスを考え出せ
  def optionMonoid[A]: Monoid[Option[A]] = ???

}
