package com.github.tango238.fpinscala.monoids

trait Monoid[A] {
  // op(op(x, y), z) == op(x, op(y, z)) を満たす
  def op(a1: A, a2: A): A
  // op(x, zero) == x および op(zero, x) == x を満たす
  def zero: A
}

object Monoid {

  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String): String = a1 + a2
    val zero = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]): List[A] = a1 ++ a2
    val zero = Nil
  }

  // Exercise 10.1
  val intAddition = new Monoid[Int] {
    def op(a1: Int, a2: Int): Int = a1 + a2
    val zero = 0
  }

  val intMultiplication = new Monoid[Int] {
    def op(a1: Int, a2: Int): Int = a1 * a2
    val zero = 1
  }

  val booleanOr = new Monoid[Boolean]{
    def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2
    def zero: Boolean = false
  }

  val booleanAnd = new Monoid[Boolean]{
    def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2
    def zero: Boolean = true
  }

  // Exercise 10.2
  // Option型の値を結合するためのMonoidインスタンスを考え出せ
//  def optionMonoid[A]: Monoid[Option[A]] = ???

}
