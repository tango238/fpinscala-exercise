package com.github.tango238.fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  def foldLeft[A,B](t: Tree[A], z:B)(f:(B,A) => B): B = t match {
    case Branch(l,r) => foldLeft(r,foldLeft(l,z)(f))(f)
    case Leaf(v) => f(z,v)
  }

  def foldRight[A,B](t: Tree[A], z:B)(f:(A,B) => B): B = t match {
    case Branch(l,r) => foldRight(l,foldRight(r,z)(f))(f)
    case Leaf(v) => f(v,z)
  }

  // Exercise 3.25
  def size[A](t: Tree[A]):Int = t match {
    case Branch(l,r) => 1 + size(l) + size(r)
    case Leaf(v) => 1
  }

  def maximum(t: Tree[Int]): Int = t match {
    case Branch(l,r) => {
      val maxl = maximum(l)
      val maxr = maximum(r)
      if (maxl > maxr) maxl else maxr
    }
    case Leaf(v) => v
  }

}


