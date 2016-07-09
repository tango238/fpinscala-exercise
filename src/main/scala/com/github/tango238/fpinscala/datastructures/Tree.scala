package com.github.tango238.fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

/**
  * Branch(
  *   Branch(Leaf("a"), Leaf("b")),
  *   Branch(Leaf("c"), Leaf("d"))
  * )
  */

// Exercise 3.25
// def size() = ???