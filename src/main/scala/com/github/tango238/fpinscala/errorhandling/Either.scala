package com.github.tango238.fpinscala.errorhandling

sealed trait Either[+E, +A] {

  def mean(xs: IndexedSeq[Double]): Either[String, Double] =
    if (xs.isEmpty)
      Left("mean of empty list")
    else
      Right(xs.sum / xs.length)
}

case class Left[+E](value: E) extends Either[E, Nothing]

case class Right[+A](value: A) extends Either[Nothing, A]
