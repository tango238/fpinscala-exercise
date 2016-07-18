package com.github.tango238.fpinscala.errorhandling

sealed trait Option[+A] {

  def map[B](f: A => B): Option[B] = this match {
    case Some(a) => Some(f(a))
    case None => None
  }

  /*
  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case Some(a) => f(a)
    case None => None
  }
  */
  def flatMap[B](f: A => Option[B]): Option[B] = map (f) getOrElse None


  def getOrElse[B >: A](default: => B): B = this match {
    case Some(a) => a
    case None => default
  }

  // Noneだったら、指定したデフォルトのOption型を返す。SomeだったらそのままそのSomeを返す
  /*
  def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
    case Some(a) => this
    case None => None
  }
  */
  def orElse[B >: A](ob: => Option[B]): Option[B] = map (Some(_)) getOrElse ob

  /*
  def filter(f: A => Boolean): Option[A] = this match {
    case Some(a) if f(a) => this
    case _ => None
  }
  */
  def filter(f: A => Boolean): Option[A] = flatMap(a => if(f(a)) Some(a) else None)

}

case class Some[+A](get: A) extends Option[A]

case object None extends Option[Nothing]


object Option {

  // Exercise 4.2
  // flatMapをベースとして variance 関数を実装せよ
  // シーケンスの平均を m、シーケンスの各要素を x とすれば、分散は math.pow(x - m, 2) の平均となる
  def variance(xs: Seq[Double]): Option[Double] = {
    val m = mean(xs)
    m flatMap(m => {
      mean(xs.map(x => math.pow(x - m , 2)))
    })
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

}