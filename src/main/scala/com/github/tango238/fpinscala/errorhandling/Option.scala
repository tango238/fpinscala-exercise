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
  def flatMap[B](f: A => Option[B]): Option[B] = map(f) getOrElse None


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
  def orElse[B >: A](ob: => Option[B]): Option[B] = map(Some(_)) getOrElse ob

  /*
  def filter(f: A => Boolean): Option[A] = this match {
    case Some(a) if f(a) => this
    case _ => None
  }
  */
  def filter(f: A => Boolean): Option[A] = flatMap(a => if (f(a)) Some(a) else None)

}

case class Some[+A](get: A) extends Option[A]

case object None extends Option[Nothing]


object Option {

  // Exercise 4.2
  // flatMapをベースとして variance 関数を実装せよ
  // シーケンスの平均を m、シーケンスの各要素を x とすれば、分散は math.pow(x - m, 2) の平均となる
  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  // Exercise 4.3
  // 2項関数を使って Option 型の２つの値を結合する総称関数
  // どちらかの Option 値が None の場合は返り値も None になる
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a flatMap (x => b map (y => f(x, y)))

  def map2_2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    for {
      x <- a
      y <- b
    } yield f(x, y)

  // Exercise 4.4
  // Option のリストを1つの Option にまとめる sequence 関数を記述せよ
  // 新しい Option には、元のリストに含まれているすべての Some 値のリストが含まれる
  // 元のリストに None が1つでも含まれていた場合、この関数の結果は None になる
  // それ以外はすべての値のリストを含んだ Some になる
  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil => Some(Nil)
    case h :: t =>
      for {
        hh <- h
        tt <- sequence(t)
      } yield hh :: tt
  }

  def Try[A](a: => A): Option[A] =
    try Some(a)
    catch { case e: Exception => None }

  def parseInts(a: List[String]): Option[List[Int]] =
    sequence(a map (i => Try(i.toInt)))

  // Exercise 4.5
  // The `traverse` function can be written with explicit recursion or use `foldRight` to do the recursion for you.
  // Implementing `sequence` using `traverse` may be more trivial than you think.
  def traverse[A,B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a match {
      case Nil => Some(Nil)
      case h :: t => map2(f(h), traverse(t)(f))(_ :: _)
    }

    // sequence(a map (x => f(x)))

}