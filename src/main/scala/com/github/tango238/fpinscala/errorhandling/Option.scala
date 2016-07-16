package com.github.tango238.fpinscala.errorhandling

sealed trait Option[+A] {

  def map[B](f: A => B): Option[B] = this match {
    case Some(a) => Some(f(a))
    case None => None
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case Some(a) => f(a)
    case None => None
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(a) => a
    case None => default
  }

  // Noneだったら、指定したデフォルトのOption型を返す。SomeだったらそのままそのSomeを返す
  def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
    case Some(a) => this
    case None => None
  }

  def filter(f: A => Boolean): Option[A] = this match {
    case Some(a) if f(a) => this
    case _ => None
  }

}

case class Some[+A](get: A) extends Option[A]

case object None extends Option[Nothing]

