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

  val booleanOr = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2

    def zero: Boolean = false
  }

  val booleanAnd = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2

    def zero: Boolean = true
  }

  // Exercise 10.2
  // Option型の値を結合するためのMonoidインスタンスを考え出せ
  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    def op(x: Option[A], y: Option[A]) = x orElse y

    val zero = None
  }

  def dual[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {
    def op(x: A, y: A): A = m.op(y, x)

    val zero = m.zero
  }

  // Exercise 10.3
  // 引数および戻り値の型が同じである関数を endo 関数と呼ぶことがある。
  // endo 関数のモノイドを記述せよ
  def endoMonoid[A]: Monoid[A ⇒ A] = new Monoid[A ⇒ A] {
    def op(f: A ⇒ A, g: A ⇒ A) = f compose g

    val zero = (a: A) ⇒ a
  }

  // Exercise 10.4
  // def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop

  def concatenate[A](as: List[A], m: Monoid[A]): A =
    as.foldLeft(m.zero)(m.op)

  // Exercise 10.5
  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A ⇒ B): B =
    as.foldLeft(m.zero)((b, a) ⇒ m.op(b, f(a)))

  // Exercise 10.6
  // foldMap関数はfoldLeftまたはfoldRightを使って実装できる。
  // ただし、foldMapを使ってfoldLeftとfoldRightを記述することも可能である

  // Exercise 10.7
  // IndexSeqのためのfoldMapを実装せよ。
  // シーケンスを半分に分割し、両半分を再帰的に処理した後、モノイドを使ってそれらの結果を結合すること
  def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A ⇒ B): B =
    if (as.isEmpty)
      m.zero
    else if (as.length == 1)
      f(as(0))
    else {
      val (l, r) = as.splitAt(as.length / 2)
      m.op(foldMapV(l, m)(f), foldMapV(r, m)(f))
    }

  def productMonoid[A, B](A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] =
    new Monoid[(A, B)] {
      def op(x: (A, B), y: (A, B)) =
        (A.op(x._1, y._1), B.op(x._2, y._2))

      val zero = (A.zero, B.zero)
    }

  def mapMergeMonoid[K, V](V: Monoid[V]): Monoid[Map[K, V]] =
    new Monoid[Map[K, V]] {
      def zero = Map[K, V]()
      def op(a: Map[K, V], b: Map[K, V]) =
        (a.keySet ++ b.keySet).foldLeft(zero){ (acc, k) ⇒
          acc + (k → V.op(a.getOrElse(k, V.zero), b.getOrElse(k, V.zero)))
        }
    }

  def functionMonoid[A,B](B: Monoid[B]): Monoid[A ⇒ B] =
    new Monoid[A ⇒ B] {
      def op(f: A ⇒ B, g: A ⇒ B) = a ⇒ B.op(f(a), g(a))
      val zero: A ⇒ B = a ⇒ B.zero
    }
}

trait Foldable[F[_]] {

  import Monoid._

  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) ⇒ B): B =
    foldMap(as)(f.curried)(endoMonoid[B])(z)

  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) ⇒ B): B =
    foldMap(as)(a ⇒ (b: B) ⇒ f(b, a))(dual(endoMonoid[B]))(z)

  def foldMap[A, B](as: F[A])(f: A ⇒ B)(mb: Monoid[B]): B =
    foldRight(as)(mb.zero)((a, b) ⇒ mb.op(f(a), b))

  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    foldLeft(as)(m.zero)(m.op)

  def toList[A](as: F[A]): List[A] =
    foldRight(as)(List[A]())(_ :: _)
}

object ListFoldable extends Foldable[List] {

  override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) ⇒ B): B =
    as.foldRight(z)(f)

  override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) ⇒ B): B =
    as.foldLeft(z)(f)

  override def foldMap[A, B](as: List[A])(f: A ⇒ B)(mb: Monoid[B]): B =
    foldLeft(as)(mb.zero)((b, a) ⇒ mb.op(b, f(a)))

  override def toList[A](as: List[A]): List[A] = as
}

object IndexedSeqFoldable extends Foldable[IndexedSeq] {

  import Monoid.foldMapV

  override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) ⇒ B): B =
    as.foldRight(z)(f)

  override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) ⇒ B): B =
    as.foldLeft(z)(f)

  override def foldMap[A, B](as: IndexedSeq[A])(f: A ⇒ B)(mb: Monoid[B]): B =
    foldLeft(as)(mb.zero)((b, a) ⇒ mb.op(b, f(a))) //foldMapV(as, mb)(f)
}

object StreamFoldable extends Foldable[Stream] {

  override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) ⇒ B): B =
    as.foldRight(z)(f)

  override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) ⇒ B): B =
    as.foldLeft(z)(f)
}

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object TreeFoldable extends Foldable[Tree] {

  override def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B = as match {
    case Leaf(a) ⇒ f(a)
    case Branch(l, r) => mb.op(foldMap(l)(f)(mb), foldMap(r)(f)(mb))
  }

  override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) ⇒ B): B = as match {
    case Leaf(a) ⇒ f(z, a)
    case Branch(l, r) ⇒ foldLeft(r)(foldLeft(l)(z)(f))(f)
  }

  override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) ⇒ B): B = as match {
    case Leaf(a) ⇒ f(a, z)
    case Branch(l, r) ⇒ foldRight(l)(foldRight(r)(z)(f))(f)
  }
}

object OptionFoldable extends Foldable[Option] {
  override def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B =
    as match {
      case None => mb.zero
      case Some(a) => f(a)
    }

  override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B) = as match {
    case None => z
    case Some(a) => f(z, a)
  }

  override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B) = as match {
    case None => z
    case Some(a) => f(a, z)
  }
}

