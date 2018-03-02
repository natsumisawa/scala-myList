package app.model

import scala.annotation.tailrec

sealed trait MyList[+A] {
  def head: A = this match {
    case Nil => throw new NoSuchElementException
    case Cons(h, t) => h
  }

  def tail: MyList[A] = this match {
    case Nil => throw new NoSuchElementException
    case Cons(h, t) => t
  }

  def last: A = this match {
    case Nil => throw new NoSuchElementException
    case Cons(h, Nil) => h
    case Cons(h, t) => t.last
  }

  def inits: MyList[A] = this match {
    case Nil => throw new NoSuchElementException
    case Cons(h, Nil) => Nil
    case Cons(h ,t) => Cons(h, t.inits)
  }

  def isEmpty: Boolean = this match {
    case Nil => false
    case Cons(h,t) => true
  }

  def ::[B >: A](x: B): MyList[B] = this match {
    case Nil => Cons(x, Nil)
    case Cons(h, t) => Cons(x, this)
  }

  def :::[B >: A](ls: MyList[B]): MyList[B] = this match {
    case Nil => ls
    case Cons(h, t) => {
      if (ls.tail == Nil) this.::(ls.head)
      else  Cons(ls.head, (this.:::(ls.tail)))
    }
  }

  def reverse: MyList[A] = this match {
    case Nil => Nil
    case Cons(h, t) => this.foldLeft(Nil: MyList[A])((acc, x) => x :: acc)
  }

  def map[B](f: A => B): MyList[B] = this match {
    case Nil => Nil
    case Cons(h, t) => this.foldRight(Nil: MyList[B])((x, acc) => f(x) :: acc)
  }

  // NOTE(sawa): クラス内のメソッドを末尾再起にする場合サブクラスでオーバーライドすると中身が変わってしまうので、そのメソッドをprivateにすればアノテーションのエラーを回避できる
  def foldLeft[B](acc: B)(f: (B, A) => B): B = {
   def loop(acc: B, ls: MyList[A]): B = ls match {
     case Nil => acc
     case Cons(h, t) => loop(f(acc, h), t)
   }
   loop(acc, this)
  }

  // NOTE(sawa): [WIP]末尾再帰ver断念
  def foldRight[B](zero: B)(f: (A, B) => B): B = {
    def loop(F: (A,B) => B)(ls: MyList[A]): Function1[B, B] = ls match {
      case Nil => (zero: B) => F(zero)
      // F(zero) => f(h, f(h, zero))
      case Cons(h, t) => loop((h, acc: B) => F(h, acc))(t)
      // 最初のf = f(A, B => B)
      // 次のf = f(f(A, B) => )
    }
    val F = loop((zero, x) => f(zero, x))(this)
    F(zero)
  }

  def foldRight[B](acc: B)(f: (A, B) => B): B = this match {
    case Nil => acc
    case Cons(h, t) => f(h, t.foldRight(acc)(f))
  }
}

object Nil extends MyList[Nothing]

case class Cons[A](h: A, t: MyList[A]) extends MyList[A]
