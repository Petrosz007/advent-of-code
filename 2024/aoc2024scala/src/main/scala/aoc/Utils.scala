package aoc

object Utils:
  def iterate[A](n: Int, init: A)(f: (Int, A) => A): A =
    (0 until n).foldLeft(init)((acc, n) => f(n, acc))

  def iterate[A](n: Int, init: A)(f: A => A): A =
    (0 until n).foldLeft(init)((acc, _) => f(acc))
