package adventofcode
package utilities

opaque type Grid[A] = Vector[Vector[A]]

object Grid:
  def unit[A](grid: Vector[Vector[A]]): Grid[A] =
    grid

  def empty[A]: Grid[A] =
    Vector.empty[Vector[A]]

  def fill[A](x: Int, y: Int)(value: => A): Grid[A] =
    Vector.fill(y, x)(value)

  extension [A](self: Grid[A])
    def apply(x: Int)(y: Int): A =
      self.get(x, y)

    def apply(x: Int, y: Int)(value: => A): Grid[A] =
      self.set(x, y)(value)

    def set(x: Int, y: Int)(value: => A): Grid[A] =
      self.updated(y, self(y).updated(x, value))

    def get(x: Int, y: Int): A =
      self(y)(x)

    def getRow(y: Int): Vector[A] =
      self(y)

    def getCol(x: Int): Vector[A] =
      self.map(_(x))

    def lift(x: Int, y: Int): Option[A] =
      for
        y <- self.lift(y)
        x <- y.lift(x)
      yield x

    def map[B](f: A => B): Grid[B] =
      self.map(_.map(f))

    def count(f: A => Boolean): Int =
      self.map(_.count(f)).sum

    def exists(f: A => Boolean): Boolean =
      self.count(f) > 0

    def contains(a: A): Boolean =
      self.exists((_: A) == a)

    def size: (Int, Int) =
      (self(0).length, self.length)

    def iterator: Iterator[A] =
      for
        y <- self.iterator
        x <- y.iterator
      yield x

    def mapWithIndex[B](f: (A, Int, Int) => B): Grid[B] =
      self
        .zipWithIndex
        .map: (va: Vector[A], y: Int) =>
          va
            .zipWithIndex
            .map: (a: A, x: Int) =>
              f(a, x, y)

    def flatten: Vector[A] =
      self.flatten

    def transpose: Grid[A] =
      self.transpose

    def rotateClockwise: Grid[A] =
      self.transpose.map(_.reverse)

    def asString: String =
      self.map(_.mkString(" ")).mkString("\n", "\n", "\n")
