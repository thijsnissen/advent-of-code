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

    def find(p: A => Boolean): Option[A] =
      iterate
        .dropWhile(a => !p(a))
        .nextOption()

    def swap(x1: Int, y1: Int)(x2: Int, y2: Int): Grid[A] =
      self
        .set(x1, y1)(self.get(x2, y2))
        .set(x2, y2)(self.get(x1, y1))

    def findWithIndex(p: ((Int, Int, A)) => Boolean): Option[(Int, Int, A)] =
      iterateWithIndex
        .dropWhile(a => !p(a))
        .nextOption()

    def map[B](f: A => B): Grid[B] =
      self.map(_.map(f))

    def flatMap[B](f: A => Vector[B]): Grid[B] =
      self.map(_.flatMap(f))

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

    def count(f: A => Boolean): Int =
      self.map(_.count(f)).sum

    def exists(f: A => Boolean): Boolean =
      self.count(f) > 0

    def contains(a: A): Boolean =
      self.exists((_: A) == a)

    def size: (Int, Int) =
      (self(0).length, self.length)

    def iterate: Iterator[A] =
      for
        y <- self.iterator
        x <- y.iterator
      yield x

    def iterateWithIndex: Iterator[(Int, Int, A)] =
      for
        (r, y) <- self.iterator.zipWithIndex
        (c, x) <- r.iterator.zipWithIndex
      yield (x, y, c)

    def transpose: Grid[A] =
      self.transpose

    def rotateClockwise: Grid[A] =
      self.transpose.map(_.reverse)

    def findPos(t: A): Pos =
      self
        .findWithIndex((_, _, a) => a == t)
        .map((x, y, _) => Pos(x, y))
        .get

    def asString: String =
      self.map(_.mkString(" ")).mkString("\n", "\n", "\n")
