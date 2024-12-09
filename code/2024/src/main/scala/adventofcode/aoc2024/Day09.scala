package adventofcode
package aoc2024

import adventofcode.utilities.AdventOfCode.*

object Day09 extends AdventOfCode(Prod):
  val diskMap: DiskMap =
    DiskMap.fromString(input)

  type DiskMap = Vector[Int]

  object DiskMap:
    def fromString(s: String): DiskMap =
      @tailrec def loop(
        todo: Vector[(Char, Int)],
        fileId: Int,
        acc: DiskMap
      ): DiskMap =
        todo.headOption match
          case Some(h, i) if i % 2 == 0 =>
            loop(todo.tail, fileId + 1, acc ++ Vector.fill(h.asDigit)(fileId))
          case Some(h, _) =>
            loop(todo.tail, fileId, acc ++ Vector.fill(h.asDigit)(-1))
          case None => acc

      loop(s.zipWithIndex.toVector, 0, Vector.empty[Int])

    extension (self: DiskMap)
      @tailrec def clean: DiskMap =
        self.last match
          case -1 => self.init.clean
          case _  => self

      def fragment: DiskMap =
        @tailrec def loop(diskMap: DiskMap, i: Int): DiskMap =
          if i >= diskMap.length then diskMap
          else if diskMap(i) != -1 then loop(diskMap, i + 1)
          else loop(diskMap.updated(i, diskMap.last).init.clean, i + 1)

        loop(self, 0)

      def defragment: DiskMap =
        val files: Map[Int, Int] =
          self.groupMapReduce(identity)(_ => 1)(_ + _)

        @tailrec def loop(diskMap: DiskMap, fileId: Int): DiskMap =
          if fileId <= 0 then diskMap
          else
            val blocks = files(fileId)
            val free   = diskMap.indexOfSlice(Vector.fill(blocks)(-1))
            val file   = diskMap.indexOf(fileId)

            if free == -1 || free >= file then loop(diskMap, fileId - 1)
            else
              loop(
                diskMap
                  .patch(free, Vector.fill(blocks)(fileId), blocks)
                  .patch(file, Vector.fill(blocks)(-1), blocks),
                fileId - 1
              )

        loop(self, self.max)

      def filesystemChecksum: Long =
        self.zipWithIndex.foldLeft(0L):
          case (acc, (b, i)) =>
            if b == -1 then acc else acc + b * i

  import DiskMap.*

  lazy val pt1: Long =
    diskMap
      .fragment
      .filesystemChecksum

  lazy val pt2: Long =
    diskMap
      .defragment
      .filesystemChecksum

  answer(1)(pt1)

  answer(2)(pt2)
