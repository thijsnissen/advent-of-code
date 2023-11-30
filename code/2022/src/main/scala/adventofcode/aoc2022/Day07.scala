package adventofcode
package aoc2022

import utilities.AdventOfCode.*
import utilities.Utilities.sumBy

object Day07 extends AdventOfCode(Prod):
  val fileSystem: FileSystem =
    FileSystem.fromInput(input.linesIterator.toVector)

  case class FileSystem(
    dirs: Set[String],
    files: Map[String, Int],
    currDir: List[String],
  ):
    lazy val totalDiskSpace: Int =
      70000000

    def totalUsedSpace: Int =
      files.sumBy((_, size) => size)

    def totalUnusedSpace: Int =
      totalDiskSpace - totalUsedSpace

    def getDirSizes: Map[String, Int] =
      dirs
        .map: dir =>
          val size: Int =
            files
              .filter((file, _) => file.startsWith(dir))
              .sumBy((_, fSize) => fSize)

          dir -> size
        .toMap

  object FileSystem:
    def empty: FileSystem =
      FileSystem(Set.empty[String], Map.empty[String, Int], List("/"))

    def fromInput(input: Vector[String]): FileSystem =
      input.foldLeft(FileSystem.empty):
        case (acc, "$ ls")      => acc
        case (acc, s"$$ cd ..") => acc.copy(currDir = acc.currDir.tail)
        case (acc, s"$$ cd $dir") =>
          acc.copy(currDir = s"${acc.currDir.head}/$dir" :: acc.currDir)
        case (acc, s"dir $dir") =>
          acc.copy(dirs = acc.dirs + s"${acc.currDir.head}/$dir")
        case (acc, s"$size $file") => acc.copy(files =
            acc.files + (s"${acc.currDir.head}/$file" -> size.toInt)
          )
        case (acc, _) => acc

  lazy val pt1: Int =
    fileSystem
      .getDirSizes
      .filter((_, size) => size <= 100000)
      .sumBy((_, size) => size)

  lazy val pt2: Int =
    val (_, directorySize) =
      fileSystem
        .getDirSizes
        .filter((_, size) => fileSystem.totalUnusedSpace + size >= 30000000)
        .minBy((_, size) => size)

    directorySize

  answer(1)(pt1)

  answer(2)(pt2)
