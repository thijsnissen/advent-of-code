package adventofcode
package utilities

object GraphTraversal:
  import scala.collection.immutable.SortedSet

  @annotation.tailrec
  def treeToPath[A](target: A, tree: Iterable[(A, A)], acc: List[A]): List[A] =
    tree.find((node, _) => node == target) match
      case Some(to, from) if to == from => target :: acc
      case Some(to, from)               => treeToPath(from, tree, to :: acc)
      case None                         => target :: acc

  private def updateToVisit[A](
    as: SortedSet[A],
    toVisit: Vector[(A, A)],
    visited: Vector[(A, A)],
    from: A,
  ): Vector[(A, A)] =
    as
      .view
      .filterNot(visited.map((node, _) => node).contains)
      .filterNot(toVisit.map((node, _) => node).contains)
      .toVector
      .map(_ -> from)

  extension [A](self: Graph[A])
    def breadthFirstSearch(source: A)(target: A => Boolean)
      : Option[Vector[(A, A)]] =
      if target(source) then
        Some(Vector(source -> source))
      else
        loop(
          Vector.empty[(A, A)] ++ self.run(source).toVector.map(_ -> source),
          Vector(source -> source),
          false,
        )(target)

    def breadthFirstSearchPathTo(source: A)(target: A => Boolean)
      : Option[List[A]] =
      breadthFirstSearch(source)(target) match
        case Some(tree) =>
          val (node, _) =
            tree.find((n, _) => target(n)).get

          Some(treeToPath(node, tree, List.empty[A]))
        case None => None

    def depthFirstSearch(source: A)(target: A => Boolean)
      : Option[Vector[(A, A)]] =
      if target(source) then
        Some(Vector(source -> source))
      else
        loop(
          Vector.empty[(A, A)] ++ self.run(source).toVector.map(_ -> source),
          Vector(source -> source),
          true,
        )(target)

    @annotation.tailrec
    private def loop(
      toVisit: Vector[(A, A)],
      visited: Vector[(A, A)],
      lifo: Boolean,
    )(target: A => Boolean): Option[Vector[(A, A)]] =
      toVisit match
        case (visit, from) +: _ if target(visit) =>
          Some(((visit -> from) +: visited).reverse)
        case (visit, from) +: tail if lifo =>
          loop(
            updateToVisit(self.run(visit), toVisit, visited, visit) ++ tail,
            (visit, from) +: visited,
            lifo,
          )(target)
        case (visit, from) +: tail =>
          loop(
            tail ++ updateToVisit(self.run(visit), toVisit, visited, visit),
            (visit, from) +: visited,
            lifo,
          )(target)
        case _ =>
          None
