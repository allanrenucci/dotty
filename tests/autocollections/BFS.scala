import scala.collection.AutoCollections._

object BFS {
  sealed trait Tree
  case class Node(x: Int, left: Tree = Leaf, right: Tree = Leaf) extends Tree
  case object Leaf extends Tree

  def apply(tree: Tree): Unit = {
    var toVisit = AutoSeq(tree)

    while (toVisit.nonEmpty) {
      val visiting = toVisit.head
      toVisit = toVisit.tail

      visiting match {
        case Node(x, left, right) =>
          toVisit = toVisit :+ left
          toVisit = toVisit :+ right
          println(x)
        case _ =>
          ()
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val tree =
      Node(1,
        Node(2,
          Node(4),
          Node(5)
        ),
        Node(3,
          Node(6),
          Node(7)
        )
      )

    BFS(tree)
  }
}
