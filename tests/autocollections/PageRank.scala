//import scala.collection.AutoCollections._

object PageRank {

  type PageRank[T] = Map[T, Double]

  val MaxIteration  = 50
  val DiffTolerance = 1E-6
  val D             = 0.85 // Damping factor

  def apply[T](pages: Map[T, Set[T]]): PageRank[T] = {
    // Pages with no outbound links are assumed to link out to all other pages in the collection
    val pagesNoSink = pages.mapValues(links => if (links.isEmpty) pages.keySet else links)
    val n           = pages.size
    val ls          = pagesNoSink.mapValues(_.size) // number of outbound links on page p_i
    val ms          = incomingLinks(pagesNoSink)    // set of pages that link to p_i

    var iteration = 0
    var lastRanks = pages.mapValues(_ => Double.MaxValue)
    var ranks     = pages.mapValues(_ => 1.0 / n)


    while (iteration < MaxIteration && diff(lastRanks, ranks) > DiffTolerance) {
      lastRanks = ranks
      ranks = pages.map {
        case (id, _) =>
          val pr = (1 - D) / n + D * ms(id).map(link => ranks(link) / ls(link)).sum
          id -> pr
      }

      println(s"Iteration $iteration: $ranks")
      iteration += 1
    }

    ranks
  }

  private def diff[T](pr1: PageRank[T], pr2: PageRank[T]): Double = {
    pr1.map {
      case (id, r1) =>
        val diff = r1 - pr2(id)
        diff * diff
    }.sum
  }

  /** Compute the set of pages that link to each page */
  private def incomingLinks[T](pages: Map[T, Set[T]]): Map[T, Set[T]] = {
    //val links = pages.mapValues(_ => AutoSet[T]()(AutoSet.Mutable)).map(identity)
    val links = pages.mapValues(_ => collection.mutable.Set.empty[T]).map(identity)

    for {
      (id, ls) <- pages
      link     <- ls
    } {
      links(link) += id
    }

    links.mapValues(ls => ls.toSet)
  }

  def main(args: Array[String]): Unit = {
    val A = 1L
    val B = 2L
    val C = 3L
    val D = 4L

    val pages: Map[Long, Set[Long]] = AutoMap( // FIXME: AutoMap
      A -> Set(D),
      B -> Set(A, C, D),
      C -> Set(B, D),
      D -> Set.empty[Long]
    )

    println(PageRank.apply(pages))
  }
}
