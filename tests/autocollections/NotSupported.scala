import scala.collection.AutoCollections._


object NotSupported {
  def main(args: Array[String]): Unit = {
    val seq = AutoSeq(1, 2)(Lazy)   // Error
    val set = AutoSet(1, 2)(Lazy)   // Error
    val map = AutoMap(1 -> 2)(Lazy) // Error
  }
}
