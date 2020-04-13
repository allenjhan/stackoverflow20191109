object ListOfListsTest {
  def main(args: Array[String]) = {
    val myList = List(List(Some(1),Some(2),Some(3),Some(4)),List(Some(1),Some(2),Some(3),Some(4)),List(Some(1),Some(2),Some(3),Some(4)))
    println(regroup(myList))
  }

  def regroup[A](list: List[List[A]]): List[List[A]] = {
    def regroupInner[A](l: List[List[A]], acc: List[List[A]]): List[List[A]] = {
      val split = l.flatMap{xs =>
        xs match {
          case y::ys => Some((y, ys))
          case Nil => None
        }
      }
      val splitHead = split.map(_._1)
      val splitTail = split.map(_._2)

      splitTail match {
        case Nil =>
          splitHead match {
            case Nil => acc
            case z::zs => splitHead::acc
          }
        case w::ws =>
          regroupInner(splitTail, splitHead::acc)
      }
    }

    regroupInner(list, List()).reverse
  }


}
