import annotation.tailrec

/**
 * User: haljik
 * Date: 2013/03/21
 * Time: 14:53
 */
object KarateChop {

  def sliceChop[A <% Ordered[A]](needle: A, list: Array[A]): Int = {
    @tailrec
    def chop(search: A, list: Array[A], discarded:Int): Option[Int] = {
      list.length match {
        case 0 => None
        case _ =>
          list.length / 2  match {
            case pivot if list(pivot) == search => Some(discarded + pivot)
            case pivot if list(pivot) >  search => chop(search, list.slice(0,pivot),discarded)
            case pivot if list(pivot) <  search => chop(search, list.slice(pivot + 1, list.length), discarded + pivot + 1)
          }
      }
    }
    chop(needle,list,0) match {
      case None => -1
      case Some(x) => x
    }
  }

  def rangeChop[A <% Ordered[A]](needle: A, list: Array[A]): Int = {
    @tailrec
    def chop(needle: A, list:Array[A], startIndex:Int, endIndex:Int): Option[Int] = {
      if (endIndex < startIndex) {
        return None
      }
      if (list(endIndex) < needle) {
        return None
      }
      if (list(startIndex) > needle) {
        return None
      }
      val pivot = startIndex + ((endIndex - startIndex) / 2)
      pivot match {
        case p if list(p) == needle => Some(p)
        case p if list(p) >  needle => chop(needle, list, startIndex, pivot - 1)
        case p if list(p) <  needle => chop(needle, list, pivot + 1 , endIndex)
      }
    }
    chop(needle,list,0,list.length - 1) match {
      case None    => -1
      case Some(x) => x
    }
  }

}
