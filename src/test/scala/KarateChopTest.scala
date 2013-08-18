import org.scalatest.FunSuite
import org.scalatest.prop.TableDrivenPropertyChecks
/**
 * User: haljik
 * Date: 2013/07/13
 * Time: 18:49
 */
class KarateChopTest extends FunSuite with TableDrivenPropertyChecks {

  val fixtures = Table(
    ("expect","search","list"),
    (-1, 0,  Array(1,2,3,4,5,6,7,8,10,11,12,13,14,15)),
    (-1, 9,  Array(1,2,3,4,5,6,7,8,10,11,12,13,14,15)),
    (-1, 16, Array(1,2,3,4,5,6,7,8,10,11,12,13,14,15)),
    (0,  1,  Array(1,2,3,4,5,6,7,8,10,11,12,13,14,15)),
    (7,  8,  Array(1,2,3,4,5,6,7,8,10,11,12,13,14,15)),
    (8,  10, Array(1,2,3,4,5,6,7,8,10,11,12,13,14,15)),
    (13, 15, Array(1,2,3,4,5,6,7,8,10,11,12,13,14,15))
  )

  import org.scalatest.matchers.ShouldMatchers._
  forAll(fixtures) { (expect:Int , search:Int, list:Array[Int]) =>
    test( s"${search} を (${ list.mkString(",") }) の中から探索した場合 ${expect} を返す") {
      KarateChop.sliceChop(search,list) should equal(expect)
      KarateChop.rangeChop(search,list) should equal(expect)
    }
  }

}
