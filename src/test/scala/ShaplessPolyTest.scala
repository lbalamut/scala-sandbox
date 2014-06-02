import org.scalatest.FunSuite
import shapeless.ops.hlist.Mapper.Aux
import shapeless.poly._
import shapeless._
import shapeless.Nat._

class ShaplessPolyTest extends FunSuite {

    test("HList examples") {
        val l = 1 :: "2" :: 3.0 :: HNil

        val h = l.tail.head

        val str = l.apply[_2]
        //won't compile
//        val foo = l[_4]

        val t = l.take[_2]
        println(t)
    }

    object headOption extends (Set ~> Option) {
        override def apply[T](s : Set[T]) = s.headOption
    }

    test("simple polymorphic function") {
        val c1 = headOption(Set(1, 2, 3))
        assert(c1 === Some(1))

        val c2 = headOption(Set('a', 'b', 'c'))
        assert(c2 === Some('a'))
    }

    object toInt extends Poly1 {
        implicit def caseInt = at[Int](identity)
        implicit def caseString = at[String](_.toInt)
        implicit def caseDouble = at[Double](_.toInt)
        implicit def caseTuple[T, U](implicit st : Case.Aux[T, Int], su : Case.Aux[U, Int]) =
            at[(T, U)](t => toInt(t._1) + toInt(t._2))
    }

    test("sum ints of hlist") {
        val l = 1 :: "2" :: 3.0 :: (4, "5") :: HNil

        val s1 = l.map(toInt).toList.reduce(_ + _)

        assert(s1 === 15)
    }

    object addInts extends Poly2 {
        implicit  def default[T](implicit st: toInt.Case.Aux[T, Int]) =
            at[Int, T]{ (acc, t) => acc + toInt(t) }
    }

    test("sum sizes of hlist 2") {

        val l = 1 :: "2" :: 3.0 :: (4, "5") :: HNil

        val s = l.foldLeft(0)(addInts)

        assert(s === 15)
    }

    import shapeless.syntax.std.tuple._

    test("sum sizes of tuples") {

        val l = (1 , "2", 3.0, (4, "5"))

        val s = l.foldLeft(0)(addInts)

        assert(s === 15)
    }

    object option extends (Id ~> Option) {
        def apply[T](t: T) = Option(t)
    }

    test("updates type of tuple") {
        assert( (Some(23), Some("foo")) === (23, "foo").map (option))
    }

    test("tuples magic") {
        23 +: ("foo", true)
        (23, "foo") ++ (true, 2.0)
    }

    //cannot be defined inside the test
    case class Foo(i: Int, s: String, b: Boolean)

    test("case classes HList support") {

        val fooGen = Generic[Foo]

        val foo = Foo(23, "foo", true)

        val r: Int :: String :: Boolean :: HNil = fooGen.to(foo)//.toList.foreach(println)
    }
}
