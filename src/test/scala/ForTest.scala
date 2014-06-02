import org.scalatest.FunSuite

class ForTest extends FunSuite {

    import Desugar._

    val list1 = (1 to 10).toList
    val list2 = (11 to 20).toList

    test("desugar for without yield") {
        println (desugar {
            for (i <- list1) i
        })
    }

    test("desugar for without yield 2") {
        println (desugar {
            for {
                i <- list1
                j <- list2
            } ()
        })
    }

    test("desugar for with yield") {
        println (desugar {
            for (i <- list1) yield i
        })
    }

    test("desugar for with yield 2") {
        println (desugar {
            for {
                i <- list1
                j <- list2
            } yield i
        })
    }

    test("desugar for with yield and if") {
        println (desugar {
            for {
                i <- list1
                if i > 5
                j <- list2
                if i < j
            } yield i
        })
    }

    test("desugar for with yield and assignment") {
        println (desugar {
            for {
                i <- list1
                j <- list2
                x = i + j
            } yield (x, j)
        })
    }

}
