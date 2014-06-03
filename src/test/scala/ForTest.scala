import org.scalatest.FunSuite

class ForTest extends FunSuite {

    import Desugar._

    val list1 = (1 to 10).toList
    val list2 = (11 to 20).toList

    test("for without yield") {
        val v1 = desugar {
            for (i <- list1) i
        }
        println(v1)

        val v2 = desugar {
            list1.foreach(i => i)
        }

        assert(v1 === v2)

    }

    test("for without yield 2") {
        val v1 = desugar {
            for {
                i <- list1
                j <- list2
            } ()
        }
        println(v1)

        val v2 = desugar {
            list1.foreach {
                i => list2.foreach(j => ())
            }
        }

        assert(v1 === v2)
    }

    test("for with yield") {
        val v1 = desugar {
            for (i <- list1) yield i
        }
        println(v1)

        val v2 = desugar {
            list1.map(i => i)
        }

        assert(v1 === v2)
    }

    test("for with yield 2") {
        val v1 = desugar {
            for {
                i <- list1
                j <- list2
            } yield i
        }
        println(v1)

        val v2 = desugar {
            list1.flatMap {
                i =>
                    list2.map {
                        j => i
                    }
            }
        }

        assert(v1 === v2)
    }

    test("for with yield and if") {
        val v1 = desugar {
            for {
                i <- list1
                if i > 5
                j <- list2
                if i < j
            } yield i
        }
        println(v1)

        val v2 = desugar {

            list1.withFilter(i => i > 5).flatMap {
                i =>
                    list2.withFilter(j => i < j).map {
                        j => i
                    }
            }
        }

        assert(v1 === v2)
    }

    test("for falls back to filter if withFilter is not there") {

        object CustomList {
            def filter(test: Int => Boolean) = this
            def withFilter(test: Int => Boolean) = this
            def map(test: Int => Int) = this
        }

        val v1 = desugar {
            for {
                i <- CustomList
                if i > 5
            } yield i
        }
        println(v1)

    }

    test("for with yield and assignment") {
        val v1 = desugar {
            for {
                i <- list1
                x = i
                j <- list2
                y = j
            } yield x + y
        }
        println(v1)

        val v2 = desugar {
            list1.map {
                i =>
                    val x = i
                    val y = i
                    val z = i
                    (i, x, y, z)
            }.map {
                _ match {
                    case (i: Int, x: Int, y: Int, z: Int) => x + y + z
                }
            }
        }

//        println(v2)
//        assert(v1 === v2)
    }


}
