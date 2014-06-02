import scala.reflect.macros.whitebox.Context
import scala.reflect.runtime.universe._
import scala.language.experimental.macros

object Desugar {

    //borrowed from https://github.com/retronym/macrocosm/blob/master/src/main/scala/com/github/retronym/macrocosm/Macrocosm.scala
    def desugarImpl(c : Context)(expr : c.Expr[Any]): c.Expr[String] = {
        import c.universe._
        val code = show(expr.tree)
        c.Expr(
            Literal(Constant(code))
        )
    }

    def desugar(expr : Any): String = macro desugarImpl
}
