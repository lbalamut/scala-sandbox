import scala.reflect.macros.whitebox.Context
import scala.reflect.runtime.universe._
import scala.language.experimental.macros

object Desugar {

    def desugarImpl(c : Context)(expr : c.Expr[Any]): c.Expr[Unit] = {
        import c.universe._
        println(show(expr.tree))
        reify {}
    }

    def desugar(expr : Any): Unit = macro desugarImpl
}
