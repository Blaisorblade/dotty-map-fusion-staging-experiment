import scala.quoted._

object PipeExample {
  sealed trait Pipe[A] { def toList: List[A] }
  case class Source[A](toList: List[A]) extends Pipe[A]

  // case class Mapped[A, B](first: Pipe[A], fn: A => B) extends Pipe[B] {
  //   def toList = first.toList.map(fn)
  // }

  // def mapImpl[A: Type, B: Type] (first: Expr[Pipe[A]], fn: Expr[A => B])(using qctx: QuoteContext): Expr[Pipe[B]] = {
  //   first match {
  //     case '{ Mapped.apply($iFirst, $iFn) } =>
  //       '{ Mapped($iFirst, $iFn andThen $fn) }
  //     case _ =>
  //       '{ Mapped($first, $fn) }
  //   }
  // }

  // extension pipeOps {
  //   inline def [A, B](inline first: => Pipe[A]).map(inline fn: => A => B): Pipe[B] =
  //     ${ mapImpl('{ first }, '{ fn }) }
  // }

  import scala.tasty._
  case class Mapped[A, B](first: Pipe[A], fn: A => B) extends Pipe[B] {
    def toList = first.toList.map(fn)
  }
  
  private def mapImpl[A: Type, B: Type](first: Expr[Pipe[A]], fn: Expr[A => B])(using qctx: QuoteContext): Expr[Pipe[B]] = 
    first match {
      case '{ Mapped.apply[$a, A]($first, $lfn): Pipe[A] } =>
        mapImpl(first, '{ (aa: $a) => ${ Expr.betaReduce(fn)(Expr.betaReduce(lfn)('{ aa })) } })
      case c =>
        qctx.warning(s"Got unsupported expression: ${c.show}", c)
        '{ Mapped[A, B]($first, $fn) }
    }
  
  private def logTree[A](expr: Expr[A])(using QuoteContext): Expr[A] = {
    println(s"Optimised code: ${expr.show}")
    expr
  }

  extension pipeOps {
    inline def [A, B](inline first: Pipe[A]).map(inline fn: A => B): Pipe[B] =
      ${ logTree(mapImpl('{ first }, '{ fn })) }
  }
}
