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
  case class Mapped(first: Pipe[Int], fn: Int => Int) extends Pipe[Int] {
    def toList = first.toList.map(fn)
  }

  def mapImpl (first: Expr[Pipe[Int]], fn: Expr[Int => Int])(using qctx: QuoteContext): Expr[Pipe[Int]] = {
    import qctx.tasty._
    def go(trm : Term): Expr[Pipe[Int]] = {
      import qctx.tasty._
      trm match {
        case Typed(t, _) =>
          go(t)
        case Inlined(_, _, t) =>
          go(t)
        case Apply(mapped, args@List(iFirst, iFn)) =>
          //  qctx.tasty.warning(s"debug4 ${mapped.showExtractors} ", mapped.pos)
          //  qctx.warning(s"debug5 ${args.map(_.showExtractors)} ${args.length}")
          '{ Mapped(${iFirst.seal.cast[Pipe[Int]]}, ${iFn.seal.cast[Int => Int]} andThen $fn) }
        case _ =>
          //qctx.tasty.warning(s"debug6 ${trm.showExtractors}", trm.pos)
          '{ Mapped($first, $fn) }
      }
    }
    go(first.unseal)
  }

  extension pipeOps {
    inline def (inline first: => Pipe[Int]).map(inline fn: => Int => Int): Pipe[Int] =
      ${ mapImpl('{ first }, '{ fn }) }
  }
}
