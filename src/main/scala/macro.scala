import scala.quoted._
import scala.tasty._

//import scala.quoted.staging._
object PipeExample {
  sealed trait Pipe[A] { def toList: List[A] }
  case class Source[A](toList: List[A]) extends Pipe[A]
  // case class Mapped[A, B](first: Pipe[A], fn: A => B) extends Pipe[B]
  // def mapImpl[A: Type, B: Type] (first: Expr[Pipe[A]], fn: Expr[A => B])(using qctx: QuoteContext): Expr[Pipe[B]] = {
  //   first match {
  //     case '{ Mapped.apply($iFirst, $iFn) } =>
  //       qctx.warning("debug2 ", first)
  //       '{ Mapped($iFirst, $iFn andThen $fn) }
  //     case _ =>
  //       qctx.warning(s"debug ${first.show}", first)
  //       '{ Mapped($first, $fn) }
  //   }

  // }

  // extension pipeOps {
  //   inline def [A, B](inline first: => Pipe[A]).map(inline fn: => A => B): Pipe[B] =
  //     ${ mapImpl('{ first }, '{ fn }) }
  //     //${ '{Mapped('first, fn)} }
  // }

  // inline def compile[A](p: => Pipe[A])(using Toolbox): List[A] =
  //   run(p.toExpr)
  // def main(args: Array[String]) = {
  //   //Source(List(1, 2, 3)).map(x => x + 1).map(x => x + 2)
  // }
  case class Mapped(first: Pipe[Int], fn: Int => Int) extends Pipe[Int] {
    def toList = first.toList.map(fn)
  }

  def mapImpl (first: Expr[Pipe[Int]], fn: Expr[Int => Int])(using qctx: QuoteContext): Expr[Pipe[Int]] = {
    import qctx.tasty._
    // qctx.warning(s"debug3 ${first.unseal.showExtractors}", first)

    // first.unseal match {
    //   case Apply(iFirst, iFn) =>
    //     qctx.warning("debug2 ", first)
    //     '{ Mapped($first, $fn) }
    //   case _ =>
    //     qctx.warning(s"debug ${first.show}", first)
    //     '{ Mapped($first, $fn) }
    // }

    def go(trm : Term): Expr[Pipe[Int]] = {
      import qctx.tasty._
      // qctx.warning(s"debug0 ${trm.showExtractors}")
      trm match {
        case Typed(t, _) =>
          go(t)
        case Inlined(_, _, t) =>
          go(t)
        // case Apply(mapped, List(iFirst, iFn)) =>
        //   qctx.error(s"debug2 ${iFirst.showExtractors} ${iFn.showExtractors}", first)
        //  '{ Mapped(${iFirst.seal.cast[Pipe[Int]]}, ${iFn.seal.cast[Int => Int]} andThen $fn) }
        case Apply(mapped, args@List(iFirst, iFn)) =>
           qctx.tasty.warning(s"debug4 ${mapped.showExtractors} ", mapped.pos)
          //  qctx.warning(s"debug4 ${mapped.showExtractors} ")
           qctx.warning(s"debug5 ${args.map(_.showExtractors)} ${args.length}")
          //'{ Mapped($first, $fn) }
          '{ Mapped(${iFirst.seal.cast[Pipe[Int]]}, ${iFn.seal.cast[Int => Int]} andThen $fn) }
        case _ =>
          qctx.tasty.warning(s"debug6 ${trm.showExtractors}", trm.pos)
          '{ Mapped($first, $fn) }
      }
    }
    go(first.unseal)
    // first match {
    //   case '{ Mapped($iFirst, $iFn) } =>
    //     qctx.warning("debug2 ", first)
    //     '{ Mapped($iFirst, $iFn andThen $fn) }
    //   case _ =>
    //     qctx.warning(s"debug ${first.show}", first)
    //     '{ Mapped($first, $fn) }
    // }
  }
  // def mapImpl (first: Expr[Pipe[Int]], fn: Expr[Int => Int])(using qctx: QuoteContext): Expr[Pipe[Int]] = {
  //   first match {
  //     case '{ Mapped($iFirst, $iFn) } =>
  //       qctx.warning("debug2 ", first)
  //       '{ Mapped($iFirst, $iFn andThen $fn) }
  //     case _ =>
  //       qctx.warning(s"debug ${first.show}", first)
  //       '{ Mapped($first, $fn) }
  //   }
  // }

  extension pipeOps {
    inline def (inline first: => Pipe[Int]).map(inline fn: => Int => Int): Pipe[Int] =
      ${ mapImpl('{ first }, '{ fn }) }
      //${ '{Mapped('first, fn)} }
  }
}
