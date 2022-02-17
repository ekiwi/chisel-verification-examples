import firrtl.backends.experimental.smt._
import chiseltest.formal.backends.smt._
import org.scalatest.freespec.AnyFreeSpec

/** This example demonstrates the SMTSolver interface that underlies our
 *  formal verification system.
 */
class SMTSolverExample extends AnyFreeSpec {
  val a = BVSymbol("a", 1)
  val b = BVSymbol("b", 1)


  "check simple formulae" in {
    assert(solve(a).nonEmpty)
    assert(solve(BVAnd(a, BVNot(a))).isEmpty)
    val r0 = solve(BVEqual(BVLiteral(1, 1), BVOp(Op.Add, a, b)))
    assert(r0.nonEmpty)
    assert(((r0("a") + r0("b")) & 1) == 1)
  }
}


/** helper function to easily check SMT formulas and return all assignments */
object solve {
  private val verbose: Boolean = false

  def apply(expr: BVExpr): Map[String, BigInt] = {
    require(expr.width == 1, s"Expression needs to be boolean, not ${expr.width}-bits wide.")
    val symbols = findSymbols(expr).distinct
    val solver = Z3SMTLib.createContext()
    solver.setLogic("ALL")
    symbols.foreach(s => solver.runCommand(DeclareFunction(s, Seq())))
    solver.assert(expr)
    val r = solver.check()
    r.toString match { // small hack because the result is package private
      case "IsSat" =>
        val values = symbols.map(s => s.name -> solver.getValue(s).get)
        val vStr = values.map{ case (n,v) => s"$n=$v" }.mkString(", ")
        solver.close()
        if(verbose) println(s"$expr is sat: $vStr")
        values.toMap
      case "IsUnSat" =>
        solver.close()
        if(verbose) println(s"$expr is unsat")
        Map()
    }
  }

  private def findSymbols(e: SMTExpr): Seq[BVSymbol] = e match {
    case s: BVSymbol    => Seq(s)
    case _: ArraySymbol => ???
    case other => other.children.flatMap(findSymbols)
  }
}