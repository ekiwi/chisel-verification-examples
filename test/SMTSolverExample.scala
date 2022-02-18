import firrtl.backends.experimental.smt._
import chiseltest.formal.backends.smt._
import org.scalatest.freespec.AnyFreeSpec

/** This example demonstrates the SMTSolver interface that underlies our
 *  formal verification system.
 */
class SMTSolverExample extends AnyFreeSpec {



  "check simple formulae" in {
    val a = BVSymbol("a", 1)
    val b = BVSymbol("b", 1)
    assert(solve(a).nonEmpty)
    assert(solve(BVAnd(a, BVNot(a))).isEmpty)
    val r0 = solve(BVEqual(BVLiteral(1, 1), BVOp(Op.Add, a, b)))
    assert(r0.nonEmpty)
    assert(((r0("a") + r0("b")) & 1) == 1)
  }


  "demonstrate two ways of subtracting" in {
    val a = BVSymbol("a", 32)
    val b = BVSymbol("b", 32)

    val d0 = BVOp(Op.Sub, a, b)
    val d1 = BVOp(Op.Add, a, BVNot(b))

    // We want to prove that for all arguments a and b, d0 === d1.
    // This mean that there does not exist arguments a and b such that d0 =/= d1
    // In other words:
    // ∀ x . f(x)  <==> ¬(∃ x . ¬(f(x)))
    // A SAT/SMT solver has two possible results (assuming it terminates):
    // (1) an assignment that makes the formula true
    // (2) unsat --> the formula is always false
    //     i.e. there does not exist arguments a and b such that the formula can be true

    val assertion0 = BVEqual(d0, d1)
    val r0 = solve(BVNot(assertion0))
    assert(r0.nonEmpty)
    println(s"$d1 =/= $d0, see: $r0")

    // we use the correct formula for two's complement
    val d2 = BVOp(Op.Add, BVLiteral(1, 32), d1)
    val assertion1 = BVEqual(d0, d2)
    val r1 = solve(BVNot(assertion1))
    assert(r1.isEmpty)
  }

  "demonstrate bounded model checking" in {
    // this is based on the keep max circuit:
    // class KeepMax(width: Int) extends Module {
    //  val in = IO(Input(UInt(width.W)))
    //  val max = RegInit(0.U(width.W))
    //  when (in > max) {
    //    max := in
    //  }
    //
    //  // get the value of io.out from 1 cycle in the past
    //  assert(max >= past(max))
    //}

    var counter = 0
    def mkSym(prefix: String, width: Int): BVSymbol = {
      val name = s"${prefix}_$counter" ; counter += 1 ; BVSymbol(name, width)
    }
    case class State(max: BVExpr, pastMax: BVExpr)
    def step(s: State): State = {
      // fresh input
      val in = mkSym("in", 32)
      // calculate next state
      val in_greater_max = BVComparison(Compare.Greater, in, s.max, signed = false)
      val max = BVIte(in_greater_max, in, s.max)
      State(max, pastMax = s.max)
    }

    // initial state
    val s0 = State(mkSym("max", 32), mkSym("pastMax", 32))

    // unroll
    val s1 = step(s0)
    val s2 = step(s1)
    val s3 = step(s2)

    def badState(s: State): BVExpr = {
      // assert(max >= past(max))
      BVNot(BVComparison(Compare.GreaterEqual, s.max, s.pastMax, signed = false))
    }

    def isBad(s: State): Boolean = {
      solve(badState(s)).nonEmpty
    }

    assert(!isBad(s1))
    assert(!isBad(s2))
    assert(!isBad(s3))

    assert(isBad(s0))
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