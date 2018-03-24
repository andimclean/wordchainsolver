/**
  
  **/
package uk.pandagrove.wcs
import org.scalatest._

class WordChainSolverSuite
    extends FunSuite
    with    Matchers {

  val solver = new WordChainSolver

  test("warm..cold") {
    val chain = solver.chain("warm", "cold")
    info(chain.toString)
    chain.isDefined shouldBe true
    chain.get.length shouldBe 5
  }

  test("lead..gold") {
    val chain = solver.chain("lead", "gold")
    info(chain.toString)
    chain.isDefined shouldBe true
    chain.get.length shouldBe 4
  }

 test("show..boat") {
    val chain = solver.chain("show", "boat")
    info(chain.toString)
    chain.isDefined shouldBe true
    chain.get.length shouldBe 5
  }
  test("show..cold") {
    val chain = solver.chain("show", "cold")
    info(chain.toString)
    chain.isDefined shouldBe true
    chain.get.length shouldBe 6
  }

  test("show..andi") {
    val chain = solver.chain("show", "andi")
    info(chain.toString)
    chain.isDefined shouldBe false
    
  }
}
