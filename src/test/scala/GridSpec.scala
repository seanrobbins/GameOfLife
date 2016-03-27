import org.scalatest.{FunSpecLike, Matchers}

import scala.util.Random

class GridSpec  extends FunSpecLike with Matchers {
  val rdm = new Random

  def fixture() = new {
    val grid = Grid.initialize
  }

  describe("create a new grid") {
    it("creates an infinite grid entirely of dead cells") {
      val f = fixture()
      val result = f.grid.getCell(
        Coordinate(rdm.nextInt(), rdm.nextInt())
      )
      result should be(Dead)
    }
  }

  describe("set cell to be alive") {
    it("allows me to set a dead cell in a grid to be alive") {
      val f = fixture()
      val coord = Coordinate(rdm.nextInt(), rdm.nextInt())
      f.grid.setAlive(coord).getCell(coord) should be(Alive)
    }
  }

  describe("set living cell to be dead") {
    it("allows me to a living cell in the grid to be dead") {
      val f = fixture()
      val coord = Coordinate(rdm.nextInt(), rdm.nextInt())
      f.grid.setAlive(coord).setDead(coord).getCell(coord) should be(Dead)
    }
  }

  describe("#applying the rules on the cells in the grid") {
    it("in next generation of a grid with one alive cell, that cell will be dead") {
      val f = fixture()
      val coord = Coordinate(rdm.nextInt(), rdm.nextInt())
      f.grid.setAlive(coord).nextGeneration.getCell(coord) should be(Dead)
    }

    it("in the next generation of a grid with a dead cell with three neighbours, that cell will be alive") {
      val f = fixture()
      val firstGen = f.grid
        .setAlive(Coordinate(0, 0))
        .setAlive(Coordinate(0, 1))
        .setAlive(Coordinate(0, 2))
      firstGen.nextGeneration.getCell(Coordinate(1, 1)) should be(Alive)
    }
  }
}
