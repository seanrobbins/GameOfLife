import org.scalatest.{Inside, Matchers, FunSpecLike}

import scala.util.Random

class GridSpec extends FunSpecLike with Matchers with Inside {
  val rdm = new Random

  describe("initialize grid") {
    it("creates a grid of the specified size of entirely dead cells") {
      val grid = Grid.initialize(5, 5)
      val result: Cell = Grid.getCell(grid)(rdm.nextInt(4), rdm.nextInt(4))
      result should be(Dead)
    }
  }

  describe("#getCell") {
    it("should return the cell at the supplied coordinates") {
      val grid = Grid.setCell(Grid.initialize(5, 5))(4,4)(Alive)
      val result: Cell = Grid.getCell(grid)(4, 4)
      result should be(Alive)
    }

    it("should return an instance of Dead if the supplied coordinates are not contained in the grid") {
      val grid = Grid.initialize(5, 5)
      val result: Cell = Grid.getCell(grid)(-1, -1)
      result should be(Dead)
    }
  }

  describe("#getCellLivingNeighbours") {
    it("should return an empty vector when the provided cell has no living neighbours") {
      val grid = Grid.initialize(2,2)
      val result = Grid.getCellLivingNeighbours(grid)(1,1)
      result should be(Vector())
    }

    it("should return a vector of the exact number of a living cells as the cells living neighbours") {
      val grid = Grid.setCell(Grid.setCell(Grid.setCell(Grid.initialize(2, 2))(0,0)(Alive))(0,1)(Alive))(1,0)(Alive)
      val result = Grid.getCellLivingNeighbours(grid)(1,1)
      result should be(Vector(Alive, Alive, Alive))
    }
  }

  describe("#xAxis") {
    it("should return the size of the x axis of the grid"){
      val testX = rdm.nextInt(100)
      val grid = Grid.initialize(testX, rdm.nextInt(100))
      val result = Grid.xAxisSize(grid)
      result should be(testX)
    }
  }

  describe("#yAxis") {
    it("should return the size of the x axis of the grid"){
      val testY = rdm.nextInt(100)
      val grid = Grid.initialize(rdm.nextInt(100), testY)
      val result = Grid.yAxisSize(grid)
      result should be(testY)
    }
  }

  describe("#setCell"){
    val grid = Grid.setCell(Grid.initialize(5, 5))(4, 4)(Alive)
    val result = Grid.getCell(grid)(4, 4)
    result should be(Alive)
  }
}
