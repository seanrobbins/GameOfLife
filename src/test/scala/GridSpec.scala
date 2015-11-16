import org.scalatest.{Inside, Matchers, FunSpecLike}

import scala.annotation.tailrec
import scala.util.Random

class GridSpec extends FunSpecLike with Matchers with Inside {
  val rdm = new Random

  describe("initialize grid") {
    it("creates a grid of the specified size of entirely dead cells") {
      val grid = Grid.initialize(5, 5)
      val result: Cell = Grid.getCell(grid)(Coordinate(rdm.nextInt(4), rdm.nextInt(4)))
      result should be(Dead)
    }
  }

  describe("#getCell") {
    it("should return the cell at the supplied coordinates") {
      val grid = Grid.setCell(Grid.initialize(5, 5))(Coordinate(4,4))(Alive)
      val result: Cell = Grid.getCell(grid)(Coordinate(4, 4))
      result should be(Alive)
    }

    it("should return an instance of Dead if the supplied coordinates are not contained in the grid") {
      val grid = Grid.initialize(5, 5)
      val result: Cell = Grid.getCell(grid)(Coordinate(-1, -1))
      result should be(Dead)
    }
  }

  describe("#getCellLivingNeighbours") {
    it("should return an empty vector when the provided cell has no living neighbours") {
      val grid = Grid.initialize(2,2)
      val result = Grid.getCellLivingNeighbours(grid)(Coordinate(1,1))
      result should be(Vector())
    }

    it("should return a vector of the exact number of a living cells as the cells living neighbours") {
      val livingCells: Vector[Coordinate] = Vector(Coordinate(0,0), Coordinate(0,1), Coordinate(1,0))
      @tailrec
      def seed(g: Grid, lc: Vector[Coordinate]): Grid = {
        if(lc.isEmpty) g
        else seed(Grid.setCell(g)(lc.head)(Alive), lc.tail)
      }
      val grid = seed(Grid.initialize(4, 4), livingCells)
      val result = Grid.getCellLivingNeighbours(grid)(Coordinate(1,1))
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
    val grid = Grid.setCell(Grid.initialize(5, 5))(Coordinate(4, 4))(Alive)
    val result = Grid.getCell(grid)(Coordinate(4, 4))
    result should be(Alive)
  }

  describe("#tick") {
    it("should return a grid with a grid one dead cell from a game with a single grid with one alive cell") {
      val grid = Grid.setCell(Grid.initialize(1,1))(Coordinate(0,0))(Alive)
      val result = grid.tick
      Grid.getCell(result)(Coordinate(0,0)) should be(Dead)
    }

    it("should provide the game with live and dead cells updated") {
      val livingCells: Vector[Coordinate] = Vector(Coordinate(0,0), Coordinate(0,1), Coordinate(1,1))
      @tailrec
      def seed(g: Grid, lc: Vector[Coordinate]): Grid = {
        if(lc.isEmpty) g
        else seed(Grid.setCell(g)(lc.head)(Alive), lc.tail)
      }
      val grid = seed(Grid.initialize(4, 4), livingCells)
      val result = grid.tick
      Grid.getCell(result)(Coordinate(0,0)) should be(Alive)
      Grid.getCell(result)(Coordinate(0,1)) should be(Alive)
      Grid.getCell(result)(Coordinate(1,0)) should be(Alive)
      Grid.getCell(result)(Coordinate(1,1)) should be(Alive)
    }
  }
}
