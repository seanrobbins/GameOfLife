import org.scalatest.{Inside, Matchers, FunSpecLike}

import scala.util.Random

class GridSpec extends FunSpecLike with Matchers with Inside {
  describe("initialize grid") {
    it("creates a grid of the specified size of entirely dead cells") {
      val grid = Grid.initialize(5, 5)
      val rdm = new Random
      val result: Cell = Grid.getCell(grid)(rdm.nextInt(4), rdm.nextInt(4))
      inside(result) {
        case Dead(i) => i should be(0)
      }
    }
  }

  describe("#getCoords") {
    it("should return the cell at the supplied coordinates") {
      val grid = Grid.initialize(5, 5)
      val result: Cell = Grid.getCell(grid)(4, 4)
      inside(result) {
        case Dead(i) => i should be(0)
      }
    }

    describe("#setCoordsAlive") {
      it("should allow the cell of a specific coordinate to be alive") {
        val grid = Grid.setAlive(Grid.initialize(5, 5))(4, 4)
        val result = Grid.getCell(grid)(4,4)
        result shouldBe a [Alive]
      }
    }

    describe("#setCoordsDead") {
      it("should allow the cell of a specific coordinate to be alive") {
        val grid = Grid.setDead(Grid.setAlive(Grid.initialize(5, 5))(4,4))(4, 4)
        val result = Grid.getCell(grid)(4,4)
        result shouldBe a [Dead]
      }
    }
  }
}
