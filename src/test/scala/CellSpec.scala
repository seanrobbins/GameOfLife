import org.scalatest.{FunSpecLike, Inside, Matchers}

class CellSpec extends FunSpecLike with Matchers with Inside {
  describe("#rules") {
    describe("underpopulation rule") {
      it("kills a live cell that less than two live neighbours") {
        val livingNeighbours = 1
        val testCell = new Alive(livingNeighbours)
        val result: Cell = Cell.rules(testCell)
        inside(result) {
          case Dead(i) => i should be(livingNeighbours)
        }
      }
    }

    describe("overpopulation rule") {
      it("will kill a live cell with greater than 3 live neighbours") {
        val livingNeighbours = 4
        val testCell = new Alive(livingNeighbours)
        val result: Cell = Cell.rules(testCell)
        inside(result) {
          case Dead(an) => an should be(livingNeighbours)
        }
      }

      describe("survival rule") {
        it("allow a live cell that has two or more live neighbours to survive") {
          val rnd = new scala.util.Random
          val range = 2 to 3
          val livingNeighbours = range(rnd.nextInt(range length))
          val testCell = new Alive(livingNeighbours)
          val result: Cell = Cell.rules(testCell)
          inside(result) {
            case Alive(i) => i should be(livingNeighbours)
          }
        }
      }

      describe("reproduction rule") {
        it("will reproduce a dead cell with 3 live neighbours to a live cell") {
          val livingNeighbours = 3
          val testCell = new Dead(livingNeighbours)
          val result: Cell = Cell.rules(testCell)
          inside(result) {
            case Alive(an) => an should be(livingNeighbours)
          }
        }
      }
    }
  }
}
