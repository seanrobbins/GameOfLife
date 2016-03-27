import org.scalatest.{FunSpecLike, Inside, Matchers}

class RulesSpec extends FunSpecLike with Matchers with Inside {
  describe("#rules") {
    describe("underpopulation rule") {
      it("kills a live cell that less than two live neighbours") {
        val result: Cell = Rules(Alive, 1)
        result should be(Dead)
      }
    }

    describe("overpopulation rule") {
      it("will kill a live cell with greater than 3 live neighbours") {
        val result: Cell = Rules(Alive, 4)
        result should be(Dead)
      }

      describe("survival rule") {
        it("allow a live cell that has two or three live neighbours to survive") {
          val rnd = new scala.util.Random
          val range = 2 to 3
          val livingNeighbours = range(rnd.nextInt(range.length))
          val result: Cell = Rules(Alive, livingNeighbours)
          result should be(Alive)
        }
      }

      describe("reproduction rule") {
        it("will reproduce a dead cell with 3 live neighbours to a live cell") {
          val result: Cell = Rules(Dead, 3)
          result should be(Alive)
        }
      }
    }
  }
}
