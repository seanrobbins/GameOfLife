import scala.annotation.tailrec
import org.scalatest.{Inside, Matchers, FunSpecLike}

class Game(val grid: Grid)

object Game {

  def initialize(grid: Grid): Game = {
    new Game(grid)
  }

  def tick(game: Game): Game = {
    val seed: Grid = game.grid

    def nextGeneration(v: Vector[Vector[Cell]]): Vector[Vector[Cell]] = {
      @tailrec
      def nextGenerationHelper(v: Vector[Vector[Cell]], r: Vector[Vector[Cell]], x: Int): Vector[Vector[Cell]] = {
        if(v.isEmpty) r
        else nextGenerationHelper(v.tail, r ++ Vector(nextGenerationOfXAxis(v.head, x)), x + 1)
      }

      def nextGenerationOfXAxis(v: Vector[Cell], x: Int): Vector[Cell] = {
        @tailrec
        def nextGenerationOfYAxis(v: Vector[Cell], r: Vector[Cell], y: Int): Vector[Cell] = {
          if(v.isEmpty) r
          else nextGenerationOfYAxis(v.tail,
            r ++ Vector(Cell.rules(v.head, Grid.getCellLivingNeighbours(seed)(x, y).size)),
            y + 1)
        }

        nextGenerationOfYAxis(v, Vector(), 0)
      }

      nextGenerationHelper(v, Vector(), 0)
    }

    val generation: Grid = new Grid(nextGeneration(seed.vector))
    new Game(generation)
  }
}

class GameSpec extends FunSpecLike with Matchers with Inside {
  describe("creating a game"){
    it("create a new game instance with my setup grid of cells"){
      val grid = Grid.initialize(5, 5)
      val result = Game.initialize(grid)
      result.grid should be(grid)
    }
  }

  describe("#tick") {
    it("should return a game with a grid one dead cell from a game with a single grid with one alive cell") {
      val grid = Grid.setCell(Grid.initialize(1,1))(0,0)(Alive)
      val game = Game.initialize(grid)
      val result = Game.tick(game)
      Grid.getCell(result.grid)(0,0) should be(Dead)
    }

    it("should provide the game with live and dead cells updated") {
      val livingCells: Vector[(Int, Int)] = Vector((0,0), (0,1), (1,1))
      @tailrec
      def seed(g: Grid, lc: Vector[(Int, Int)]): Grid = {
        if(lc.isEmpty) g
        else seed(Grid.setCell(g)(lc.head._1, lc.head._2)(Alive), lc.tail)
      }
      val grid = seed(Grid.initialize(4, 4), livingCells)
      val game = Game.initialize(grid)
      val result = Game.tick(game)
      Grid.getCell(result.grid)(0,0) should be(Alive)
      Grid.getCell(result.grid)(0,1) should be(Alive)
      Grid.getCell(result.grid)(1,0) should be(Alive)
      Grid.getCell(result.grid)(1,1) should be(Alive)
    }
  }
}
