import scala.annotation.tailrec

class Grid(val vector: Vector[Vector[Cell]]) {
  def tick: Grid = {
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
          r ++ Vector(Cell.rules(v.head, Grid.getCellLivingNeighbours(this)(x, y).size)),
          y + 1)
      }

      nextGenerationOfYAxis(v, Vector(), 0)
    }

    new Grid(nextGenerationHelper(vector, Vector(), 0))
  }

}

object Grid {
  def initialize(x: Int, y: Int): Grid = new Grid(Vector.fill[Cell](x, y) { Dead })

  def xAxisSize(grid: Grid): Int = grid.vector.size

  def yAxisSize(grid: Grid): Int = grid.vector(xAxisSize(grid) - 1).size

  def getCell(grid: Grid)(x: Int, y: Int): Cell = grid.vector.lift(x).flatMap(_.lift(y)).getOrElse[Cell](Dead)

  def getCellLivingNeighbours(grid: Grid)(x: Int, y: Int): Vector[Cell] = {
    def neighbourhood: Vector[(Int, Int)] = for {
      x <- Vector(x - 1, x, x + 1)
      y <- Vector(y - 1, y, y + 1)
    } yield (x, y)

    def neighbours = neighbourhood filterNot (x, y).==

    def getNeighbours = for { n <- neighbours } yield Grid.getCell(grid)(n._1, n._2)

    getNeighbours filter Alive.==
  }

  def setCell(grid: Grid)(x: Int, y: Int)(c: Cell) = {
    val updatedY: Vector[Cell] = grid.vector(x).updated(y, c)
    new Grid(grid.vector.updated(x, updatedY))
  }
}