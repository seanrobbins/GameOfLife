class Grid(val vector: Vector[Vector[Cell]])

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