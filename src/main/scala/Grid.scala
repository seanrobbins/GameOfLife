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
          r ++ Vector(Cell.rules(v.head, Grid.getCellLivingNeighbours(this)(Coordinate(x, y)).size)),
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

  def getCell(grid: Grid)(c: Coordinate): Cell = grid.vector.lift(c.x).flatMap(_.lift(c.y)).getOrElse[Cell](Dead)

  def getCellLivingNeighbours(grid: Grid)(coord: Coordinate): Vector[Cell] = {
    val getNeighbourCells = for { n <- coord.neighbours } yield Grid.getCell(grid)(Coordinate(n.x, n.y))
    getNeighbourCells filter Alive.==
  }

  def setCell(grid: Grid)(coord: Coordinate)(cell: Cell) = {
    val updatedY: Vector[Cell] = grid.vector(coord.x).updated(coord.y, cell)
    new Grid(grid.vector.updated(coord.x, updatedY))
  }
}