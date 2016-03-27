import scala.annotation.tailrec

case class Grid private(private val vector: Vector[Coordinate] = Vector()) {
  def setDead(coord: Coordinate): Grid = Grid(vector filterNot coord.==)

  def setAlive(coords: Coordinate): Grid = Grid(vector :+ coords)

  def getCell(coordinate: Coordinate): Cell = if(vector contains coordinate) Alive else Dead

  def nextGeneration: Grid = {
    val nextVector = createNextGenerationVector(vector, Vector())
    Grid(nextVector)
  }

  @tailrec
  private def createNextGenerationVector(v: Vector[Coordinate], result: Vector[Coordinate]): Vector[Coordinate] = {
    if(v.isEmpty) result.distinct
    else createNextGenerationVector(v.tail, result ++ applyCellRules(v.head.neighbourhood, Vector()))
  }

  @tailrec
  private def applyCellRules(v: Vector[Coordinate], result: Vector[Coordinate]): Vector[Coordinate] = {
    if(v.isEmpty) result
    else {
      Rules(getCell(v.head), livingNeighbours(v.head)) match {
        case Alive => applyCellRules(v.tail, result :+ v.head)
        case Dead => applyCellRules(v.tail, result)
      }
    }
  }

  private def livingNeighbours(coord: Coordinate): Integer = {
    val neighbourCells = for { neighbour <- coord.neighbours } yield getCell(neighbour)
    neighbourCells count Alive.==
  }
}

object Grid {
  def initialize: Grid = Grid()
}