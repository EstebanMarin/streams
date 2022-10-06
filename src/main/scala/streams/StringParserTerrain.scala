package streams

import scala.util.Try
import scala.util.Success.apply
import scala.util.Success
import scala.util.Failure

/** This component implements a parser to define terrains from a graphical ASCII
  * representation.
  *
  * When mixing in that component, a level can be defined by defining the field
  * `level` in the following form:
  *
  * val level = """------ \|--ST-- \|--oo-- \|--oo-- \|------""".stripMargin
  *
  *   - The `-` character denotes parts which are outside the terrain
  *   - `o` denotes fields which are part of the terrain
  *   - `S` denotes the start position of the block (which is also considered
  *     inside the terrain)
  *   - `T` denotes the final position of the block (which is also considered
  *     inside the terrain)
  *
  * In this example, the first and last lines could be omitted, and also the
  * columns that consist of `-` characters only.
  */
trait StringParserTerrain extends GameDef:

  /** A ASCII representation of the terrain. This field should remain abstract
    * here.
    */
  val level: String

  /** This method returns terrain function that represents the terrain in
    * `levelVector`. The vector contains parsed version of the `level` string.
    * For example, the following level
    *
    * val level = """ST \|oo \|oo""".stripMargin
    *
    * is represented as
    *
    * Vector(Vector('S', 'T'), Vector('o', 'o'), Vector('o', 'o'))
    *
    * The resulting function should return `true` if the position `pos` is a
    * valid position (not a '-' character) inside the terrain described by
    * `levelVector`.
    */

  type Level = Vector[Char]

  def terrainFunction(levelVector: Vector[Level]): Pos => Boolean =
    (pos: Pos) =>
      def checkChar(lV: Level, row: Int) =
        Try(lV(row)) match
          case Success(v) =>
            v match
              case 'o' | 'S' | 'T' => true
              case _               => false
          case Failure(_) => false

      val test = for
        (cV: Level, row: Int) <- levelVector.zipWithIndex
        value = if row == pos.row then checkChar(cV, pos.row) else false
        teopt = println(s"[COL] $row [LEVEL] => $cV")
      yield value
      
      val returnT = test.fold(false)(_ || _)
      returnT

  /** This function should return the position of character `c` in the terrain
    * described by `levelVector`. You can assume that the `c` appears exactly
    * once in the terrain.
    *
    * Hint: you can use the functions `indexWhere` and / or `indexOf` of the
    * `Vector` class
    */
  def findChar(c: Char, levelVector: Vector[Vector[Char]]): Pos = ???

  private lazy val vector: Vector[Vector[Char]] =
    Vector(level.split("\r?\n").map(str => Vector(str*)).toIndexedSeq*)

  lazy val terrain: Terrain = terrainFunction(vector)
  lazy val startPos: Pos = findChar('S', vector)
  lazy val goal: Pos = findChar('T', vector)
