package streams

import org.scalacheck.Prop.forAll

import Bloxorz.*

class BloxorzSuite extends munit.FunSuite:
  trait SolutionChecker extends GameDef with Solver with StringParserTerrain:
    /** This method applies a list of moves `ls` to the block at position
      * `startPos`. This can be used to verify if a certain list of moves is a
      * valid solution, i.e. leads to the goal.
      */
    import Move.*
    def solve(ls: List[Move]): Block =
      ls.foldLeft(startBlock) { case (block, move) =>
        require(block.isLegal) // The solution must always lead to legal blocks
        move match
          case Left  => block.left
          case Right => block.right
          case Up    => block.up
          case Down  => block.down
      }

  trait Level0 extends SolutionChecker:
    val level =
      """------
        |--S---
        |--o---
        |--o---
        |--T---""".stripMargin

  trait Level1 extends SolutionChecker:
    /* terrain for level 1*/

    val level =
      """ooo-------
      |oSoooo----
      |ooooooooo-
      |-ooooooooo
      |-----ooToo
      |------ooo-""".stripMargin

    val mapTest =
      Vector(Vector('S', 'T'), Vector('o', '-'), Vector('o', '-'))

    import Move.*
    val optsolution = List(Right, Right, Down, Right, Right, Right, Down)

  test("Basic terrainFunction check") {
    new Level1:
      val in = Pos(0, 0)
      val out = Pos(2, 1)
      val out2 = Pos(50, -90)
      val inAs = terrainFunction(mapTest)(in)
      val outAs = terrainFunction(mapTest)(out)
      val outAs2 = terrainFunction(mapTest)(out2)
      assertEquals(inAs, true)
      assertEquals(outAs, false)
      assertEquals(outAs, false)
  }

  test("testing find Char with bigger terrain") {
    new Level1:
      val in1 = Pos(1, 1)
      val in2 = Pos(4, 7)
      val out1 = Pos(5, 2)
      val out2 = Pos(50, 2)
      val in1T = terrain(in1)
      val in2T = terrain(in2)
      val out1T = terrain(out1)
      val out2T = terrain(out2)
      assertEquals(in1T, true)
      assertEquals(in2T, true)
      assertEquals(out1T, false)
      assertEquals(out2T, false)
  }

  test("terrain function level 1 (10pts)") {
    new Level1:
      assert(terrain(Pos(0, 0)), "0,0")
      assert(terrain(Pos(1, 1)), "1,1") // start
      assert(terrain(Pos(4, 7)), "4,7") // goal
      assert(terrain(Pos(5, 8)), "5,8")
      assert(!terrain(Pos(5, 9)), "5,9")
      assert(terrain(Pos(4, 9)), "4,9")
      assert(!terrain(Pos(6, 8)), "6,8")
      assert(!terrain(Pos(4, 11)), "4,11")
      assert(!terrain(Pos(-1, 0)), "-1,0")
      assert(!terrain(Pos(0, -1)), "0,-1")
  }

  test("find char level 1 (10pts)") {
    new Level1:
      assertEquals(startPos, Pos(1, 1))
  }

  test("Test Block isStanding") {
    new Level1:
      val block1 = Pos(0, 0)
      val block2 = Pos(0, 0)
      val block3 = Pos(0, 1)
      val blockUp = Block(block1, block2)
      val blockDown = Block(block1, block3)
      assertEquals(blockUp.isStanding, true)
      assertEquals(blockDown.isStanding, false)
  }

  test("Test Legal positions") {
    new Level1:
      val block1 = Pos(0, 0)
      val block2 = Pos(0, 0)
      val block3 = Pos(0, 4)
      val block4 = Pos(0, 5)
      val blockLegal = Block(block1, block2)
      val blockiLegal = Block(block3, block4)
      assertEquals(blockLegal.isLegal, true)
      assertEquals(blockiLegal.isLegal, false)
  }

  test("neighbors legal") {
    new Level1:
      val block1 = Pos(0, 0)
      val block2 = Pos(0, 0)
      val block3 = Pos(0, 4)
      val block4 = Pos(0, 5)
      val blockLegal = Block(block1, block2).legalNeighbors
      val legalOption = List(
        (Block(Pos(0, 1), Pos(0, 2)), Move.Right),
        (Block(Pos(1, 0), Pos(2, 0)), Move.Down)
      )
      val blockiLegal = Block(block3, block4).legalNeighbors
      assertEquals(blockiLegal, Nil)
      assertEquals(blockLegal, legalOption)
  }

  test("neighbors with history test") {
    new Level0:
      val blockLegal = startBlock
      val test = neighborsWithHistory(blockLegal, Nil).head
      val legalMove = (Block(Pos(2,2),Pos(3,2)),List(Move.Down))
      assertEquals(test, legalMove)
  }

  // test("optimal solution for level 1 (5pts)") {
  //   new Level1:
  //     assertEquals(solve(solution), Block(goal, goal))
  // }

  // test("optimal solution length for level 1 (5pts)") {
  //   new Level1:
  //     assertEquals(solution.length, optsolution.length)
  // }

  import scala.concurrent.duration.*
  override val munitTimeout = 10.seconds
