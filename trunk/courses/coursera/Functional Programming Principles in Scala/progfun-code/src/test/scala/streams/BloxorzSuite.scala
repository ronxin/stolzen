package streams

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import Bloxorz._

@RunWith(classOf[JUnitRunner])
class BloxorzSuite extends FunSuite {

  trait SolutionChecker extends GameDef with Solver with StringParserTerrain {
    /**
     * This method applies a list of moves `ls` to the block at position
     * `startPos`. This can be used to verify if a certain list of moves
     * is a valid solution, i.e. leads to the goal.
     */
    def solve(ls: List[Move]): Block =
      ls.foldLeft(startBlock) { case (block, move) =>  move match {
        case Left => block.left
        case Right => block.right
        case Up => block.up
        case Down => block.down
      }
    }
  }

  trait Level0 extends SolutionChecker {
    val level =
        """ST
          |oo
          |oo""".stripMargin
  }
  
  trait Level1 extends SolutionChecker {
      /* terrain for level 1*/
    val level =
    """ooo-------
      |oSoooo----
      |ooooooooo-
      |-ooooooooo
      |-----ooToo
      |------ooo-""".stripMargin

    val optsolution = List(Right, Right, Down, Right, Right, Right, Down)
  }

  test("findChar level 1 find T and S") {
    new Level1() {
      assert(startPos == Pos(1, 1))
      assert(goal == Pos(4, 7))
    }
  }

  test("terrain function level 1") {
    new Level1 {
      assert(terrain(Pos(0, 0)), "0,0")
      assert(!terrain(Pos(4, 11)), "4,11")
      assert(!terrain(Pos(-1, -1)), "-1,-1")
    }
  }

  test("level 1 stands at start") {
    new Level1 {
      assert(startBlock.isStanding)
    }
  }

  test("level 1 doesn't stand one step afer start") {
    new Level1 {
      assert(!startBlock.right.isStanding)
      assert(!startBlock.down.isStanding)
    }
  }

  test("level 1 stands 2 steps afer start") {
    new Level1 {
      assert(startBlock.right.right.isStanding)
    }
  }

  test("level 1 left and up after start aren't legal, but down and right are") {
    new Level1 {
      assert(!startBlock.left.isLegal, "left is not legal")
      assert(!startBlock.up.isLegal, "up is not legal")
      assert(startBlock.right.isLegal, "right is legal")
      assert(startBlock.down.isLegal, "down is legal")
    }
  }

  test("level 1 legalNeighbors") {
    new Level1 {
      val legal = startBlock.legalNeighbors
      assert(legal.toSet === Set((startBlock.right, Right), (startBlock.down, Down)))
    }
  }

  test("level1 from 1,1 finding neightbours") {
    new Level1 {
      val block = Block(Pos(1,1), Pos(1,1))
      val history = List(Left, Up)
      val actual = neighborsWithHistory(block, history).toSet
      val expected = Set(
        (Block(Pos(1, 2), Pos(1, 3)), List(Right, Left, Up)),
        (Block(Pos(2, 1), Pos(3, 1)), List(Down, Left, Up))
      )
      assert(actual === expected)
    }
  }

  test("level1 newNeighborsOnly") {
    new Level1 {    
      val neighbors = Stream(
        (Block(Pos(1, 2), Pos(1, 3)), List(Right, Left, Up)),
        (Block(Pos(2, 1), Pos(3, 1)), List(Down, Left, Up))
      )
      val explored: Set[Block] = Set(
          Block(Pos(1, 2), Pos(1, 3)), 
          Block(Pos(1, 1), Pos(1, 1))
      )

      val actual = newNeighborsOnly(neighbors, explored).toSet

      val expected = Set(
        (Block(Pos(2, 1), Pos(3, 1)), List(Down, Left, Up))
      )
      assert(actual === expected)
    }
  }

  test("level 0 pathsFromStart") {
    new Level0 {
      val path = pathsFromStart
      assert(path.nonEmpty)
    }
  }

  test("level 1, 3 first steps for pathsFromStart") {
    new Level1 {
      val actual = pathsFromStart.drop(1).take(3).toList
      
      val expected = List(
          (Block(Pos(1, 2), Pos(1, 3)), List(Right)),
          (Block(Pos(2, 1), Pos(3, 1)), List(Down)),
          (Block(Pos(0, 2), Pos(0, 3)), List(Up, Right))
      )
      
      assert(expected == actual)
    }
  }

  test("level 0 pathsToGoal") {
    new Level0 {
      val (actualEndPosition, actualPath) = pathsToGoal.head
      val expectedEndPosition = Block(Pos(0, 1), Pos(0, 1))
      val expectedPath = List(Up, Right, Down)
      assert(actualEndPosition == expectedEndPosition)
      assert(actualPath == expectedPath)
    }
  }

  test("level 0 solution") {
    new Level0 {
      assert(solution == List(Down, Right, Up))
    }
  }

  test("optimal solution for level 1") {
    new Level1 {
      assert(solve(solution) == Block(goal, goal))
    }
  }

  test("optimal solution length for level 1") {
    new Level1 {
      assert(solution.length == optsolution.length)
    }
  }
}
