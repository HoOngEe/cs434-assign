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
      ls.foldLeft(startBlock) { case (block, move) => move match {
        case Left => block.left
        case Right => block.right
        case Up => block.up
        case Down => block.down
      }
    }
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

  test("terrain function level 1") {
    new Level1 {
      assert(terrain(Pos(0,0)), "0,0")
      assert(!terrain(Pos(4,11)), "4,11")
    }
  }

  test("findChar level 1") {
    new Level1 {
      assert(startPos == Pos(1,1))
    }
  }

  test("neighbors with history level 1") {
    new Level1 {
      val oracle = Set(
        (Block(Pos(1, 2), Pos(1, 3)), List(Right, Left, Up)),
        (Block(Pos(2, 1), Pos(3, 1)), List(Down, Left, Up))
      )
      val target = neighborsWithHistory(Block(Pos(1, 1), Pos(1, 1)), List(Left, Up))
      assert(target.toSet == oracle)
    }
  }

  test("check new neighbors only for level 1") {
    new Level1 {
      val oracle = Set(
        (Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up))
      ).toStream
      val target = newNeighborsOnly(
        Set(
          (Block(Pos(1,2),Pos(1,3)), List(Right,Left,Up)),
          (Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up))
        ).toStream,
        Set(Block(Pos(1,2),Pos(1,3)), Block(Pos(1,1),Pos(1,1)))
      )
      assert(target == oracle)
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

  trait Level2 extends SolutionChecker {
    /* terrain for level 2*/

    val level =
      """------ooooooo--
        |oooo--ooo--oo--
        |ooooooooo--oooo
        |oSoo-------ooTo
        |oooo-------oooo
        |------------ooo""".stripMargin

    val optsolution = List(Right, Up, Right, Right ,Right, Up, Left, Down,
      Right, Up, Up, Right, Right, Right, Down, Down, Down, Right, Up)
  }

  test("terrain function level 2") {
    new Level2 {
      assert(!terrain(Pos(3,7)), "3,7")
      assert(terrain(Pos(4,11)), "4,11")
    }
  }

  test("findChar level 2") {
    new Level2 {
      assert(startPos == Pos(3,1))
      assert(goal == Pos(3, 13))
    }
  }

  test("neighbors with history level 2") {
    new Level2 {
      val oracle = Set(
        (Block(Pos(0, 8), Pos(0, 8)), List(Up, Right, Right)),
        (Block(Pos(1, 7), Pos(2, 7)), List(Left, Right, Right))
      )
      val target = neighborsWithHistory(Block(Pos(1, 8), Pos(2, 8)), List(Right, Right))
      assert(target.toSet == oracle)
    }
  }

  test("check on step forward neighbors only for level 2") {
    new Level2 {
      val oracle = Stream.empty
      val neighbors = Set(
        (Block(Pos(2, 11), Pos(2, 12)), List(Right, Left, Up)),
        (Block(Pos(1, 8), Pos(2, 8)), List(Right, Down, Left, Up))
      ).toStream
      val target = newNeighborsOnly(
        neighbors flatMap { case (block, history) => neighborsWithHistory(block, history) },
        Set(
          Block(Pos(1, 11), Pos(1, 12)), Block(Pos(2, 13), Pos(2, 13)),
          Block(Pos(3, 11), Pos(3, 12)), Block(Pos(0, 8), Pos(0, 8)),
          Block(Pos(1, 7), Pos(2, 7))
        )
      )
      assert(target == oracle)
    }
  }

  test("optimal solution for level 2") {
    new Level2 {
      assert(solve(solution) == Block(goal, goal))
    }
  }

  test("optimal solution length for level 2") {
    new Level2 {
      assert(solution.length == optsolution.length)
    }
  }
}
