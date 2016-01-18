package puzzle

import puzzle.state._

import scala.annotation.tailrec

/**
 * Created by inakov on 16-1-18.
 */
object FrogLeapPuzzle extends App{
  println("Enter n:")
  val n: Int = scala.io.StdIn.readInt()

  private def generateInitialState(n: Int): GameState ={
    var state: List[Option[Frog]] = List()

    for(i <- 1 to n) state = Some(Frog(Left)) :: state
    state = None :: state
    for(i <- 1 to n) state = Some(Frog(Right)) :: state

    GameState(state)
  }

  private def printState(gameState: GameState): Unit ={
    for(e <- gameState.state) e match {
        case Some(frog) => frog match {
          case Frog(Left) => print("L")
          case Frog(Right) => print("R")
        }
        case None => print("_")
      }
  }

  private def solve(gameState: GameState): List[GameState] ={

    @tailrec
    def traverse(solution: List[GameState], checkedOutcomes: List[GameState]): List[GameState] ={

      def solutionPath(currentPath: List[GameState], outcomes: List[GameState]): List[GameState] ={
          for(possibleSolution <- currentPath.head.generatePossibleOutcomes())
            if(!outcomes.contains(possibleSolution))
              return possibleSolution :: currentPath

        solutionPath(currentPath.tail, outcomes)
      }

      if(solution.head.isSolved()) solution
      else traverse(solutionPath(solution, checkedOutcomes), solution.head :: checkedOutcomes)
    }

    traverse(List(gameState), Nil)
  }

  val initState = generateInitialState(n)

  val solution = solve(initState)
  for(state <- solution){
    println()
    printState(state)
  }
}
