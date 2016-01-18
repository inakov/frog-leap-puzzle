package puzzle

/**
 * Created by inakov on 16-1-18.
 */
package object state {

  trait Direction
  case object Left extends Direction
  case object Right extends Direction

  case class Frog(direction: Direction)

  case class GameState(state: List[Option[Frog]]){

    def isSolved(): Boolean = {
      val indexToSkip = state.indexOf(None)
      val left = state.take(indexToSkip)
      val right = state.drop(indexToSkip + 1)

      !left.contains(Some(Frog(Right))) && !right.contains(Some(Frog(Left)))
    }

    def generatePossibleOutcomes(): List[GameState] = {
      var possibleOutcomes: Set[GameState] = Set()

      for(currentIndex <- 0 until state.size){
        val currentPosition: Option[Frog] = state(currentIndex)
        currentPosition match {
          case None => ;
          case Some(frog) =>
            frog match {
              case Frog(Right) => {
                var nextIndex = currentIndex + 1
                if(nextIndex < state.size){
                  val nextPosition: Option[Frog] = state(nextIndex)
                  nextPosition match {
                    case None =>
                      val newState = scala.collection.mutable.ArrayBuffer(state : _*)
                      newState(nextIndex) = currentPosition
                      newState(currentIndex) = nextPosition
                      possibleOutcomes += GameState(newState.toList)
                    case Some(_) =>
                      nextIndex+=1
                      if(nextIndex < state.size-1){
                        val nextPosition: Option[Frog] = state(nextIndex)
                        nextPosition match {
                          case None =>
                            val newState = scala.collection.mutable.ArrayBuffer(state : _*)
                            newState(nextIndex) = currentPosition
                            newState(currentIndex) = nextPosition
                            possibleOutcomes += GameState(newState.toList)
                          case Some(_) =>
                        }
                      }
                  }
                }
              }
              case Frog(Left) => {
                var nextIndex = currentIndex - 1
                if(nextIndex >= 0){
                  val nextPosition: Option[Frog] = state(nextIndex)
                  nextPosition match {
                    case None =>
                      val newState = scala.collection.mutable.ArrayBuffer(state : _*)
                      newState(nextIndex) = currentPosition
                      newState(currentIndex) = nextPosition
                      possibleOutcomes += GameState(newState.toList)
                    case Some(_) =>
                      nextIndex-=1
                      if(nextIndex >= 0){
                        val nextPosition: Option[Frog] = state(nextIndex)
                        nextPosition match {
                          case None =>
                            val newState = scala.collection.mutable.ArrayBuffer(state : _*)
                            newState(nextIndex) = currentPosition
                            newState(currentIndex) = nextPosition
                            possibleOutcomes += GameState(newState.toList)
                          case Some(_) =>
                        }
                      }
                  }
                }
              }
            }
        }
      }

      possibleOutcomes.toList
    }

  }

}
