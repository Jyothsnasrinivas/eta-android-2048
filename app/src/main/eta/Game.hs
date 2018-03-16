{-# LANGUAGE Arrows #-}

module Game (wholeGame) where

import System.Random (StdGen)
import FRP.Yampa

import Types
import GameModel
import GameLogic

-- | Run the game that still has moves ('gameAlive'), until ('switch')
-- there are no more moves ('outOfMoves'), in which case the game is over
-- and needs to be restarted ('restartGame')
wholeGame :: StdGen -> SF GameInput GameState
wholeGame g = switch
  (gameAlive g >>> (identity &&& outOfMoves))
  (restartGame g)

-- | Detect when there are no more possible moves on the given board
outOfMoves :: SF GameState (Event GameState)
outOfMoves = proc s -> do
  lost <- edge -< isGameOver s
  let snapshot = lost `tag` s
  returnA -< snapshot

-- | Start the game using the initial game state (empty board with score 0)
-- and placing two initial tiles randomly onto the board
gameAlive :: StdGen -> SF GameInput GameState
gameAlive g =
    let (float1, g') = random g
        (float2, g'') = random g'
        (float3, g''') = random g''
        (float4, g'''') = random g'''
    in runGame $ placeRandomTile float1 float2
               $ placeRandomTile float3 float4
                 (initialGameState g'''')

-- | When the game is lost we want to show the GameOver text for some time
-- and then restart the game
restartGame :: StdGen -> GameState -> SF GameInput GameState
restartGame g s = switch
  (gameOver s &&& after 5 ())
  (const $ wholeGame g)

-- | When we have lost the game we want to keep the board in a state that
-- the user reached and show some GameOver message over it
gameOver :: GameState -> SF a GameState
gameOver s = arr $ const $ s { status = GameOver }

-- | Run the game, keeping the internal state using dHold, updating the
-- game state based on user's input (if any)
runGame :: GameState -> SF GameInput GameState
runGame state = proc input -> do
  rec currentState <- dHold state -< gameUpdated
      gameUpdated <- arr update -< (currentState, input)

  returnA -< currentState

-- | Based on the input received either try to slide the board in the given
-- direction or do not do anything (NoEvent). If the sliding in the given
-- direction is not possible then we are once again not doing anything
-- (NoEvent)
update :: (GameState, GameInput) -> Event GameState
update (gameState, input) =
    case input of
      Event None -> NoEvent
      Event direction ->
        let newBoardScore = slideBoard direction (board gameState)
            (float1, gen') = random (gen gameState)
            (float2, gen'') = random gen'
        in if fst newBoardScore == board gameState
             then NoEvent
             else Event $ placeRandomTile float1 float2
                        $ gameState { board = fst newBoardScore
                                    , score = score gameState + snd newBoardScore
                                    , gen = gen''
                                    }
      _ -> NoEvent
