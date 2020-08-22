module Bowling exposing (BowlingGame(..), roll, Frame (..))
import List

import Debug

type Frame = EmptyFrame | StartedFrame (Int) | FinalizedFrame (Int, Int)

type BowlingGame = BowlingGame (List Frame)

roll: Int -> BowlingGame -> Result String BowlingGame
roll pins game = 
    let
        -- case BowlingGame of
        --    BowlingGame frames -> case length frames of
        --       0 -> Ok <| BowlingGame [StartedFrame(pins)]
        --       xs -> Ok <| BowlingGame StartedFrame(pins) :: xs
        extractFrames: BowlingGame -> List Frame
        extractFrames g = case g of
            BowlingGame frames -> frames

        initFrames: List Frame -> List Frame
        initFrames frames = case frames of
           []       -> [handleFrame EmptyFrame]
           x :: xs -> case x of
                StartedFrame _ -> handleFrame x :: xs
                FinalizedFrame _ -> handleFrame EmptyFrame :: (x :: xs)
                _ -> x::xs
        
        handleFrame: Frame -> Frame
        handleFrame frame = case frame of
            EmptyFrame -> StartedFrame pins 
            StartedFrame fst -> FinalizedFrame (fst, pins)
            _ -> frame

        newBowlingGame = game
            |> extractFrames
            |> initFrames
            |> BowlingGame
    in
    Ok <| newBowlingGame