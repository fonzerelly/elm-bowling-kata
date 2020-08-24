module Bowling exposing (BowlingGame(..), roll, Frame (..))
import List

import Debug
import List
import Maybe

type Frame = EmptyFrame | StartedFrame (Int) | FinalizedFrame (Int, Int) -- | TenthFrame (Int, Int, Int)

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

        evaluatesFrameToHandle: List Frame -> Frame
        evaluatesFrameToHandle frames = case frames |> List.head |> Maybe.withDefault EmptyFrame of
           FinalizedFrame _ -> EmptyFrame
           StartedFrame f -> StartedFrame f
           EmptyFrame -> EmptyFrame
            
        prependFrame:List Frame -> Frame -> List Frame
        prependFrame frames frame = 
            let
                rest = frames
                    |> List.tail
                    |> Maybe.withDefault []
            in 
            case frame of
                FinalizedFrame _ -> frame :: rest
                _ -> frame :: frames
        
        handleFrame: Frame -> Frame
        handleFrame frame = case frame of
            EmptyFrame -> StartedFrame pins 
            StartedFrame fst -> FinalizedFrame (fst, pins)
            _ -> frame

        frameCount = game 
            |> extractFrames
            |> List.length

        gameFrames = extractFrames game

        frameToHandle = gameFrames
            |> evaluatesFrameToHandle

        newBowlingGame = frameToHandle
            |> handleFrame
            |> prependFrame gameFrames
            |> BowlingGame
    in
        if (pins > 10) || (pins < 0) then
            Err <| String.append "Invalid number of Pins: " <| String.fromInt pins
        else
            case frameToHandle of
                EmptyFrame ->
                    if frameCount >= 10 then
                        Err "It is not allowed roll more than 10 frames!"
                    else
                        Ok <| newBowlingGame
                _ -> Ok <| newBowlingGame