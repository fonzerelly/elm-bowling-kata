module BowlingTests exposing (..)

import Test exposing (..)
import Expect
import Bowling exposing (BowlingGame(..), roll, Frame(..))
import Debug
import Html exposing (a)

initialGame = BowlingGame []

repeatedRoll: Int -> Result String BowlingGame
repeatedRoll count = if count == 0 then
                        roll 0 initialGame
                     else
                        Result.map (roll count) (repeatedRoll (count - 1))
                            |> Result.andThen identity 

all: Test
all = describe "Bowling"
        [ describe "roll"
            [ test "first roll should init a new Frame" <|
                \_-> 
                    let
                        expectedResult = Ok <| BowlingGame [StartedFrame (0)]
                    in
                        roll 0 initialGame |> Expect.equal expectedResult
            , test "second roll should finalize frame" <|
                \_-> let
                        expectedResult = Ok <| BowlingGame [FinalizedFrame (0 , 1)]
                        result = repeatedRoll 1

                    in
                        result |> Expect.equal expectedResult
            , test "third roll append 2nd frame" <|
                \_-> let
                        expectedResult = Ok <| BowlingGame [StartedFrame (2), FinalizedFrame (0 , 1)]
                        result = repeatedRoll 2
                    in
                        result |> Expect.equal expectedResult

            , test "fourth roll finalize 2nd frame" <|
                \_-> let
                        expectedResult = Ok <| BowlingGame [FinalizedFrame (2 , 3), FinalizedFrame (0, 1)]
                        result = repeatedRoll 3
                    in
                        result |> Expect.equal expectedResult


            , test "fifth roll should append 3d frame" <|
                \_-> let
                        expectedResult = Ok <| BowlingGame [StartedFrame 4, FinalizedFrame (2 , 3), FinalizedFrame (0, 1)]
                        result = repeatedRoll 4
                    in
                        result |> Expect.equal expectedResult
            -- should work now in general
            -- missing case: pins > 9
            -- missing case Tenth Frame
            
            ]
        ]