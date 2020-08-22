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
                        game = roll 0 initialGame

                    in
                        Result.map (roll 1) game
                            |> Result.andThen identity
                            |> Expect.equal expectedResult
            , test "third roll append 2nd frame" <|
                \_-> let
                        expectedResult = Ok <| BowlingGame [StartedFrame (2), FinalizedFrame (0 , 1)]
                        firstRoll = roll 0 initialGame
                        game = Result.map (roll 1) firstRoll
                                |> Result.andThen identity
                    in
                        Result.map (roll 2) game
                            |> Result.andThen identity
                            |> Expect.equal expectedResult

            , test "fourth roll finalize 2nd frame" <|
                \_-> let
                        expectedResult = Ok <| BowlingGame [FinalizedFrame (2 , 3), FinalizedFrame (0, 1)]
                        firstRoll = roll 0 initialGame

                        secondRoll = Result.map (roll 1) firstRoll
                                |> Result.andThen identity

                        game = Result.map (roll 2) secondRoll
                                |> Result.andThen identity

                    in
                        Result.map (roll 3) game
                            |> Result.andThen identity
                            |> Expect.equal expectedResult

            -- , test "fifth roll append 3rd frame" <|
            --     \_-> let
            --             expectedResult = Ok <| BowlingGame [StartedFrame (4), FinalizedFrame (2 , 3), FinalizedFrame (0, 1)]
            --             firstRoll = roll 0 initialGame

            --             secondRoll = Result.map (roll 1) firstRoll
            --                     |> Result.andThen identity

            --             thirdRoll = Result.map (roll 2) secondRoll
            --                     |> Result.andThen identity

            --             game = Result.map (roll 3) thirdRoll
            --                     |> Result.andThen identity

            --         in
            --             Result.map (roll 4) game
            --                 |> Result.andThen identity
            --                 |> Expect.equal expectedResult
            ]
        ]