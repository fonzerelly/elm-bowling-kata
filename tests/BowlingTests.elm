module BowlingTests exposing (..)

import Test exposing (..)
import Expect
import Bowling exposing (BowlingGame(..), roll, Frame(..))
import Debug
import Html exposing (a)
import Result

initialGame = BowlingGame []

repeatedRoll: Int -> Result String BowlingGame
repeatedRoll count = if count == 0 then
                        roll 0 initialGame
                     else
                        let
                            pins = min count 10
                        in
                        Result.map (roll pins) (repeatedRoll (count - 1))
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
            
            , test "should error on too many pins" <|
                \_ ->  roll 11 initialGame |> Expect.err

            , test "should error on too few pins" <|
                \_ ->  roll -1 initialGame |> Expect.err
            , test "should error on too much Frames" <|
                \_ -> 
                    let
                        result = repeatedRoll 21
                    in
                        result |> Expect.err

            -- , test "should allow three rolls on strike in 10th frame" <|
            --     \_ -> let
            --             extractFrames g = case g of
            --                 BowlingGame frames -> frames

            --             expectedResult = TenthFrame (10,0,0)
            --             gameAt10thFrame = repeatedRoll 17

            --             strike = 10

            --             strikeRoll = Result.map (roll strike) gameAt10thFrame
            --                 |> Result.andThen identity

            --             firstStrikeValuedRoll = Result.map (roll 0) strikeRoll
            --                 |> Result.andThen identity

            --             secondStrikeValuedRoll = Result.map (roll 0) firstStrikeValuedRoll
            --                 |> Result.andThen identity

            --             fuck = Debug.log "**********" secondStrikeValuedRoll

            --             tenthFrame = secondStrikeValuedRoll
            --                 |> Result.withDefault (BowlingGame [])
            --                 |> extractFrames
            --                 |> List.head
            --                 |> Maybe.withDefault EmptyFrame
                        
            --           in
            --             tenthFrame |> Expect.equal expectedResult
            ]
        ]