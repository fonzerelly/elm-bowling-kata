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
            --             lastFrame frames = case frames of
            --                []   -> Bowling.EmptyFrame
            --                [f]  -> f
            --                _::l -> lastFrame l

            --             expectedResult = TenthFrame (10,0,0)
            --             gameAt10thFrame = repeatedRoll 17

            --             strike = 10

            --             resultingGame = Result.map (roll strike) gameAt10thFrame
            --                 |> Result.andThen identity

            --             tenthFrame case resultingGame of
            --                Bowling.BowlingGame frames -> lastFrame frames                       
                        
            --           in
            --             Err "doof" |> Expect.err
            -- should work now in general
            -- missing case Tenth Frame

                    -- if first throw in tenth frame is a strike
                    -- or if the finalizedFrame endsup to 10 then we
                    -- need a third roll
            
            ]
        ]