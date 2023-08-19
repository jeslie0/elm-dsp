module Fourier exposing (..)

import Array exposing (Array)
import Complex exposing (..)
import List as L exposing (..)


fourier : Array Float -> Array Float
fourier array =
    let
        n =
            Array.length array

        func k =
            Array.map
                (\j ->
                    case Array.get j array of
                        Nothing ->
                            zero

                        Just xn ->
                            multiply (real xn) <| polar 1 <| pi * (toFloat <| 2 * j * k) / toFloat n
                )
                (Array.initialize n (\i -> i))
    in
    Array.map (\k -> .abs << Complex.toPolar << Array.foldl add zero <| func k) <|
        Array.initialize n (\i -> i)


complexListToPlot : List Complex -> List { x : Float, y : Float, z : Float }
complexListToPlot =
    L.indexedMap
        (\n z ->
            let
                cartZ =
                    toCartesian z
            in
            { x = toFloat n
            , y = cartZ.re
            , z = cartZ.im
            }
        )
