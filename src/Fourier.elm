module Fourier exposing (dft, dftFloat, dftInt)

import Array exposing (Array)
import Complex exposing (Complex, add, multiply, polar, real, zero)


dft : Array Complex -> Array Complex
dft array =
    let
        size =
            Array.length array

        indexArray =
            Array.initialize size identity

        foldFunction n k acc =
            let
                hk =
                    Maybe.withDefault zero <| Array.get k array

                expTerm =
                    polar 1 <| 2 * pi * (toFloat <| k * n) / toFloat size
            in
            acc |> add (hk |> multiply expTerm)

        fourierFunction n =
            Array.foldl (foldFunction n) zero indexArray
    in
    Array.map fourierFunction indexArray


dftInt : Array Int -> Array Complex
dftInt =
    dft << Array.map (real << toFloat)


dftFloat : Array Float -> Array Complex
dftFloat =
    dft << Array.map real
