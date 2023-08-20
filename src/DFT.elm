module DFT exposing (dft, dftFloat, dftInt)

import Array exposing (Array)
import Complex exposing (Complex, add, multiply, polar, real, zero)

{-| O(n^2) in the length of the array. Compute the Discrete Fourier Transform of the given
array.
 -}
dft : Array Complex -> Array Complex
dft array =
    let
        size =
            Array.length array

        indexArray =
            Array.initialize size identity

        foldFunction n k acc =
            let
                -- The kth term of array
                hk =
                    Maybe.withDefault zero <| Array.get k array

                -- exp (2 pi i k n / N)
                expTerm =
                    polar 1 <| 2 * pi * (toFloat <| k * n) / toFloat size
            in
            acc |> add (hk |> multiply expTerm)

        fourierFunction n =
            Array.foldl (foldFunction n) zero indexArray
    in
    Array.initialize size fourierFunction


{-| O(n^2) in the length of the array. Compute the Discrete Fourier Transform of the given
integer array.
 -}
dftInt : Array Int -> Array Complex
dftInt =
    dft << Array.map (real << toFloat)


{-| O(n^2) in the length of the array. Compute the Discrete Fourier Transform of the given
float array.
 -}
dftFloat : Array Float -> Array Complex
dftFloat =
    dft << Array.map real
