module FFT exposing (fft, fftInt, fftFloat)

import Array exposing (Array)
import Complex exposing (Complex, add, multiply, polar, real, zero)


{-| O(n*log n) in the length of the array. Compute the Fast Fourier Transform of the given
array.
-}
fft : Array Complex -> Array Complex
fft array =
    let
        size =
            Array.length array
    in
    if size == 1 then
        array

    else
        let
            evensArray =
                fft <| Array.initialize (size // 2) (\n -> Maybe.withDefault zero (Array.get (2 * n) array))

            oddsArray =
                fft <| Array.initialize (size // 2) (\n -> Maybe.withDefault zero (Array.get (2 * n + 1) array))

            fourierFunction n =
                let
                    evensN =
                        Maybe.withDefault zero (Array.get (modBy (size // 2) n) evensArray)

                    oddsN =
                        Maybe.withDefault zero (Array.get (modBy (size // 2) n) oddsArray)

                    twiddleFactor =
                        polar 1 <| 2 * pi * toFloat n / toFloat size
                in
                add evensN (multiply twiddleFactor oddsN)
        in
        Array.initialize size fourierFunction


{-| O(n*log n) in the length of the array. Compute the Fast Fourier Transform of the given
integer array.
-}
fftInt : Array Int -> Array Complex
fftInt =
    fft << Array.map (real << toFloat)

{-| O(n*log n) in the length of the array. Compute the Fast Fourier Transform of the given
float array.
-}
fftFloat : Array Float -> Array Complex
fftFloat =
    fft << Array.map real

