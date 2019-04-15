module ComplexNumbers

open System

type ComplexNumber = { Real : double; Imaginary : double }

let create real imaginary = { Real = real; Imaginary = imaginary }

let mul z1 z2 =
    let real = z1.Real * z2.Real - z1.Imaginary * z2.Imaginary
    let imaginary = z1.Imaginary * z2.Real + z1.Real * z2.Imaginary

    create real imaginary

let add z1 z2 =
    create (z1.Real + z2.Real) (z1.Imaginary + z2.Imaginary)

let sub z1 z2 =
    create (z1.Real - z2.Real) (z1.Imaginary - z2.Imaginary)

let div z1 z2 =
    let quotient = z2.Real ** 2. + z2.Imaginary ** 2.

    let real =
        (z1.Real * z2.Real + z1.Imaginary * z2.Imaginary) / quotient

    let imaginary =
        (z1.Imaginary * z2.Real - z1.Real * z2.Imaginary) / quotient

    create real imaginary

let abs z = z.Real ** 2. + z.Imaginary ** 2. |> sqrt

let conjugate z = create z.Real -z.Imaginary

let real z = z.Real

let imaginary z = z.Imaginary

let multiplyScalar z x = create (z.Real * x) (z.Imaginary * x)

let exp z =
    (create (cos z.Imaginary) (sin z.Imaginary), exp z.Real)
    ||> multiplyScalar