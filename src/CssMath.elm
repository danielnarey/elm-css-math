module CssMath exposing
  ( add, subtract, scale, ratio, numOp, absToPx, relToPx, vpRelToPx, absToRem
  , toNumber, toNumberList, toUnit, isNonZero
  )

{-|
## Arithmetic and unit conversions with CSS values

This library extends `CssBasics` by providing helpers for dealing with numeric
components of `CssValue` types. It allows you to add, subtract, scale, and
calculate ratios of numeric CSS values, to convert between units and numbers,
and to convert among absolute and relative units of length.

See
[here](https://developer.mozilla.org/en-US/docs/Web/CSS/length)
for CSS unit specifications.

# Basic Numeric Operations
@docs add, subtract, scale, ratio

## Applying Custom Operations
@docs numOp

# Unit Conversions
@docs absToPx, relToPx, vpRelToPx, absToRem

# Helpers
@docs toUnit, toNumber, toNumberList, isNonZero

-}

import Toolkit.Operators exposing (..)
import Toolkit.Helpers as Helpers
import CssBasics exposing (CssValue(..), UnitType(..))


-- BASIC NUMBER/UNIT OPERATIONS

{-| Add the value in the first argument to the value(s) in the second argument
and return the result, or return an error message if one or more of the
values is of an incompatible type.

**Type compatibility:**
You can add two `Num` values, two `Unit` values with the same unit, or two
`Unit` values with different units that are both absolute (`Px`, `In`, `Cm`,
`Mm`, `Pt`, `Pc`). You can also add a single `Num` or `Unit` value to a `Sides`
or `Multiple`, so long as all of the values are compatible. See the examples
below.

*Zero values are an exception:*
`Num 0` or `Unit 0 _` (with any unit type) may be added to a non-zero unit value
without producing an error.

    Num 1
      |> add (Num 1)

    --> Ok (Num 2)

    Unit 0.5 In
      |> add (Unit 36 Pt)

    --> Ok (Unit 92 Px)

    Unit 1 Em
      |> add (Unit 0.5 Em)

    --> Ok (Unit 1.5 Em)

    Unit 12 Px
      |> add (Num 1)

    --> Err ".."

    Unit 12 Px
      |> add (Unit 1 Em)

    --> Err ".."

    Sides [Unit 0.5 In, Unit 36 Pt, Unit 0 NoUnit, Unit 0 Em]
      |> add (Unit 0.5 In)

    --> Ok (Sides [Unit 1 In, Unit 92 Px, Unit 46 Px, Unit 46 Px])

    Unit 12 Px
      |> add (Num 0)

    --> Ok (Unit 12 Px)

    Unit 12 Px
      |> add (Num 1)

    --> Err ".."

-}
add : CssValue number -> CssValue number -> Result String (CssValue number)
add second first =
  (first, second)
    |> numOp (+)


{-| Subtract the value in the first argument from the value(s) in the second
argument and return the result, or return an error message if one or more of the
values is of an incompatible type. For details on type compatibility, see the
documentation for `add`.

    Unit 1 In
      |> subtract (Unit 36 Pt)

    --> Ok (Unit 46 Px)
-}
subtract : CssValue number -> CssValue number -> Result String (CssValue number)
subtract second first =
  (first, second)
    |> numOp (-)


{-| Scale the value(s) in the second argument by the factor given as the first
argument, or return an error message if the argument contains one or more
non-numeric values.

The argument may be a `Sides` or `Multiple` value, which will return a `Sides`
or `Multiple` containing the scaled unit values.

    Unit 2 Em
      |> scale 0.5

    --> Ok (Unit 1 Em)

    Sides [Unit 12 Px, Unit 2 Em]
      |> scale 0.5

    --> Ok (Sides [Unit 6 Px, Unit 1 Em])
-}
scale : number -> CssValue number -> Result String (CssValue number)
scale factor value =
  case value of
    Num number ->
      (number, factor)
      @@|> (*)
        |> Num
        |> Ok

    Unit number unitType ->
      (number, factor)
      @@|> (*)
        |> toUnit unitType
        |> Ok

    Sides values ->
      values
       .|> scale factor
        |> Helpers.resultList errorMsg
       !|> Sides

    Multiple separator values ->
      values
       .|> scale factor
        |> Helpers.resultList errorMsg
       !|> Multiple separator

    _ ->
      Err errorMsg


{-| Given a tuple of `Num` or `Unit` values, calculate the ratio of the first
value to the second, or return an error message if one or more of the values is
of an incompatible type.

Type compatibility works the same as for `add` and `subtract`, but `Sides` and
`Multiple` values are not allowed.

    (Unit 46 Px, Unit 1 In)
      |> ratio

    --> Ok 0.5

    (Num 0, Unit 1 In)
      |> ratio

    --> Ok 0
-}
ratio : (CssValue Float, CssValue Float) -> Result String Float
ratio (first, second) =
  (first, second)
    |> numOp (/)
   !+> toNumber


-- CUSTOM OPERATIONS

{-| Apply a numeric operation to a pair of `CssValue` values. This is the
generic function called by `add` and `subtract`, so see the documentation above
for details on type compatibility.
-}
numOp : (number -> number -> number) -> (CssValue number, CssValue number) -> Result String (CssValue number)
numOp op (first, second) =
  case (first, second) of
    (Num n1, Num n2) ->
      (n1, n2)
      @@|> op
        |> Num
        |> Ok

    (Unit n1 u1, Unit n2 u2)  ->
      if u1 == u2 then
        (n1, n2)
        @@|> op
          |> toUnit u1
          |> Ok

      else
        (first, second)
        ..|> absToPx
        ..|> Result.andThen toNumber
        @@|> Result.map2 op
         !|> toUnit Px

    (Sides values, _ ) ->
      values
       .|> flip (,) second
       .|> numOp op
        |> Helpers.resultList errorMsg
       !|> Sides

    (Multiple separator values, _ ) ->
      values
       .|> flip (,) second
       .|> numOp op
        |> Helpers.resultList errorMsg
       !|> Multiple separator

    _ ->
      (first, second)
      ..|> absToPx
      ..|> Result.andThen toNumber
      @@|> Result.map2 op
       !|> toUnit Px


-- UNIT CONVERSIONS

{-| Convert any value in absolute units (`Px`, `In`, `Cm`, `Mm`, `Pt`, `Pc`) to
a pixel (`Px`) value. Returns an error if the argument is a non-zero value with
a relative unit, or if the argument contains one or more non-unit values.

The argument may be a `Sides` or `Multiple` value, which will return a `Sides`
or `Multiple` with the converted unit values. `Num 0` will convert to
`Unit 0 Px`, but non-zero `Num` values will return an error message.

    Unit 1 In
      |> absToPx

    --> Ok (Unit 92 Px)

    Unit 1 Em
      |> absToPx

    --> Err ".."

    Sides [Unit 0.5 In, Unit 36 Pt, Unit 0 NoUnit, Unit 0 Em]
      |> absToPx

    --> Ok (Sides [Unit 46 Px, Unit 46 Px, Unit 0 Px, Unit 0 Px])

    Num 0
      |> absToPx

    --> Ok (Unit 0 Px)

    Num 1
      |> absToPx

    --> Err ".."

-}
absToPx : CssValue number -> Result String (CssValue number)
absToPx value =
  let
    convertToPx fromUnit number =
      case fromUnit of
        Px ->
          number
            |> toUnit Px
            |> Ok

        In ->
          number
            |> (*) 92
            |> toUnit Px
            |> Ok

        Cm ->
          number
            |> flip (/) 2.54
            |> convertToPx In

        Mm ->
          number
            |> flip (/) 25.4
            |> convertToPx In

        Pt ->
          number
            |> flip (/) 72
            |> convertToPx In

        Pc ->
          number
            |> (*) 12
            |> convertToPx Pt

        _ ->
          "`UnitType` must be an absolute unit: In, Cm, Mm, Pt, Pc"
            |> Err

  in
    case value of
      Num number ->
        if number == 0 then
          Unit 0 Px
            |> Ok

        else
          errorMsg
            |> Err

      Unit number unitType ->
        if number == 0 then
          Unit 0 Px
            |> Ok

        else
          number
            |> convertToPx unitType

      Sides values ->
        values
         .|> absToPx
          |> Helpers.resultList errorMsg
         !|> Sides

      Multiple separator values ->
        values
         .|> absToPx
          |> Helpers.resultList errorMsg
         !|> Multiple separator

      _ ->
        errorMsg
          |> Err


{-| Convert any value in relative units (`Percent`, `Em`, `Ex`, `Ch`, `Rem`,
`Vh`, `Vw`, `Vmin`, `Vmax`) to a pixel (`Px`) value. The first argument supplies
the pixel length to which the unit value is relative. Conversions from `Ex` and
`Ch` values are approximate, as the exact values of these units are dependent on
font properties.

Returns an error if the argument is a non-zero value with a relative unit, or if
the argument contains one or more non-unit values.

The argument may be a `Sides` or `Multiple` value, which will return a `Sides`
or `Multiple` with the converted unit values. `Num 0` will convert to
`Unit 0 Px`, but non-zero `Num` values will return an error message.

    Unit 1 Em
      |> relToPx 16

    --> Ok (Unit 16 Px)
-}
relToPx : Float -> CssValue Float -> Result String (CssValue Float)
relToPx base value =
  let
    multiply values =
      values
      @@|> (*)
        |> toUnit Px
        |> Ok

    convertToPx fromUnit (number, base) =
      case fromUnit of
        Percent ->
          (number / 100, base)
            |> multiply

        Em ->
          (number, base)
            |> multiply

        Ex ->
          (number, base / 2)
            |> multiply

        Ch ->
          (number, base / 2)
            |> multiply

        Rem ->
          (number, base)
            |> multiply

        Vh ->
          (number / 100, base)
            |> multiply

        Vw ->
          (number / 100, base)
            |> multiply

        Vmin ->
          (number / 100, base)
            |> multiply

        Vmax ->
          (number / 100, base)
            |> multiply

        _ ->
          "`UnitType` must be a relative unit: `Percent`, `Em`, `Ex`, `Ch`, "
            |++ "`Rem`, `Vh`, `Vw`, `Vmin`, `Vmax`"
            |> Err

  in
    case value of
      Num number ->
        if number == 0 then
          Unit 0 Px
            |> Ok

        else
          errorMsg
            |> Err

      Unit number unitType ->
        if number == 0 then
          Unit 0 Px
            |> Ok

        else
          (number, base)
            |> convertToPx unitType

      Sides values ->
        values
         .|> relToPx base
          |> Helpers.resultList errorMsg
         !|> Sides

      Multiple separator values ->
        values
         .|> relToPx base
          |> Helpers.resultList errorMsg
         !|> Multiple separator

      _ ->
        errorMsg
          |> Err


{-| Convert any value in viewport-relative units (`Vh`, `Vw`, `Vmin`, `Vmax`) to
a pixel (`Px`) value. The first argument supplies the width and height of the
viewport in pixels as a 2-tuple.

Returns an error if the argument is a non-zero value with a
non-viewport-relative unit, or if the argument contains one or more non-unit
values.

The argument may be a `Sides` or `Multiple` value, which will return a `Sides`
or `Multiple` with the converted unit values. `Num 0` will convert to
`Unit 0 Px`, but non-zero `Num` values will return an error message.

    Unit 10 Vmin
      |> vpRelToPx (600, 800)

    --> Ok (Unit 60 Px)
-}
vpRelToPx : (Float, Float) -> CssValue Float -> Result String (CssValue Float)
vpRelToPx (width, height) value =
  let
    multiply values =
      values
      @@|> (*)
        |> toUnit Px
        |> Ok

    convertToPx fromUnit (width, height) number =
      case fromUnit of
        Vh ->
          (number / 100, height)
            |> multiply

        Vw ->
          (number / 100, width)
            |> multiply

        Vmin ->
          (number / 100, min width height)
            |> multiply

        Vmax ->
          (number / 100, max width height)
            |> multiply

        _ ->
          "`UnitType` must be a viewport-relative unit: `Vh`, `Vw`, `Vmin`, "
            |++ "`Vmax`"
            |> Err

  in
    case value of
      Num number ->
        if number == 0 then
          Unit 0 Px
            |> Ok

        else
          errorMsg
            |> Err

      Unit number unitType ->
        if number == 0 then
          Unit 0 Px
            |> Ok

        else
          number
            |> convertToPx unitType (width, height)

      Sides values ->
        values
         .|> vpRelToPx (width, height)
          |> Helpers.resultList errorMsg
         !|> Sides

      Multiple separator values ->
        values
         .|> vpRelToPx (width, height)
          |> Helpers.resultList errorMsg
         !|> Multiple separator

      _ ->
        errorMsg
          |> Err


{-| Convert any value in absolute units (`Px`, `In`, `Cm`, `Mm`, `Pt`, `Pc`) to
a `Rem` value. In CSS, "rem" is a relative unit defined by the font size of the
HTML `<body>` element. To convert to rems, the first argument to this function
must supply this base font size in pixels.

Returns an error if the argument is a non-zero value with a relative unit, or if
the argument contains one or more non-unit values.

The argument may be a `Sides` or `Multiple` value, which will return a `Sides`
or `Multiple` with the converted unit values. `Num 0` will convert to
`Unit 0 Rem`, but non-zero `Num` values will return an error message.

    Unit 12 Px
      |> absToRem 16

    --> Ok (Unit 0.75 Rem)
-}
absToRem : Float -> CssValue Float -> Result String (CssValue Float)
absToRem baseFontSize value =
  case value of
    Num number ->
      if number == 0 then
        Unit 0 Px
          |> Ok

      else
        errorMsg
          |> Err

    Unit number unitType ->
      if number == 0 then
        Unit 0 Px
          |> Ok

      else
        value
          |> absToPx
         !+> toNumber
         !|> flip (/) baseFontSize
         !|> toUnit Rem

    Sides values ->
      values
       .|> absToRem baseFontSize
        |> Helpers.resultList errorMsg
       !|> Sides

    Multiple separator values ->
      values
       .|> absToRem baseFontSize
        |> Helpers.resultList errorMsg
       !|> Multiple separator

    _ ->
      errorMsg
        |> Err


-- HELPERS

{-| Convenience function to convert a number to a `Unit`
-}
toUnit : UnitType -> number -> CssValue number
toUnit unitType number =
  Unit number unitType


{-| Extracts the numeric part of a `Num` or `Unit` value, or returns an error
message if the `CssValue` is not a `Num` or `Unit`
-}
toNumber : CssValue number -> Result String number
toNumber value =
  case value of
    Num number ->
      number
        |> Ok

    Unit number unitType ->
      number
        |> Ok

    _ ->
      "Argument must be a `Num` or `Unit`"
        |> Err


{-| Extracts a list of numbers from a `Sides` or `Multiple` value, or a list
containing a single number from a `Num` or `Unit`. Returns an error message if
any of the values are non-numeric.
-}
toNumberList : CssValue number -> Result String (List number)
toNumberList value =
  case value of
    Num number ->
      number
        |> Helpers.wrapList
        |> Ok

    Unit number unitType ->
      number
        |> Helpers.wrapList
        |> Ok

    Sides list ->
      list
       .|> toNumber
        |> Helpers.resultList errorMsg

    Multiple separator list ->
      list
       .|> toNumber
        |> Helpers.resultList errorMsg

    _ ->
      errorMsg
        |> Err


{-| Returns a `True` result if the argument is a `Num` or `Unit` containing a
non-zero value or a `Sides` or `Multiple` value containing at least one
non-zero value. Returns an error message if the argument contains one or more
non-numeric values.
-}
isNonZero : CssValue number -> Result String Bool
isNonZero value =
  case value of
    Num number ->
      number
        |> (/=) 0
        |> Ok

    Unit number unitType ->
      number
        |> (/=) 0
        |> Ok

    Sides values ->
      values
       .|> isNonZero
        |> Helpers.resultList errorMsg
       !|> List.all identity

    Multiple separator values ->
      values
       .|> isNonZero
        |> Helpers.resultList errorMsg
       !|> List.all identity

    _ ->
      errorMsg
        |> Err


-- INTERNAL

errorMsg =
  "Operation failed because of one or more incompatible values. "
    |++ "Check to see if any of your values are non-numeric or if you are "
    |++ "trying to do arithmetic with a mix of a `Num` and a `Unit` values."
