module Attr exposing (error, greenButton, greyButton, input)

import Color exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font


greenButton : List (Attribute msg)
greenButton =
    [ Background.color lightGreen
    , Border.color green
    , Border.rounded 3
    , Border.widthEach { bottom = 3, top = 0, right = 0, left = 0 }
    , Font.bold
    , Font.color white
    , paddingXY 20 6
    ]


greyButton : List (Attribute msg)
greyButton =
    [ Background.color lightGray
    , Border.color gray
    , Border.rounded 3
    , Border.widthEach { bottom = 1, right = 1, top = 0, left = 0 }
    , Font.bold
    , Font.color lightCharcoal
    ]


input : List (Attribute msg)
input =
    [ Border.width 1, Border.rounded 3, Border.color lightCharcoal, padding 3 ]


error : List (Attribute msg)
error =
    [ Font.color red ]
