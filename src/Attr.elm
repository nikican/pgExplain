module Attr exposing (greenButton, greyButton)

import Color exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import FlatColors.AmericanPalette exposing (..)


greenButton : List (Attribute msg)
greenButton =
    [ Background.color lightGreenishBlue
    , Border.color mintLeaf
    , Border.rounded 3
    , Border.widthEach { bottom = 3, top = 0, right = 0, left = 0 }
    , Font.bold
    , Font.color white
    , paddingXY 20 6
    ]


greyButton : List (Attribute msg)
greyButton =
    [ Background.color cityLights
    , Border.color soothingBreeze
    , Border.rounded 3
    , Border.widthEach { bottom = 1, right = 1, top = 0, left = 0 }
    , Font.bold
    , Font.color americanRiver
    ]
