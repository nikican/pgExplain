module Color exposing (blue, darkCharcoal, gray, green, lightBlue, lightCharcoal, lightGray, lightGreen, lightYellow, red, white)

import Element exposing (Color, rgb255)
import FlatColors.AmericanPalette as FlatColors exposing (..)


white : Color
white =
    rgb255 255 255 255


lightGreen : Color
lightGreen =
    FlatColors.lightGreenishBlue


green : Color
green =
    FlatColors.mintLeaf


red : Color
red =
    FlatColors.chiGong


lightGray : Color
lightGray =
    FlatColors.cityLights


gray : Color
gray =
    FlatColors.soothingBreeze


lightCharcoal : Color
lightCharcoal =
    FlatColors.americanRiver


darkCharcoal : Color
darkCharcoal =
    FlatColors.draculaOrchid


lightBlue : Color
lightBlue =
    FlatColors.greenDarnerTail


blue : Color
blue =
    FlatColors.electronBlue


lightYellow : Color
lightYellow =
    FlatColors.sourLemon
