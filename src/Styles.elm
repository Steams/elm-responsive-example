module Styles exposing (..)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font



-- Colors


black =
    rgb255 0 0 0


red =
    rgb255 250 50 50


white =
    rgb255 255 255 255


text_grey =
    rgb255 135 135 135


dark_grey =
    rgb255 179 179 179


light_grey =
    rgb255 240 240 240


lighter_grey =
    rgb255 250 250 250


transparent =
    rgba 0 0 0 0



-- Fonts


font_small =
    Font.size 16



-- Utils


edges =
    { bottom = 0
    , left = 0
    , right = 0
    , top = 0
    }


corners =
    { topLeft = 0
    , topRight = 0
    , bottomLeft = 0
    , bottomRight = 0
    }


padding_small =
    20


padding_med =
    30


padding_large =
    50


spacing_small =
    20


spacing_med =
    30


spacing_large =
    50
