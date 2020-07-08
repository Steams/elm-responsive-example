module Layout exposing (Layout(..), TitleAndContent, render)

import Browser exposing (Document)
import Element exposing (..)
import Route exposing (Route)
import Styles


type Layout
    = Other
    | Home
    | Details
    | Payment


type alias TitleAndContent msg =
    { title : String, content : Element msg }


render : Layout -> TitleAndContent msg -> Document msg
render layout page =
    let
        build content =
            Element.layoutWith
                { options =
                    [ focusStyle
                        { borderColor = Nothing
                        , backgroundColor = Nothing
                        , shadow = Nothing
                        }
                    ]
                }
                [ height fill, width fill ]
                (view_container layout content)
    in
    { title = page.title
    , body = [ build page.content ]
    }


view_container : Layout -> Element msg -> Element msg
view_container layout content =
    Element.column
        [ width fill, height fill]
        [
          content
        ]
