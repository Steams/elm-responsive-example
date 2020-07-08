module Pages.Payment exposing (Model, Msg, init, subscriptions, update, view)

import Browser.Dom as Dom
import Browser.Events as BrowserEvents
import Browser.Navigation exposing (pushUrl)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick)
import Element.Font as Font
import Element.Input as Input
import Layout exposing (TitleAndContent)
import RemoteData exposing (RemoteData(..), WebData)
import Route
import Session exposing (Session)
import Styles
import Task



-- Model


type alias Model =
    { tab : Maybe Tab
    , window : Size
    , search : String
    , cardNumber : String
    , cvv : String
    , expiry : String
    , banks : List ( String, Bool )
    , wallets : List ( String, Bool )
    }


type alias Size =
    { width : Int, height : Int }


type Tab
    = Netbanking
    | Credit
    | Wallet
    | UPI



-- State


type Msg
    = ChangeTab Tab
    | WindowInfo Dom.Viewport
    | Resize Int Int
    | Back
    | SearchInput String
    | CardNumberInput String
    | ExpiryInput String
    | CVVInput String
    | SelectBank Int
    | SelectWallet Int
    | NoOp


banks =
    [ "Allahabad Bank"
    , "Axis Corporate Netbanking"
    , "BNP Paribas"
    , "Bandhan Bank"
    , "Bank of Bahrain and Kuwait"
    , "Bank of Baroda"
    , "Bank of Baroda Corporate"
    , "Bank of India"
    , "Bank of Maharashtra"
    , "Bassein Catholic Bank"
    ]


wallets =
    [ "Paytm"
    ]


init : Session -> ( Model, Cmd Msg )
init session =
    ( { tab = Nothing
      , window = Size 0 0
      , search = ""
      , cardNumber = ""
      , expiry = ""
      , cvv = ""
      , banks = List.map (\x -> Tuple.pair x False) banks
      , wallets = List.map (\x -> Tuple.pair x False) wallets
      }
    , get_screen_info
    )


get_screen_info =
    Task.perform WindowInfo Dom.getViewport


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        WindowInfo info ->
            let
                _ =
                    Debug.log "init window" info

                size =
                    Size (round info.viewport.width) (round info.viewport.height)

                device =
                    Element.classifyDevice size

                init_tab =
                    case device.class of
                        Phone ->
                            Nothing

                        _ ->
                            Just Credit
            in
            ( { model | window = size, tab = init_tab }
            , Cmd.none
            )

        Resize w h ->
            let
                _ =
                    Debug.log "width " w

                _ =
                    Debug.log "height " h

                size =
                    Size w h

                device =
                    Element.classifyDevice size

                tab =
                    case ( device.class, model.tab ) of
                        ( Phone, _ ) ->
                            model.tab

                        ( _, Nothing ) ->
                            Just Credit

                        -- ^ Sets the credit tab open if we resize from phone view to desktop view while the tab is set to "Nothing".
                        ( _, _ ) ->
                            model.tab
            in
            ( { model | window = size, tab = tab }
            , Cmd.none
            )

        ChangeTab tab ->
            ( { model | tab = Just tab }
            , Cmd.none
            )

        Back ->
            ( { model | tab = Nothing }
            , Cmd.none
            )

        SearchInput val ->
            ( { model | search = val }
            , Cmd.none
            )

        CardNumberInput val ->
            ( { model | cardNumber = val }
            , Cmd.none
            )

        CVVInput val ->
            ( { model | cvv = val }
            , Cmd.none
            )

        ExpiryInput val ->
            ( { model | expiry = val }
            , Cmd.none
            )

        SelectBank i ->
            let
                updated_banks =
                    List.indexedMap
                        (\index ( name, checked ) ->
                            if i == index then
                                ( name, not checked )

                            else
                                ( name, checked )
                        )
                        model.banks
            in
            ( { model | banks = updated_banks }
            , Cmd.none
            )

        SelectWallet i ->
            let
                updated_wallets =
                    List.indexedMap
                        (\index ( name, checked ) ->
                            if i == index then
                                ( name, not checked )

                            else
                                ( name, checked )
                        )
                        model.wallets
            in
            ( { model | wallets = updated_wallets }
            , Cmd.none
            )

        _ ->
            ( model
            , Cmd.none
            )



-- View


max_width =
    1300


edges =
    Styles.edges


arrow_right_url =
    "http://localhost:8080/right.png"


arrow_left_url =
    "http://localhost:8080/left.png"


check_url =
    "http://localhost:8080/check.png"


circle_url =
    "http://localhost:8080/circle.png"


placeholder_url =
    "http://localhost:8080/placeholder.png"


placeholder_img : Int -> List (Attribute Msg) -> Element Msg
placeholder_img size attrs =
    let
        styles =
            [ height (px size), width (px size) ] ++ attrs
    in
    Element.image styles
        { src = placeholder_url
        , description = "placeholder"
        }



-- Credit Tab


credit_tab : Model -> DeviceClass -> Element Msg
credit_tab model device =
    let
        is_phone =
            case device of
                Phone ->
                    True

                _ ->
                    False

        popular_bank val =
            Element.column [ width (px 30), height (px 60) ]
                [ placeholder_img 40 [ centerX ]
                , Element.el [ centerX ] <| text val
                ]

        input val callback temp =
            Input.text
                [ width fill
                , height fill
                ]
                { onChange = callback
                , text = val
                , placeholder = Just (Input.placeholder [] <| Element.row [ centerY, spacing 5, Font.color (rgb255 200 200 200) ] [ text temp ])
                , label = Input.labelHidden ""
                }

        pay_button =
            Element.el
                [ width
                    (if is_phone then
                        fill

                     else
                        fill |> maximum 330
                    )
                , height (px 40)
                , Font.color Styles.white
                , Font.bold
                , Font.size 15
                ]
            <|
                Element.el [ width fill, height fill, Background.color Styles.dark_grey, Border.rounded 2 ] <|
                    Element.el [ centerX, centerY ] <|
                        text "Proceed to Pay"

        tab_header =
            case device of
                Phone ->
                    Element.el
                        [ height (px 10)
                        ]
                        Element.none

                _ ->
                    Element.el
                        [ height (px 60)
                        , width fill
                        , Border.widthEach { edges | bottom = 1 }
                        , Border.color Styles.light_grey
                        , paddingEach { edges | bottom = 20, left = 30, top = 25 }
                        ]
                    <|
                        Element.el [ alignBottom ] (text "Enter Debit / Credit Card Details")

        bg_color =
            case device of
                Phone ->
                    Styles.light_grey

                _ ->
                    Styles.white

        border_shadow =
            if is_phone then
                Border.shadow
                    { offset = ( 0, 0 )
                    , size = 1
                    , blur = 2
                    , color = rgba255 0 0 0 0.1
                    }

            else
                Border.width 0
    in
    Element.column
        [ width fill
        , height fill
        , spacing 20
        , Background.color bg_color
        ]
        [ tab_header
        , Element.column [ width fill, paddingXY 30 0, spacing 20 ]
            [ Element.column
                [ width fill
                , spacing 20
                , Background.color Styles.white
                , Border.rounded 10
                , border_shadow
                , if is_phone then
                    paddingEach { edges | left = 20, right = 20, bottom = 50 }

                  else
                    padding 0
                ]
                [ Element.column [ width fill, spacing 10, paddingXY 0 20, Font.color Styles.text_grey ]
                    [ Element.el [ Font.color Styles.text_grey ] <| text "Card Number"
                    , Element.row [ width (fill |> maximum 500), spacing 20 ]
                        [ Element.el [ height fill, width (fill |> maximum 400) ] <| input model.cardNumber CardNumberInput "Enter card number here"
                        , Element.el [] <| placeholder_img 40 []
                        ]
                    ]
                , Element.row [ width fill, spacing 20 ]
                    [ Element.column [ width (fill |> maximum 150), spacing 10, Font.color Styles.text_grey ]
                        [ Element.el [ Font.color Styles.text_grey ] <| text "Expiry"
                        , Element.row [ width fill, spacing 20 ]
                            [ Element.el [ height fill, width fill ] <| input model.expiry ExpiryInput "MM / YY"
                            ]
                        ]
                    , Element.column [ width (fill |> maximum 150), spacing 10, Font.color Styles.text_grey ]
                        [ Element.el [ Font.color Styles.text_grey ] <| text "CVV"
                        , Element.row [ width fill, spacing 20 ]
                            [ Element.el [ height fill, width fill, inFront <| placeholder_img 20 [ alignRight, centerY, paddingEach { edges | right = 25 } ] ] <| input model.cvv CVVInput "CVV"
                            ]
                        ]
                    ]
                , Element.row [ Font.color Styles.text_grey, spacing 15, paddingXY 0 15 ] [ placeholder_img 20 [], text "Save this card for faster payments" ]
                , pay_button
                ]
            ]
        ]



-- Netbanking Tab


searchbar : String -> Element Msg
searchbar val =
    let
        search =
            Input.text
                [ width fill
                , height fill
                ]
                { onChange = SearchInput
                , text = val
                , placeholder = Just (Input.placeholder [] <| Element.row [ centerY, spacing 5, Font.color (rgb255 200 200 200) ] [ placeholder_img 20 [], text "Search" ])
                , label = Input.labelHidden ""
                }
    in
    Element.el [ width fill, height (px 40) ] <| search


netbanking_tab : Model -> DeviceClass -> Element Msg
netbanking_tab model device =
    let
        popular_bank val =
            Element.column [ width (px 30), height (px 60) ]
                [ placeholder_img 40 [ centerX ]
                , Element.el [ centerX ] <| text val
                ]

        pay_button =
            Element.el [ paddingXY 60 0, width fill, height (px 40), Font.color Styles.white, Font.bold, Font.size 13 ] <|
                Element.el [ width (px 200), height fill, Background.color Styles.red, Border.rounded 2 ] <|
                    Element.el [ centerX, centerY ] <|
                        text "Proceed to Pay"

        bank_list_item i ( val, active ) =
            Element.column [ width fill ]
                [ Element.row [ width fill, height (px 60), spacing 20, onClick (SelectBank i) ]
                    [ placeholder_img 40 [ alignLeft ]
                    , Element.el [] <| text val
                    , Element.el [ alignRight ] <|
                        Element.image [ height (px 20), width (px 20), centerY ]
                            { src =
                                if active then
                                    check_url

                                else
                                    circle_url
                            , description = "check circle"
                            }
                    ]
                , if active then
                    pay_button

                  else
                    Element.none
                ]

        is_phone =
            case device of
                Phone ->
                    True

                _ ->
                    False

        bg_color =
            case device of
                Phone ->
                    Styles.light_grey

                _ ->
                    Styles.white

        tab_header =
            case device of
                Phone ->
                    Element.el
                        [ height (px 50)
                        , paddingXY 30 0
                        ]
                    <|
                        Element.el [ width fill, alignBottom, Font.color Styles.text_grey ] <|
                            text "Popular Banks"

                _ ->
                    Element.el
                        [ height (px 60)
                        , width fill
                        , Border.widthEach { edges | bottom = 1 }
                        , Border.color Styles.light_grey
                        , paddingEach { edges | bottom = 20, left = 30, top = 25 }
                        ]
                    <|
                        Element.el [ alignBottom ] (text "NetBanking")

        border_shadow =
            if is_phone then
                Border.shadow
                    { offset = ( 0, 0 )
                    , size = 1
                    , blur = 2
                    , color = rgba255 0 0 0 0.1
                    }

            else
                Border.width 0
    in
    Element.column
        [ width fill
        , height fill
        , spacing 20
        , Background.color bg_color
        ]
        [ tab_header
        , Element.column [ width fill, paddingXY 30 0, spacing 20 ]
            [ Element.row
                [ height (px 100)
                , width fill
                , spacing 50
                , Background.color Styles.white
                , Border.rounded 10
                , border_shadow
                , if is_phone then
                    paddingXY 20 0

                  else
                    padding 0
                ]
                [ popular_bank "Axis"
                , popular_bank "ICIC"
                , popular_bank "Punjab"
                , popular_bank "SBI"
                ]
            ]
        , Element.column [ height fill, width fill, paddingXY 30 0, spacing 20 ]
            [ Element.el [ width fill ] <| searchbar model.search
            , Element.column
                [ width fill
                , height fill
                , if is_phone then
                    paddingXY 20 0

                  else
                    padding 0
                , Background.color Styles.white
                , Border.rounded 10
                , border_shadow
                ]
              <|
                List.indexedMap bank_list_item model.banks
            ]
        ]


wallet_tab : Model -> DeviceClass -> Element Msg
wallet_tab model device =
    let
        is_phone =
            case device of
                Phone ->
                    True

                _ ->
                    False

        pay_button =
            Element.el [ paddingXY 60 0, width fill, height (px 40), Font.color Styles.white, Font.bold, Font.size 13 ] <|
                Element.el [ width (px 200), height fill, Background.color Styles.red, Border.rounded 2 ] <|
                    Element.el [ centerX, centerY ] <|
                        text "Proceed to Pay"

        wallet_list_item i ( val, active ) =
            Element.column [ width fill ]
                [ Element.row [ width fill, height (px 60), spacing 20, onClick (SelectWallet i) ]
                    [ placeholder_img 40 [ alignLeft ]
                    , Element.el [] <| text val
                    , Element.el [ alignRight ] <|
                        Element.image [ height (px 20), width (px 20), centerY ]
                            { src =
                                if active then
                                    check_url

                                else
                                    circle_url
                            , description = "check circle"
                            }
                    ]
                , if active then
                    pay_button

                  else
                    Element.none
                ]

        tab_header =
            case device of
                Phone ->
                    Element.el
                        [ height (px 50)
                        , paddingXY 30 0
                        ]
                    <|
                        Element.el [ width fill, alignBottom, Font.color Styles.text_grey ] <|
                            text "Wallets"

                _ ->
                    Element.el
                        [ height (px 60)
                        , width fill
                        , Border.widthEach { edges | bottom = 1 }
                        , Border.color Styles.light_grey
                        , paddingEach { edges | bottom = 20, left = 30, top = 25 }
                        ]
                    <|
                        Element.el [ alignBottom ] (text "Wallets")

        bg_color =
            case device of
                Phone ->
                    Styles.light_grey

                _ ->
                    Styles.white

        border_shadow =
            if is_phone then
                Border.shadow
                    { offset = ( 0, 0 )
                    , size = 1
                    , blur = 2
                    , color = rgba255 0 0 0 0.1
                    }

            else
                Border.width 0
    in
    Element.column
        [ width fill
        , height fill
        , spacing 20
        , Background.color bg_color
        ]
        [ tab_header
        , Element.column [ width fill, paddingXY 30 0, spacing 20 ]
            [ Element.column
                [ width fill
                , spacing 20
                , Background.color Styles.white
                , Border.rounded 10
                , border_shadow
                , if is_phone then
                    paddingXY 20 30

                  else
                    padding 0
                ]
              <|
                List.indexedMap wallet_list_item model.wallets
            ]
        ]


upi_tab =
    Element.column [ width fill, height fill ] [ Element.none ]


view_tab : Model -> DeviceClass -> Element Msg
view_tab model device =
    let
        stuff =
            case model.tab of
                Just Credit ->
                    credit_tab model device

                Just Netbanking ->
                    netbanking_tab model device

                Just Wallet ->
                    wallet_tab model device

                Just UPI ->
                    upi_tab

                Nothing ->
                    text ""
    in
    Element.column [ width fill, height fill, Background.color Styles.white ]
        [ stuff
        ]



-- Sidebar/Menu


link : String -> Msg -> Bool -> Element Msg
link val callback active =
    let
        active_styles =
            [ Background.color Styles.white, Border.color Styles.red ]

        extra_styles =
            if active then
                active_styles

            else
                []
    in
    Element.el
        ([ onClick callback
         , width fill
         , height (px 60)
         , paddingEach { edges | left = 20 }
         , Border.widthEach { edges | left = 3 }
         , Border.color Styles.transparent
         , mouseOver active_styles
         ]
            ++ extra_styles
        )
    <|
        Element.el [ centerY ] <|
            text val


menu_link : String -> Msg -> Bool -> Element Msg
menu_link val callback active =
    Element.row
        [ width fill
        , height (px 70)
        , Border.rounded 7
        , Border.width 1
        , Border.color Styles.light_grey
        , Background.color Styles.white
        , paddingXY 10 0
        , onClick callback
        , spacing 30
        ]
        [ Element.el [ alignLeft ] <| placeholder_img 40 []
        , Element.el [] <| text val
        , Element.el [ alignRight ] <| Element.image [ height (px 20), width (px 20), centerY ] { src = arrow_right_url, description = "" }
        ]


number_box =
    Element.el [ width fill, height (px 15), Background.color Styles.light_grey ] <| text ""


menu : Model -> DeviceClass -> Element Msg
menu model device =
    let
        ( link_container, styles ) =
            case device of
                Phone ->
                    ( menu_link, [ paddingXY 20 30, spacing 15 ] )

                _ ->
                    ( link, [ spacing 2 ] )

        ( active, selected ) =
            case model.tab of
                Nothing ->
                    ( False, Credit )

                Just x ->
                    ( True, x )

        footer =
            case device of
                Phone ->
                    Element.none

                _ ->
                    Element.row [ alignBottom, centerX, Font.size 16, Font.bold ]
                        [ Element.el [ Font.color Styles.text_grey ] <| text "Powered by "
                        , placeholder_img 30 []
                        , Element.el [] <| text "JUSPAY"
                        ]
    in
    Element.column ([ width fill, height fill, paddingEach { edges | bottom = 40 }, Background.color Styles.lighter_grey ] ++ styles)
        [ link_container "Credit / Debit Card" (ChangeTab Credit) (active && selected == Credit)
        , link_container "Netbanking" (ChangeTab Netbanking) (active && selected == Netbanking)
        , link_container "Wallet" (ChangeTab Wallet) (active && selected == Wallet)
        , link_container "UPI" (ChangeTab UPI) (active && selected == UPI)
        , footer
        ]



-- TODO separate the base reusable components from the ones concerned with layout
-- if the el has the same functionality then keep the same container and pass in the device type


labeling =
    Element.el [] <| text "Payment Method"


desktop_body model =
    Element.el [ width fill, height fill, paddingXY 20 0 ] <|
        Element.column
            [ width (fill |> maximum max_width)
            , height fill
            , centerX
            , Background.color Styles.light_grey
            , spacing 20
            ]
            [ labeling
            , Element.row [ height fill, width fill ]
                [ Element.el [ width (px 350), height fill ] <| menu model Desktop
                , view_tab model Desktop
                ]
            ]


phone_body model =
    -- TODO can merge this with desktop body tbh
    let
        content =
            case model.tab of
                Nothing ->
                    menu model Phone

                Just x ->
                    view_tab model Phone
    in
    Element.el [ height fill, width fill ] <| content


header_info : DeviceClass -> Element Msg
header_info device =
    let
        divider =
            case device of
                Phone ->
                    Element.none

                _ ->
                    Element.el
                        [ width (px 1)
                        , height fill
                        , centerX
                        , Border.widthEach { edges | left = 2 }
                        , Border.color Styles.light_grey
                        ]
                        Element.none
    in
    Element.row [ width fill, height (fillPortion 1) ]
        [ Element.column [ alignLeft, spacing 5 ]
            [ Element.el [ Font.color Styles.text_grey, Font.size 14 ] <| text "Mobile Number "
            , number_box
            ]
        , divider
        , Element.column [ alignRight, spacing 5 ]
            [ Element.el [ Font.color Styles.text_grey, Font.size 14 ] <| text "Amount "
            , Element.el [ Font.bold, Font.size 15 ] <| text "₹457"
            ]
        ]


header_title : String -> Msg -> DeviceClass -> Element Msg
header_title title callback device =
    let
        styles =
            case device of
                Phone ->
                    [ Font.color Styles.red, height (fillPortion 1) ]

                _ ->
                    [ height fill, Font.bold, Font.size 15, Font.color Styles.black ]

        back_icon =
            case device of
                Phone ->
                    Element.image [ height (px 15), width (px 15), centerY ] { src = arrow_left_url, description = "" }

                _ ->
                    Element.none
    in
    Element.row (styles ++ [ onClick callback, width fill ])
        [ back_icon
        , text title
        ]


header : Maybe Tab -> DeviceClass -> Element Msg
header tab device =
    let
        back =
            case tab of
                Nothing ->
                    header_title "Payment Methods" Back device

                Just Credit ->
                    header_title "Add Card" Back device

                Just Netbanking ->
                    header_title "Netbanking" Back device

                Just Wallet ->
                    header_title "Wallet" Back device

                Just UPI ->
                    header_title "UPI" Back device

        container =
            case device of
                Phone ->
                    Element.column [ width fill, height (px 80) ]

                _ ->
                    Element.row [ width (fill |> maximum max_width), height (px 60), centerX ]

        content =
            case device of
                Phone ->
                    [ back, header_info device ]

                _ ->
                    [ Element.el [ width (fill |> maximum 350) ] <| header_title "Order summary" Back device
                    , Element.el [ width (fill |> maximum 350) ] <| header_info device
                    ]
    in
    Element.el [ width fill, paddingXY 20 0, Background.color Styles.white ] <| container content


desktop_view model =
    Element.column [ width fill, height fill, spacing 20, Background.color Styles.light_grey ]
        [ header model.tab Desktop
        , desktop_body model
        ]


phone_view model =
    Element.column [ width fill, height fill, spacing 20 ]
        [ header model.tab Phone
        , phone_body model
        ]


render : Model -> Element Msg
render model =
    let

        device =
            Element.classifyDevice model.window

        _ =
            Debug.log "device" device

        page_elements =
            case device.class of
                Phone ->
                    [ phone_view model ]

                _ ->
                    [ desktop_view model ]
    in
    row
        [ width fill
        , height fill
        , Font.family [ Font.typeface "Open Sans" ]
        ]
        page_elements


view : Model -> TitleAndContent Msg
view model =
    { title = "Payments"
    , content = render model
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ BrowserEvents.onResize (\w h -> Resize w h)
        ]
