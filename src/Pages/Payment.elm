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
      , window = Size session.flags.width session.flags.height
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



-- Tabs


tab_header val device =
    case device of
        Phone ->
            el [ height (px 50), paddingXY 30 0 ] <|
                el [ width fill, alignBottom, Font.color Styles.text_grey ] <|
                    text val

        _ ->
            el
                [ height (px 60)
                , width fill
                , Border.widthEach { edges | bottom = 1 }
                , Border.color Styles.light_grey
                , paddingEach { edges | bottom = 20, left = 30, top = 25 }
                ]
            <|
                el [ alignBottom ] (text val)


pay_button =
    el [ paddingXY 60 0, width fill, height (px 40), Font.color Styles.white, Font.bold, Font.size 13 ] <|
        el [ width (px 200), height fill, Background.color Styles.red, Border.rounded 2 ] <|
            el [ centerX, centerY ] <|
                text "Proceed to Pay"



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
            column [ width (px 30), height (px 60) ]
                [ placeholder_img 40 [ centerX ]
                , el [ centerX ] <| text val
                ]

        input val callback temp =
            Input.text
                [ width fill
                , height fill
                ]
                { onChange = callback
                , text = val
                , placeholder = Just (Input.placeholder [] <| row [ centerY, spacing 5, Font.color (rgb255 200 200 200) ] [ text temp ])
                , label = Input.labelHidden ""
                }

        proceed_button =
            el
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
                el [ width fill, height fill, Background.color Styles.dark_grey, Border.rounded 2 ] <|
                    el [ centerX, centerY ] <|
                        text "Proceed to Pay"

        custom_tab_header =
            case device of
                Phone ->
                    el [ height (px 10) ] Element.none

                _ ->
                    el
                        [ height (px 60)
                        , width fill
                        , Border.widthEach { edges | bottom = 1 }
                        , Border.color Styles.light_grey
                        , paddingEach { edges | bottom = 20, left = 30, top = 25 }
                        ]
                    <|
                        el [ alignBottom ] (text "Enter Debit / Credit Card Details")

        bg_color =
            case device of
                Phone ->
                    Styles.light_grey

                _ ->
                    Styles.white

        border_shadow =
            if is_phone then
                Styles.border_shadow

            else
                Border.width 0

        card_number =
            column [ width fill, spacing 10, paddingXY 0 20, Font.color Styles.text_grey ]
                [ el [ Font.color Styles.text_grey ] <| text "Card Number"
                , row [ width (fill |> maximum 500), spacing Styles.sp_sml ]
                    [ el [ height fill, width (fill |> maximum 400) ] <| input model.cardNumber CardNumberInput "Enter card number here"
                    , el [] <| placeholder_img 40 []
                    ]
                ]

        expiry_field =
            column [ width (fill |> maximum 150), spacing 10, Font.color Styles.text_grey ]
                [ el [ Font.color Styles.text_grey ] <| text "Expiry"
                , row [ width fill, spacing Styles.sp_sml ]
                    [ el [ height fill, width fill ] <| input model.expiry ExpiryInput "MM / YY"
                    ]
                ]

        cvv_field =
            column [ width (fill |> maximum 150), spacing 10, Font.color Styles.text_grey ]
                [ el [ Font.color Styles.text_grey ] <| text "CVV"
                , row [ width fill, spacing Styles.sp_sml ]
                    [ el [ height fill, width fill, inFront <| placeholder_img 20 [ alignRight, centerY, paddingEach { edges | right = 25 } ] ] <| input model.cvv CVVInput "CVV"
                    ]
                ]
    in
    column
        [ width fill
        , height fill
        , spacing Styles.sp_sml
        , Background.color bg_color
        ]
        [ custom_tab_header
        , column [ width fill, paddingXY Styles.pd_med 0, spacing Styles.sp_sml ]
            [ column
                [ width fill
                , spacing Styles.sp_sml
                , Background.color Styles.white
                , Border.rounded 10
                , border_shadow
                , if is_phone then
                    paddingEach { edges | left = 20, right = 20, bottom = 50 }

                  else
                    padding 0
                ]
                [ card_number
                , row [ width fill, spacing 20 ]
                    [ expiry_field
                    , cvv_field
                    ]
                , row [ Font.color Styles.text_grey, spacing 15, paddingXY 0 15 ]
                    [ placeholder_img 20 []
                    , text "Save this card for faster payments"
                    ]
                , proceed_button
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
                , placeholder = Just (Input.placeholder [] <| row [ centerY, spacing 5, Font.color (rgb255 200 200 200) ] [ placeholder_img 20 [], text "Search" ])
                , label = Input.labelHidden ""
                }
    in
    el [ width fill, height (px 40) ] <| search


netbanking_tab : Model -> DeviceClass -> Element Msg
netbanking_tab model device =
    let
        popular_bank val =
            column [ width (px 30), height (px 60) ]
                [ placeholder_img 40 [ centerX ]
                , el [ centerX ] <| text val
                ]

        icon_url active =
            if active then
                check_url

            else
                circle_url

        bank_list_item i ( val, active ) =
            column [ width fill ]
                [ row [ width fill, height (px 60), spacing 20, onClick (SelectBank i) ]
                    [ placeholder_img 40 [ alignLeft ]
                    , el [] <| text val
                    , Element.image [ height (px 20), width (px 20), centerY, alignRight ]
                        { src = icon_url active
                        , description = ""
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

        border_shadow =
            if is_phone then
                Styles.border_shadow

            else
                Border.width 0
    in
    column
        [ width fill
        , height fill
        , spacing Styles.sp_sml
        , Background.color bg_color
        ]
        [ tab_header "NetBanking" device
        , column [ width fill, paddingXY Styles.pd_med 0, spacing Styles.sp_sml ]
            [ row
                [ height (px 100)
                , width fill
                , spacing 50
                , Background.color Styles.white
                , Border.rounded 10
                , border_shadow
                , if is_phone then
                    paddingXY Styles.pd_med 0

                  else
                    padding 0
                ]
                [ popular_bank "Axis"
                , popular_bank "ICIC"
                , popular_bank "Punjab"
                , popular_bank "SBI"
                ]
            ]
        , column [ height fill, width fill, paddingXY 30 0, spacing 20 ]
            [ el [ width fill ] <| searchbar model.search
            , column
                [ width fill
                , height fill
                , if is_phone then
                    paddingXY Styles.pd_med 0

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

        icon_url active =
            if active then
                check_url

            else
                circle_url

        wallet_list_item i ( val, active ) =
            column [ width fill ]
                [ row [ width fill, height (px 60), spacing 20, onClick (SelectWallet i) ]
                    [ placeholder_img 40 [ alignLeft ]
                    , text val
                    , Element.image [ height (px 20), width (px 20), centerY, alignRight ]
                        { src = icon_url active
                        , description = ""
                        }
                    ]
                , if active then
                    pay_button

                  else
                    Element.none
                ]

        bg_color =
            case device of
                Phone ->
                    Styles.light_grey

                _ ->
                    Styles.white

        border_shadow =
            if is_phone then
                Styles.border_shadow

            else
                Border.width 0
    in
    column
        [ width fill
        , height fill
        , spacing Styles.sp_sml
        , Background.color bg_color
        ]
        [ tab_header "Wallets" device
        , column [ width fill, paddingXY Styles.pd_med 0, spacing Styles.sp_sml ]
            [ column
                [ width fill
                , spacing Styles.sp_sml
                , Background.color Styles.white
                , Border.rounded 10
                , border_shadow
                , if is_phone then
                    paddingXY Styles.pd_sml Styles.pd_med

                  else
                    padding 0
                ]
              <|
                List.indexedMap wallet_list_item model.wallets
            ]
        ]


upi_tab =
    column [ width fill, height fill ] [ Element.none ]


view_tab : Model -> DeviceClass -> Element Msg
view_tab model device =
    let
        content =
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
    column [ width fill, height fill, Background.color Styles.white ]
        [ content
        ]



-- Sidebar/Menu


desktop_link : String -> Msg -> Bool -> Element Msg
desktop_link val callback active =
    let
        base_styles =
            [ onClick callback
            , width fill
            , height (px 60)
            , paddingEach { edges | left = Styles.pd_sml }
            , Border.widthEach { edges | left = 3 }
            , Border.color Styles.transparent
            , mouseOver active_styles
            ]

        active_styles =
            [ Background.color Styles.white, Border.color Styles.red ]

        styles =
            if active then
                base_styles ++ active_styles

            else
                base_styles
    in
    el styles <| el [ centerY ] <| text val


phone_link : String -> Msg -> Bool -> Element Msg
phone_link val callback active =
    row
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
        [ el [ alignLeft ] <| placeholder_img 40 []
        , el [] <| text val
        , el [ alignRight ] <| Element.image [ height (px 20), width (px 20), centerY ] { src = arrow_right_url, description = "" }
        ]


menu : Model -> DeviceClass -> Element Msg
menu model device =
    let
        ( link_container, attrs ) =
            case device of
                Phone ->
                    ( phone_link, [ paddingXY 20 30, spacing 15 ] )

                _ ->
                    ( desktop_link, [ spacing 2 ] )

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
                    row [ alignBottom, centerX, Font.size 16, Font.bold ]
                        [ el [ Font.color Styles.text_grey ] <| text "Powered by "
                        , placeholder_img 30 []
                        , el [] <| text "JUSPAY"
                        ]

        base_styles =
            [ width fill, height fill, paddingEach { edges | bottom = 40 }, Background.color Styles.lighter_grey ]

        styles =
            base_styles ++ attrs
    in
    column styles
        [ link_container "Credit / Debit Card" (ChangeTab Credit) (active && selected == Credit)
        , link_container "Netbanking" (ChangeTab Netbanking) (active && selected == Netbanking)
        , link_container "Wallet" (ChangeTab Wallet) (active && selected == Wallet)
        , link_container "UPI" (ChangeTab UPI) (active && selected == UPI)
        , footer
        ]



-- Body


body model device =
    let
        base_styles =
            [ width fill, height fill ]

        attrs =
            case device of
                Phone ->
                    []

                _ ->
                    [ paddingXY Styles.pd_sml 0 ]

        styles =
            base_styles ++ attrs

        labeling =
            el [] <| text "Payment Method"

        phone_content =
            case model.tab of
                Nothing ->
                    menu model Phone

                Just x ->
                    view_tab model Phone

        desktop_content =
            column
                [ width (fill |> maximum max_width)
                , height fill
                , centerX
                , Background.color Styles.light_grey
                , spacing Styles.sp_sml
                ]
                [ labeling
                , row [ height fill, width fill ]
                    [ el [ width (px 350), height fill ] <| menu model Desktop
                    , view_tab model Desktop
                    ]
                ]

        content =
            case device of
                Phone ->
                    phone_content

                _ ->
                    desktop_content
    in
    el styles <| content



-- Header


number_box =
    el [ width fill, height (px 15), Background.color Styles.light_grey ] <| text ""


header_info : DeviceClass -> Element Msg
header_info device =
    let
        divider =
            case device of
                Phone ->
                    Element.none

                _ ->
                    el
                        [ width (px 1)
                        , height fill
                        , centerX
                        , Border.widthEach { edges | left = 2 }
                        , Border.color Styles.light_grey
                        ]
                        Element.none
    in
    row [ width fill, height (fillPortion 1) ]
        [ column [ alignLeft, spacing 5 ]
            [ el [ Font.color Styles.text_grey, Font.size 14 ] <| text "Mobile Number "
            , number_box
            ]
        , divider
        , column [ alignRight, spacing 5 ]
            [ el [ Font.color Styles.text_grey, Font.size 14 ] <| text "Amount "
            , el [ Font.bold, Font.size 15 ] <| text "₹457"
            ]
        ]


header_title : String -> Msg -> DeviceClass -> Element Msg
header_title title callback device =
    let
        back_icon =
            case device of
                Phone ->
                    Element.image [ height (px 15), width (px 15), centerY ] { src = arrow_left_url, description = "" }

                _ ->
                    Element.none

        attrs =
            case device of
                Phone ->
                    [ Font.color Styles.red, height (fillPortion 1) ]

                _ ->
                    [ height fill, Font.bold, Font.size 15, Font.color Styles.black ]

        styles =
            attrs ++ [ onClick callback, width fill ]
    in
    row styles
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
                    column [ width fill, height (px 80) ]

                _ ->
                    row [ width (fill |> maximum max_width), height (px 60), centerX ]

        content =
            case device of
                Phone ->
                    [ back, header_info device ]

                _ ->
                    [ el [ width (fill |> maximum 350) ] <| header_title "Order summary" Back device
                    , el [ width (fill |> maximum 350) ] <| header_info device
                    ]
    in
    el [ width fill, paddingXY Styles.pd_sml 0, Background.color Styles.white ] <| container content



-- Page


render : Model -> Element Msg
render model =
    let
        device =
            Element.classifyDevice model.window

        _ =
            Debug.log "device" device

        base_styles =
            [ width fill, height fill, spacing Styles.sp_sml ]

        container attrs content =
            column (base_styles ++ attrs) content

        page =
            case device.class of
                Phone ->
                    container []
                        [ header model.tab Phone
                        , body model Phone
                        ]

                _ ->
                    container [ Background.color Styles.light_grey ]
                        [ header model.tab Desktop
                        , body model Desktop
                        ]
    in
    el
        [ width fill
        , height fill
        , Font.family [ Font.typeface "Open Sans" ]
        ]
        page


view : Model -> TitleAndContent Msg
view model =
    { title = "Payments"
    , content = render model
    }



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ BrowserEvents.onResize (\w h -> Resize w h)
        ]
