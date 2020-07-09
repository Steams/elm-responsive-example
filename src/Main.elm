module Main exposing (main)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Element exposing (..)
import Html exposing (..)
import Json.Decode as Decode exposing (Value)
import Json.Encode as Encode
import Layout as Layout
import Pages.Blank as Blank
import Pages.NotFound as NotFound
import Pages.Payment as Payment
import RemoteData as RemoteData exposing (RemoteData(..), WebData)
import Route exposing (Route)
import Session exposing (Session,Flags)
import Task
import Url exposing (Url)


type Page
    = Redirect
    | Payment Payment.Model
    | NotFound


type alias Model =
    { session : Session, page : Page }



-- MODEL


init : Flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url navKey =
    let
        model =
            Model (Session navKey flags) Redirect

        route = Just Route.Payment
    in
    goto route model



-- UPDATE


type Msg
    = ChangeUrl Url
    | RequestUrl Browser.UrlRequest
    | PaymentMsg Payment.Msg
    | NoOp



-- | GotSession Session


goto : Maybe Route -> Model -> ( Model, Cmd Msg )
goto maybeRoute model =
    case maybeRoute of
        Nothing ->
            ( { model | page = NotFound }, Cmd.none )

        Just Route.Payment ->
            let
                ( payment, payment_msg ) =
                    Payment.init model.session
            in
            ( { model | page = Payment payment }
            , Cmd.map PaymentMsg payment_msg
            )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.page ) of
        ( RequestUrl urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.session.navKey (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        ( ChangeUrl url, _ ) ->
            goto (Route.fromUrl url) model


        ( PaymentMsg subMsg, Payment payment ) ->
            let
                ( payment_model, payment_msg ) =
                    Payment.update subMsg payment
            in
            ( { model | page = Payment payment_model }
            , Cmd.map PaymentMsg payment_msg
            )

        ( _, _ ) ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.page of
        Payment payment ->
            Sub.batch
                [
                 Sub.map PaymentMsg (Payment.subscriptions payment)
                ]

        _ ->
            Sub.none



-- VIEW


view : Model -> Document Msg
view model =
    let
        render : Layout.Layout -> (subMsg -> Msg) -> Layout.TitleAndContent subMsg -> Document Msg
        render layout msg_wrapper page =
            Layout.render layout { title = page.title, content = Element.map msg_wrapper page.content }
    in
    case model.page of
        Redirect ->
            render Layout.Other (\_ -> NoOp) Blank.view

        NotFound ->
            render Layout.Other (\_ -> NoOp) NotFound.view

        Payment payment ->
            render Layout.Payment PaymentMsg (Payment.view payment)



-- MAIN

main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , onUrlChange = ChangeUrl
        , onUrlRequest = RequestUrl
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
