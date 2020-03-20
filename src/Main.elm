module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Json.Decode
import PlanParsers.Json exposing (..)



---- MODEL ----


type Page
    = InputPage
    | DisplayPage


type alias Model =
    { currPage : Page
    , currPlanText : String
    }


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( { currPage = InputPage
      , currPlanText = ""
      }
    , Cmd.none
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



---- UPDATE ----


type Msg
    = NoOp
    | ChangePlanText String
    | SubmitPlan


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangePlanText s ->
            ( { model | currPlanText = s }, Cmd.none )

        SubmitPlan ->
            ( { model | currPage = DisplayPage }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )



---- VIEW ----


blue : Color
blue =
    rgb255 52 101 164


navBar : Element Msg
navBar =
    row
        [ width fill
        , paddingXY 60 10
        , Border.widthEach { bottom = 1, top = 0, left = 0, right = 0 }
        , Border.color blue
        ]
        [ el [ alignLeft ] <| text "VisExp"
        , el [ alignRight ] <| text "Menu"
        ]


lightCharcoal : Color
lightCharcoal =
    rgb255 136 138 133


green : Color
green =
    rgb255 0 128 0


darkGreen : Color
darkGreen =
    rgb255 0 100 0


white : Color
white =
    rgb255 255 255 255


inputPage : Model -> Element Msg
inputPage model =
    column
        [ width (px 800)
        , spacingXY 0 10
        , centerX
        ]
        [ Input.multiline
            [ height (px 300)
            , Border.width 1
            , Border.rounded 3
            , Border.color lightCharcoal
            , padding 3
            ]
            { onChange = ChangePlanText
            , text = model.currPlanText
            , placeholder = Nothing
            , label = Input.labelAbove [] <| text "Paste the EXPLAIN output in JSON format:"
            , spellcheck = False
            }
        , Input.button
            [ Background.color green
            , Border.color darkGreen
            , Border.rounded 3
            , Border.widthEach { bottom = 3, top = 0, right = 0, left = 0 }
            , Font.bold
            , Font.color white
            , paddingXY 20 6
            , alignRight
            , width (px 200)
            , height (px 40)
            ]
            { onPress = Just SubmitPlan
            , label = el [ centerX ] <| text "Go!"
            }
        ]


displayPage : Model -> Element Msg
displayPage model =
    let
        tree =
            case Json.Decode.decodeString decodePlanJson model.currPlanText of
                Ok planJson ->
                    text "Success"

                Err err ->
                    text <| Json.Decode.errorToString err
    in
    column [] [ tree ]


view : Model -> Browser.Document Msg
view model =
    let
        content =
            case model.currPage of
                DisplayPage ->
                    displayPage model

                InputPage ->
                    inputPage model
    in
    { title = "VisExp"
    , body =
        [ layout [] <|
            column [ width fill, spacingXY 0 20 ]
                [ navBar
                , content
                ]
        ]
    }



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.document
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
