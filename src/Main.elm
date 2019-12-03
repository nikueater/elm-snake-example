module Main exposing (main)

import Browser
import Browser.Events as Event
import Element exposing (..)
import Element.Background as Bg
import Element.Border as Border
import Html exposing (Html)
import Json.Decode as Decode
import List.Extra as List
import Maybe.Extra as Maybe
import Random
import Time


type alias Model =
    { head : ( Int, Int )
    , body : List ( Int, Int )
    , direction : ( Int, Int )
    , food : Maybe ( Int, Int )
    }


type Msg
    = Tick Time.Posix
    | ChangeDirection ( Int, Int )
    | NewFood ( Int, Int )


type Cell
    = None
    | Snake
    | Food


stageSize : { w : Int, h : Int }
stageSize =
    { w = 20, h = 10 }


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


setFood : Cmd Msg
setFood =
    Random.pair (Random.int 0 (stageSize.w - 1)) (Random.int 0 (stageSize.h - 1))
        |> Random.generate NewFood


init : ( Model, Cmd Msg )
init =
    let
        body =
            List.initialize 4 (\n -> ( 9 - n, 5 ))
    in
    ( { head = ( 10, 5 ), body = body, direction = ( 1, 0 ), food = Nothing }, setFood )


move : Model -> Model
move model =
    let
        applyTuple f ( a, b ) ( c, d ) =
            ( f a c, f b d )

        head =
            model.head
                |> applyTuple (+) model.direction
                |> applyTuple modBy ( stageSize.w, stageSize.h )

        body =
            model.body
                |> List.init
                |> Maybe.unwrap [] ((::) model.head)
    in
    { model | head = head, body = body }


checkEaten : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
checkEaten ( model, msg ) =
    if Just model.head == model.food then
        ( { model
            | food = Nothing
            , body =
                List.last model.body
                    |> Maybe.unwrap model.body (\x -> List.append model.body [ x ])
          }
        , setFood
        )

    else
        ( model, msg )


checkCollapse : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
checkCollapse (( model, _ ) as default) =
    model.body
        |> List.elemIndex model.head
        |> Maybe.unwrap default (always init)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick _ ->
            ( move model, Cmd.none )
                |> checkEaten
                |> checkCollapse

        ChangeDirection direction ->
            ( { model | direction = direction }, Cmd.none )

        NewFood food ->
            ( { model | food = Just food }, Cmd.none )


keyDecoder : ( Int, Int ) -> Decode.Decoder ( Int, Int )
keyDecoder default =
    Decode.field "key" Decode.string
        |> Decode.map
            (\k ->
                case ( default, k ) of
                    ( ( _, 0 ), "ArrowUp" ) ->
                        ( 0, -1 )

                    ( ( _, 0 ), "ArrowDown" ) ->
                        ( 0, 1 )

                    ( ( 0, _ ), "ArrowLeft" ) ->
                        ( -1, 0 )

                    ( ( 0, _ ), "ArrowRight" ) ->
                        ( 1, 0 )

                    _ ->
                        default
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every 300 Tick
        , Event.onKeyDown (Decode.map ChangeDirection (keyDecoder model.direction))
        ]


stage : Model -> List Cell
stage model =
    let
        indexToCor i =
            ( modBy stageSize.w i, i // stageSize.w )

        corToCell c =
            if model.food == Just c then
                Food

            else if List.member c (model.head :: model.body) then
                Snake

            else
                None
    in
    List.initialize (stageSize.w * stageSize.h) (indexToCor >> corToCell)


view : Model -> Html Msg
view model =
    layout [ Bg.color (rgb255 220 220 220) ] (viewStage model)


viewStage : Model -> Element Msg
viewStage model =
    wrappedRow
        [ width (px (stageSize.w * 20)) ]
        (List.map viewCell (stage model))


viewCell : Cell -> Element Msg
viewCell cell =
    el
        [ width (px 20), height (px 20), padding 1 ]
    <|
        case cell of
            None ->
                none

            Snake ->
                el [ width fill, height fill, Bg.color (rgb255 200 160 0), Border.rounded 2 ] none

            Food ->
                el [ width fill, height fill, Bg.color (rgb255 0 200 0), Border.rounded 2 ] none
