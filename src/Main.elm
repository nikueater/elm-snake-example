module Main exposing (main)

import Browser
import Browser.Events as Event
import Element exposing (..)
import Element.Background as Bg
import Html exposing (Html)
import Json.Decode as Decode
import List.Extra as List
import Random
import Time


type alias Model =
    { body : List ( Int, Int )
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
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


setFood : Cmd Msg
setFood =
    Random.pair (Random.int 0 (stageSize.w - 1)) (Random.int 0 (stageSize.h - 1))
        |> Random.generate NewFood


init : () -> ( Model, Cmd Msg )
init _ =
    let
        body =
            List.initialize 5 (\n -> ( 10 - n, 5 ))
    in
    ( { body = body, direction = ( 1, 0 ), food = Nothing }, setFood )


move : Model -> List ( Int, Int )
move model =
    let
        applyTuple f ( a, b ) ( c, d ) =
            ( f a c, f b d )

        head =
            model.body
                |> List.head
                |> Maybe.map (applyTuple (+) model.direction)
                |> Maybe.withDefault ( 0, 0 )
                |> applyTuple modBy ( stageSize.w, stageSize.h )
    in
    model.body
        |> List.init
        |> Maybe.map ((::) head)
        |> Maybe.withDefault []


checkEaten : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
checkEaten ( model, msg ) =
    if List.head model.body == model.food then
        ( { model
            | food = Nothing
            , body =
                List.last model.body
                    |> Maybe.map (\x -> List.append model.body [ x ])
                    |> Maybe.withDefault model.body
          }
        , setFood
        )

    else
        ( model, msg )


checkCollapse : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
checkCollapse ( model, cmd ) =
    List.uncons model.body
        |> Maybe.andThen (\( x, xs ) -> List.elemIndex x xs)
        |> Maybe.map (always (init ()))
        |> Maybe.withDefault ( model, cmd )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick _ ->
            ( { model | body = move model }, Cmd.none )
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
            if Maybe.map ((==) c) model.food == Just True then
                Food

            else if List.member c model.body then
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
                el [ width fill, height fill, Bg.color (rgb255 200 160 0) ] none

            Food ->
                el [ width fill, height fill, Bg.color (rgb255 0 200 0) ] none
