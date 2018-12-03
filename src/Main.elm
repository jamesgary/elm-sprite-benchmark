module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import Browser.Events
import Html exposing (Html)
import Html.Attributes


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Flags =
    {}


type alias Model =
    { sprites : List Sprite
    }


type alias Sprite =
    { x : Float
    , y : Float
    , xVel : Float
    , yVel : Float
    , bounceSpeed : Float
    }


type Msg
    = Tick Float


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { sprites =
            [ { x = 0.5
              , y = 0.5
              , xVel = 0.3
              , yVel = -0.4
              , bounceSpeed = 3
              }
            ]
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick msDelta ->
            let
                delta =
                    msDelta / 1000

                gravity =
                    10
            in
            ( { model
                | sprites =
                    model.sprites
                        |> List.map
                            (\sprite ->
                                let
                                    ( xTravelDist, yTravelDist ) =
                                        ( delta * sprite.xVel
                                        , delta * sprite.yVel
                                        )

                                    ( newX, newY ) =
                                        ( sprite.x + xTravelDist
                                        , sprite.y + yTravelDist
                                        )

                                    ( x, xVel ) =
                                        if newX < 0 || newX > 1 then
                                            ( sprite.x - xTravelDist, -sprite.xVel )

                                        else
                                            ( newX, sprite.xVel )

                                    ( y, yVel ) =
                                        if sprite.y + yTravelDist < 0 then
                                            ( sprite.y - yTravelDist, sprite.bounceSpeed )

                                        else
                                            ( sprite.y + yTravelDist, sprite.yVel )
                                in
                                { sprite
                                    | x = x
                                    , y = y
                                    , xVel = xVel
                                    , yVel = yVel - (gravity * delta)
                                }
                            )
              }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Browser.Events.onAnimationFrameDelta Tick


view : Model -> Html Msg
view model =
    let
        width =
            200

        height =
            200
    in
    Html.div
        [ Html.Attributes.style "width" (px width)
        , Html.Attributes.style "height" (px height)
        , Html.Attributes.style "padding-right" (px 10)
        , Html.Attributes.style "border" "1px solid black"
        , Html.Attributes.style "position" "relative"
        ]
        (model.sprites
            |> List.map
                (\sprite ->
                    Html.div
                        [ Html.Attributes.style "background" "orange"
                        , Html.Attributes.style "width" "10px"
                        , Html.Attributes.style "height" "10px"
                        , Html.Attributes.style "position" "absolute"
                        , Html.Attributes.style "left" (width * sprite.x |> px)
                        , Html.Attributes.style "bottom" (width * sprite.y |> px)
                        ]
                        []
                )
        )


px : Float -> String
px num =
    String.fromFloat num ++ "px"
