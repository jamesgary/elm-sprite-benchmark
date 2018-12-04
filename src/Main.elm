module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import Browser.Events
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Random


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Flags =
    { timestamp : Float
    }


type alias Model =
    { sprites : List Sprite
    , seed : Random.Seed
    , renderer : Renderer
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
    | ChangeRenderer Renderer


type Renderer
    = HtmlTopLeft
    | HtmlTransformTranslate


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        seed =
            Random.initialSeed (round flags.timestamp)

        ( newSprites, newSeed ) =
            Random.step
                (Random.list 100 spriteGenerator)
                seed
    in
    ( { sprites = newSprites
      , seed = newSeed
      , renderer = HtmlTopLeft
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

                smidge =
                    min (List.length model.sprites - 1) (0.01 * toFloat (List.length model.sprites) |> ceiling)

                ( newSprites, newSeed ) =
                    if delta < (1 / 55) then
                        Random.step
                            (Random.list smidge spriteGenerator)
                            model.seed
                            |> (\( generatedSprites, s ) -> ( generatedSprites ++ model.sprites, s ))

                    else
                        ( List.drop smidge model.sprites, model.seed )
            in
            ( { model
                | sprites =
                    newSprites
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

        ChangeRenderer renderer ->
            ( { model
                | renderer = renderer
              }
            , Cmd.none
            )


spriteGenerator : Random.Generator Sprite
spriteGenerator =
    Random.map3
        (\x xVel bounceSpeed ->
            { x = 0
            , y = 0
            , xVel = xVel
            , yVel = bounceSpeed
            , bounceSpeed = bounceSpeed
            }
        )
        (Random.float 0 1)
        (Random.float -2 2)
        (Random.float 1 4)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onAnimationFrameDelta Tick
        ]


view : Model -> Html Msg
view model =
    let
        width =
            200

        height =
            200
    in
    Html.div
        []
        [ Html.div
            [ Html.Attributes.style "width" (px width)
            , Html.Attributes.style "height" (px height)
            , Html.Attributes.style "padding-right" (px 10)
            , Html.Attributes.style "border" "1px solid black"
            , Html.Attributes.style "position" "relative"
            ]
            (case model.renderer of
                HtmlTopLeft ->
                    viewHtmlTopLeft width height model.sprites

                HtmlTransformTranslate ->
                    viewHtmlTransformTranslate width height model.sprites
            )
        , Html.div []
            (List.map
                (\( renderer, str ) ->
                    Html.button
                        [ Html.Events.onClick (ChangeRenderer renderer) ]
                        [ Html.text str ]
                )
                [ ( HtmlTopLeft, "HTML: top, left" )
                , ( HtmlTransformTranslate, "HTML: transform" )
                ]
            )
        , Html.div []
            [ Html.text ("Total sprites: " ++ String.fromInt (List.length model.sprites)) ]
        ]


viewHtmlTopLeft : Float -> Float -> List Sprite -> List (Html Msg)
viewHtmlTopLeft width height sprites =
    -- around 800
    sprites
        |> List.map
            (\sprite ->
                Html.div
                    [ Html.Attributes.style "background" "orange"
                    , Html.Attributes.style "width" "10px"
                    , Html.Attributes.style "height" "10px"
                    , Html.Attributes.style "position" "absolute"
                    , Html.Attributes.style "border" "1px solid #a50"
                    , Html.Attributes.style "left" (width * sprite.x |> px)
                    , Html.Attributes.style "bottom" (width * sprite.y |> px)
                    ]
                    []
            )


viewHtmlTransformTranslate : Float -> Float -> List Sprite -> List (Html Msg)
viewHtmlTransformTranslate width height sprites =
    -- around 700
    sprites
        |> List.map
            (\sprite ->
                Html.div
                    [ Html.Attributes.style "background" "orange"
                    , Html.Attributes.style "width" "10px"
                    , Html.Attributes.style "height" "10px"
                    , Html.Attributes.style "border" "1px solid #a50"
                    , Html.Attributes.style "position" "absolute"
                    , Html.Attributes.style "transform"
                        ("translate("
                            ++ (width * sprite.x |> px)
                            ++ ","
                            ++ (height - 10 + (height * -sprite.y) |> px)
                        )
                    ]
                    []
            )


px : Float -> String
px num =
    String.fromFloat num ++ "px"
