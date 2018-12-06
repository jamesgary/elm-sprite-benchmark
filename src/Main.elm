module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import Browser.Events
import Game.Resources
import Game.TwoD
import Game.TwoD.Camera
import Game.TwoD.Render
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Encode
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

    -- Zinggi's Game 2D library
    , resources : Game.Resources.Resources
    }


type Renderer
    = HtmlTopLeft -- 1000
    | HtmlTransformTranslate -- 900
    | None -- 30,000
    | Zinggi -- 1000
    | DataAttrs -- 8000
    | PixiJsDataAttrs -- 5000


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
    | Resources Game.Resources.Msg


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
      , resources = Game.Resources.init
      }
    , Game.Resources.loadTextures [ "cat.png" ]
        |> Cmd.map Resources
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick msDelta ->
            let
                delta =
                    msDelta / 1000

                gravity =
                    5

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
                                { x = x |> max 0 |> min 1
                                , y = y |> max 0 |> min 1
                                , xVel = xVel
                                , yVel = yVel - (gravity * delta)
                                , bounceSpeed = sprite.bounceSpeed
                                }
                            )
                , seed = newSeed
              }
            , Cmd.none
            )

        ChangeRenderer renderer ->
            ( { model
                | renderer = renderer
              }
            , Cmd.none
            )

        Resources resourcesMsg ->
            ( { model
                | resources =
                    Game.Resources.update resourcesMsg model.resources
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
        (Random.float 1 2.8)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onAnimationFrameDelta Tick
        ]


view : Model -> Html Msg
view model =
    let
        width =
            600

        height =
            400

        spriteSize =
            32
    in
    Html.div
        []
        [ Html.div
            [ Html.Attributes.style "width" (px width)
            , Html.Attributes.style "height" (px height)
            , Html.Attributes.style "padding-right" (px spriteSize)
            , Html.Attributes.style "border" "1px solid black"
            , Html.Attributes.style "position" "relative"
            ]
            (case model.renderer of
                HtmlTopLeft ->
                    viewHtmlTopLeft width height spriteSize model.sprites

                HtmlTransformTranslate ->
                    viewHtmlTransformTranslate width height spriteSize model.sprites

                None ->
                    []

                Zinggi ->
                    viewZinggi model.resources width height spriteSize model.sprites

                DataAttrs ->
                    viewDataAttrs width height spriteSize model.sprites

                PixiJsDataAttrs ->
                    viewPixiJsDataAttrs width height spriteSize model.sprites
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
                , ( Zinggi, "Zinggi Game.TwoD" )
                , ( DataAttrs, "Just data attrs" )
                , ( PixiJsDataAttrs, "PixiJS with data attrs" )
                , ( None, "None" )
                ]
            )
        , Html.div []
            [ Html.text ("Total sprites: " ++ String.fromInt (List.length model.sprites)) ]
        ]


viewHtmlTopLeft : Float -> Float -> Float -> List Sprite -> List (Html Msg)
viewHtmlTopLeft width height spriteSize sprites =
    -- around 800
    sprites
        |> List.map
            (\sprite ->
                Html.img
                    [ Html.Attributes.src "cat.png"
                    , Html.Attributes.style "width" (px spriteSize)
                    , Html.Attributes.style "height" (px spriteSize)
                    , Html.Attributes.style "position" "absolute"
                    , Html.Attributes.style "left" (width * sprite.x |> px)
                    , Html.Attributes.style "bottom" (height * sprite.y |> px)
                    ]
                    []
            )


viewHtmlTransformTranslate : Float -> Float -> Float -> List Sprite -> List (Html Msg)
viewHtmlTransformTranslate width height spriteSize sprites =
    -- around 700
    sprites
        |> List.map
            (\sprite ->
                Html.img
                    [ Html.Attributes.src "cat.png"
                    , Html.Attributes.style "width" (px spriteSize)
                    , Html.Attributes.style "height" (px spriteSize)
                    , Html.Attributes.style "position" "absolute"
                    , Html.Attributes.style "transform"
                        ("translate("
                            ++ (width * sprite.x |> px)
                            ++ ","
                            ++ (height - spriteSize + (height * -sprite.y) |> px)
                        )
                    ]
                    []
            )


viewZinggi : Game.Resources.Resources -> Float -> Float -> Float -> List Sprite -> List (Html Msg)
viewZinggi resources width height spriteSize sprites =
    [ Game.TwoD.render
        { time = 0
        , size = ( round width, round height )
        , camera = Game.TwoD.Camera.fixedArea (width * height) ( 0.5 * width, 0.5 * height )
        }
        (sprites
            |> List.map
                (\sprite ->
                    Game.TwoD.Render.sprite
                        { texture = Game.Resources.getTexture "cat.png" resources
                        , position = ( sprite.x * width, sprite.y * height )
                        , size = ( spriteSize, spriteSize )
                        }
                )
        )
    ]


viewDataAttrs : Float -> Float -> Float -> List Sprite -> List (Html Msg)
viewDataAttrs width height spriteSize sprites =
    [ Html.div
        [ Html.Attributes.attribute "data-sprites" (spritesToAttrVal sprites)
        , Html.Attributes.id "sprite-data"
        ]
        []
    ]


viewPixiJsDataAttrs : Float -> Float -> Float -> List Sprite -> List (Html Msg)
viewPixiJsDataAttrs width height spriteSize sprites =
    [ Html.div
        [ Html.Attributes.attribute "data-sprites" (spritesToAttrVal sprites)
        , Html.Attributes.id "sprite-data-for-pixijs"
        ]
        []
    ]


spritesToAttrVal : List Sprite -> String
spritesToAttrVal sprites =
    if False then
        encodeSpritesWithString sprites

    else
        encodeSprites sprites |> Json.Encode.encode 0


encodeSprites : List Sprite -> Json.Encode.Value
encodeSprites sprites =
    sprites
        |> Json.Encode.list
            (\sprite ->
                Json.Encode.object
                    [ ( "x", Json.Encode.float sprite.x )
                    , ( "y", Json.Encode.float sprite.y )
                    ]
            )


encodeSpritesWithString : List Sprite -> String
encodeSpritesWithString sprites =
    sprites
        |> List.map
            (\sprite ->
                "{\"x\":" ++ String.fromFloat sprite.x ++ ",\"y\":" ++ String.fromFloat sprite.y ++ "}"
            )
        |> String.join ","
        |> (\str -> "[" ++ str ++ "]")


px : Float -> String
px num =
    String.fromFloat num ++ "px"
