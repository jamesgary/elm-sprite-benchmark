port module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

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
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Random
import Task
import WebGL exposing (Mesh, Shader)
import WebGL.Settings
import WebGL.Texture exposing (Texture, defaultOptions)


width =
    600


height =
    400


canvasMargin =
    10


spriteSize =
    32


port sendSprites : Json.Encode.Value -> Cmd msg


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
    , hoveringRenderer : Maybe Renderer

    -- elm-explorations/webgl
    , texture : Maybe Texture

    -- Zinggi's Game 2D library
    , resources : Game.Resources.Resources
    }


type Renderer
    = HtmlTopLeft -- 1000
    | HtmlTransformTranslate -- 900
    | None -- 30,000
    | Zinggi -- 1000
    | DataAttrs -- 8000
    | PixiJsDataAttrs -- 6000
    | PixiJsPorts -- 14,000
    | WebGLRenderer -- ???


type alias Sprite =
    { x : Float
    , y : Float
    , angle : Float
    , speed : Float
    }


type Msg
    = Tick Float
    | ChangeRenderer Renderer
    | ButtonEnter Renderer
    | ButtonLeave
    | TextureLoaded Texture
    | TextureError
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
      , hoveringRenderer = Nothing
      , texture = Nothing
      , resources = Game.Resources.init
      }
    , Cmd.batch
        [ "cat.png"
            |> WebGL.Texture.loadWith
                { defaultOptions
                    | magnify = WebGL.Texture.linear
                    , minify = WebGL.Texture.nearest
                }
            |> Task.attempt
                (\result ->
                    case result of
                        Ok texture ->
                            TextureLoaded texture

                        Err _ ->
                            TextureError
                )
        , Game.Resources.loadTextures [ "cat.png" ]
            |> Cmd.map Resources
        ]
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

                minTick =
                    1 / 55

                spritesLength =
                    model.sprites
                        |> List.length

                ( newSprites, newSeed ) =
                    if delta > minTick then
                        -- too slow! remove some sprites (down to 1 sprite)
                        spritesLength
                            |> toFloat
                            |> (*) (2 * minTick)
                            |> ceiling
                            |> min (spritesLength - 1)
                            |> (\decreaseAmt ->
                                    ( List.drop decreaseAmt model.sprites, model.seed )
                                --( List.take (List.length model.sprites - decreaseAmt) model.sprites, model.seed )
                               )

                    else
                        -- so fast, add more sprites!
                        model.sprites
                            |> List.length
                            |> toFloat
                            |> (*) 0.01
                            |> round
                            |> (\increaseAmt ->
                                    Random.step
                                        (Random.list increaseAmt spriteGenerator)
                                        model.seed
                                        |> (\( generatedSprites, s ) -> ( generatedSprites ++ model.sprites, s ))
                               )
            in
            ( { model
                | sprites =
                    newSprites
                        --|> always model.sprites
                        |> List.map
                            (\sprite ->
                                let
                                    ( newX, newY ) =
                                        ( sprite.speed, sprite.angle )
                                            |> fromPolar
                                            |> (\( x, y ) ->
                                                    ( sprite.x + x
                                                    , sprite.y + y
                                                    )
                                               )

                                    minWidth =
                                        -0.5 * spriteSize

                                    maxWidth =
                                        width + (0.5 * spriteSize)

                                    minHeight =
                                        -spriteSize

                                    maxHeight =
                                        height

                                    wrappedX =
                                        if newX < minWidth then
                                            maxWidth

                                        else if newX > maxWidth then
                                            minWidth

                                        else
                                            newX

                                    wrappedY =
                                        if newY < minHeight then
                                            maxHeight

                                        else if newY > maxHeight then
                                            minHeight

                                        else
                                            newY
                                in
                                { x = wrappedX
                                , y = wrappedY
                                , angle = sprite.angle
                                , speed = sprite.speed
                                }
                            )
                , seed = newSeed
              }
            , case model.renderer of
                PixiJsPorts ->
                    sendSprites (encodeSprites model.sprites)

                _ ->
                    Cmd.none
            )

        ChangeRenderer renderer ->
            ( { model
                | renderer = renderer
              }
            , Cmd.none
            )

        ButtonEnter renderer ->
            ( { model | hoveringRenderer = Just renderer }, Cmd.none )

        ButtonLeave ->
            ( { model | hoveringRenderer = Nothing }, Cmd.none )

        Resources resourcesMsg ->
            ( { model
                | resources =
                    Game.Resources.update resourcesMsg model.resources
              }
            , Cmd.none
            )

        TextureLoaded texture ->
            ( { model | texture = Just texture }, Cmd.none )

        TextureError ->
            -- TODO error?
            ( model, Cmd.none )


spriteGenerator : Random.Generator Sprite
spriteGenerator =
    Random.map4
        (\x y angle speed ->
            { x = x
            , y = y
            , angle = angle
            , speed = speed
            }
        )
        (Random.float 0 width)
        (Random.float 0 height)
        (Random.float 0 (2 * pi))
        (Random.float 0.01 1)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onAnimationFrameDelta Tick
        ]


view : Model -> Html Msg
view model =
    Html.div
        [ Html.Attributes.style "display" "flex"
        , Html.Attributes.style "flex-direction" "column"
        , Html.Attributes.style "margin" (px canvasMargin)
        , Html.Attributes.style "font-family" "sans-serif"
        ]
        [ -- canvas, buttons, and description
          Html.div
            [ Html.Attributes.style "display" "flex"
            , Html.Attributes.style "align-items" "flex-start"
            ]
            [ -- canvases
              Html.div
                [ Html.Attributes.style "width" (px width)
                , Html.Attributes.style "height" (px height)
                , Html.Attributes.style "border" "1px solid black"
                , Html.Attributes.style "position" "relative"
                , Html.Attributes.style "overflow" "hidden"
                ]
                (case model.renderer of
                    HtmlTopLeft ->
                        viewHtmlTopLeft model.sprites
                            |> withWhiteBg

                    HtmlTransformTranslate ->
                        viewHtmlTransformTranslate model.sprites
                            |> withWhiteBg

                    None ->
                        []
                            |> withWhiteBg

                    Zinggi ->
                        viewZinggi model.resources model.sprites
                            |> withWhiteBg

                    DataAttrs ->
                        viewDataAttrs model.sprites
                            |> withWhiteBg

                    PixiJsDataAttrs ->
                        viewPixiJsDataAttrs model.sprites

                    PixiJsPorts ->
                        []

                    WebGLRenderer ->
                        case model.texture of
                            Just tex ->
                                viewWebGL tex model.sprites
                                    |> withWhiteBg

                            Nothing ->
                                [ Html.text "Loading texture..." ]
                                    |> withWhiteBg
                )
            , -- buttons
              Html.div
                [ Html.Attributes.style "margin" "0 10px"
                , Html.Attributes.style "display" "flex"
                , Html.Attributes.style "flex-direction" "column"
                ]
                ([ ( "HTML"
                   , [ ( HtmlTopLeft, "top / left" )
                     , ( HtmlTransformTranslate, "transform: translate" )
                     ]
                   )
                 , ( "WebGL"
                   , [ ( WebGLRenderer, "WebGL" )
                     , ( Zinggi, "Zinggi Game.TwoD" )
                     ]
                   )
                 , ( "PixiJS"
                   , [ ( PixiJsDataAttrs, "PixiJS with data attrs" )
                     , ( PixiJsPorts, "PixiJS ports" )
                     ]
                   )
                 , ( "Misc (non-rendering)"
                   , [ ( DataAttrs, "Just data attrs" )
                     , ( None, "None" )
                     ]
                   )
                 ]
                    |> List.map
                        (\( title, buttons ) ->
                            Html.div
                                [ Html.Attributes.style "font-size" "16px"
                                , Html.Attributes.style "margin" "1px 0 0"
                                , Html.Attributes.style "font-weight" "bold"
                                , Html.Attributes.style "text-decoration" "underline"
                                , Html.Attributes.style "color" "#014"
                                ]
                                [ Html.text title ]
                                :: (buttons
                                        |> List.map
                                            (\( renderer, str ) ->
                                                Html.button
                                                    ([ Html.Attributes.style "font-size" "16px"
                                                     , Html.Attributes.style "margin" "0 0 5px"
                                                     , Html.Attributes.style "padding" "5px 10px"
                                                     , Html.Attributes.style "border-color" "#6cf #7bf #1af"
                                                     , Html.Events.onMouseEnter (ButtonEnter renderer)
                                                     , Html.Events.onMouseLeave ButtonLeave
                                                     ]
                                                        ++ (if model.renderer == renderer then
                                                                [ --Html.Attributes.disabled True -- can't listen to onMouseLeave
                                                                  Html.Attributes.style "background" "#dff"
                                                                , Html.Attributes.style "color" "black"
                                                                ]

                                                            else
                                                                [ Html.Events.onClick (ChangeRenderer renderer)
                                                                , Html.Attributes.style "background" "#adf"
                                                                , Html.Attributes.style "color" "black"
                                                                , Html.Attributes.style "cursor" "pointer"
                                                                ]
                                                           )
                                                    )
                                                    [ Html.text str ]
                                            )
                                   )
                        )
                    |> List.concat
                )
            , case model.hoveringRenderer of
                Nothing ->
                    Html.text ""

                Just hoveringRenderer ->
                    Html.span
                        [ Html.Attributes.style "background" "white"
                        , Html.Attributes.style "border" "3px ridge #ccc"
                        , Html.Attributes.style "padding" "10px"
                        , Html.Attributes.style "display" "inline-block"
                        , Html.Attributes.style "position" "relative"
                        , Html.Attributes.style "width" "200px"
                        , Html.Attributes.style "line-height" "20px"
                        ]
                        (case hoveringRenderer of
                            HtmlTopLeft ->
                                [ Html.text "Sprites are HTML "
                                , code "<img>"
                                , Html.text " elements positioned with CSS properties "
                                , code "top"
                                , Html.text " and "
                                , code "left"
                                , Html.text "."
                                ]

                            HtmlTransformTranslate ->
                                [ Html.text "Sprites are HTML "
                                , code "<img>"
                                , Html.text " elements positioned with CSS properties "
                                , code "transform: translate(x, y)"
                                , Html.text "."
                                , Html.br [] []
                                , Html.br [] []
                                , Html.text "This should be faster since it uses the GPU, but it does not. Maybe it's better suited for fewer sprites and doesn't scale to hundreds? I'm not sure!"
                                , Html.br [] []
                                , Html.br [] []
                                , Html.strong [] [ Html.text "Warning: " ]
                                , Html.text "This may lock up your browser if there are already many sprites to draw!"
                                ]

                            Zinggi ->
                                [ Html.text "This uses Zinggi's rendering engine "
                                , code "elm-2d-game"
                                , Html.text " written in Elm and uses "
                                , code "elm-explorations/webgl"
                                , Html.text ". It gets pretty good results, but since it creates a new entity for every sprite, it doesn't seem scale beyond a thousand sprites."
                                ]

                            DataAttrs ->
                                [ Html.text "This encodes the sprite data to a JSON string and sets it to the data attribute value of an HTML element."
                                , Html.br [] []
                                , Html.br [] []
                                , Html.text "This demonstrates how slow writing to the DOM is, even for non-visible changes."
                                ]

                            PixiJsDataAttrs ->
                                [ Html.text "This encodes the sprite data to a JSON string and sets it to the data attribute value of an HTML element."
                                , Html.br [] []
                                , Html.br [] []
                                , Html.text "On the Javascript side, we use PixiJS to read from that data attribute and draw to its own canvas."
                                ]

                            PixiJsPorts ->
                                [ Html.text "This encodes the sprite data to a JSON string and sends it through a port."
                                , Html.br [] []
                                , Html.br [] []
                                , Html.text "On the Javascript side, we listen to this port and use PixiJS to read the sprite data and draw to its own canvas."
                                , Html.br [] []
                                , Html.br [] []
                                , Html.text "This is fastest by a long shot!"
                                ]

                            None ->
                                [ Html.text "This renders nothing! But it still calculates the positions of the sprites. It's just used as a max baseline." ]

                            WebGLRenderer ->
                                [ Html.text "This is unfinished!" ]
                        )
            ]

        -- count
        , Html.div
            [ Html.Attributes.style "margin" "10px 0"
            ]
            [ Html.text ("Total sprites: " ++ String.fromInt (List.length model.sprites)) ]
        ]


code : String -> Html Msg
code str =
    Html.code
        [ Html.Attributes.style "background" "#eee"
        , Html.Attributes.style "padding" "2px"
        ]
        [ Html.text str ]


withWhiteBg : List (Html Msg) -> List (Html Msg)
withWhiteBg elements =
    [ Html.div
        [ Html.Attributes.style "background" "white"
        , Html.Attributes.style "width" "100%"
        , Html.Attributes.style "height" "100%"
        ]
        elements
    ]


viewHtmlTopLeft : List Sprite -> List (Html Msg)
viewHtmlTopLeft sprites =
    -- around 800
    sprites
        |> List.map
            (\sprite ->
                Html.img
                    [ Html.Attributes.src "cat.png"
                    , Html.Attributes.style "width" (px spriteSize)
                    , Html.Attributes.style "height" (px spriteSize)
                    , Html.Attributes.style "position" "absolute"
                    , Html.Attributes.style "left" (sprite.x |> px)
                    , Html.Attributes.style "bottom" (sprite.y |> px)
                    ]
                    []
            )


viewHtmlTransformTranslate : List Sprite -> List (Html Msg)
viewHtmlTransformTranslate sprites =
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
                            ++ (sprite.x |> px)
                            ++ ","
                            ++ (height - spriteSize - sprite.y |> px)
                        )
                    , Html.Attributes.style "will-change" "transform"
                    ]
                    []
            )


viewZinggi : Game.Resources.Resources -> List Sprite -> List (Html Msg)
viewZinggi resources sprites =
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
                        , position = ( sprite.x, sprite.y )
                        , size = ( spriteSize, spriteSize )
                        }
                )
        )
    ]


viewDataAttrs : List Sprite -> List (Html Msg)
viewDataAttrs sprites =
    [ Html.div
        [ Html.Attributes.attribute "data-sprites" (spritesToAttrVal sprites)
        , Html.Attributes.id "sprite-data"
        ]
        []
    ]


viewPixiJsDataAttrs : List Sprite -> List (Html Msg)
viewPixiJsDataAttrs sprites =
    [ Html.div
        [ Html.Attributes.attribute "data-sprites" (spritesToAttrVal sprites)
        , Html.Attributes.id "sprite-data-for-pixijs"
        ]
        []
    ]


spritesToAttrVal : List Sprite -> String
spritesToAttrVal sprites =
    encodeSprites sprites |> Json.Encode.encode 0


encodeSprites : List Sprite -> Json.Encode.Value
encodeSprites sprites =
    sprites
        |> Json.Encode.list
            (\sprite ->
                Json.Encode.object
                    -- offset anchor just to match html views
                    [ ( "x", Json.Encode.float (sprite.x + (0.5 * spriteSize)) )
                    , ( "y", Json.Encode.float (sprite.y + (0.5 * spriteSize)) )
                    ]
            )


hasInit =
    False


viewWebGL : Texture -> List Sprite -> List (Html Msg)
viewWebGL texture sprites =
    [ WebGL.toHtmlWith
        [ WebGL.alpha True
        , WebGL.antialias
        ]
        [ Html.Attributes.width width
        , Html.Attributes.height height
        , Html.Attributes.style "background" "white"
        ]
        (sprites
            |> List.map
                (\{ x, y } ->
                    WebGL.entityWith
                        [ WebGL.Settings.sampleAlphaToCoverage ]
                        vertexShader
                        fragmentShader
                        squareMesh
                        { texture = texture
                        , perspective = perspective x y
                        }
                )
        )
    ]


perspective : Float -> Float -> Mat4
perspective x y =
    let
        mod =
            1 / spriteSize
    in
    Mat4.makeScale3 (spriteSize / width) (spriteSize / height) 1
        |> Mat4.translate3
            (1 + -1.0 * (width / spriteSize) + ((2 / spriteSize) * x))
            (1 + -1.0 * (height / spriteSize) + ((2 / spriteSize) * y))
            0


type alias Vertex =
    { position : Vec3
    , coord : Vec2
    }


type alias Uniforms =
    { perspective : Mat4
    , texture : Texture
    }


vertexShader : Shader Vertex Uniforms { vcoord : Vec2 }
vertexShader =
    [glsl|
        attribute vec3 position;
        attribute vec2 coord;
        uniform mat4 perspective;
        varying vec2 vcoord;
        void main () {
          gl_Position = perspective * vec4(position, 1.0);
          vcoord = coord.xy;
        }
    |]


fragmentShader : Shader {} Uniforms { vcoord : Vec2 }
fragmentShader =
    [glsl|
        precision mediump float;
        uniform sampler2D texture;
        varying vec2 vcoord;
        void main () {
          vec3 rgb = texture2D(texture, vcoord).xyz;
          vec4 rgba = vec4(rgb, 0.5);
          gl_FragColor = rgba;

          gl_FragColor = texture2D(texture, vcoord);
        }
    |]


squareMesh : Mesh Vertex
squareMesh =
    let
        vertex position coord =
            { position = position
            , coord = coord
            }

        topLeft =
            Vertex (vec3 -1 1 1) (vec2 0 1)

        topRight =
            Vertex (vec3 1 1 1) (vec2 1 1)

        bottomLeft =
            Vertex (vec3 -1 -1 1) (vec2 0 0)

        bottomRight =
            Vertex (vec3 1 -1 1) (vec2 1 0)
    in
    [ ( topLeft, topRight, bottomLeft )
    , ( bottomLeft, topRight, bottomRight )
    ]
        |> WebGL.triangles


px : Float -> String
px num =
    String.fromFloat num ++ "px"
