import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Keyboard
import Mouse
import Signal exposing (..)
import Time exposing (..)
import Window

-- MODEL
type alias Model =
    {   x : Float
    ,   y : Float
    ,   kx : Int
    ,   ky : Int
    ,   t : Float
    }

type alias Keys = { x:Int, y:Int }

appModel : Model
appModel =
    { x = 0
    , y = 0
    , kx = 0
    , ky = 0
    , t = 0
    }

-- UPDATE
update : (Float, Keys) -> Model -> Model
update (dt, keys) model =
    model
        |> keysUpdate keys
        |> timeUpdate dt

update2 : (Float, (Int, Int)) -> Model -> Model
update2 (dt,(mx,my)) model =
    model
        |> timeUpdate dt
        |> mouseUpdate (mx, my)

update3 : (Float, Keys, (Int, Int)) -> Model -> Model
update3 (dt, keys, (mx,my)) model =
    model
        |> timeUpdate dt
        |> mouseUpdate (mx, my)
        |> keysUpdate keys

keysUpdate : Keys -> Model -> Model
keysUpdate keys model =
    { model | kx = model.kx + keys.x
    ,         ky = model.ky + keys.y
    }

mouseUpdate : (Int, Int) -> Model -> Model
mouseUpdate (x', y') model =
    { model | x = toFloat x'
    ,         y = toFloat y'
    }

timeUpdate : Float -> Model -> Model
timeUpdate dt model =
    { model | t = model.t + dt
    }

input : Signal (Float, Keys)
input =
    let
        delta = map (\t -> t / 20) (fps 30)
    in
        sampleOn delta (map2 (,) delta Keyboard.arrows)

input2 : Signal (Float, (Int,Int))
input2 =
    let
        delta = map (\t -> t / 20) (fps 30)
    in
        map2 (,) delta Mouse.position

input3 : Signal (Float, Keys, (Int,Int))
input3 =
    let
        delta = map (\t -> t / 20) (fps 30)
    in
        map3 (,,) delta Keyboard.arrows Mouse.position

view : (Int,Int) -> Model -> Element
view (w',h') model =
    let (dx, dy) = (model.x, model.y)
    in
        collage w' h'
            [ building
                |> move (0,0)
            , buildingDebug (dx,dy)
                |> move (100,0)
            , texturedBuilding
                |> move (0,-200)
            , ngon 3 25
                |> filled red
                |> move (dx,dy)
            , debugStuff model
                |> move (toFloat (-w') / 3, toFloat (w') / 3)
            ]

main : Signal Element
main =
    -- map2 view Window.dimensions (foldp update appModel input2)
    map2 view Window.dimensions (foldp update3 appModel input3)

debugStuff : Model -> Form
debugStuff model =
    let m = (model.x, model.y)
        dt = round model.t
        keys = (model.kx, model.ky)
    in
        flow down
            [ show ("mouse: " ++ toString m)
            , show ("dt: " ++ toString dt)
            , show ("keys: " ++ toString keys)
            ]
                |> toForm

clearGrey : Color
clearGrey =
    rgba 111 111 111 0.6

darkGrey : Color
darkGrey =
    rgba 50 50 50 0.6

building : Form
building =
    group
    [   rect 50 100
            |> filled clearGrey
            |> move (50,10)
    ,   ngon 3 50
            |> filled clearGrey
            |> move (0,0)
            |> rotate (degrees (90))
    ]

buildingDebug : (Float,Float) -> Form
buildingDebug (x,y) =
    let w = 50
        h = 100
        topWidth = 50
        topHeight = 5
    in
        group
        [ rect w h
            |> filled clearGrey
            |> move (x + w, 0)
        , polygon
            [ (0, 0)
            , (topWidth, 0)
            , (topWidth - topHeight, h / 8)
            , (topHeight, h / 8)
            ]
                |> filled darkGrey
                |> move (x + topWidth / 2, h / 2)
        ]

texturedBuilding : Form
texturedBuilding =
    image 35 35 "minimal_industry.png"
        |> toForm
