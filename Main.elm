import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Keyboard
import Mouse
import Random
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
    ,   seed : Random.Seed
    ,   buildings : List Building
    ,   numBuildingsToAdd : Int
    ,   randomValues : List Float
    ,   randomValueIndex : Int
    }

initialModel : Model
initialModel =
    let
        initialSeed = Random.initialSeed 42
    in
        { x = 0
        , y = 0
        , kx = 0
        , ky = 0
        , t = 0
        , seed = initialSeed
        -- , buildings = generateBuildings [getRandomFloat , getRandomFloat]
        -- , buildings = generateBuildings initialSeed [-5..5]
        , buildings = []
        , numBuildingsToAdd = 10
        , randomValues = []
        , randomValueIndex = 0
        }

type Layer = Front | Middle | Back
type alias Keys = { x:Int, y:Int }

type alias Building =
    { x : Float
    , y : Float
    , w : Float
    , h : Float
    , layer : Layer
    }

newBuilding : Float -> Float -> Building
newBuilding x' h' =
    { x = x'
    , y = 0
    , w = 40
    , h = h'
    , layer = Front
    }

-- UPDATE

update : (Float, Keys, (Int, Int)) -> Model -> Model
update (dt, keys, (mx,my)) model =
    model
        |> timeUpdate dt
        |> mouseUpdate (mx, my)
        |> keysUpdate keys
        |> randomUpdate
        |> addBuildingsUpdate

getRandomFloat : Float -> Float -> Model -> (Float, Model)
getRandomFloat min max model =
    let value = Maybe.withDefault 0.5 (List.head model.randomValues)
        modifiedModel =
        { model | randomValueIndex = model.randomValueIndex + 1
        }
    in
        (min + value * max, modifiedModel)

-- getRandomFloat : Model -> Model
-- getRandomFloat model =
--     model

randomUpdate : Model -> Model
randomUpdate model =
    let generator = Random.list 100 (Random.float 0 1)
        tuple = Random.generate generator model.seed
        vs = fst tuple
        newSeed = snd tuple
        modifiedModel =
            { model | seed = newSeed
            ,         randomValues = vs
            ,         randomValueIndex = 0
            }
    in
        modifiedModel

increment : Bool -> Int
increment condition =
    if condition then
        1
    else
        0

keysUpdate : Keys -> Model -> Model
keysUpdate keys model =
    { model | kx = keys.x
    ,         ky = keys.y
    ,         numBuildingsToAdd = model.numBuildingsToAdd + increment (keys.x > 0)
    }

addBuildingsUpdate : Model -> Model
addBuildingsUpdate model =
    if model.numBuildingsToAdd > 0 then
        let
            tuple = getRandomFloat -300 300 model
            x = fst tuple
            modifiedModel2 = snd tuple

            tuple2 = getRandomFloat 30 500 modifiedModel2
            h = fst tuple2
            modifiedModel = snd tuple
        in
            { modifiedModel | buildings = modifiedModel.buildings ++ [newBuilding x h]
            ,                 numBuildingsToAdd = modifiedModel.numBuildingsToAdd - 1
            -- ,                 x = v
            }
    else
        model

mouseUpdate : (Int, Int) -> Model -> Model
mouseUpdate (x', y') model =
    { model | x = toFloat x'
    ,         y = toFloat y'
    }

timeUpdate : Float -> Model -> Model
timeUpdate dt model =
    { model | t = model.t + dt
    }

input : Signal (Float, Keys, (Int,Int))
input =
    let
        delta = map (\t -> t / 20) (fps 30)
    in
        map3 (,,) delta Keyboard.arrows Mouse.position

view : (Int,Int) -> Model -> Element
view (w',h') model =
    let (dx, dy) = (model.x, -model.y)
        buildings = List.map displayBuilding model.buildings
        things = buildings ++ (displayModelInfo model)
            -- [ displayModelInfo model
            --     |> move (toFloat (-w') / 3, toFloat (h') / 3)
            -- , ngon 3 5
            --     |> filled red
            --     |> move (dx,dy)
            --     |> rotate (degrees model.t)
            -- ]
    in
        collage w' h' things

main : Signal Element
main =
    map2 view Window.dimensions (foldp update initialModel input)

displayModelInfo : Model -> List Form
displayModelInfo model =
    let m = (model.x, model.y)
        dt = round model.t
        keys = (model.kx, model.ky)
        -- numBuildings = List.length model.buildings
        formsToDisplay =
                [ show ("mouse: " ++ toString m)
                , show ("dt: " ++ toString dt)
                , show ("keys: " ++ toString keys)
                , show ("buildings: " ++ toString (List.length model.buildings))
                -- , show ("randomValues: " ++ toString model.randomValues)
                , show ("numBuildingsToAdd: " ++ toString model.numBuildingsToAdd)
                , show ("randomValueIndex: " ++ toString model.randomValueIndex)
                ]

        -- randomValues = List.map displayRandomValue ( (List.map2 (,) [1..(List.length model.randomValues)] model.randomValues) )

        randomValues = model.randomValues
                            |> List.map2 (,) [1..(List.length model.randomValues)]
                            |> List.map displayRandomValue
    in
        [ toForm <| flow down formsToDisplay ] ++ randomValues

displayRandomValue : (Int, Float) -> Form
displayRandomValue (x', value') =
    let x = toFloat x'
    in
        traced (solid red) (path [ (x, 0), (x, value' * 100) ])
            -- |> rotate (degrees (value * 360))

clearGrey : Color
clearGrey =
    rgba 111 111 111 0.6

darkGrey : Color
darkGrey =
    rgba 50 50 50 0.6

displayBuilding : Building -> Form
displayBuilding b =
    let topWidth = b.w
        topHeight = 10
    in
        group
        [   rect b.w (b.h - topHeight * 2)
                |> filled clearGrey
                |> move (b.x, b.y + b.h / 2 - topHeight)
        ,   polygon
                [ (-b.w / 2, b.h)
                , ( b.w / 2, b.h)
                , (-b.w / 2 + topWidth, b.h + topHeight)
                , ( b.w / 2 - topWidth, b.h + topHeight)
                ]
                    |> filled darkGrey
                    |> move (b.x, b.y - topHeight * 2)
        -- ,   ngon 3 50
        --         |> filled clearGrey
        --         |> move (0,0)
        --         |> rotate (degrees (90))
        ]

-- texturedBuilding : Form
-- texturedBuilding =
--     image 35 35 "minimal_industry.png"
--         |> toForm
