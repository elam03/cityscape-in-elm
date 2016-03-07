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
        }

generateBuildings2 : Model -> Model
generateBuildings2 model' =
    -- model
    let
        -- tuple = getRandomInt model 20 40
        tuple = (20, model')
        pos = (List.length model.buildings)
        -- height = fst tuple
        height = pos * 4
        model = snd tuple
    in
        { model | buildings = List.append model.buildings [(newBuilding pos height)] }

type Layer = Front | Middle | Back
type alias Keys = { x:Int, y:Int }

type alias Building =
    { x : Float
    , y : Float
    , w : Float
    , h : Float
    , layer : Layer
    }

getRandomFloat : Model -> Float -> Float -> (Float, Model)
getRandomFloat model min max =
    let
        tuple = Random.generate (Random.float min max) model.seed
        value = fst tuple
        updatedModel = { model | seed = snd tuple }
    in
        (value, updatedModel)

getRandomInt : Model -> Int -> Int -> (Int, Model)
getRandomInt model min max =
    let
        tuple = Random.generate (Random.int min max) model.seed
        value = fst tuple
        updatedModel = { model | seed = snd tuple }
    in
        (value, updatedModel)

getRandomHeight : Random.Seed -> Float
getRandomHeight seed =
    seed
        |> Random.generate (Random.float 40 80)
        |> fst

building : Random.Seed -> Int -> Building
building seed pos =
    { x = toFloat (pos * 50)
    , y = 0
    , w = 40
    , h = getRandomHeight seed
    , layer = Front
    }

newBuilding : Int -> Int -> Building
newBuilding pos height =
    { x = toFloat (pos * 50)
    , y = 0
    , w = 40
    , h = toFloat height
    , layer = Front
    }

generateBuildings : Random.Seed -> List (Int) -> List Building
generateBuildings seed list =
    case list of
        [] ->
            []

        first :: rest ->
            -- (building [first]) + generateBuildings rest
            List.append [building seed first] (generateBuildings seed rest)
            -- List.append ([ building 0 ] (generateBuildings rest))
    -- if count == 0 then
    --     building count
    -- else then
    --     (generateBuildings count - 1)

-- newBuilding : Building
-- newBuilding =
--     { x = 50
--     , y = 0
--     , w = 40
--     , h = 80
--     , layer = Front
--     }

-- seed = Random.initialSeed 42

-- randomLayer : Random.Generator Layer
-- randomLayer =
--     map ( \b -> if b then Front else Back) Random.bool

-- newBuilding : Building
-- newBuilding =
--     { x = 42--Random.generate (Random.float -300 300) seed
--     , y = 0
--     , w = 42--Random.generate (Random.float 30 60) seed
--     , h = 42--Random.generate (Random.float 30 150) seed
--     , layer = Front--randomLayer
--     }

-- UPDATE

update : (Float, Keys, (Int, Int)) -> Model -> Model
update (dt, keys, (mx,my)) model =
    model
        |> timeUpdate dt
        |> mouseUpdate (mx, my)
        |> keysUpdate keys

keysUpdate : Keys -> Model -> Model
keysUpdate keys model =
    let m =
        { model | kx = model.kx + keys.x
        ,         ky = model.ky + keys.y
        }
    in
        if keys.y > 0 then
            generateBuildings2 m
        else
            m

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
        things = buildings ++
            [ debugInfo model
                |> move (toFloat (-w') / 3, toFloat (w') / 3)
            , ngon 3 5
                |> filled red
                |> move (dx,dy)
                |> rotate (degrees model.t)
            ]
    in
        collage w' h' things

main : Signal Element
main =
    map2 view Window.dimensions (foldp update initialModel input)

debugInfo : Model -> Form
debugInfo model =
    let m = (model.x, model.y)
        dt = round model.t
        keys = (model.kx, model.ky)
        numBuildings = List.length model.buildings
    in
        flow down
            [ show ("mouse: " ++ toString m)
            , show ("dt: " ++ toString dt)
            , show ("keys: " ++ toString keys)
            , show ("buildings: " ++ toString numBuildings)
            ]
                |> toForm

clearGrey : Color
clearGrey =
    rgba 111 111 111 0.6

darkGrey : Color
darkGrey =
    rgba 50 50 50 0.6

-- displayBuilding : Building -> Form
-- displayBuilding b =
--     displayBuilding b.x b.y b.w b.h

-- displayBuilding : Float -> Float -> Float -> Float -> Form
displayBuilding : Building -> Form
displayBuilding b =
    let topWidth = b.w / 10
        topHeight = b.h / 10
    in
        group
        [   rect b.w (b.h - topHeight * 2)
                |> filled clearGrey
                |> move (b.x, b.y)
        ,   polygon
                [ (-b.w / 2, b.h / 2)
                , (-b.w / 2 + topWidth, b.h / 2 + topHeight)
                , ( b.w / 2 - topWidth, b.h / 2 + topHeight)
                , ( b.w / 2, b.h / 2)
                ]
                    |> filled darkGrey
                    |> move (b.x, b.y - topHeight)
        -- ,   ngon 3 50
        --         |> filled clearGrey
        --         |> move (0,0)
        --         |> rotate (degrees (90))
        ]

-- buildingDebug : (Float,Float) -> Form
-- buildingDebug (x,y) =
--     let w = 50
--         h = 100
--         topWidth = 50
--         topHeight = 5
--     in
--         group
--         [ rect w h
--             |> filled clearGrey
--             |> move (x + w, 0)
--         , polygon
--             [ (0, 0)
--             , (topWidth, 0)
--             , (topWidth - topHeight, h / 8)
--             , (topHeight, h / 8)
--             ]
--                 |> filled darkGrey
--                 |> move (x + topWidth / 2, h / 2)
--         ]

-- texturedBuilding : Form
-- texturedBuilding =
--     image 35 35 "minimal_industry.png"
--         |> toForm
