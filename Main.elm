import Array
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
    ,   randomValues : Array.Array Float
    ,   windowWidth : Int
    ,   windowHeight : Int
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
        , buildings = []
        , numBuildingsToAdd = 10
        , randomValues = Array.fromList []
        , windowWidth = 0
        , windowHeight = 0
        }

type Layer = Front | Middle | Back
type alias Keys = { x:Int, y:Int }

type alias GlassWindow =
    { x : Float
    , y : Float
    , w : Float
    , h : Float
    }

type alias Building =
    { x : Float
    , y : Float
    , w : Float
    , h : Float
    , layer : Layer
    , windows : List GlassWindow
    }

newBuilding : Float -> Float -> Building
newBuilding x' h' =
    { x = x'
    , y = 0
    , w = buildingWidth
    , h = h'
    , layer = Front
    , windows = generateGlassWindows h'
        -- Array.fromList
        -- [   newGlassWindow 5 0 glassWindowSize glassWindowSize
        -- ,   newGlassWindow 25 0 glassWindowSize glassWindowSize
        -- ,   newGlassWindow 5 90 glassWindowSize glassWindowSize
        -- ,   newGlassWindow 25 135 glassWindowSize glassWindowSize
        -- ]
    }

newGlassWindow : Float -> Float -> Float -> Float -> GlassWindow
newGlassWindow x' y' w' h' =
    {   x = x'
    ,   y = y'
    ,   w = w'
    ,   h = h'
    }

glassWindowSize = 10
buildingWidth = 40

newGlassWindowPair =
    [   newGlassWindow 5 0 glassWindowSize glassWindowSize
    ,   newGlassWindow (buildingWidth - glassWindowSize - 5) 0 glassWindowSize glassWindowSize
    ]

generateGlassWindows : Float -> List GlassWindow
generateGlassWindows height =
    newGlassWindowPair ++
    List.map (\a -> { a | y = a.y + 25 }) newGlassWindowPair

-- UPDATE

update : (Float, Keys, (Int, Int), (Int, Int)) -> Model -> Model
update (dt, keys, (mx,my), (ww, wh)) model =
    model
        |> timeUpdate dt
        |> mouseUpdate (mx, my)
        |> keysUpdate keys
        |> randomUpdate
        |> addBuildingsUpdate
        |> updateWindowDimensions (ww, wh)

updateWindowDimensions : (Int, Int) -> Model -> Model
updateWindowDimensions (w, h) model =
    { model | windowWidth = w, windowHeight = h }

randomUpdate : Model -> Model
randomUpdate model =
    let generator = Random.list 100 (Random.float 0 1)
        tuple = Random.generate generator model.seed
        vs = Array.fromList (fst tuple)
        newSeed = snd tuple
        modifiedModel =
            { model | seed = newSeed
            ,         randomValues = vs
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

getNumBuildings : Model -> Int
getNumBuildings model =
    List.length model.buildings

getRandomValues : Model -> Int -> Array.Array Float
getRandomValues model numValues =
    Array.slice 0 numValues model.randomValues

popRandomValues : Int -> Model -> Model
popRandomValues numOfValuesToPop model =
    { model | randomValues = Array.slice numOfValuesToPop (Array.length model.randomValues) model.randomValues
    }

toValue : Float -> Float -> Maybe Float -> Float
toValue min max v =
    (Maybe.withDefault 0.5 v) * (max - min) + min

addBuilding : Model -> Building -> Model
addBuilding model building =
    { model | buildings = model.buildings ++ [ building ] }

reduceNewBuildingCount : Model -> Model
reduceNewBuildingCount model =
    { model | numBuildingsToAdd = model.numBuildingsToAdd - 1 }

addBuildingsUpdate : Model -> Model
addBuildingsUpdate model =
    if model.numBuildingsToAdd > 0 then
        let
            randomValues = getRandomValues model 2

            x = toValue -300 300 <| Array.get 0 randomValues
            h = toValue 50 500 <| Array.get 1 randomValues

            modifiedModel = popRandomValues 2 model
        in
            newBuilding x h |> addBuilding modifiedModel |> reduceNewBuildingCount
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

input : Signal (Float, Keys, (Int, Int), (Int, Int))
input =
    let
        timeDelta = map (\t -> t / 20) (fps 30)
    in
        Signal.map4 (,,,) timeDelta Keyboard.arrows Mouse.position Window.dimensions

isBack b = b.layer == Back
isMiddle b = b.layer == Middle
isFront b = b.layer == Front

view : Model -> Element
view model =
    let w' = model.windowWidth
        h' = model.windowHeight
        (dx, dy) = (model.x, -model.y)

        -- List.filter (a -> Bool) List a

        backBuildings = List.map displayBuilding (List.filter isBack model.buildings)
        middleBuildings = List.map displayBuilding (List.filter isMiddle model.buildings)
        -- frontBuildings = List.map displayBuilding (List.filter isFront model.buildings)
        frontBuildings = model.buildings
                            |> List.filter isFront
                            |> List.map (\b -> { b | x = b.x + model.t / 10 } )
                            |> List.map displayBuilding

        -- List.map (layerShift) frontBuildings

        -- List.filterMap (a -> Maybe.Maybe b) List a

        things = frontBuildings ++ middleBuildings ++ backBuildings ++ (displayModelInfo model) ++ (displayMouseCursor (dx, dy) model)
    in
        collage w' h' things

main : Signal Element
main =
    Signal.map view (Signal.foldp update initialModel input)

displayMouseCursor : (Float, Float) -> Model -> List Form
displayMouseCursor (x, y) model =
    let p = (x - (toFloat model.windowWidth / 2), y + (toFloat model.windowHeight / 2))
        a = model.t
    in
        [ ngon 3 5 |> filled red |> move p |> rotate (degrees a)]

displayModelInfo : Model -> List Form
displayModelInfo model =
    let m = (model.x, model.y)
        dt = round model.t
        keys = (model.kx, model.ky)

        ww = toFloat model.windowWidth
        wh = toFloat model.windowHeight

        allInfo =
                [ show ("mouse: " ++ toString m)
                , show ("dt: " ++ toString dt)
                , show ("keys: " ++ toString keys)
                , show ("win dims: " ++ toString (ww, wh))
                , show ("buildings: " ++ toString (List.length model.buildings))
                -- , show ("randomValues: " ++ toString model.randomValues)
                -- , show ("numBuildingsToAdd: " ++ toString model.numBuildingsToAdd)
                ]

        randomValues = model.randomValues
                            |> Array.toList
                            |> List.map2 (,) [1..(Array.length model.randomValues)]
                            |> List.map displayRandomValue

        formsToDisplay = [ toForm <| flow down allInfo ] ++ randomValues
    in
        formsToDisplay |> List.map (\a -> (a |> move (-ww / 2 + 80, wh / 2 - 100)))

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

glassWindowToForm : GlassWindow -> Form
glassWindowToForm window =
    rect window.w window.h
        |> filled darkGrey
        |> move (window.x + window.w / 2, window.y + window.h / 2)

displayBuilding : Building -> Form
displayBuilding b =
    let windows = List.map (\a -> { a | x = a.x + b.x, y = a.y + b.y } ) b.windows
            |> List.map glassWindowToForm

        allForms =
            [   rect b.w b.h
                    |> filled clearGrey
                    |> move (b.x + b.w / 2, b.y + b.h / 2)
            ] ++ windows
    in
        group allForms
