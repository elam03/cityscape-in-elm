import Array
import Char
import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Keyboard
import Mouse
import Random
import Set
import Signal exposing (..)
import Time exposing (..)
import Window

-- MODEL
type Layer = Front | Middle | Back
type alias Keys = Set.Set Char.KeyCode
type MovementType = TimeMove | MouseMove

type alias Model =
    {   x : Float
    ,   y : Float
    ,   dx : Float
    ,   dy : Float
    ,   kx : Int
    ,   ky : Int
    ,   keys : List Char.KeyCode
    ,   t : Float
    ,   dt : Float
    ,   seed : Random.Seed
    ,   buildings : List Building
    ,   numBuildingsToAdd : Int
    ,   randomValues : Array.Array Float
    ,   windowWidth : Int
    ,   windowHeight : Int
    ,   movementType : MovementType
    }

initialModel : Model
initialModel =
    { x = 0
    , y = 0
    , dx = 0
    , dy = 0
    , kx = 0
    , ky = 0
    , keys = []
    , t = 0
    , dt = 0
    , seed = Random.initialSeed 42
    , buildings = []
    , numBuildingsToAdd = 10
    , randomValues = Array.fromList []
    , windowWidth = 0
    , windowHeight = 0
    , movementType = TimeMove
    }

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

newBuilding : Float -> Float -> Layer -> Building
newBuilding x' h' l'=
    { nullBuilding |
      x = x'
    , y = 0
    , w = buildingWidth
    , h = h'
    , layer = l'
    , windows = generateGlassWindows h'
    }

nullBuilding : Building
nullBuilding =
    { x = 0
    , y = 0
    , w = buildingWidth
    , h = 0
    , layer = Front
    , windows = []
    }

newGlassWindow : Float -> Float -> Float -> Float -> GlassWindow
newGlassWindow x' y' w' h' =
    {   x = x'
    ,   y = y'
    ,   w = w'
    ,   h = h'
    }

newGlassWindowRecurse : Float -> List Float -> Float -> Float -> List GlassWindow
newGlassWindowRecurse x' ys' w' h' =
    case ys' of
        [] -> []
        front::rest -> [ (newGlassWindow x' front w' h') ] ++ (newGlassWindowRecurse x' rest w' h')

glassWindowSize = 10
buildingWidth = 40

newGlassWindowPair =
    [   newGlassWindow 5 0 glassWindowSize glassWindowSize
    ,   newGlassWindow (buildingWidth - glassWindowSize - 5) 0 glassWindowSize glassWindowSize
    ]

generateGlassWindows : Float -> List GlassWindow
generateGlassWindows height =
    let
        cols = 2
        rows = height / (glassWindowSize * 2) - 1
        rowsList = List.map (\v -> v * 20) [1..rows]

        result1 = newGlassWindowRecurse 10 rowsList glassWindowSize glassWindowSize
        result2 = List.map (\w -> { w | x = w.x + glassWindowSize * 2}) result1
        result3 = List.map (\w -> { w | x = w.x - glassWindowSize / 2}) (result1 ++ result2)
    in
        result3

        -- newGlassWindowPair ++ List.map (\a -> { a | y = a.y + 25 }) newGlassWindowPair

-- UPDATE

update : (Float, Keys, (Int, Int), (Int, Int)) -> Model -> Model
update (dt, keys, (mx, my), (ww, wh)) model =
    model
        |> updateWindowDimensions (ww, wh)
        |> randomUpdate
        |> timeUpdate dt
        |> mouseUpdate (mx, my)
        |> keysUpdate keys
        |> addBuildingsUpdate
        |> updateBuildings (mx, my) (ww, wh)

updateBuildings : (Int, Int) -> (Int, Int) -> Model -> Model
updateBuildings (mx, my) (w, h) model =
    let
        backSpeed = 3
        middleSpeed = 2
        frontSpeed = 1

        delta =
            case model.movementType of
                MouseMove -> model.dx
                TimeMove -> model.dt

        backBuildings = model.buildings
                            |> List.filter isBack
                            |> List.map (\b -> { b | x = b.x + delta / backSpeed } )
        middleBuildings = model.buildings
                            |> List.filter isMiddle
                            |> List.map (\b -> { b | x = b.x + delta / middleSpeed } )
        frontBuildings = model.buildings
                            |> List.filter isFront
                            |> List.map (\b -> { b | x = b.x + delta / frontSpeed } )

        updatedBuildings = wrapBuildings model.windowWidth (backBuildings ++ middleBuildings ++ frontBuildings)
    in
        { model | buildings = updatedBuildings }

wrapBuildings : Int -> List Building -> List Building
wrapBuildings widthWrap buildings =
    buildings |> List.map (\b -> { b | x = toFloat (((round b.x + (widthWrap // 2)) % widthWrap) - (widthWrap // 2)) } )

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

isDown : Keys -> Char -> Bool
isDown keys keyCode =
    Set.member (Char.toCode keyCode) keys

isDownInModel : Model -> Char -> Bool
isDownInModel model key =
    let
        k = List.filter (\k -> Char.toUpper key == Char.fromCode k) model.keys
    in
        List.length k > 0

keysUpdateMovementType : Keys -> Model -> Model
keysUpdateMovementType keys model =
    let
        mIsDown = isDownInModel model 'M'
        tIsDown = isDownInModel model 'T'
    in
        if mIsDown then
            { model | movementType = MouseMove }
        else if tIsDown then
            { model | movementType = TimeMove }
        else
            model

keysUpdateAddBuildings : Keys -> Model -> Model
keysUpdateAddBuildings keys model =
    { model | numBuildingsToAdd = model.numBuildingsToAdd + increment (isDown keys '1') }

keysUpdateModel : Keys -> Model -> Model
keysUpdateModel keys model =
    { model | keys = Set.toList keys }

keysUpdate : Keys -> Model -> Model
keysUpdate keys model =
    model
        |> keysUpdateModel keys
        |> keysUpdateAddBuildings keys
        |> keysUpdateMovementType keys

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

pickLayer : Float -> Layer
pickLayer value =
    if value >= 66 then
        Front
    else if value >= 33 then
        Middle
    else
        Back

addBuildingsUpdate : Model -> Model
addBuildingsUpdate model =
    if model.numBuildingsToAdd > 0 then
        let
            randomValues = getRandomValues model 3

            x = toValue -300 300 <| Array.get 0 randomValues
            h = toValue 50 500 <| Array.get 1 randomValues
            l = pickLayer <| toValue 0 100 <| Array.get 1 randomValues

            modifiedModel = popRandomValues 3 model
        in
            newBuilding x h l |> addBuilding modifiedModel |> reduceNewBuildingCount
    else
        model

mouseUpdate : (Int, Int) -> Model -> Model
mouseUpdate (mx, my) model =
    let
        (px, py) = (model.x, model.y)
    in
        { model | x  = toFloat mx
        ,         y  = toFloat my
        ,         dx = toFloat mx - px
        ,         dy = toFloat my - py
        }

timeUpdate : Float -> Model -> Model
timeUpdate dt model =
    { model |   t = model.t + dt
    ,           dt = dt
    }

input : Signal (Float, Keys, (Int, Int), (Int, Int))
input =
    let
        timeDelta = map (\t -> t / 20) (fps 30)
    in
        Signal.map4 (,,,) timeDelta Keyboard.keysDown Mouse.position Window.dimensions

isBack b = b.layer == Back
isMiddle b = b.layer == Middle
isFront b = b.layer == Front

view : Model -> Element
view model =
    let
        (mx, my) = (model.x, -model.y)

        allBuildings = model.buildings |> List.map displayBuilding

        things = allBuildings ++ (displayModelInfo model) ++ (displayMouseCursor (mx, my) model)
    in
        collage model.windowWidth model.windowHeight things

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
        d = (model.dx, model.dy)
        t = round model.t
        dt = round model.dt
        keys = List.map (\key -> Char.fromCode key) model.keys
        (ww, wh) = (toFloat model.windowWidth, toFloat model.windowHeight)
        firstBuilding = List.head model.buildings |> Maybe.withDefault nullBuilding
        movementType = model.movementType

        allInfo =
                [ show ("(x,y): " ++ toString m)
                , show ("(dx,dy): " ++ toString d)
                , show ("t: " ++ toString t)
                , show ("dt: " ++ toString dt)
                , show ("keys: " ++ toString keys)
                , show ("(ww, wh): " ++ toString (ww, wh))
                , show ("buildings: " ++ toString (List.length model.buildings))
                , show ("first building: " ++ toString (round firstBuilding.x))
                , show ("movementType: " ++ toString (movementType))

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
