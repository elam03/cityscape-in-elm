module Building where

-- import List

-------------------------------------------------------------------------------
-- Common/Constants
-------------------------------------------------------------------------------

type Layer = Front | Middle | Back

glassWindowSize : number
glassWindowSize = 10

buildingWidth : number
buildingWidth   = 40

isBack : Building -> Bool
isBack   b = b.layer == Back

isMiddle : Building -> Bool
isMiddle b = b.layer == Middle

isFront : Building -> Bool
isFront  b = b.layer == Front

-------------------------------------------------------------------------------
-- GlassWindow
-------------------------------------------------------------------------------

type alias GlassWindow =
    { x : Float
    , y : Float
    , w : Float
    , h : Float
    }

newGlassWindow : Float -> Float -> Float -> Float -> GlassWindow
newGlassWindow x' y' w' h' =
    {   x = x'
    ,   y = y'
    ,   w = w'
    ,   h = h'
    }

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

newGlassWindowRecurse : Float -> List Float -> Float -> Float -> List GlassWindow
newGlassWindowRecurse x' ys' w' h' =
    case ys' of
        [] -> []
        front::rest -> [ (newGlassWindow x' front w' h') ] ++ (newGlassWindowRecurse x' rest w' h')


-------------------------------------------------------------------------------
-- Building
-------------------------------------------------------------------------------

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
