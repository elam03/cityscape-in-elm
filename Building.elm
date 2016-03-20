module Building where

import Color exposing (..)
import Graphics.Collage exposing (..)
import List exposing (..)

-------------------------------------------------------------------------------
-- Common/Constants
-------------------------------------------------------------------------------

type Layer = Front | Middle | Back | Static

glassWindowSize : number
glassWindowSize = 10

buildingWidth : number
buildingWidth   = 40

isBack : Building -> Bool
isBack b = b.layer == Back

isMiddle : Building -> Bool
isMiddle b = b.layer == Middle

isFront : Building -> Bool
isFront b = b.layer == Front

isStatic : Building -> Bool
isStatic b = b.layer == Static

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

-- generateGlassWindows : Float -> Float -> List GlassWindow
-- generateGlassWindows width height =
--     let
--         cols = 2
--         rows = height / (glassWindowSize * 2) - 1
--         rowsList = List.map (\v -> v * 20) [1..rows]
--
--         result1 = newGlassWindowRecurse 10 rowsList glassWindowSize glassWindowSize
--         result2 = List.map (\w -> { w | x = w.x + glassWindowSize * 2}) result1
--         result3 = List.map (\w -> { w | x = w.x - glassWindowSize / 2}) (result1 ++ result2)
--     in
--         result3

newGlassWindowRecurse : Float -> List Float -> Float -> Float -> List GlassWindow
newGlassWindowRecurse x' ys' w' h' =
    case ys' of
        [] -> []
        front::rest -> [ (newGlassWindow x' front w' h') ] ++ (newGlassWindowRecurse x' rest w' h')

-- generateGlassWindows : Float -> Float -> List GlassWindow -> List GlassWindow
-- generateGlassWindows width height windows =
--     windows

generateGlassWindows : Float -> Float -> Float -> Float -> Float -> Float -> List GlassWindow -> List GlassWindow
generateGlassWindows x y w h xSpacing ySpacing windows =
    let
        fixMe = ySpacing
    in
        if (y + glassWindowSize) > h then
            generateGlassWindows (x + glassWindowSize + xSpacing) (fixMe) w h xSpacing ySpacing windows
        else if (x + glassWindowSize) > w then
            windows
        else
            generateGlassWindows x (y + glassWindowSize + ySpacing) w h xSpacing ySpacing (windows ++ [newGlassWindow x y glassWindowSize glassWindowSize])

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
    let
        w' = buildingWidth
    in
        { nullBuilding |
          x = x'
        , y = 0
        , w = w'
        , h = h'
        , layer = l'
        , windows = generateGlassWindows 5 5 w' h' 5 5 []
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
