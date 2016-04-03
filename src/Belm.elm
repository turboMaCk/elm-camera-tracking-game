import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import List
import Transform2D
import Time exposing (..)
import Keyboard
import Window
import Color exposing (..)

-- MODEL

type alias Dimensions = (Int, Int)

type alias Keys = { x : Int
                  , y : Int }

type alias Input = { direction : Keys
                   , dt : Time }

type Move = Jump
          | Stand
          | Run

type Direction = Left | Right

type alias Hero = { x : Float
                  , y : Float
                  , vy : Float
                  , direction : Direction
                  , state : Move
                  , spriteNum : Int }

type alias Environment = { x : Float
                         , y : Float
                         , bg : String }

type alias Camera = { x : Float
                    , y : Float
                    , vx : Float
                    , vy : Float }

type alias Game = { hero : Hero
                  , environment : Environment
                  , camera : Camera }

-- Utils

absolute : Float -> Float
absolute number =
  if number > 0 then number else number * -1

-- takeN : Int -> List a -> Maybe a
-- takeN n list =
--   case list of
--     [] -> Nothing
--     h::t ->
--       if n == 0 then Just h else takeN (n - 1) t

input : Signal Input
input = Signal.map2 Input Keyboard.arrows (fps 60)

initialGame : Game
initialGame =
  { hero = { x = 0, y = 200, vy = 0, direction = Right, state = Stand, spriteNum = 0 }
  , environment = { x = 0, y = 0, bg = "/imgs/bg.png" }
  , camera = { x = 0, y = 0, vx = 0, vy = 0 } }

-- UPDATES

updateHeroState : Input -> Game -> Game
updateHeroState { direction, dt } ({ hero } as game) =
  let
    x = direction.x
    state' =
      if x == 0 then Stand else Run
    direction' =
      case x of
        1 -> Right
        (-1) -> Left
        _ -> hero.direction
  in
    { game
      | hero = { hero
                 | state = state'
                 , direction = direction' }}

gravity : Input -> Game -> Game
gravity { dt } ({ hero } as game) =
  let
    nextY =
      hero.y + hero.vy
    (y, vy, state) =
        if nextY <= 0 && absolute hero.x < 500
        then (0, 0, hero.state)
        else (nextY, hero.vy - dt/50, Jump)
  in
    { game
      | hero = { hero
                 | y = y
                 , vy = vy
                 , state = state}}

moveHero : Input -> Game -> Game
moveHero { dt, direction } ({ hero } as game) =
  let
    step =
      dt * toFloat direction.x/3
  in
    { game
      | hero = { hero
                 | x = hero.x + step }}

jump : Input -> Game -> Game
jump { dt, direction } ({ hero } as game) =
  if hero.y == 0 && direction.y > 0
  then { game
         | hero = { hero
                    | vy = 10
                    , state = Jump }}
  else game

trackHero : Input -> Game -> Game
trackHero { dt } ({ hero, camera } as game) =
  let
    k heroPos camPos =
      (heroPos + camPos)/200
  in
    { game
        | camera = { camera
                        | x = camera.x - (camera.vx * dt)
                        , y = camera.y - (camera.vy * dt)
                        , vx = k camera.x hero.x
                        , vy = k camera.y hero.y }}

heroSprite : Game -> Game
heroSprite ({ hero } as game) =
  let
    state = hero.state
    lim =
        case state of
        Run -> 27
        Stand -> 2
        Jump -> 1
    num' = if hero.spriteNum > lim - 1 then 1 else hero.spriteNum + 1
  in
    { game | hero = { hero
                      | spriteNum = num' }}

update : Input -> Game -> Game
update input game =
  game
    |> updateHeroState input
    |> gravity input
    |> jump input
    |> moveHero input
    |> trackHero input
    |> heroSprite
    |> reset

reset : Game -> Game
reset ({ hero } as game) =
  if hero.y < -2000 then initialGame else game

gameState : Signal Game
gameState = Signal.foldp update initialGame input

-- VIEW

heroView : Dimensions -> Game -> Form
heroView (width, height) { hero } =
  let
    state = hero.state
    direction = hero.direction
    dirFolder =
      case direction of
        Left -> "left"
        Right -> "right"
    src =
      case state of
        Stand ->
          "/imgs/tim/" ++ dirFolder ++ "/stand/" ++ toString hero.spriteNum ++ ".png"
        Run ->
          "/imgs/tim/" ++ dirFolder ++ "/run/" ++ toString hero.spriteNum ++ ".png"
        Jump ->
          "/imgs/tim/" ++ dirFolder ++ "/jump/" ++ toString hero.spriteNum ++ ".png"
  in
    image 125 152 src
      |> toForm
      |> scale 0.5
      |> move (hero.x, hero.y + 35)

view : Dimensions -> Game -> Element
view ((w', h') as dimensions) ({ hero, environment, camera } as game) =
  let
    (w, h) = (toFloat w', toFloat h')

    background =
      rect w h
        |> filled (rgb 174 238 238)

    env =
      tiledImage 1000 100 "http://www.geeks3d.com/public/jegx/200812/game-texture-01.jpg"
  in
    collage w' h'
        [ background
        , groupTransform (Transform2D.translation camera.x (camera.y - h/4))
            [ env
                |> toForm
                |> move (environment.x, environment.y - 50)
            , heroView dimensions game ]]

main : Signal Element
main =
  Signal.map2 view Window.dimensions gameState
