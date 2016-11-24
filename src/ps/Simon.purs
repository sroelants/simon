module Simon where

import Prelude (class Eq, class Show, Unit, show, const, not, eq, pure, bind, 
               (<<<), (<>), ($), (+), (<), (>), (==))
import Partial.Unsafe (unsafePartial)
import Control.Monad.Eff(Eff)
import Control.Monad.Aff(later')
import Control.Monad.Eff.Class(liftEff)
import Control.Monad.Eff.Random(RANDOM, randomInt)
import Control.Monad.Eff.Console(CONSOLE)
import Data.Maybe (Maybe(..))
import Pux (EffModel, noEffects, onlyEffects)
import Pux.Html (Html, Attribute, div, text, span, audio, br)
import Pux.Html.Attributes (className, id_, src)
import Pux.Html.Events (onClick)
import Data.List (List(..), length, (:), reverse)
import Data.List.Partial (head, tail)

--------------------------------------------------------------------------------
-- Data definitions
--------------------------------------------------------------------------------
data Color = Red | Green | Blue | Yellow
instance showColor :: Show Color where
  show Red = "red"
  show Green = "green"
  show Blue = "blue"
  show Yellow = "yellow"
instance eqColor :: Eq Color where
  eq Red Red = true
  eq Green Green = true
  eq Blue Blue = true
  eq Yellow Yellow = true
  eq _ _ = false


type State = { challenge :: List Color
             , todo :: List Color
             , hard :: Boolean
             , playing :: Boolean
             , activeColor :: Maybe Color
             , stage :: Int
             , lost :: Boolean
             , started :: Boolean
             , won :: Boolean}

data Action = Start
            | Stop
            | Win
            | Play Color
            | AddColor Color
            | PlayerTurn
            | ComputerTurn
            | NextStage
            | Lose
            | ActivateButton Color
            | DeactivateButton
            | ReplayList (List Color)
            | ReplayChallenge
            | GenerateColor
            | Restart
            | ToggleHard

initial :: State
initial = { challenge: Nil, todo: Nil, hard: false , playing: false, 
            activeColor: Nothing, stage: 1, lost: false, started: false,
            won: false}

--------------------------------------------------------------------------------
-- Update
--------------------------------------------------------------------------------
update :: forall e. Action -> State -> EffModel State Action (random :: RANDOM, console :: CONSOLE, sound :: SOUND | e)
update Start state = 
  {state: state {started = true},
   effects: [pure $ ComputerTurn]}
update Stop state =
  noEffects $ initial {started = false}
update (Play color) state = 
    if color == (unsafePartial $ head state.todo) then
      { state: state {todo = unsafePartial $ tail state.todo}
      , effects: [pure $ ActivateButton color] <>
                  if length state.todo > 1 then 
                   [] 
                 else [pure $ NextStage] }
    else onlyEffects state [pure $ ActivateButton color, later' 600 $ pure Lose]
update NextStage state =
  if length state.challenge < 10 then
    {  state: state { playing = false, stage = state.stage + 1}
    ,  effects: [later' 1000 $ pure ComputerTurn] }
  else
    onlyEffects state [later' 600 $ pure Win]
update ComputerTurn state = 
    onlyEffects state [ 
          pure GenerateColor
          ]
update GenerateColor state =
  onlyEffects state [ do
        color <- liftEff getRandomColor
        pure $ AddColor color
        ] 
update ReplayChallenge state = 
  onlyEffects state [pure $ ReplayList $ reverse state.challenge]
update (ReplayList Nil) state = 
  onlyEffects state [pure PlayerTurn]
update (ReplayList (c:cs)) state =
  onlyEffects state [pure $ ActivateButton c, later' 1000 $ pure (ReplayList cs)]
update (AddColor color) state = 
  { state: state { challenge = color:state.challenge}
  , effects: [ pure ReplayChallenge] }
update PlayerTurn state = 
    noEffects $ state { playing = true, todo = reverse state.challenge} 
update (ActivateButton color) state =
  { state: state {activeColor = Just color}
  , effects:[
    do
      later' 100 $ liftEff $ playSound color
      later' 300 $ pure DeactivateButton
    ]}
update DeactivateButton state = 
  noEffects $ state {activeColor = Nothing}
update Lose state = 
  {state: state {lost = true},
   effects: [later' 1000 $ pure $ Restart]}
update Restart state =
  if not state.hard then 
    {state: state {lost = false},
    effects: [pure $ ReplayList $ reverse state.challenge]} 
  else
  {state: initial {hard = state.hard, started = true},
   effects: [pure $ ComputerTurn]}
update ToggleHard state =
  noEffects $ state {hard = not state.hard}
update Win state = 
    { state: state {won = true} 
    , effects: [later' 3000 $ pure Stop] }

getRandomColor :: forall e. Eff (random :: RANDOM | e) Color
getRandomColor = do
  i <- randomInt 1 4
  pure case i of 
    1 -> Red
    2 -> Blue
    3 -> Green
    _ -> Yellow

--------------------------------------------------------------------------------
-- Views
--------------------------------------------------------------------------------
view :: State -> Html Action
view state = gameHtml state

gameHtml :: State -> Html Action
gameHtml state = 
  div [className "game"] 
    [ div (buttonAttributes state Red) [div [] []]
    , div (buttonAttributes state Green) [div [] []] 
    , div (buttonAttributes state Blue) [div [] [] ] 
    , div (buttonAttributes state Yellow) [div [][]] 
    , controlsHtml state
    , soundsHtml Red
    , soundsHtml Green
    , soundsHtml Blue
    , soundsHtml Yellow
    ]

buttonAttributes :: State -> Color -> Array (Attribute Action)
buttonAttributes state color = 
  [className $ "button " <> show color <>
              (if state.playing then " playing" else "") <>
              if eq state.activeColor (Just color) then " active" else "" <>
              if state.won then " won" else "" <>
              if state.lost then " lost" else "" ] <>
  if state.playing then [onClick (const (Play color))] else []

controlsHtml :: State -> Html Action
controlsHtml state = 
  div [className "controls-box"] [ 
    div [className "controls"] [
      div [className "mode"] [
        span ([className $ "easy" <> if state.hard then " clickable" else ""] <>
               if state.hard then [onClick $ const ToggleHard] else [])
             [text "Easy"],
        span ([className $ "hard" <> if state.hard then  "" else " clickable"] <>
               if not state.hard then [onClick $ const ToggleHard] else [])
             [text "Hard"],
        div [className $ "mode-background"<> if state.hard then " offset" else "" ][]
      ],
      div [className "stage"] [text  "Stage: "],-- <> show state.stage ],
      br [] [],
      div [className "stage"] [text $ show state.stage ],
      div ([className "start"] <> if state.started then  [onClick $ const Stop]
                                                  else [onClick $ const Start]) 
          [text if state.started then "Stop" else "Start"]
    ]
  ]

soundsHtml :: Color -> Html Action
soundsHtml color = 
  audio [id_ $ show color <> "-audio", src $ "snd/"<> show color <>".mp3"][]

--------------------------------------------------------------------------------
foreign import data SOUND :: !
foreign import playSound' :: forall e. String -> Eff (sound :: SOUND | e ) Unit

playSound :: forall e. Color -> Eff (sound :: SOUND | e ) Unit
playSound = playSound' <<< show
