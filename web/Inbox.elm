module Inbox where

import Date
import Time
import Signal
import Graphics.Element (..)
import Graphics.Input.Field as Field
import Graphics.Input (button)
import Text
import List
import Json.Encode

type alias Model =
  { tasks      : List Task
  }

type alias Task =
  { text: String
  , createdAt: Time.Time
  }


model : Signal Model
model =
  let
    start = case getStorage of
      Nothing -> {tasks = []}
      Just m -> m
  in
    Signal.foldp (\t m -> {m | tasks <- m.tasks ++ [t]}) start newTasks

newTasks : Signal Task
newTasks =
  Signal.map (\(t, c) -> {text = c.string, createdAt = t})
    <| Signal.sampleOn (Signal.subscribe addClicked) (Time.timestamp <| Signal.subscribe taskContent)

main : Signal Element
main = Signal.map2 view model (Signal.subscribe taskContent)

taskContent : Signal.Channel Field.Content
taskContent = Signal.channel Field.noContent

addClicked : Signal.Channel ()
addClicked = Signal.channel ()

view : Model -> Field.Content -> Element
view m fc = let
    listView =
      flow down <|
        (List.map (\t ->
          flow down [
            (Text.plainText t.text)
          , (Text.plainText " ")
          , (Text.plainText <| dateString t)
          ]
        ) m.tasks)
    inputView = flow right [
      Field.field Field.defaultStyle (Signal.send taskContent) "input your task" fc
    , button (Signal.send addClicked ()) "Add"
    , Text.plainText fc.string
    ]
  in
    flow down [inputView, listView]

dateString : Task -> String
dateString t =
  let
    d = Date.fromTime t.createdAt
  in
    (toString <| Date.year d)
  ++ "/" ++ (toString <| Date.month d)
  ++ "/" ++ (toString <| Date.day d)
  ++ " " ++ (toString <| Date.hour d)
  ++ ":" ++ (toString <| Date.minute d)


-- interactions with localStorage to save the model
port getStorage : Maybe Model

port setStorage : Signal Model
port setStorage = model
