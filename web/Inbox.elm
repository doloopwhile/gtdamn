module Inbox where

import Date
import Time (..)
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
  , createdAt: Time
  }

model : Signal Model
model =
  -- Signal.constant <|
  --   case getStorage of
  --     Nothing -> {tasks = []}
  --     Just m -> m
  --
  Signal.constant {
      tasks = [
        { text = "foo"
        , createdAt = 1421054358 * second
        }
      , { text = "bar"
        , createdAt = 1421054000 * second
        }
      ]
    }

main : Signal Element
main = Signal.map2 view model (Signal.subscribe taskContent)

taskContent : Signal.Channel Field.Content
taskContent =
  Signal.channel Field.noContent

taskEnter : Signal.Channel ()
taskEnter =
  Signal.channel ()


view : Model -> Field.Content-> Element
view m fc = let
    listView =
      flow down <|
        (List.map (\t ->
          flow right [
            (Text.plainText t.text)
          , (Text.plainText " ")
          , (Text.plainText <| dateString t)
          ]
        ) m.tasks)
    inputView = flow right [
      Field.field Field.defaultStyle (Signal.send taskContent) "input your task" fc
    , button (Signal.send taskEnter ()) "Add"
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
