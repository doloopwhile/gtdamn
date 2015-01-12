module Inbox where

import Date
import Time
import Signal
import Graphics.Element (..)
import Graphics.Input.Field as Field
import Text
import List

type alias Model =
  { tasks      : List Task
  }

type alias Task =
  { text: String
  }

model : Signal Model
model = Signal.constant {
      tasks = [
        { text = "foo"
        -- , createdAt = Date.fromTime <| 1421054358 * Time.second
        }
      ]
    }

main : Signal Element
main = Signal.map view model

view : Model -> Element
view m =
  flow down <| List.map (\t -> Text.plainText t.text ) m.tasks

-- interactions with localStorage to save the model
port getStorage : Maybe Model

port setStorage : Signal Model
port setStorage = model
