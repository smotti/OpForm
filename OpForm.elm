module OpForm exposing (main)

import Debug
import Html exposing (Html, button, div, form, input, label, li, span, text, ul)
import Html.App as App
import Html.Attributes exposing (placeholder, style, type', value)
import Html.Events exposing (onClick, onInput)
import Maybe exposing (andThen, withDefault)


main = 
  App.beginnerProgram
    { model = model
    , update = update
    , view = view
    }


-- MODEL

type alias Entry =
  { id : Int
  , name : String
  , address : String
  }


newEntry : Entry
newEntry =
  Entry 0 "" ""


type alias Model =
  { editingEntry : Bool
  , entries : List Entry
  , formEntry : Maybe Entry
  , currentEntryId : Int
  }


model =
  let
    entries =
      [ { id = 1, name = "Norbert Nordstroem", address = "Civic Blvd" }
      , { id = 2, name = "Hilinda Southstar", address = "North Greek Rd" }
      ]
  in
    { editingEntry = False
    , entries = entries
    , formEntry = Nothing
    , currentEntryId = 3
    }


-- UPDATE

type Msg
  = InputName String
  | InputAddress String
  | EditEntry Int
  | Save
  | Abort


update : Msg -> Model -> Model
update msg model =
  case msg of
    InputName newName ->
      let
        entry =
          getFormEntry model
      in
        { model | formEntry = Just <| setEntryName entry newName }

    InputAddress newAddr ->
      let
        entry =
          getFormEntry model
      in
        { model | formEntry = Just <| setEntryAddress entry newAddr }

    EditEntry id ->
      let
        entry =
          getEntry model.entries id
      in
        { model | editingEntry = True, formEntry = entry}

    Save ->
      let
        entry =
          getFormEntry model
        entries =
          if model.editingEntry then
            entry :: (removeEntry model.entries entry.id)
          else
            (setEntryId entry model.currentEntryId) :: model.entries
        newId =
          if model.editingEntry then
            model.currentEntryId
          else
            model.currentEntryId + 1
      in
        { model | editingEntry = False
                , formEntry = Nothing
                , entries = entries
                , currentEntryId = newId
        }

    Abort ->
      { model | editingEntry = False, formEntry = Nothing }


removeEntry : List Entry -> Int -> List Entry
removeEntry es id =
  Debug.log "List" List.filter (\e -> e.id /= id) es


setEntryId : Entry -> Int -> Entry
setEntryId e id =
  { e | id = id }


setEntryName : Entry -> String -> Entry
setEntryName e n =
  { e | name = n }


setEntryAddress : Entry -> String -> Entry
setEntryAddress e a =
  { e | address = a }


getFormEntry : Model -> Entry
getFormEntry model =
  case model.formEntry of
    Just entry -> entry
    Nothing -> newEntry


getEntry : List Entry -> Int -> Maybe Entry
getEntry entries id =
  let
    filtered =
      List.filter (\e -> e.id == id) entries
  in
    case List.head filtered of
      Just entry -> Just entry
      Nothing -> Nothing


-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ (viewForm model)
    , (viewEntries model.entries)
    , text <| toString model -- debug
    ] -- outer div


viewForm : Model -> Html Msg
viewForm { formEntry } =
  let
    entry =
      case formEntry of
        Just e -> e
        Nothing -> (Entry 0 "" "")
  in
    form []
      [ div []
        [ label [] [ text "Name: " ]
        , input
          [ type' "text"
          , value entry.name
          , placeholder "Name"
          , onInput InputName
          ] []
        ] -- name input
      , div []
        [ label [] [ text "Address: " ]
        , input
          [ type' "text"
          , value entry.address
          , placeholder "Address"
          , onInput InputAddress
          ] []
        ] -- address input
      , div []
        [ button [ type' "button", onClick Save ] [ text "Save" ]
        , button [ type' "button", onClick Abort ] [ text "Abort" ]
        ] -- buttons
      ] -- form


viewEntries : List Entry -> Html Msg
viewEntries entries =
  div []
    [ ul [] (List.map viewEntry entries) ]


viewEntry : Entry -> Html Msg
viewEntry e =
  li []
    [ span [ style [("margin-right", "10px")] ] [ text e.name ]
    , span [ style [("margin-right", "10px")] ] [ text e.address ]
    , button [ onClick <| EditEntry e.id ] [ text "Edit" ]
    ] 
