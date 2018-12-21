module Main exposing (main)

import Array exposing (Array)
import Browser
import Dict exposing (Dict)
import Html exposing (Html, div, img, node, span, text)
import Html.Attributes exposing (attribute, class, classList, src, style)
import Html.Events exposing (onClick)
import Staff


type alias Staff =
    Dict String Bool


type alias Model =
    { staff : Staff
    , clues : Array String
    , clueIndex : Int
    }


type Msg
    = MemberClick String
    | NextClue
    | PreviousClue


main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }


init : Model
init =
    { staff = Dict.fromList (Staff.all |> List.map (\x -> ( x, False )))
    , clues = Staff.clues |> Array.fromList
    , clueIndex = 0
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        MemberClick member ->
            let
                staff =
                    toggleStaffMember model.staff member
            in
            { model | staff = staff }

        NextClue ->
            { model | clueIndex = modBy (Array.length model.clues) (model.clueIndex + 1) }

        PreviousClue ->
            { model | clueIndex = modBy (Array.length model.clues) (model.clueIndex - 1) }


toggleStaffMember : Staff -> String -> Staff
toggleStaffMember staff name =
    let
        toggle m =
            not m
    in
    Dict.update name (Maybe.map toggle) staff


view : Model -> Html Msg
view model =
    div []
        [ message
        , clueControl model.clueIndex (model.clues |> Array.length)
        , clueView (Array.get model.clueIndex model.clues |> Maybe.withDefault "")
        , staffGrid model.staff
        , cssLink
        ]


cssLink : Html Msg
cssLink =
    node "link"
        [ attribute "rel" "stylesheet"
        , attribute "href" "/style.css"
        ]
        []


message : Html Msg
message =
    div [ class "message" ]
        [ div [] [ text "Hola Manu!! Espero que te haya gustado el regalo (y que lo hayas abierto antes de jugar a este juego)." ]
        , div [] [ text "Seguí las pistas para ir descartando gente y descubrir tu Secret Santa 2018" ]
        ]


clueControl : Int -> Int -> Html Msg
clueControl clueIndex clueCount =
    div [ class "clue-control" ]
        [ span [ class "button", onClick PreviousClue ] [ text "<" ]
        , text "Pista "
        , text (String.fromInt <| clueIndex + 1)
        , text "/"
        , text (String.fromInt clueCount)
        , span [ class "button", onClick NextClue ] [ text ">" ]
        ]


clueView : String -> Html a
clueView clue =
    div [ class "clue" ]
        [ text clue
        ]


staffGrid : Staff -> Html Msg
staffGrid staff =
    div
        [ class "staff-grid" ]
        (Staff.all
            |> List.map (\x -> ( x, Dict.get x staff |> Maybe.withDefault False ))
            |> List.map avatar
        )


avatar : ( String, Bool ) -> Html Msg
avatar ( name, marked ) =
    div
        [ classList
            [ ( "staff-member", True )
            , ( "marked", marked )
            ]
        ]
        [ img
            [ src ("/images/" ++ name ++ ".jpg")
            , onClick <| MemberClick name
            ]
            []
        ]
