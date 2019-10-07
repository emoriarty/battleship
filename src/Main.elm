module Main exposing (main)

import Browser
import Debug
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Random
import Random.Extra
import Svg
import Svg.Attributes



-- CONSTANTS


boxSize =
    40


maxCol =
    10


maxRow =
    10



-- MAIN


main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { ships : List Ship }


type alias Ship =
    { class : ShipType
    , size : Int
    , position : Point
    , orientation : Orientation
    }


type ShipType
    = Destroyer
    | Submarine
    | Cruiser
    | Battleship
    | Carrier


type Orientation
    = Horizontal
    | Vertical


type alias Point =
    { x : Int, y : Int }



-- INIT


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model []
    , Cmd.none
    )



-- UPDATE


type Msg
    = None
    | Start
    | SetOrientation
    | SetPosition
    | UpdateOrientation (List Orientation)
    | UpdatePosition (List ( Int, Int ))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        None ->
            ( model, Cmd.none )

        Start ->
            update
                SetOrientation
                { model | ships = initShips }

        SetOrientation ->
            ( model
            , Random.generate UpdateOrientation
                (randomizeOrientation model.ships)
            )

        UpdateOrientation orientations ->
            update
                SetPosition
                { model | ships = updateOrientation orientations model.ships }

        SetPosition ->
            ( model
            , Random.generate UpdatePosition
                (randomizePositions model.ships)
            )

        UpdatePosition positions ->
            ( { model | ships = updatePositions (Debug.log "positions" positions) model.ships }
            , Cmd.none
            )


initShips : List Ship
initShips =
    [ Ship Destroyer 2 (Point 0 0) Vertical
    , Ship Submarine 3 (Point 0 0) Vertical
    , Ship Cruiser 3 (Point 0 0) Vertical
    , Ship Battleship 4 (Point 0 0) Vertical
    , Ship Carrier 5 (Point 0 0) Vertical
    ]


randomizeOrientation : List Ship -> Random.Generator (List Orientation)
randomizeOrientation ships =
    Random.map
        (\list -> List.map mapToOrientation list)
        (Random.list (List.length ships) (Random.int 0 1))


mapToOrientation : Int -> Orientation
mapToOrientation n =
    if n == 0 then
        Horizontal

    else
        Vertical


updateOrientation : List Orientation -> List Ship -> List Ship
updateOrientation orientations ships =
    List.map2
        (\orientation ship -> { ship | orientation = orientation })
        orientations
        ships


randomizePositions : List Ship -> Random.Generator (List ( Int, Int ))
randomizePositions ships =
    let
        sizes =
            List.map mapToSizeTuple ships
    in
    Random.Extra.sequence (List.map mapToRandomTuplePair sizes)


mapToSizeTuple : Ship -> ( Int, Int )
mapToSizeTuple ship =
    if ship.orientation == Horizontal then
        ( ship.size, 0 )

    else
        ( 0, ship.size )


mapToRandomTuplePair : ( Int, Int ) -> Random.Generator ( Int, Int )
mapToRandomTuplePair tuple =
    let
        cols =
            Tuple.first tuple

        rows =
            Tuple.second tuple
    in
    Random.pair
        (Random.map2
            pickFirstDigit
            (Random.int 0 (maxCol - 1 - cols))
            (Random.Extra.choice 0 1)
        )
        (Random.map2
            pickFirstDigit
            (Random.int 0 (maxRow - 1 - rows))
            (Random.Extra.choice 0 1)
        )


pickFirstDigit : Int -> Int -> Int
pickFirstDigit d flag =
    modBy 10 (2 * d + flag)


updatePositions : List ( Int, Int ) -> List Ship -> List Ship
updatePositions positions ships =
    List.map2 updatePosition positions ships


updatePosition : ( Int, Int ) -> Ship -> Ship
updatePosition point ship =
    let
        x =
            Tuple.first point

        y =
            Tuple.second point
    in
    { ship | position = Point x y }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Battleship"
    , body =
        [ div []
            [ div [ style "margin-bottom" "1rem" ]
                [ button
                    [ onClick Start
                    ]
                    [ text "Start" ]
                ]
            , viewGrid
                model
                []
                (makeShips model.ships)
            , viewGrid model
                [ style "float" "right" ]
                []
            ]
        ]
    }


viewGrid : Model -> List (Html.Attribute msg) -> List (Svg.Svg msg) -> Html msg
viewGrid model attrs nodes =
    Svg.svg
        (List.concat
            [ [ Svg.Attributes.width "420"
              , Svg.Attributes.height "420"
              , Svg.Attributes.viewBox "0 0 400 400"
              ]
            , attrs
            ]
        )
        (List.concat
            [ [ box
              , verticalLines
              , horizontalLines
              ]
            , nodes
            ]
        )



-- FUNCTIONS


makeShips : List Ship -> List (Svg.Svg msg)
makeShips ships =
    List.map svgShip ships


shipSize : Orientation -> Ship -> Int
shipSize orientation ship =
    if ship.orientation == orientation then
        ship.size * boxSize

    else
        boxSize



-- SHAPES


svgShip : Ship -> Svg.Svg msg
svgShip ship =
    Svg.g []
        [ Svg.rect
            [ Svg.Attributes.x
                (String.fromInt (ship.position.x * boxSize))
            , Svg.Attributes.y
                (String.fromInt (ship.position.y * boxSize))
            , Svg.Attributes.width
                (String.fromInt (shipSize Horizontal ship))
            , Svg.Attributes.height
                (String.fromInt (shipSize Vertical ship))
            , Svg.Attributes.fill "green"
            , Svg.Attributes.fillOpacity "0.7"
            , Svg.Attributes.stroke "green"
            , Svg.Attributes.strokeWidth "2px"
            ]
            []
        ]


box : Svg.Svg msg
box =
    Svg.rect
        [ Svg.Attributes.x "0"
        , Svg.Attributes.y "0"
        , Svg.Attributes.width "400"
        , Svg.Attributes.height "400"
        , Svg.Attributes.fill "transparent"
        , Svg.Attributes.stroke "black"
        , Svg.Attributes.strokeWidth "5px"
        ]
        []


verticalLines : Svg.Svg msg
verticalLines =
    Svg.g []
        [ Svg.line
            [ Svg.Attributes.x1 "40"
            , Svg.Attributes.y1 "0"
            , Svg.Attributes.x2 "40"
            , Svg.Attributes.y1 "400"
            , Svg.Attributes.stroke "black"
            , Svg.Attributes.strokeWidth "1px"
            ]
            []
        , Svg.line
            [ Svg.Attributes.x1 "80"
            , Svg.Attributes.y1 "0"
            , Svg.Attributes.x2 "80"
            , Svg.Attributes.y1 "400"
            , Svg.Attributes.stroke "black"
            , Svg.Attributes.strokeWidth "1px"
            ]
            []
        , Svg.line
            [ Svg.Attributes.x1 "120"
            , Svg.Attributes.y1 "0"
            , Svg.Attributes.x2 "120"
            , Svg.Attributes.y1 "400"
            , Svg.Attributes.stroke "black"
            , Svg.Attributes.strokeWidth "1px"
            ]
            []
        , Svg.line
            [ Svg.Attributes.x1 "160"
            , Svg.Attributes.y1 "0"
            , Svg.Attributes.x2 "160"
            , Svg.Attributes.y1 "400"
            , Svg.Attributes.stroke "black"
            , Svg.Attributes.strokeWidth "1px"
            ]
            []
        , Svg.line
            [ Svg.Attributes.x1 "200"
            , Svg.Attributes.y1 "0"
            , Svg.Attributes.x2 "200"
            , Svg.Attributes.y1 "400"
            , Svg.Attributes.stroke "black"
            , Svg.Attributes.strokeWidth "1px"
            ]
            []
        , Svg.line
            [ Svg.Attributes.x1 "240"
            , Svg.Attributes.y1 "0"
            , Svg.Attributes.x2 "240"
            , Svg.Attributes.y1 "400"
            , Svg.Attributes.stroke "black"
            , Svg.Attributes.strokeWidth "1px"
            ]
            []
        , Svg.line
            [ Svg.Attributes.x1 "280"
            , Svg.Attributes.y1 "0"
            , Svg.Attributes.x2 "280"
            , Svg.Attributes.y1 "400"
            , Svg.Attributes.stroke "black"
            , Svg.Attributes.strokeWidth "1px"
            ]
            []
        , Svg.line
            [ Svg.Attributes.x1 "320"
            , Svg.Attributes.y1 "0"
            , Svg.Attributes.x2 "320"
            , Svg.Attributes.y1 "400"
            , Svg.Attributes.stroke "black"
            , Svg.Attributes.strokeWidth "1px"
            ]
            []
        , Svg.line
            [ Svg.Attributes.x1 "360"
            , Svg.Attributes.y1 "0"
            , Svg.Attributes.x2 "360"
            , Svg.Attributes.y1 "400"
            , Svg.Attributes.stroke "black"
            , Svg.Attributes.strokeWidth "1px"
            ]
            []
        ]


horizontalLines : Svg.Svg msg
horizontalLines =
    Svg.g []
        [ Svg.line
            [ Svg.Attributes.y1 "40"
            , Svg.Attributes.x1 "0"
            , Svg.Attributes.y2 "40"
            , Svg.Attributes.x1 "400"
            , Svg.Attributes.stroke "black"
            , Svg.Attributes.strokeWidth "1px"
            ]
            []
        , Svg.line
            [ Svg.Attributes.y1 "80"
            , Svg.Attributes.x1 "0"
            , Svg.Attributes.y2 "80"
            , Svg.Attributes.x1 "400"
            , Svg.Attributes.stroke "black"
            , Svg.Attributes.strokeWidth "1px"
            ]
            []
        , Svg.line
            [ Svg.Attributes.y1 "120"
            , Svg.Attributes.x1 "0"
            , Svg.Attributes.y2 "120"
            , Svg.Attributes.x1 "400"
            , Svg.Attributes.stroke "black"
            , Svg.Attributes.strokeWidth "1px"
            ]
            []
        , Svg.line
            [ Svg.Attributes.y1 "160"
            , Svg.Attributes.x1 "0"
            , Svg.Attributes.y2 "160"
            , Svg.Attributes.x1 "400"
            , Svg.Attributes.stroke "black"
            , Svg.Attributes.strokeWidth "1px"
            ]
            []
        , Svg.line
            [ Svg.Attributes.y1 "200"
            , Svg.Attributes.x1 "0"
            , Svg.Attributes.y2 "200"
            , Svg.Attributes.x1 "400"
            , Svg.Attributes.stroke "black"
            , Svg.Attributes.strokeWidth "1px"
            ]
            []
        , Svg.line
            [ Svg.Attributes.y1 "240"
            , Svg.Attributes.x1 "0"
            , Svg.Attributes.y2 "240"
            , Svg.Attributes.x1 "400"
            , Svg.Attributes.stroke "black"
            , Svg.Attributes.strokeWidth "1px"
            ]
            []
        , Svg.line
            [ Svg.Attributes.y1 "280"
            , Svg.Attributes.x1 "0"
            , Svg.Attributes.y2 "280"
            , Svg.Attributes.x1 "400"
            , Svg.Attributes.stroke "black"
            , Svg.Attributes.strokeWidth "1px"
            ]
            []
        , Svg.line
            [ Svg.Attributes.y1 "320"
            , Svg.Attributes.x1 "0"
            , Svg.Attributes.y2 "320"
            , Svg.Attributes.x1 "400"
            , Svg.Attributes.stroke "black"
            , Svg.Attributes.strokeWidth "1px"
            ]
            []
        , Svg.line
            [ Svg.Attributes.y1 "360"
            , Svg.Attributes.x1 "0"
            , Svg.Attributes.y2 "360"
            , Svg.Attributes.x1 "400"
            , Svg.Attributes.stroke "black"
            , Svg.Attributes.strokeWidth "1px"
            ]
            []
        ]
