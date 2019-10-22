module Ship exposing (Orientation(..), Position, Ship, Type(..))

-- SHIP


type alias Ship =
    { class : Type
    , size : Int
    , position : Position
    , orientation : Orientation
    }


type alias Position =
    ( Int, Int )


type Type
    = Destroyer
    | Submarine
    | Cruiser
    | Battleship
    | Carrier


type Orientation
    = Horizontal
    | Vertical
