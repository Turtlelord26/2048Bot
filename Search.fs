module Search

open Game
open StringUtils

type Action =
    | Root
    | Left
    | Right
    | Up
    | Down
    with

    static member toString action =
        match action with
        | Root -> ""
        | Left -> "L"
        | Right -> "R"
        | Up -> "U"
        | Down -> "D"

    static member manyToString =
        List.map Action.toString
        >> List.reduce concatWithComma

type SearchTree =
    | Empty
    | Tree of state: GameState * action: Action * parent: SearchTree * left: SearchTree * right: SearchTree * up: SearchTree * down: SearchTree
    with

    static member scoreOf searchTree =
        match searchTree with
        | Empty -> 0
        | Tree (state, _, _, _, _, _, _) -> state.score

    static member actionOf tree =
        match tree with
        | Tree (_, action, _, _, _, _, _) ->
            action
        | Empty ->
            Root

    static member toSearchTreeRoot state =
        Tree (state, Root, Empty, Empty, Empty, Empty, Empty)

    static member getChildren tree =
        match tree with
        | Tree (_, _, _, left, right, up, down) ->
            seq {left; right; up; down}
        | Empty ->
            Seq.empty

    static member pathToRoot tree =
        
        let rec climb path tree =
            match tree with
            | Tree (_, Root, _, _, _, _, _)
            | Empty ->
                path
            | Tree (_, _, parent, _, _, _, _) ->
                climb (parent :: path) parent
        
        climb [tree] tree
