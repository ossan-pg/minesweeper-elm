module Main exposing (main)

import Browser
import Html.Events.Extra.Mouse as Mouse
import List.Extra as ListEx
import Matrix exposing (Matrix)
import Maybe.Extra as MaybeEx
import Origami exposing (property, with)
import Origami.Html exposing (Attribute, Html, a, button, div, hr, styled, text, toHtml)
import Origami.Html.Attributes exposing (css, fromHtmlAttribute, href)
import Random
import Random.List as RandomList


type Marker
    = Unmarked
    | MarkedFix
    | MarkedSus
    | MarkedErr


type MineStatus
    = NoMine Int
    | Exploded
    | Exposed


type CellStatus
    = Covered Marker
    | Opened MineStatus


type alias Cell =
    { hasMine : Bool
    , status : CellStatus
    }


initialCell : Cell
initialCell =
    Cell False (Covered Unmarked)


type Phase
    = GameStart
    | GamePlaying
    | GameEndCompleted
    | GameEndFailed


type alias Model =
    { phase : Phase
    , maxMines : Int
    , board : Matrix Cell
    }


initialModel : Model
initialModel =
    Model GameStart 0 Matrix.empty


type Msg
    = Initialize Int (Matrix Cell)
    | Open ( Int, Int )
    | Mark ( Int, Int )
    | ChangeFirstCell ( Int, Int ) ( Int, Int )
    | Restart
    | SetBoardAndMines ( Int, Int ) Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        getCell : Int -> Int -> Matrix Cell -> Cell
        getCell x_ y_ board_ =
            -- デフォルトの値は任意でよい(ここの Matrix.get が Nothing になることはないので)
            Maybe.withDefault initialCell <| Matrix.get ( x_, y_ ) board_

        exposeMines : Matrix Cell -> Matrix Cell
        exposeMines board_ =
            let
                width =
                    Tuple.first <| Matrix.size model.board
            in
            board_
                |> (Matrix.toList >> List.concat)
                >> List.map
                    (\cell_ ->
                        -- 地雷マスを開く(確定マークのマスと爆発したマスを除く)
                        if cell_.hasMine then
                            case cell_.status of
                                Covered MarkedFix ->
                                    cell_

                                Opened Exploded ->
                                    cell_

                                _ ->
                                    { cell_ | status = Opened Exposed }

                        else
                            cell_
                    )
                >> (ListEx.groupsOf width >> Matrix.fromList)

        -- 開いていないマスの個数と地雷の最大数が同じ場合はゲームクリア
        isCompleted : Matrix Cell -> Bool
        isCompleted board =
            board
                |> (Matrix.toList >> List.concat)
                >> List.filter
                    (\cell_ ->
                        case cell_.status of
                            Covered _ ->
                                True

                            _ ->
                                False
                    )
                >> List.length
                >> (==) model.maxMines

        -- 爆発マスが存在する場合はゲームオーバー
        isFailed : Matrix Cell -> Bool
        isFailed board =
            board
                |> (Matrix.toList >> List.concat)
                >> List.filter (.status >> (==) (Opened Exploded))
                >> List.length
                >> (<) 0
    in
    case msg of
        Initialize maxMines board ->
            ( { phase = GameStart, maxMines = maxMines, board = board }, Cmd.none )

        Open ( x, y ) ->
            let
                cell : Cell
                cell =
                    getCell x y model.board
            in
            if model.phase == GameEndCompleted || model.phase == GameEndFailed then
                -- ゲームクリア or ゲームオーバー時はなにもしない
                ( model, Cmd.none )

            else if cell.hasMine then
                if model.phase == GameStart then
                    -- 1手目の場合、通常マスと入れ替える
                    let
                        -- 通常マスの座標のリスト
                        coordsNoMine : List ( Int, Int )
                        coordsNoMine =
                            model.board
                                |> Matrix.toList
                                >> List.indexedMap
                                    (\y_ cells ->
                                        cells
                                            |> List.indexedMap
                                                (\x_ cell_ ->
                                                    if cell_.hasMine then
                                                        Nothing

                                                    else
                                                        Just ( x_, y_ )
                                                )
                                    )
                                >> List.concat
                                >> List.filterMap identity

                        -- 通常マスの座標の内、任意の1つを取得する Generator
                        generator : Random.Generator ( Int, Int )
                        generator =
                            case coordsNoMine of
                                coord :: coords ->
                                    Random.uniform coord coords

                                _ ->
                                    -- ダミー。こちらが採用されることはない(はず)
                                    Random.constant ( 0, 0 )
                    in
                    ( model, Random.generate (ChangeFirstCell ( x, y )) generator )

                else
                    -- 開くマスに地雷がある場合はゲームオーバー
                    let
                        newBoard : Matrix Cell
                        newBoard =
                            Matrix.set ( x, y ) { cell | status = Opened Exploded } model.board
                                |> exposeMines
                    in
                    ( { model | phase = GameEndFailed, board = newBoard }, Cmd.none )

            else if cell.status == Covered Unmarked then
                -- 開くマスに地雷がない＋マスがマークされていない場合はそのマス(および適宜周囲のマス)を開く
                let
                    newBoard : Matrix Cell
                    newBoard =
                        openCells ( x, y ) model.board

                    newPhase : Phase
                    newPhase =
                        if isCompleted newBoard then
                            GameEndCompleted

                        else
                            GamePlaying
                in
                ( { model | phase = newPhase, board = newBoard }, Cmd.none )

            else
                case cell.status of
                    Opened (NoMine n) ->
                        -- 当該マスを既に開いている＋周囲の地雷数と確定マーク(Covered MarkedFix)の個数が一致する場合、
                        -- 確定マークのない周囲マスを開く。疑惑マーク(Covered MarkedSus)も開く
                        let
                            getOffsetMarkedFix : ( Int, Int ) -> Maybe ( Int, Int )
                            getOffsetMarkedFix ( dx, dy ) =
                                Matrix.get ( x + dx, y + dy ) model.board
                                    |> MaybeEx.filter (.status >> (==) (Covered MarkedFix))
                                    |> Maybe.map (\_ -> ( dx, dy ))

                            -- 周囲の確定マークマスの相対座標
                            offsetsMarkedFix : List ( Int, Int )
                            offsetsMarkedFix =
                                List.filterMap getOffsetMarkedFix offsets
                        in
                        if List.length offsetsMarkedFix == n then
                            let
                                -- 周囲の確定マークマス以外の相対座標
                                offsetsOpenable : List ( Int, Int )
                                offsetsOpenable =
                                    List.filter (\offset -> ListEx.notMember offset offsetsMarkedFix) offsets

                                -- 周囲に爆破対象(マークなし＋地雷あり)のマスが存在する→爆破する＋マーク誤りを設定
                                -- 周囲に爆破対象のマスが存在しない→マーク誤りマスを開く
                                boardVerifiedExplosion =
                                    let
                                        getOffset : ( Int, Int ) -> Maybe ( Int, Int )
                                        getOffset ( dx, dy ) =
                                            Matrix.get ( x + dx, y + dy ) model.board
                                                |> MaybeEx.filter .hasMine
                                                >> Maybe.map (\_ -> ( dx, dy ))
                                    in
                                    List.filterMap getOffset offsetsOpenable
                                        |> List.head
                                        >> Maybe.andThen
                                            (\( dx, dy ) ->
                                                Matrix.get ( x + dx, y + dy ) model.board
                                                    |> Maybe.map (\cell_ -> { cell_ | status = Opened Exploded })
                                                    |> Maybe.map (\cell_ -> Matrix.set ( x + dx, y + dy ) cell_ model.board)
                                            )
                                        >> Maybe.map exposeMines
                                        >> Maybe.withDefault model.board

                                isFailed_ : Bool
                                isFailed_ =
                                    isFailed boardVerifiedExplosion

                                boardVerifiedMarkingErr : Matrix Cell
                                boardVerifiedMarkingErr =
                                    let
                                        width =
                                            Tuple.first <| Matrix.size model.board
                                    in
                                    boardVerifiedExplosion
                                        |> (Matrix.toList >> List.concat)
                                        >> List.map
                                            (\cell_ ->
                                                if isFailed_ && not cell_.hasMine && cell_.status == Covered MarkedFix then
                                                    -- 通常マスを確定マークしている場合はマーク誤りを設定
                                                    { cell_ | status = Covered MarkedErr }

                                                else
                                                    cell_
                                            )
                                        >> (ListEx.groupsOf width >> Matrix.fromList)

                                newBoard : Matrix Cell
                                newBoard =
                                    List.foldl
                                        (\( dx, dy ) -> openCells ( x + dx, y + dy ))
                                        boardVerifiedMarkingErr
                                        offsetsOpenable

                                newPhase : Phase
                                newPhase =
                                    if isFailed_ then
                                        GameEndFailed

                                    else if isCompleted newBoard then
                                        GameEndCompleted

                                    else
                                        GamePlaying
                            in
                            ( { model | phase = newPhase, board = newBoard }, Cmd.none )

                        else
                            -- 周囲の地雷数と確定マーク(Covered MarkedFix)の個数が一致しない場合は何もしない
                            ( model, Cmd.none )

                    _ ->
                        -- 当該マスがマークされている場合は何もしない
                        ( model, Cmd.none )

        Mark ( x, y ) ->
            if model.phase == GameEndCompleted || model.phase == GameEndFailed then
                -- ゲームクリア or ゲームオーバー時はなにもしない
                ( model, Cmd.none )

            else
                let
                    cell : Cell
                    cell =
                        getCell x y model.board

                    newCell =
                        case cell.status of
                            Covered type_ ->
                                case type_ of
                                    Unmarked ->
                                        { cell | status = Covered MarkedFix }

                                    MarkedFix ->
                                        { cell | status = Covered MarkedSus }

                                    MarkedSus ->
                                        { cell | status = Covered Unmarked }

                                    _ ->
                                        cell

                            _ ->
                                -- 既に開いてるマスに対しては何もしない
                                cell
                in
                ( { model | board = Matrix.set ( x, y ) newCell model.board }, Cmd.none )

        ChangeFirstCell ( mineX, mineY ) ( noMineX, noMineY ) ->
            let
                -- 指定された地雷マスと通常マスを入れ替える(地雷の有無を逆にする)
                newBoard =
                    model.board
                        |> Matrix.set ( mineX, mineY ) (Cell False (Covered Unmarked))
                        |> Matrix.set ( noMineX, noMineY ) (Cell True (Covered Unmarked))
            in
            update (Open ( mineX, mineY )) { model | phase = GamePlaying, board = newBoard }

        Restart ->
            let
                ( width, height ) =
                    Matrix.size model.board
            in
            update (SetBoardAndMines ( width, height ) model.maxMines) model

        SetBoardAndMines ( width, height ) maxMines ->
            let
                -- 地雷マス＋通常マスのリスト
                cells : List Cell
                cells =
                    List.repeat maxMines (Cell True (Covered Unmarked))
                        ++ List.repeat (width * height - maxMines) (Cell False (Covered Unmarked))

                -- cells の各要素をシャッフルして Matrix へ変換する、ための generator を作成する
                -- シャッフル1回では分布が偏る(気がする)ので、とりあえずシャッフル×2と転置を複数回行っておく
                generator : Random.Generator (Matrix Cell)
                generator =
                    cells
                        |> RandomList.shuffle
                        >> Random.andThen RandomList.shuffle
                        >> Random.map (ListEx.groupsOf width >> ListEx.transpose >> List.concat)
                        >> Random.andThen RandomList.shuffle
                        >> Random.andThen RandomList.shuffle
                        >> Random.map (ListEx.groupsOf width >> ListEx.transpose >> List.concat)
                        >> Random.andThen RandomList.shuffle
                        >> Random.andThen RandomList.shuffle
                        >> Random.map (ListEx.groupsOf width >> Matrix.fromList)
            in
            ( model, Random.generate (Initialize maxMines) generator )


offsets : List ( Int, Int )
offsets =
    [ ( -1, -1 )
    , ( 0, -1 )
    , ( 1, -1 )
    , ( -1, 0 )
    , ( 1, 0 )
    , ( -1, 1 )
    , ( 0, 1 )
    , ( 1, 1 )
    ]


openCells : ( Int, Int ) -> Matrix Cell -> Matrix Cell
openCells ( x, y ) board =
    let
        ( width, height ) =
            Matrix.size board

        cell : Cell
        cell =
            -- デフォルトの値は任意でよい(指定されたマスの座標が範囲外の場合は何も処理しないので)
            Maybe.withDefault initialCell <| Matrix.get ( x, y ) board

        noOp : Bool
        noOp =
            case cell.status of
                Opened _ ->
                    True

                Covered MarkedFix ->
                    cell.hasMine

                Covered MarkedErr ->
                    True

                _ ->
                    False
    in
    if x < 0 || width <= x || y < 0 || height <= y || noOp then
        -- 指定されたマスの座標が範囲外 or 開く対象ではないマスの場合は何もしない
        board

    else
        let
            countMines : Int -> Int -> Matrix Cell -> Int
            countMines x_ y_ board_ =
                let
                    hasMine : ( Int, Int ) -> Bool
                    hasMine ( dx, dy ) =
                        Matrix.get ( x_ + dx, y_ + dy ) board_
                            |> Maybe.map .hasMine
                            |> Maybe.withDefault False
                in
                ListEx.count hasMine offsets

            nrOfMines : Int
            nrOfMines =
                countMines x y board

            newCell : Cell
            newCell =
                { cell | status = Opened (NoMine nrOfMines) }

            newBoard : Matrix Cell
            newBoard =
                Matrix.set ( x, y ) newCell board
        in
        if nrOfMines > 0 then
            -- 周囲に地雷がある
            newBoard

        else
            -- 周囲に地雷がない場合は周囲の各マスも開く
            List.foldl (\( dx, dy ) -> openCells ( x + dx, y + dy )) newBoard offsets


view : Model -> Html Msg
view model =
    let
        -- 確定マークのマスの個数
        nrOfMarkedFix : Int
        nrOfMarkedFix =
            if model.phase == GameEndCompleted then
                -- ゲームクリア時は全ての地雷マスに確定マークされているものとして扱う
                model.maxMines

            else
                model.board
                    |> (Matrix.toList >> List.concat)
                    >> List.filter (.status >> (==) (Covered MarkedFix))
                    >> List.length
    in
    div
        [ css
            [ property "background" "rgb(255, 240, 240)"
            , property "padding" "2px"
            ]
        ]
    <|
        [ div []
            [ text "original: "
            , a [ href "https://minesweeper.online/" ] [ text "https://minesweeper.online/" ]
            ]
        , hr [] []
        , div []
            [ button [ fromHtmlAttribute <| Mouse.onClick (\_ -> SetBoardAndMines ( 9, 9 ) 10) ] [ text "9x9(10)" ]
            , button [ fromHtmlAttribute <| Mouse.onClick (\_ -> SetBoardAndMines ( 16, 16 ) 40) ] [ text "16x16(40)" ]
            , button [ fromHtmlAttribute <| Mouse.onClick (\_ -> SetBoardAndMines ( 30, 16 ) 99) ] [ text "30x16(99)" ]
            ]
        , div
            [ css
                [ property "background" "rgb(198, 198, 198)"
                , property "border-top" "4px solid rgb(250, 250, 250)"
                , property "border-left" "4px solid rgb(250, 250, 250)"
                , property "border-right" "4px solid rgb(128, 128, 128)"
                , property "border-bottom" "4px solid rgb(128, 128, 128)"
                , property "width" "fit-content"
                , property "padding" "8px"
                , property "margin-top" "10px"
                ]
            ]
            [ div
                [ css
                    [ property "display" "grid"

                    -- TODO fr単位だと盤面のサイズ変更時にRestartボタンのサイズが変わるので2個目は固定長がいいかも
                    , property "grid-template-columns" "1fr 1fr 1fr"
                    , property "gap" "10px"
                    ]
                ]
                [ text <| String.fromInt (model.maxMines - nrOfMarkedFix)
                , button [ fromHtmlAttribute <| Mouse.onClick (\_ -> Restart) ]
                    [ text <|
                        case model.phase of
                            GameEndCompleted ->
                                "OK!!"

                            GameEndFailed ->
                                "Failed..."

                            _ ->
                                "Restart"
                    ]
                ]
            , div
                [ css
                    [ property "border-top" "3px solid rgb(128, 128, 128)"
                    , property "border-left" "3px solid rgb(128, 128, 128)"
                    , property "border-right" "3px solid rgb(250, 250, 250)"
                    , property "border-bottom" "3px solid rgb(250, 250, 250)"
                    , property "width" "fit-content"
                    , property "margin-top" "8px"
                    ]
                ]
              <|
                viewCells model
            ]
        ]


viewCells : Model -> List (Html Msg)
viewCells { phase, board } =
    let
        viewCell : ( Int, Int ) -> Cell -> Html Msg
        viewCell ( x, y ) cell =
            case cell.status of
                Opened (NoMine nrOfMines) ->
                    let
                        s =
                            if nrOfMines <= 0 then
                                ""

                            else
                                String.fromInt nrOfMines
                    in
                    cellOpened
                        [ fromHtmlAttribute <| Mouse.onClick (\_ -> Open ( x, y ))
                        , fromHtmlAttribute <| Mouse.onContextMenu (\_ -> Mark ( x, y ))
                        ]
                        [ text s ]

                Opened Exploded ->
                    -- 爆発マス(ゲームオーバー時に表示)
                    cellExploded
                        [ fromHtmlAttribute <| Mouse.onContextMenu (\_ -> Mark ( x, y )) ]
                        [ text "x" ]

                Opened Exposed ->
                    -- 爆発マス以外の地雷マス(ゲームオーバー時に表示)
                    cellExposed
                        [ fromHtmlAttribute <| Mouse.onContextMenu (\_ -> Mark ( x, y )) ]
                        [ text "x" ]

                Covered type_ ->
                    case type_ of
                        -- TODO ゲームクリア時のマーク変更は view より update でやっておく方がいいかも
                        Unmarked ->
                            if phase == GameEndCompleted then
                                -- ゲームクリア時は確定マーク "!" を表示し、セルを押せない(ボタンが凹まない)ようにする
                                -- TODO active を未設定にしてるだけなので、もっといいやり方があったらそれを採用したい
                                cellInactive
                                    [ fromHtmlAttribute <| Mouse.onContextMenu (\_ -> Mark ( x, y )) ]
                                    [ text "!" ]

                            else if phase == GameEndFailed then
                                -- ゲームオーバー時はセルを押せない(ボタンが凹まない)ようにする
                                cellInactive
                                    [ fromHtmlAttribute <| Mouse.onContextMenu (\_ -> Mark ( x, y )) ]
                                    [ text "" ]

                            else
                                cellUnmarked
                                    [ fromHtmlAttribute <| Mouse.onClick (\_ -> Open ( x, y ))
                                    , fromHtmlAttribute <| Mouse.onContextMenu (\_ -> Mark ( x, y ))
                                    ]
                                    [ text "" ]

                        MarkedFix ->
                            cellMarked
                                [ fromHtmlAttribute <| Mouse.onContextMenu (\_ -> Mark ( x, y )) ]
                                [ text "!" ]

                        MarkedSus ->
                            if phase == GameEndCompleted then
                                cellInactive
                                    [ fromHtmlAttribute <| Mouse.onContextMenu (\_ -> Mark ( x, y )) ]
                                    [ text "!" ]

                            else
                                cellMarked
                                    [ fromHtmlAttribute <| Mouse.onContextMenu (\_ -> Mark ( x, y )) ]
                                    [ text "?" ]

                        MarkedErr ->
                            cellMarkedErr
                                [ fromHtmlAttribute <| Mouse.onContextMenu (\_ -> Mark ( x, y )) ]
                                [ text "!" ]
    in
    board
        |> Matrix.toList
        |> List.indexedMap
            (\y cells ->
                cells
                    |> List.indexedMap (\x cell -> viewCell ( x, y ) cell)
                    |> div
                        [ css
                            [ property "display" "grid"
                            , property
                                "grid-template-columns"
                                ("repeat(" ++ (Matrix.size board |> Tuple.first |> String.fromInt) ++ ", 24px)")
                            , property "gap" "0px"
                            ]
                        ]
            )


cellOpened : List (Attribute msg) -> List (Html msg) -> Html msg
cellOpened =
    styled button
        [ property "width" "24px"
        , property "height" "24px"
        , property "border-radius" "0"
        , property "background" "rgb(200, 200, 200)"
        , property "border-top" "2px solid rgb(128, 128, 128)"
        , property "border-left" "2px solid rgb(128, 128, 128)"
        , property "border-right" "0px solid rgb(200, 200, 200)"
        , property "border-bottom" "0px solid rgb(200, 200, 200)"
        ]


cellExploded : List (Attribute msg) -> List (Html msg) -> Html msg
cellExploded =
    styled button
        [ property "width" "24px"
        , property "height" "24px"
        , property "border-radius" "0"
        , property "background" "rgb(255, 0, 0)"
        , property "border-top" "2px solid rgb(128, 128, 128)"
        , property "border-left" "2px solid rgb(128, 128, 128)"
        , property "border-right" "0px solid rgb(200, 200, 200)"
        , property "border-bottom" "0px solid rgb(200, 200, 200)"
        , property "font-weight" "bold"
        ]


cellExposed : List (Attribute msg) -> List (Html msg) -> Html msg
cellExposed =
    styled button
        [ property "width" "24px"
        , property "height" "24px"
        , property "border-radius" "0"
        , property "background" "rgb(230, 230, 230)"
        , property "border-top" "2px solid rgb(128, 128, 128)"
        , property "border-left" "2px solid rgb(128, 128, 128)"
        , property "border-right" "0px solid rgb(200, 200, 200)"
        , property "border-bottom" "0px solid rgb(200, 200, 200)"
        , property "font-weight" "bold"
        ]


cellInactive : List (Attribute msg) -> List (Html msg) -> Html msg
cellInactive =
    styled button
        [ property "background" "rgb(198, 198, 198)"
        , property "width" "24px"
        , property "height" "24px"
        , property "border-radius" "0"
        , property "border-top" "3px solid rgb(250, 250, 250)"
        , property "border-left" "3px solid rgb(250, 250, 250)"
        , property "border-right" "3px solid rgb(128, 128, 128)"
        , property "border-bottom" "3px solid rgb(128, 128, 128)"
        , property "font-weight" "bold"
        ]


cellUnmarked : List (Attribute msg) -> List (Html msg) -> Html msg
cellUnmarked =
    styled button
        [ property "background" "rgb(198, 198, 198)"
        , property "width" "24px"
        , property "height" "24px"
        , property "border-radius" "0"
        , property "border-top" "3px solid rgb(250, 250, 250)"
        , property "border-left" "3px solid rgb(250, 250, 250)"
        , property "border-right" "3px solid rgb(128, 128, 128)"
        , property "border-bottom" "3px solid rgb(128, 128, 128)"
        , with ":active"
            [ property "background" "rgb(200, 200, 200)"
            , property "border-top" "2px solid rgb(128, 128, 128)"
            , property "border-left" "2px solid rgb(128, 128, 128)"
            , property "border-right" "0px solid rgb(200, 200, 200)"
            , property "border-bottom" "0px solid rgb(200, 200, 200)"
            ]
        ]


cellMarked : List (Attribute msg) -> List (Html msg) -> Html msg
cellMarked =
    styled button
        [ property "background" "rgb(198, 198, 198)"
        , property "width" "24px"
        , property "height" "24px"
        , property "border-radius" "0"
        , property "border-top" "3px solid rgb(250, 250, 250)"
        , property "border-left" "3px solid rgb(250, 250, 250)"
        , property "border-right" "3px solid rgb(128, 128, 128)"
        , property "border-bottom" "3px solid rgb(128, 128, 128)"
        , property "font-weight" "bold"
        ]


cellMarkedErr : List (Attribute msg) -> List (Html msg) -> Html msg
cellMarkedErr =
    styled button
        [ property "background" "rgb(255, 160, 160)"
        , property "width" "24px"
        , property "height" "24px"
        , property "border-radius" "0"
        , property "border-top" "3px solid rgb(250, 250, 250)"
        , property "border-left" "3px solid rgb(250, 250, 250)"
        , property "border-right" "3px solid rgb(128, 128, 128)"
        , property "border-bottom" "3px solid rgb(128, 128, 128)"
        , property "font-weight" "bold"
        ]


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> update (SetBoardAndMines ( 16, 16 ) 40) initialModel
        , view = view >> toHtml
        , update = update
        , subscriptions = \_ -> Sub.none
        }
