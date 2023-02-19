type Game = 
    { 
        KnownFaces: int list
        Score: int
        N: int
        Position: (int * int)
    }

module Game = 
    let init() = {
        KnownFaces = []
        Score = 0
        N = 1
        Position = (0,0)
    }

type Square =   
    { 
        X: int 
        Y: int
        Value: int
    }

module Square = 
    let create x y value = 
        {
            X = x
            Y = y
            Value = value
        }
    
let board = [
    Square.create 0 0 0
    Square.create 0 1 5
    Square.create 0 2 -7
    Square.create 0 3 186
    Square.create 0 4 81
    Square.create 0 5 57

    Square.create 1 0 77
    Square.create 1 1 23
    Square.create 1 2 2
    Square.create 1 3 42
    Square.create 1 4 123
    Square.create 1 5 33

    Square.create 2 0 32
    Square.create 2 1 -4
    Square.create 2 2 357
    Square.create 2 3 195
    Square.create 2 4 240
    Square.create 2 5 132

    Square.create 3 0 403
    Square.create 3 1 592
    Square.create 3 2 452
    Square.create 3 3 704
    Square.create 3 4 443
    Square.create 3 5 268

    Square.create 4 0 337
    Square.create 4 1 445
    Square.create 4 2 317
    Square.create 4 3 452
    Square.create 4 4 353
    Square.create 4 5 492

    Square.create 5 0 452
    Square.create 5 1 620
    Square.create 5 2 395
    Square.create 5 3 228
    Square.create 5 4 508
    Square.create 5 5 732
]

let initialGame = Game.init()

let getAdjacentMoves position = 
    //match game.Position with 
    //| (0, 0) -> [ (0, 1), (1, 0) ]
    //| (0, 5) -> [ (0, 4), (1, 5) ]
    //| (0, x) -> [ (0, x-1), (1, )]
    let (x, y) = position

    [ (x, y+1); (x, y-1); (x-1, y); (x+1, y)] |> List.filter (fun (x, y) -> x >= 0 && x <6 && y>=0 && y< 6)

let adjacentMoves = getAdjacentMoves (0, 4)

let canMove game (move: (int * int)) = 
    let (x, y) = move
    let toSquare = board |> List.find (fun s -> s.X = x && s.Y = y)
    let scoreDifferenceRequired = toSquare.Value - game.Score

    // Check difference is a factor of N
    match scoreDifferenceRequired % game.N with 
    | x when x <> 0 -> false
    | 0 -> 
        // Can we use an existing face?
        match game.KnownFaces |> List.where (fun face -> face * game.N = scoreDifferenceRequired) with 
        | validFaceChoices when validFaceChoices.Length > 0 -> true
        | [] -> 
            // Do we have any die faces left we can allocate? 
            match game.KnownFaces.Length with 
            | 6 -> false
            | _ -> true
        
let x = canMove (Game.init()) (0, 1)

printfn "%A" x