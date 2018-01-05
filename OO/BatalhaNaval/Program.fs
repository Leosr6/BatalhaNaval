open System.Collections.Generic
open System

let randomizer = System.Random()

type Point(x : int, y : int) = 

    let posx = x
    let posy = y

    member this.getX() =
        posx

    member this.getY() =
        posy

    override a.Equals(bobj) =
        match bobj with
        | :? Point as b -> a.getX() = b.getX() && a.getY() = b.getY()
        | _ -> false

    member this.teste() =
        printf "%d, %d\n" posx posy

[<AbstractClass>]
type Object(cobject : char, position : List<Point>) =

    let Cobject = cobject
    let PositionList = position

    member this.hasPoint(pos : Point) = 
        PositionList.Contains(pos)

    member this.removePoint(pos : Point) =
        ignore (PositionList.Remove(pos))
        PositionList.Count = 0

    abstract member getName : unit -> string

    member this.teste() =
        PositionList.ForEach(fun elem ->
            elem.teste())

type Mine(position : List<Point>) = 
    inherit Object('m', position)

    override this.getName() = "mine"

type Submarine(position : List<Point>) =
    inherit Object('s', position)

    override this.getName() = "submarine"

type Ship(position : List<Point>) =
    inherit Object('n', position)

    override this.getName() = "ship"


type Board(objects : List<Object>) = 
    
    let Objects = objects

    let generateRandomPosition(length : int, direction : int) =
        let pointList = new List<Point>()
        let startPositionStable = randomizer.Next(0, 16 - length)
        let startPositionIncrementable = randomizer.Next(0, 16 - length)
        for i = 1 to length do
            let newPositionIncrementable = startPositionIncrementable + i - 1
            if direction = 0 then
                pointList.Add(new Point(newPositionIncrementable, startPositionStable))
            else
                pointList.Add(new Point(startPositionStable, newPositionIncrementable))
        pointList            

    let generateRandomDirection() = 
        randomizer.Next(0, 2)

    let doesPositionExists(position : Point) =
        Objects.Exists(fun (elem : Object) ->
            elem.hasPoint(position)
        )
    
    let doPositionsExist(positions : List<Point>) =
        positions.Exists(fun (elem : Point) ->
            doesPositionExists(elem)
        )
        
    member this.createMine() = 
        let mutable positionOnBoard = generateRandomPosition(1, generateRandomDirection())
        while doPositionsExist positionOnBoard do
            positionOnBoard <- generateRandomPosition(1, generateRandomDirection())
            
        Objects.Add(new Mine(positionOnBoard))

    member this.createSubmarine() = 
        let mutable positionOnBoard = generateRandomPosition(2, generateRandomDirection())
        while doPositionsExist positionOnBoard do
            positionOnBoard <- generateRandomPosition(2, generateRandomDirection())
            
        Objects.Add(new Submarine(positionOnBoard))

    member this.createShip() = 
        let mutable positionOnBoard = generateRandomPosition(3, generateRandomDirection())
        while doPositionsExist positionOnBoard do
            positionOnBoard <- generateRandomPosition(3, generateRandomDirection())
            
        Objects.Add(new Ship(positionOnBoard))

    member this.tryHit(position : Point) = 
        try 
            let object = Objects.Find(fun (elem : Object) ->
                elem.hasPoint(position))

            printf "You hit a %s\n" (object.getName())

            if object.removePoint(position) then
                ignore (Objects.Remove(object))
            
            Objects.Count = 0
        with
            | :? NullReferenceException -> 
                printf "Nothing was hit\n"
                false

    member this.teste() =
       Objects.ForEach(fun elem ->
            elem.teste())

    new() =
        Board(new List<Object>())

let rec readUserInput() =
    printf "Digite a coordenada X:"
    let resultX, coordX = System.Int32.TryParse (Console.ReadLine())
    if resultX then
        printf "Digite a coordenada Y:"
        let resultY, coordY = System.Int32.TryParse (Console.ReadLine())
        if resultY then
            if coordX >= 0 && coordX <= 14 && coordY >= 0 && coordY <= 14 then
                (coordX, coordY)
            else
                printf "Coordenadas devem estar entre 0 e 14\n"
                readUserInput()
        else
            (-1, -1)
    else
        (-1, -1)


let playGame(board : Board) = 
    let mutable coords = readUserInput()
    while not (coords = (-1, -1)) do
        if (board.tryHit(new Point(coords))) then
            printf "You won!"
            coords <- (-1, -1)
        else
            coords <- readUserInput()
        
        
let Board = new Board()

Board.createMine()
Board.createMine()
Board.createMine()
Board.createMine()
Board.createMine()

Board.createSubmarine()
Board.createSubmarine()
Board.createSubmarine()
Board.createSubmarine()

Board.createShip()
Board.createShip()
Board.createShip()

Board.teste()
playGame(Board)