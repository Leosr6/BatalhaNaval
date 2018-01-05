open System

// Funções para o tabuleiro

let randomizer = System.Random()

let rec insert_object (board : list<int * int * char>) (cobject : char) (start_pos : int * int) (length : int) (direction : int) =
    if length > 0 then
        let new_board = (fst start_pos, snd start_pos, cobject) :: board in
            let new_length = length - 1 in
                match direction with
                    | 0 -> let new_posx = fst start_pos + 1 in
                            insert_object new_board cobject (new_posx, snd start_pos) new_length direction
                    | 1 -> let new_posy = snd start_pos + 1 in
                            insert_object new_board cobject (fst start_pos, new_posy) new_length direction
    else
        board


let rec get_object (board : list<int * int * char>) (pos : int * int)  =
    if board.IsEmpty then
        '0'
    else
        let board_pos = board.Head in
            let posx, posy, cobject = board_pos in
                if fst pos = posx && snd pos = posy then
                    cobject
                else
                    get_object board.Tail pos


let rec is_empty (board : list<int * int * char>) (start_pos : int * int) (length : int) (direction : int) =
    if length > 0 then
        let cobject = get_object board start_pos in
            match cobject with
                | '0' -> let new_length = length - 1 in
                            match direction with
                                | 0 -> let new_posx = fst start_pos + 1 in
                                        is_empty board (new_posx, snd start_pos) new_length direction
                                | 1 -> let new_posy = snd start_pos + 1 in
                                        is_empty board (fst start_pos, new_posy) new_length direction
                | _ -> false
    else
        true

let rec create_object (board : list<int * int * char>) (cobject : char) (length : int) =
    let start_pos = (randomizer.Next(0, 16 - length), randomizer.Next(0, 16 - length)) in
        let direction = randomizer.Next(0, 2) in
            if is_empty board start_pos length direction then
                insert_object board cobject start_pos length direction
            else
                create_object board cobject length


// Funções para leitura de input

let rec read_coord_x() =

    printf "Digite a coordenada X:"
    
    let user_input = Console.ReadLine() in
        match System.Int32.TryParse user_input with
            | true, v -> let coordx = v in
                            if coordx >= 0 && coordx <= 14 then
                                coordx
                            else
                                read_coord_x ()
            | false, _ -> -1

let rec read_coord_y() =

    printf "Digite a coordenada Y:"
    
    let user_input = Console.ReadLine() in
        match System.Int32.TryParse user_input with
            | true, v -> let coordy = v in
                            if coordy >= 0 && coordy <= 14 then
                                coordy
                            else
                                read_coord_y ()
            | false, _ -> -1
            
let get_user_input() =
    
    let coordx = read_coord_x () in
        match coordx with
           | -1 -> (-1, -1)
           | _ -> let coordy = read_coord_y () in
                        match coordy with
                            | -1 -> (-1, -1)
                            | _ -> (coordx, coordy)


// Funções de controle do jogo

let print_object (cobject : char) =

    match cobject with
        | '0' -> printf "Nothing was hit\n"
        | 's' -> printf "You hit a submarine\n"
        | 'n' -> printf "You hit a ship\n"
        | 'm' -> printf "You hit a mine\n"

let rec remove_object (board : list<int * int * char>) (pos : int * int) =

    if board.IsEmpty then
        []
    else
        let board_pos = board.Head in
            let posx, posy, _ = board_pos in
                if fst pos = posx && snd pos = posy then
                    board.Tail
                else
                    board.Head :: remove_object board.Tail pos

let rec play_game (board : list<int * int * char>) =

    let coords = get_user_input() in
        match coords with
            | (-1, -1) -> printf "Game ended"
            | _ -> let cobject = get_object board coords in
                        let _ = print_object cobject in
                            match cobject with
                                | '0' -> play_game board
                                | _ ->  let new_board = remove_object board coords in                                        
                                            if new_board.IsEmpty then
                                                printf "You won!"
                                            else
                                                play_game new_board
                    

// Criação do tabuleiro
let submarino1 = create_object [] 's' 2
let submarino2 = create_object submarino1 's' 2
let submarino3 = create_object submarino2 's' 2
let submarino4 = create_object submarino3 's' 2

let navio1 = create_object submarino4 'n' 3
let navio2 = create_object navio1 'n' 3
let navio3 = create_object navio2 'n' 3

let mina1 = create_object navio3 'm' 1
let mina2 = create_object mina1 'm' 1
let mina3 = create_object mina2 'm' 1
let mina4 = create_object mina3 'm' 1
let mina5 = create_object mina4 'm' 1

let board = mina5

// Fim da criação do tabuleiro

printf "%A\n" board

play_game board