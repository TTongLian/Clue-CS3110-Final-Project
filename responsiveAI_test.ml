open OUnit2
open Data
open Responsive

(* creating a default sheet for the players *)
let default_sheet full_deck =
  let f acc el = CardMap.add el {card_info=Unknown; note=No_Note} acc
  in  List.fold_left f CardMap.empty full_deck

let blue = Suspect "blue"
let green = Suspect "green"
let red = Suspect "red"
let yellow = Suspect "yellow"
let s_lst = [blue;green;red;yellow]

let gun = Weapon "gun"
let rope = Weapon "rope"
let sword = Weapon "sword"
let w_lst = [gun;rope;sword]

let room1 = Room "1"
let room2 = Room "2"
let room3 = Room "3"
let room4 = Room "4"
let r_lst = [room1;room2;room3;room4]

let loc1 = {info = Room_Rect("1",(1,1,1,1));
            edges = []}

let redp = {suspect="red";
               hand=[gun; room3];
               curr_loc=loc1;
               sheet=default_sheet (s_lst@w_lst@r_lst);
               agent=ResponsiveAI_t;
               is_out=false;
               listen= Array.make_matrix 11 4 Pure_unknown}

let bluep = {suspect="blue";
               hand=[sword; room2];
               curr_loc=loc1;
               sheet=default_sheet (s_lst@w_lst@r_lst);
               agent=ResponsiveAI_t;
               is_out=false;
               listen= Array.make_matrix 11 4 Pure_unknown}

let greenp = {suspect="green";
               hand=[red; yellow];
               curr_loc=loc1;
               sheet=default_sheet (s_lst@w_lst@r_lst);
               agent=ResponsiveAI_t;
               is_out=false;
               listen= Array.make_matrix 11 4 Pure_unknown}

let yellowp = {suspect="yellow";
               hand=[green; room4];
               curr_loc=loc1;
               sheet=default_sheet (s_lst@w_lst@r_lst);
               agent=ResponsiveAI_t;
               is_out=false;
               listen= Array.make_matrix 11 4 Pure_unknown}

let pub1 = {
    curr_player = "red";
    acc_room = "acc";
    board = {
      dim = (-1,-1);
      loc_map = CoordMap.empty;
      room_coords = StringMap.empty;
    };
    deck = (s_lst,w_lst,r_lst);
    player_order = ["red";"blue";"green";"yellow"];
  }

let pub2 = {
    curr_player = "blue";
    acc_room = "acc";
    board = {
      dim = (-1,-1);
      loc_map = CoordMap.empty;
      room_coords = StringMap.empty;
    };
    deck = (s_lst,w_lst,r_lst);
    player_order = ["red";"blue";"green";"yellow"];
  }

let pub3 = {
    curr_player = "green";
    acc_room = "acc";
    board = {
      dim = (-1,-1);
      loc_map = CoordMap.empty;
      room_coords = StringMap.empty;
    };
    deck = (s_lst,w_lst,r_lst);
    player_order = ["red";"blue";"green";"yellow"];
  }

let pub4 = {
    curr_player = "green";
    acc_room = "acc";
    board = {
      dim = (-1,-1);
      loc_map = CoordMap.empty;
      room_coords = StringMap.empty;
    };
    deck = (s_lst,w_lst,r_lst);
    player_order = ["red";"blue";"green";"yellow"];
  }

let m =   [|[|Not_in_hand; Pure_unknown; Pure_unknown; Pure_unknown|];
          [|Not_in_hand; Pure_unknown; Pure_unknown; Pure_unknown|];
          [|Not_in_hand; Pure_unknown; Pure_unknown; Pure_unknown|];
          [|Not_in_hand; Pure_unknown; Pure_unknown; Pure_unknown|];
          [|Known; Not_in_hand; Not_in_hand; Not_in_hand|];
          [|Not_in_hand; Pure_unknown; Pure_unknown; Pure_unknown|];
          [|Not_in_hand; Pure_unknown; Pure_unknown; Pure_unknown|];
          [|Not_in_hand; Pure_unknown; Pure_unknown; Pure_unknown|];
          [|Not_in_hand; Pure_unknown; Pure_unknown; Pure_unknown|];
          [|Known; Not_in_hand; Not_in_hand; Not_in_hand|];
          [|Not_in_hand; Pure_unknown; Pure_unknown; Pure_unknown|]|]

let responsive_tests =
[
  "first_take_note" >:: (fun _ -> assert_equal 0 (first_take_note redp pub1));
]