open Base

module Subset = struct
  type t = {
    red: int;
    green: int;
    blue: int
  }

  let create ?(red=0) ?(green=0) ?(blue=0) () =
    {
      red;
      green;
      blue
    }

  let from_string s =
    String.split ~on:',' s 
    |> List.map ~f:(fun s -> String.drop_prefix s 1) 
    |> List.fold ~init:(create ()) 
      ~f:(fun acc s -> 
        match (String.split ~on:' ' s) with
        |[n; "blue"] -> { acc with blue = Int.of_string n }
        |[n; "green"] -> { acc with green = Int.of_string n }
        |[n; "red"] -> { acc with red = Int.of_string n }
        |_ -> acc
      )

    let check_under_or_equal ~red ~blue ~green subset =
      (subset.green <= green) && (subset.red <= red) && (subset.blue <= blue)

    let power {red; green; blue} = red*green*blue
end

module Game = struct
  type t = {
    id: int;
    subsets: Subset.t list
  }

  let create ?(id=0) ?(subsets=[]) () =
  {
    id;
    subsets
  }

  let from_string s =
    let [game_part; subsets_part] = String.split ~on:':' s in
    let [_;id_string] = String.split ~on:' ' game_part in
    let subsets = 
      String.split ~on:';' subsets_part
      |> List.map ~f:Subset.from_string
    in
    create ~id:(Int.of_string id_string) ~subsets ()

  let is_possible_with ~red ~green ~blue game =
    List.for_all ~f:(Subset.check_under_or_equal ~red ~blue ~green) game.subsets

  let power game =
    List.fold ~init:(Subset.create ()) 
      ~f:(fun acc s -> {
        red = Int.max acc.red s.red;
        green = Int.max acc.green s.green;
        blue = Int.max acc.blue s.blue
      }) game.subsets
    |> Subset.power
end

let parse_input_file filename =
  let open Stdio in
  In_channel.create filename
  |> In_channel.input_lines
  |> List.map ~f:Game.from_string

let input = parse_input_file "/Users/bbataille/Developer/Advent of Code 2023/Day 2/input.txt"

let part1 = 
  List.fold ~init:0 
    ~f:(fun acc game -> 
      if Game.is_possible_with ~red:12 ~green:13 ~blue:14 game 
      then acc + game.id else acc) input

let part2 =
  List.fold ~init:0 ~f:(fun acc g -> acc + Game.power g) input

let _ =
  let open Stdio in
  printf "Part1: %d\n" part1;
  printf "Part2: %d\n" part2
