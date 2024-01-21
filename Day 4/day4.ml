open Base

let match_exactly_two l =
  match l with
  |[x;y] -> (x,y)
  |_ -> assert false

module Game = struct
  type t = {
    id: int;
    winning: (int, Int.comparator_witness) Set.t;
    scratched: int list
  }

  let create ?(id=0) ?(winning=Set.empty (module Int)) ?(scratched=[]) () =
    {
      id;
      winning;
      scratched
    } 

  let of_string s =
    let (game_id_part,game_part) = match_exactly_two @@ String.split ~on:':' s in
    let id = 
      String.split ~on:' ' game_id_part
      |> List.last_exn
      |> Int.of_string
    in
    let (winning_part,scratched_part) = match_exactly_two @@ String.split ~on:'|' game_part in
    let winning =
      String.split ~on:' ' winning_part
      |> List.filter ~f:(fun s -> not (String.equal "" s))
      |> List.map ~f:(Int.of_string)
      |> List.fold ~init:(Set.empty (module Int)) ~f:(fun acc i -> Set.add acc i)
    in
    let scratched =
      String.split ~on:' ' scratched_part
      |> List.filter ~f:(fun s -> not (String.equal "" s))
      |> List.map ~f:(Int.of_string)
    in
    create ~id ~winning ~scratched ()

    let matching g =
        List.fold ~init:0 ~f:(
          fun acc n -> if Set.mem g.winning n then acc+1 else acc) g.scratched

    let score g =
      let count = matching g in
      if count>0 then Int.pow 2 (count-1) else 0
end

let parse_input_file filename =
  let open Stdio in
  In_channel.create filename
  |> In_channel.input_lines
  |> List.map ~f:Game.of_string


let rec incr_count table ~max_id ~id ~count ~by =
  if count>0 then begin
    if (id+1) <= max_id then begin
      Hashtbl.incr ~by table (id+1);
      incr_count table ~max_id ~id:(id+1) ~by ~count:(count-1)
    end
  end


let build_count_table input_table =
  let count_table = Hashtbl.create (module Int) in
  Hashtbl.keys input_table
  |> List.iter ~f:(fun key -> Hashtbl.add_exn count_table ~key ~data:1);
  let max_id = 
    Hashtbl.keys input_table 
    |> List.fold ~init:0 ~f:(fun acc id -> Int.max acc id)
  in
  Hashtbl.keys input_table
  |> List.sort ~compare:(Int.compare)
  |> List.iter ~f:(fun id -> 
    let game = Hashtbl.find_exn input_table id in
    let by = Hashtbl.find_exn count_table id in
    incr_count count_table ~max_id ~by ~id ~count:(Game.matching game));
  count_table

let input = parse_input_file "/Users/bbataille/Developer/Advent of Code 2023/Day 4/input.txt"

let input_table = 
  List.map ~f:(fun game -> (game.id, game)) input
  |> Hashtbl.of_alist (module Int)
  |> fun result -> match result with |`Ok x -> x |`Duplicate_key _ -> assert false

let _ =
  let part1 = List.fold ~init:0 ~f:(fun acc g -> acc + Game.score g) input in
  let part2 = 
    build_count_table input_table
    |> Hashtbl.data
    |> List.fold ~init:0 ~f:(fun acc count -> acc+count)
  in
  Stdio.printf "Part1: %d\n" part1;
  Stdio.printf "Part2: %d\n" part2