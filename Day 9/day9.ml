open Base

let extrapolate l =
  let rec next_step ~acc l =
    match l with
    |x :: y :: xs -> next_step ~acc:((x-y)::acc) (y::xs)
    |_ -> List.rev acc
  in
  let rec aux l =
    let next = next_step ~acc:[] l in
    if List.for_all ~f:(Int.equal 0) next then
      List.hd_exn l
    else 
      List.hd_exn l + (aux next)
    in
    aux (List.rev l)

let extrapolate_past l =
  let rec next_step ~acc l =
    match l with
    |x :: y :: xs -> next_step ~acc:((y-x)::acc) (y::xs)
    |_ -> List.rev acc
  in
  let rec aux l =
    let next = next_step ~acc:[] l in
    if List.for_all ~f:(Int.equal 0) next then
      List.hd_exn l
    else 
      List.hd_exn l - (aux next)
    in
    aux l

let parse_input_file filename =
  let open Stdio in
  In_channel.create filename
  |> In_channel.input_lines
  |> List.map ~f:(String.split ~on:' ')
  |> List.map ~f:(List.map ~f:Int.of_string)

let solve_part1 input =
  List.fold ~init:0 ~f:(fun acc list -> acc + extrapolate list) input

let solve_part2 input =
  List.fold ~init:0 ~f:(fun acc list -> acc + extrapolate_past list) input

let _ =
  let input = parse_input_file "/Users/bbataille/Developer/Advent of Code 2023/Day 9/input.txt" in
  let part1 = solve_part1 input in
  let part2 = solve_part2 input in
  Stdio.printf "Part1: %d\n" part1;
  Stdio.printf "Part2: %d\n" part2