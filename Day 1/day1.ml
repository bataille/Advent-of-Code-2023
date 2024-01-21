open Base

let (let*) = fun m f -> Option.bind ~f m

type ('a, 'b) matcher = 
  | Not_matched 
  | Result of 'b 
  | Matcher of ('a -> ('a, 'b) matcher)

let digit_matcher = Matcher (function c -> 
  let ascii_digit_shift = Char.to_int '0' in
  if Char.is_digit c then 
    Result ((Char.to_int c) - ascii_digit_shift)
  else Not_matched)

let rec merge_matchers f g = 
  match (f,g) with 
  |(Result r, _) -> Result r 
  |(_, Result r) -> Result r 
  |(f, Not_matched) -> f 
  |(Not_matched, g) -> g 
  |(Matcher f1, Matcher g1) -> Matcher (fun c -> merge_matchers (f1 c) (g1 c))

let rec do_matching ~f ~is_empty ~shift ~extract x = 
  match f with 
  |Result r -> Some r 
  |_ when is_empty x -> None 
  |Not_matched -> do_matching ~f ~is_empty ~shift ~extract (shift x) 
  |Matcher g -> do_matching ~is_empty ~shift ~extract 
    ~f:(merge_matchers f (g @@ extract x)) (shift x)

let match_string ~f s = 
  do_matching ~is_empty:String.is_empty ~extract:(fun s -> s.[0]) 
  ~shift:(fun s -> String.drop_prefix s 1) ~f s

let match_rev_string ~f s = 
  do_matching ~is_empty:String.is_empty ~extract:(fun s -> s.[(String.length s)-1]) 
  ~shift:(fun s -> String.drop_suffix s 1) ~f s

let rec collection_to_matcher ~is_empty ~equal ~shift ~extract ~result c = 
  if is_empty c then 
    Result result 
  else Matcher (fun x -> 
    if equal x (extract c) then 
      collection_to_matcher ~is_empty ~equal ~shift ~extract ~result (shift c) 
    else Not_matched)

let string_to_matcher ~result s = 
  collection_to_matcher ~is_empty:String.is_empty ~equal:Char.equal 
  ~extract:(fun s -> s.[0]) 
  ~shift:(fun s -> String.drop_prefix s 1) 
  ~result s

let number_as_word_list = [
  ("one",1);
  ("two",2);
  ("three",3);
  ("four",4);
  ("five",5);
  ("six",6);
  ("seven",7);
  ("eight",8);
  ("nine",9)
  ]

let string_to_rev_matcher ~result s = 
  collection_to_matcher ~is_empty:String.is_empty ~equal:Char.equal 
  ~extract:(fun s -> s.[(String.length s)-1]) 
  ~shift:(fun s -> String.drop_suffix s 1) 
  ~result s

let digit_as_word_matcher = 
  List.fold_left ~init:(Not_matched) 
  ~f:(fun acc (s,r) -> merge_matchers acc (string_to_matcher ~result:r s)) 
  number_as_word_list

let digit_as_rev_word_matcher = 
  List.fold_left ~init:(Not_matched) 
  ~f:(fun acc (s,r) -> merge_matchers acc (string_to_rev_matcher ~result:r s)) 
  number_as_word_list

let input = 
  let open Stdio in
  In_channel.create "/Users/bbataille/Developer/Advent of Code 2023/Day 1/input.txt"
  |> In_channel.input_lines

let get_calibration_value_part1 s = 
  let* d1 = match_string ~f:digit_matcher s in
  let* d2 = match_rev_string ~f:digit_matcher s in
  Option.return (10*d1+d2)

let get_calibration_value_part2 s = 
  let part2_matcher = merge_matchers digit_as_word_matcher digit_matcher in
  let part2_rev_matcher = 
    merge_matchers digit_as_rev_word_matcher digit_matcher 
  in
  let* d1 = match_string ~f:part2_matcher s in
  let* d2 = match_rev_string ~f:part2_rev_matcher s in
  Option.return (10*d1+d2)

let result_part1 = 
  List.fold ~init:(Some 0) 
    ~f:(fun acc s -> 
      let* a = acc in 
      let* r = get_calibration_value_part1 s in 
      Option.return (a+r))
    input

let result_part2 = 
  List.fold ~init:(Some 0) 
    ~f:(fun acc s -> 
      let* a = acc in
      let* r = get_calibration_value_part2 s in
      Option.return (a+r)) 
    input

let _ =
  let* part1 = result_part1 in
  let* part2 = result_part2 in
  Option.return (
    Stdio.printf "Part1: %d\n" part1;
    Stdio.printf "Part2: %d\n" part2
  )
