open Base

type cell = {
  content: char;
  is_part_number: bool;
  is_symbol: bool;
  is_gear: bool;
  value: int;
  number_id: int
}

let create_cell content =
  {
    content;
    is_part_number = false;
    is_symbol = false;
    is_gear = false;
    value = 0;
    number_id = (-1);
}

let iteri_around matrix x y ~f =
  let max_row = (Array.length matrix) - 1 in
  if max_row >= 0 then begin
    let max_column = (Array.length matrix.(0) - 1) in 
    if max_column >= 0 then begin

      let prev_row = y-1 in
      let prev_column = x-1 in
      let next_row = y+1 in
      let next_column = x+1 in

      (* Previous row *)
      if prev_row >= 0 then begin
        if prev_column >= 0 then f matrix prev_column prev_row;
        f matrix x prev_row;
        if next_column <= max_column then f matrix next_column prev_row
      end;

      (* Current row *)
      if prev_column >= 0 then f matrix prev_column y;
      if next_column <= max_column then f matrix next_column y;
      
      (* Next row *)
      if next_row <= max_row then begin
        if prev_column >= 0 then f matrix prev_column next_row;
        f matrix x next_row;
        if next_column <= max_column then f matrix next_column next_row
      end
    end
  end

let foldi_around matrix x y ~init ~f =
  let result = ref init in
  let max_row = (Array.length matrix) - 1 in
  if max_row >= 0 then begin
    let max_column = (Array.length matrix.(0) - 1) in 
    if max_column >= 0 then begin

      let prev_row = y-1 in
      let prev_column = x-1 in
      let next_row = y+1 in
      let next_column = x+1 in

      (* Previous row *)
      if prev_row >= 0 then begin
        if prev_column >= 0 then result := f !result matrix prev_column prev_row;
        result := f !result matrix x prev_row;
        if next_column <= max_column then result := f !result matrix next_column prev_row
      end;

      (* Current row *)
      if prev_column >= 0 then result := f !result matrix prev_column y;
      if next_column <= max_column then result := f !result matrix next_column y;
      
      (* Next row *)
      if next_row <= max_row then begin
        if prev_column >= 0 then result := f !result matrix prev_column next_row;
        result := f !result matrix x next_row;
        if next_column <= max_column then result := f !result matrix next_column next_row
      end
    end
  end;
  !result

let rec iteri_left_while row x ~check ~f =
  if x >= 0 then begin
    if check row x then begin
      f row x;
      if (x-1)>=0 then iteri_left_while row (x-1) ~check ~f
    end
  end

let color_cell matrix x y =
  if Char.is_digit matrix.(y).(x).content then
    matrix.(y).(x) <- { matrix.(y).(x) with is_part_number = true }

let color_around_symbol matrix x y =
  iteri_around matrix x y ~f:color_cell

let check_cell_for_symbols matrix x y =
  match matrix.(y).(x).content with
  |'.' -> ()
  |c when Char.is_digit c -> ()
  |_ -> begin 
    matrix.(y).(x) <- { matrix.(y).(x) with is_symbol = true };
    color_around_symbol matrix x y
  end

let propagate_part_left row x =
  iteri_left_while row x ~check:(fun row x -> Char.is_digit row.(x).content)
    ~f:(fun row x -> row.(x) <- { row.(x) with is_part_number = true })

let parse_matrix_for_symbols matrix =
  Array.iteri ~f:(fun y row -> 
    Array.iteri ~f:(fun x _ -> check_cell_for_symbols matrix x y) row) matrix

let propagate_color_in_rows matrix =
  if Array.length matrix > 0 then begin
    let max_column = (Array.length matrix.(0) - 1) in 
    let process_cell row x cell =
      (* Check for non is_part_number digit *)
      if not cell.is_part_number then begin
        if Char.is_digit cell.content then begin
          (* Look right *)
          let next_column = x+1 in
          if next_column <= max_column then begin
            if row.(next_column).is_part_number then propagate_part_left row x
          end;
          (* Look left *)
          let prev_column = x-1 in
          if prev_column >= 0 then begin
            if row.(prev_column).is_part_number then 
              row.(x) <- { row.(x) with is_part_number = true }
          end
        end
      end
    in
    if max_column >= 0 then
      Array.iteri ~f:(fun _ row -> Array.iteri ~f:(process_cell row) row) matrix
  end

let sum_part_numbers matrix =
  let ascii_char_to_int c = (Char.to_int c) - (Char.to_int '0') in
  let sum_digit_in_row row =
    Array.fold ~init:(0,0) ~f:(fun (total, current) cell ->
      if cell.is_part_number then (total, 10*current + ascii_char_to_int cell.content)
      else (total+current, 0)) row
    |> fun (total, current) -> total + current
  in Array.fold ~init:0 ~f:(fun acc row -> acc + sum_digit_in_row row) matrix

(* Part 2 parsing garbage. Why is this year AoC so uninteresting... *)

let set_value_and_id_in_left_cells value id row x =
  iteri_left_while row x ~check:(fun row x -> Char.is_digit row.(x).content)  
    ~f:(fun row x -> row.(x) <- { row.(x) with value=value; number_id=id })

let set_digit_value_and_id matrix table =
  let id = ref 0 in
  let ascii_char_to_int c = (Char.to_int c) - (Char.to_int '0') in
  let max_column = (Array.length matrix.(0) - 1) in
  let get_and_set_value_and_id_in_row row =
    Array.foldi ~init:0 ~f:(fun x current cell ->
      if cell.is_part_number then
        (10*current + ascii_char_to_int cell.content)
      else begin
        set_value_and_id_in_left_cells current !id row (x-1);
        Hashtbl.add_exn table ~key:!id ~data:current; 
        Int.incr id;
        0
      end) row
    |> fun value -> if value <> 0 then begin
      set_value_and_id_in_left_cells value !id row max_column;
      Hashtbl.add_exn table ~key:!id ~data:value; 
      Int.incr id;
    end
  in Array.iter ~f:(fun row -> get_and_set_value_and_id_in_row row) matrix

let check_cell_for_gear table matrix x y =
  if Char.equal matrix.(y).(x).content '*' then begin
    let numbers_set = foldi_around matrix x y ~init:(Set.empty (module Int))
      ~f:(fun set matrix x y -> 
        if matrix.(y).(x).is_part_number then 
          Set.add set matrix.(y).(x).number_id
        else set)
    in
    if Set.length numbers_set >= 2 then begin
      matrix.(y).(x) <- { matrix.(y).(x) with
        is_gear = true;
        value = Set.fold numbers_set ~init:1 ~f:(fun acc id -> acc * (Hashtbl.find_exn table id)) }
    end
  end

let parse_matrix_for_gears matrix table =
  Array.iteri ~f:(fun y row -> 
    Array.iteri ~f:(fun x _ -> check_cell_for_gear table matrix x y) row) matrix

let sum_gears matrix =
  Array.fold ~init:0 ~f:(fun acc row ->
    acc + Array.fold ~init:0 ~f:(fun acc cell ->
      if cell.is_gear then acc+cell.value else acc) row) 
  matrix
  
(* Input and answers *)

let input =
  let open Stdio in
  In_channel.create "/Users/bbataille/Developer/Advent of Code 2023/Day 3/input.txt"
  |> In_channel.input_lines
  |> List.map ~f:String.to_array
  |> Array.of_list
  |> Array.map ~f:(Array.map ~f:create_cell)

let print_cell cell =
  if cell.is_part_number then
    Stdio.printf "\027[1;32m%c\027[m" cell.content
  else if cell.is_gear then
    Stdio.printf "\027[1;31m%c\027[m" cell.content
  else if cell.is_symbol then
    Stdio.printf "\027[1;34m%c\027[m" cell.content
  else
    Stdio.printf "%c" cell.content

let print_matrix matrix =
  Array.iter ~f:(fun row -> Array.iter ~f:print_cell row; Stdio.printf "\n") matrix

let _ =
  let number_table = Hashtbl.create (module Int) in
  parse_matrix_for_symbols input;
  propagate_color_in_rows input;
  set_digit_value_and_id input number_table;
  parse_matrix_for_gears input number_table; 
  print_matrix input; 
  Stdio.printf "Part1: %d\n" @@ sum_part_numbers input;
  Stdio.printf "Part2: %d\n" @@ sum_gears input
