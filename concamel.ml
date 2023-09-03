open Base

type grid =
  { w : int
  ; h : int
  ; cells : bool array
  }

let random_bool_seq = Sequence.unfold ~init:() ~f:(fun () -> Some (Random.bool (), ()))

let empty_grid w h =
  let s = Sequence.repeat false in
  let cells = Sequence.take s (w * h) |> Sequence.to_array in
  { w; h; cells }
;;

let random_grid w h =
  let cells = Sequence.take random_bool_seq (w * h) |> Sequence.to_array in
  { w; h; cells }
;;

let get_index x y grid = (grid.w * y) + x

let get_cell x y grid =
  let index = get_index x y grid in
  grid.cells.(index)
;;

let set_cell v x y grid =
  let index = get_index x y grid in
  grid.cells.(index) <- v;
  ()
;;

let count_living_neihbours x y grid =
  let c = ref 0 in
  for i = x - 1 to x + 1 do
    for j = y - 1 to y + 1 do
      if (phys_equal i x && phys_equal y j)
         || (i < 0 || j < 0)
         || i > grid.w - 1
         || j > grid.h - 1
      then ()
      else (
        let cell = get_cell i j grid in
        if cell then c := !c + 1 else ())
    done
  done;
  !c
;;

let show_bool = function
  | false -> "\u{25A1}"
  | true -> "\u{25A0}"
;;

let show_grid grid =
  for y = 0 to grid.h - 1 do
    for x = 0 to grid.w - 1 do
      let cell = get_cell x y grid in
      cell |> show_bool |> Stdio.printf "%s"
    done;
    Stdio.print_endline ""
  done;
  ()
;;

(** Birth: a cell that is dead at time t will be alive at time t + 1 if exactly 3 of its eight neighbors were alive at time t.

    Death: a cell can die by:
    -> Overcrowding: if a cell is alive at time t + 1 and 4 or more of its neighbors are also alive at time t, the cell will be dead at time t + 1.

    -> Exposure: If a live cell at time t has only 1 live neighbor or no live neighbors, it will be dead at time t + 1.

    Survival: a cell survives from time t to time t + 1 if and only if 2 or 3 of its neighbors are alive at time t. *)
let next grid =
  let next_grid = empty_grid grid.w grid.h in
  for y = 0 to grid.h - 1 do
    for x = 0 to grid.w - 1 do
      let alive = get_cell x y grid in
      let living_neigbours = count_living_neihbours x y grid in
      match alive, living_neigbours with
      (* Birth *)
      | false, 3 -> set_cell true x y next_grid
      (* Stay dead *)
      | false, _ -> ()
      (* Death Exposure *)
      | true, 0 -> set_cell false x y next_grid
      | true, 1 -> set_cell false x y next_grid
      (* Survival *)
      | true, 2 -> set_cell true x y next_grid
      | true, 3 -> set_cell true x y next_grid
      (* Death Overcowding *)
      | true, _ -> set_cell false x y next_grid
    done
  done;
  next_grid
;;

let clear_lines n =
  let open Stdio in
  for _ = 1 to n do
    (* Move the cursor up n lines using ANSI escape code *)
    printf "\x1b[%dA" 1;
    (* Clear the line using ANSI escape code *)
    printf "\x1b[K"
  done
;;

let () =
  let grid = ref (random_grid 65 30) in
  while true do
    show_grid !grid;
    Unix.sleepf 0.1;
    grid := next !grid;
    clear_lines !grid.h;
  done;
  ()
;;
