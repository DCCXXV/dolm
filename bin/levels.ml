type range = Range of int * int

type level = {
  grid: string array option;
  par:  range option;
}

let make grid par = { grid = Some grid; par }
let empty = { grid = None; par = None }

let level1 = make [|
    "o o o o o o o o";
    "o o o o o o o o";
    "o o o o o o o o";
    "o o o x o o o o";
    "o o x x x o o o";
    "o o o x o o o o";
    "o o o o o o o o";
    "o o o o o o o o";
|] (Some (Range (6, 6)))

let level2 = make [|
    "o o o o o o o o";
    "o o o o o o o o";
    "o o o o o o o o";
    "o o x x x o o o";
    "o o x x x o o o";
    "o o x x x o o o";
    "o o o o o o o o";
    "o o o o o o o o";
|] (Some (Range (6, 6)))

let level3 = make [|
    "o o o o o o o o";
    "o o o o o o o o";
    "o o o o x o o o";
    "o o o o x x o o";
    "o o x x o o o o";
    "o o o x o o o o";
    "o o o o o o o o";
    "o o o o o o o o";
|] (Some (Range (6, 6)))

let level12 = make [|
    "o o o o x o x x";
    "x x o x x x x x";
    "o x x x x o x x";
    "x x o o o o o x";
    "x x x o x o o x";
    "x x x x x x o x";
    "o o o o x o o x";
    "o o o o o o o x";
|] (Some (Range (6, 6)))

let level4 = make [|
    "o o o o o o o o";
    "o o o o o o o o";
    "o o o o x o o o";
    "o o o o x o o o";
    "o o x o x o o o";
    "o x x o o x o o";
    "o o x o o o o o";
    "o o o o o o o o";
|] (Some (Range (3, 6)))

let level15 = make [|
    "o o x o o x o o";
    "o o x o o x o o";
    "x x x o o x x x";
    "o o o o o o o o";
    "o o o o o o o o";
    "x x x o o x x x";
    "o o x o o x o o";
    "o o x o o x o o";
|] (Some (Range (6, 12)))

let all = [|
  level1; level2; level3; level4; empty; empty;
  empty; empty; empty; empty; empty; empty;
  empty; empty; empty; empty; level15; empty;
|]
