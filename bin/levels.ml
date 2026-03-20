type range = Range of int * int

type level = {
    title: string;
    grid: string array option;
    range: range option;
}

let make title grid range = { title; grid = Some grid; range = Some range }
let empty = { title = "?"; grid = None; range = None }

let level1 = make ("I. The Star")
[|
    "o o o o o o o o";
    "o o o o o x o o";
    "o o o o x x x o";
    "o o o o o x o o";
    "o o o o o o o o";
    "o o o o o o o o";
    "o o o o o o o o";
    "o o o o o o o o";
|] (Range (1, 4))

let level2 = make ("II. The Pillar")
[|
    "o o o o o o o o";
    "o o o o o o o o";
    "o o o o o o o o";
    "o o x x x o o o";
    "o o x x x o o o";
    "o o x x x o o o";
    "o o o o o o o o";
    "o o o o o o o o";
|] (Range (2, 8))

let level3 = make ("III. The Hearts")
[|
    "o o o o o o o o";
    "o o o o o o o o";
    "o o o o x o o o";
    "o o o o x x o o";
    "o o x x o o o o";
    "o o o x o o o o";
    "o o o o o o o o";
    "o o o o o o o o";
|] (Range (2, 5))

let level4 = make ("IV. The Scissors")
[|
    "o x o o o o o o";
    "x o x x x o o o";
    "o x o o o o o o";
    "o x o o o o x o";
    "o x o o o o x o";
    "o o o o o o x o";
    "o o o x x x o x";
    "o o o o o o x o";
|] (Range (4, 15))

let level8 = make ("The Hermit")
[|
    "o o o o o o o o";
    "o o o o o o o o";
    "o o o o x o o o";
    "o o o o x o o o";
    "o o x o x o o o";
    "o x x o o x o o";
    "o o x o o o o o";
    "o o o o o o o o";
|] (Range (3, 6))

let level5 = make ("V. The Loop")
[|
    "o x o o o o o o";
    "x o x x x o o o";
    "o x o o x o o o";
    "o x o o x x x o";
    "o x x x o o x o";
    "o o o x o o x o";
    "o o o x x x o x";
    "o o o o o o x o";
|] (Range (6, 12))

let level6 = make ("VI. The Space")
[|
    "o o x o o o o o";
    "o x o x o o o o";
    "x o o o x x x x";
    "o x o x o o x o";
    "o o x o x x o x";
    "o o x o x o x o";
    "o o x x o x x o";
    "o o x o x o o o";
|] (Range (6, 12))

let level7 = make ("VII. The Hanged Man")
[|
    "o o o o x o x x";
    "x x o x x x x x";
    "o x x x x o x x";
    "x x o o o o o x";
    "x x x o x o o x";
    "x x x x x x o x";
    "o o o o x o o x";
    "o o o o o o o x";
|] (Range (6, 6))

let level13 = make ("XIII. The Flowers")
[|
    "o x o o o o x o";
    "x o x o o x o x";
    "o x o o o o x o";
    "o o o o o o o o";
    "o o o o o o o o";
    "o x o o o o x o";
    "x o x o o x o x";
    "o x o o o o x o";
|] (Range (8, 12))

let level16 = make ("XVI. The Wheel of Fortune")
[|
	"x o x o o x o x";
    "o o x o o x o o";
    "x x x x x x x x";
    "o o x o o x o o";
    "o o x o o x o o";
    "x x x x x x x x";
    "o o x o o x o o";
    "x o x o o x o x";
|] (Range (12, 31))

let level17 = make ("XVII. The Cross")
[|
    "o o x o o x o o";
    "o o x o o x o o";
    "x x x o o x x x";
    "o o o o o o o o";
    "o o o o o o o o";
    "x x x o o x x x";
    "o o x o o x o o";
    "o o x o o x o o";
|] (Range (6, 12))

let all = [|
    level1; level2; level3; level4; level5; level6;
    level7; empty; empty; empty; empty; empty;
    level13; empty; empty; level16; level17; empty;
|]
