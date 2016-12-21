open Common;

let generateFilledRect width::width height::height => Array.make_matrix width height Filled;

let collidesWithAnything dungeon::dungeon x::x y::y room::room => {
  let height = Array.length dungeon;
  let width = Array.length dungeon.(0);
  /* Dear lord this is terrible. TODO: write meaningful comment here. */
  let (_, ret) =
    Array.fold_right
      (
        fun row (prevJ, prevVal) => {
          let (_, ret) =
            Array.fold_right
              (
                fun cell (prevI, prevVal2) => (
                  prevI + 1,
                  prevVal2 ||
                  x + prevI + 1 >= width ||
                  y + prevJ + 1 >= height || dungeon.(prevJ + y).(prevI + x) === Filled
                )
              )
              row
              ((-1), false);
          (prevJ + 1, prevVal || ret)
        }
      )
      room
      ((-1), false);
  ret
};

let generate width::width height::height => {
  let dungeon = Array.make_matrix width height Empty;
  let numOfRooms = 50;
  let maxVisibility = 6;
  let minRoomSize = 2;
  for roomIndex in 0 to numOfRooms {
    /* Retry with smaller width/height if room can't fit */
    let room =
      generateFilledRect
        width::(minRoomSize + Random.int (maxVisibility * 2)) height::(minRoomSize + Random.int (maxVisibility * 2));
    let x = Random.int width;
    let y = Random.int height;
    if (not (collidesWithAnything dungeon::dungeon x::x y::y room::room)) {
      print_endline @@
      "generating random room at (" ^
      string_of_int x ^
      ", " ^
      string_of_int y ^
      ") of width: " ^
      string_of_int (Array.length room.(0)) ^ ", height: " ^ string_of_int (Array.length room);
      for j in 0 to (Array.length room - 1) {
        for i in 0 to (Array.length room.(0) - 1) {
          dungeon.(x + j).(y + i) = Filled
        }
      }
    }
  };
  dungeon
};
