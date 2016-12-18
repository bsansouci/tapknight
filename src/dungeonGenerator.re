open Common;

let generate width::width height::height => {
  let dungeon = ref [];
  for i in 0 to (width - 1) {
    let row = ref [];
    for j in 0 to (height - 1) {
      row := [Random.float 1. < 0.5 ? Filled : Empty, ...!row]
    };
    dungeon := [Array.of_list !row, ...!dungeon]
  };
  Array.of_list !dungeon
};
