import gleam/io
import gleam/bit_string
import gleam/string
import gleam/list
import gleam/map.{Map}
import gleam/int
import gleam/result
import gleam/erlang
import gleam/order.{Order}
import advent09/file

const max_cell_number = 9

const start_idx = 0

pub fn main() {
  let [num, file] = erlang.start_arguments()

  case num {
    "1" -> {
      let matrix = get_data(file)
      let #(low_n, _pos) = solve1(matrix)
      io.debug(int.sum(low_n) + list.length(low_n))
    }
    "2" -> {
      let matrix = get_data(file)
      let #(_low_n, pos) = solve1(matrix)
      let [a, b, c, .._] = solve2(matrix, pos)
      io.debug(a * b * c)
    }
  }
}

fn get_data(file: String) -> List(List(Int)) {
  assert Ok(bit_content) = file.read_to_bitstring(file)
  assert Ok(content) = bit_string.to_string(bit_content)

  content
  |> string.split(on: "\n")
  |> list.filter(fn(x: String) { x != "" })
  |> list.map(fn(row: String) {
    row
    |> string.to_graphemes()
    |> list.map(fn(cell: String) -> Int {
      assert Ok(value) = int.parse(cell)
      value
    })
  })
}

fn solve1(data) {
  do_solve1(data, start_idx, start_idx, [], [])
}

fn do_solve1(data, i, j, acc, pos) {
  let [row, .._] = data
  case #(list.length(data) < i, list.length(row) < j) {
    #(True, _) -> #(acc, pos)
    #(False, True) -> do_solve1(data, i + 1, start_idx, acc, pos)
    #(False, False) -> {
      let n = cell(i, j, data)
      let adjacent = [
        cell(i + 1, j, data),
        cell(i - 1, j, data),
        cell(i, j + 1, data),
        cell(i, j - 1, data),
      ]
      case list.all(adjacent, fn(m) { m > n }) {
        True -> do_solve1(data, i, j + 1, [n, ..acc], [#(i, j), ..pos])
        False -> do_solve1(data, i, j + 1, acc, pos)
      }
    }
  }
}

fn cell(i, j, data) {
  let [row, .._] = data
  case #(i < 0, j < 0, list.length(data) <= i, list.length(row) <= j) {
    #(False, False, False, False) -> {
      let Ok(row) = list.at(data, get: i)
      let Ok(cell) = list.at(row, get: j)
      cell
    }
    _ -> max_cell_number
  }
}

fn solve2(data, pos) {
  do_solve2(data, pos, [])
}

fn do_solve2(
  data: List(List(Int)),
  pos: List(#(Int, Int)),
  acc: List(Int),
) -> List(Int) {
  case pos {
    [] -> list.sort(acc, desc)
    [pos, ..rest_pos] -> {
      let len_basin = list.length(solve2_pos(data, pos, []))
      do_solve2(data, rest_pos, [len_basin, ..acc])
    }
  }
}

fn solve2_pos(
  data: List(List(Int)),
  ini_pos: #(Int, Int),
  acc: List(#(Int, Int)),
) -> List(#(Int, Int)) {
  let #(i, j) = ini_pos
  let n = cell(i, j, data)
  case #(n == max_cell_number, list.contains(acc, any: #(i, j))) {
    #(False, False) -> {
      let adjacent = [#(i - 1, j), #(i + 1, j), #(i, j - 1), #(i, j + 1)]
      list.fold(
        over: adjacent,
        from: [#(i, j), ..acc],
        with: fn(a, pos) { solve2_pos(data, pos, a) },
      )
    }
    _ -> acc
  }
}

fn desc(a: Int, b: Int) -> Order {
  case a == b {
    True -> order.Eq
    False ->
      case a > b {
        True -> order.Lt
        False -> order.Gt
      }
  }
}
