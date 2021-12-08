import gleam/io
import gleam/bit_string
import gleam/string
import gleam/list
import gleam/map.{Map}
import gleam/int
import gleam/result
import gleam/erlang
import advent07/file

pub fn main() {
  let [num, file] = erlang.start_arguments()

  case num {
    "1" ->
      get_data(file)
      |> shortest_moves()
      |> io.debug()
    "2" ->
      get_data(file)
      |> shortest_moves_sum()
      |> io.debug()
  }
}

fn get_data(file: String) -> List(Int) {
  assert Ok(bit_content) = file.read_to_bitstring(file)
  assert Ok(content) = bit_string.to_string(bit_content)

  content
  |> string.trim_right()
  |> string.split(on: ",")
  |> list.map(fn(x: String) {
    assert Ok(value) = int.parse(x)
    value
  })
}

external fn min(l: List(Int)) -> Int =
  "lists" "min"

external fn max(l: List(Int)) -> Int =
  "lists" "max"

fn shortest_moves(crab_positions) {
  crab_positions
  |> list.unique()
  |> list.map(fn(position) {
    crab_positions
    |> list.map(fn(crab) { int.absolute_value(crab - position) })
    |> int.sum()
  })
  |> min()
}

fn shortest_moves_sum(crab_positions) {
  let min_val = min(crab_positions)
  let max_val = max(crab_positions)

  list.fold(
    over: list.range(min_val, max_val + 1),
    from: -1,
    with: fn(minor_cost, position) {
      let value =
        crab_positions
        |> list.map(fn(crab) { fuel(int.absolute_value(crab - position)) })
        |> int.sum()

      case minor_cost {
        -1 -> value
        _ -> int.min(value, minor_cost)
      }
    },
  )
}

fn fuel(x: Int) -> Int {
  x * { x + 1 } / 2
}
