import gleam/io
import gleam/bit_string
import gleam/string
import gleam/list
import gleam/map.{Map}
import gleam/int
import gleam/result
import gleam/erlang
import advent06/file

const default_days = 80

pub fn main() {
  // let input = get_data("demo_input.txt")
  let #(days, file) = case erlang.start_arguments() {
    [days, file] -> {
      let Ok(days) = int.parse(days)
      #(days, file)
    }

    [file] -> #(default_days, file)
  }

  let data = children(days)

  get_data(file)
  |> list.map(fn(i) {
    let Ok(value) = map.get(data, i)
    value
  })
  |> list.reduce(fn(acc, i) { acc + i })
  |> io.debug()
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

fn spawned(day: Int, acc: Int, mem) -> #(Int, Map(Int, Int)) {
  case day <= 0 {
    True -> #(acc + 1, mem)

    False -> {
      let #(acc, mem) = case map.get(mem, day - 2) {
        Error(Nil) -> {
          let #(child_acc, mem) = spawned(day - 9, 0, mem)
          let mem = map.insert(mem, day - 2, child_acc)
          #(acc + child_acc, mem)
        }
        Ok(child_acc) -> #(acc + child_acc, mem)
      }
      spawned(day - 7, acc, mem)
    }
  }
}

fn children(gen: Int) -> Map(Int, Int) {
  let #(_mem, values) =
    list.map_fold(
      over: list.range(1, 6),
      from: map.new(),
      with: fn(acc, i) {
        let #(value, acc) = spawned(gen - i, 0, acc)
        #(acc, #(i, value))
      },
    )

  map.from_list(values)
}
