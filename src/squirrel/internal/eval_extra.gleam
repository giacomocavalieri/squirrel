//// This package has some additional helpers to work with the `Eval` package.
////

import eval.{type Eval}
import gleam/list

pub fn try_map(
  list: List(a),
  fun: fn(a) -> Eval(b, _, _),
) -> Eval(List(b), _, _) {
  try_fold(list, [], fn(acc, item) {
    use mapped_item <- eval.try(fun(item))
    eval.return([mapped_item, ..acc])
  })
  |> eval.map(list.reverse)
}

/// Runs a list of `Eval` actions in sequence sharing the same context.
///
pub fn run_all(list: List(Eval(a, b, c)), context: c) -> List(Result(a, b)) {
  let acc = #([], context)
  let #(results, _) = {
    use #(results, context), script <- list.fold(list, acc)
    let #(context, result) = eval.step(script, context)
    #([result, ..results], context)
  }

  results
}

pub fn try_index_map(
  list: List(a),
  fun: fn(a, Int) -> Eval(b, _, _),
) -> Eval(List(b), _, _) {
  try_index_fold(list, [], fn(acc, item, i) {
    use mapped_item <- eval.try(fun(item, i))
    eval.return([mapped_item, ..acc])
  })
  |> eval.map(list.reverse)
}

pub fn try_fold(
  over list: List(a),
  from acc: b,
  with fun: fn(b, a) -> Eval(b, _, _),
) -> Eval(b, _, _) {
  case list {
    [] -> eval.return(acc)
    [first, ..rest] -> {
      use acc <- eval.try(fun(acc, first))
      try_fold(rest, acc, fun)
    }
  }
}

fn try_index_fold(
  over list: List(a),
  from acc: b,
  with fun: fn(b, a, Int) -> Eval(b, _, _),
) -> Eval(b, _, _) {
  do_try_index_fold(0, over: list, from: acc, with: fun)
}

fn do_try_index_fold(
  index: Int,
  over list: List(a),
  from acc: b,
  with fun: fn(b, a, Int) -> Eval(b, _, _),
) -> Eval(b, _, _) {
  case list {
    [] -> eval.return(acc)
    [first, ..rest] -> {
      use acc <- eval.try(fun(acc, first, index))
      do_try_index_fold(index + 1, rest, acc, fun)
    }
  }
}
