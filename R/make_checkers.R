#' Create a checker function
#'
#' Construct a function that performs a specific check on an ADaM dataset.
#'
#' @param fn A function that accepts an ADaM dataset and returns a named list
#' with two elements: 'check' - a logical value that indicates if the ADaM
#' dataset passed the check, and 'msg' - a character string that provides a
#' description of the failure if the check did not pass.
#'
#' @template checker
#'
#' @seealso `make_multi_checker` for combining checker functions into one.
#'
#' @export
make_checker <- function(fn) {
  \(adam) {
    assert_adam(adam, "adam")

    check_msg_list <-
      fn(adam)

    check <-
      pluck(check_msg_list, "check")

    msg <-
      if (isTRUE(check)) {
        NULL
      } else {
        pluck(check_msg_list, "msg")
      }

    check |>
      magrittr::set_attr("msg", msg) |>
      magrittr::set_attr("data", adam)
  }
}

#' Create a function for multiple checks
#'
#' Combine one or more checker functions into one.
#'
#' @details
#' Checks ran by functions passed to `...` are performed sequentially and check results are aggregated.
#'
#' @param ... One or more checker functions created by `make_checker`.
#'
#' @template checker
#'
#' @seealso `make_checker` for creating individual checker functions.
#'
#' @export
make_multi_checker <- function(...) {
  \(adam) {
    check_fns <-
      list(...)

    reduce(
      check_fns[-1L],
      check_and_then,
      .init = check_fns[[1L]](adam)
    )
  }
}

check_and_then <- function(check, fn) {
  new_check <-
    fn(attr(check, "data"))

  new_msg <-
    c(
      attr(check, "msg"),
      attr(new_check, "msg")
    )

  new_data <-
    attr(new_check, "data")

  magrittr::and(check, new_check) |>
    magrittr::set_attr("msg", new_msg) |>
    magrittr::set_attr("data", new_data)
}

