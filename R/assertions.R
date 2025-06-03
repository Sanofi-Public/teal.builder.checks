assert_column_names <- function(x, name) {
  check_result <- checkmate::check_character(
    x,
    min.chars = 1L,
    any.missing = FALSE,
    min.len = 1L
  )

  if (isTRUE(check_result)) {
    invisible(x)
  } else {
    stop(
      str_glue("Argument '{name}' is invalid. {check_result}."),
      call. = FALSE
    )
  }
}

assert_column_name <- function(x, name) {
  check_result <- checkmate::check_character(
    x,
    min.chars = 1L,
    any.missing = FALSE,
    len = 1L
  )

  if (isTRUE(check_result)) {
    invisible(x)
  } else {
    stop(
      str_glue("Argument '{name}' is invalid. {check_result}."),
      call. = FALSE
    )
  }
}

assert_values <- function(x, name) {
  check_result <- checkmate::check_atomic(
    x,
    min.len = 1L,
    unique = TRUE
  )

  if (isTRUE(check_result)) {
    invisible(x)
  } else {
    stop(
      str_glue("Argument '{name}' is invalid. {check_result}."),
      call. = FALSE
    )
  }
}

assert_adam <- function(x, name) {
  check_result <- checkmate::check_data_frame(
    x,
    min.rows = 1L,
    min.cols = 1L
  )

  if (isTRUE(check_result)) {
    invisible(x)
  } else {
    stop(
      str_glue("Argument '{name}' is invalid. {check_result}."),
      call. = FALSE
    )
  }
}
