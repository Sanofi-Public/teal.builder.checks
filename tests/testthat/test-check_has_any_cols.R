test_that("fails with invalid 'column_names'", {
  check_has_any_cols(column_names = "") |>
    expect_error_message(
      "Argument 'column_names' is invalid. All elements must have at least 1",
      " characters, but element 1 has 0 characters"
    )

  check_has_any_cols(column_names = character()) |>
    expect_error_message(
      "Argument 'column_names' is invalid. Must have length >= 1, but has",
      " length 0."
    )

  check_has_any_cols(column_names = list("col_a", "col_b")) |>
    expect_error_message(
      "Argument 'column_names' is invalid. Must be of type 'character', not",
      " 'list'."
    )

  check_has_any_cols(column_names = c("col_a", NA)) |>
    expect_error_message(
      "Argument 'column_names' is invalid. Contains missing values (element 2)."
    )

  check_has_any_cols(column_names = 1) |>
    expect_error_message(
      "Argument 'column_names' is invalid. Must be of type 'character', not",
      " 'double'."
    )

  check_has_any_cols(column_names = TRUE) |>
    expect_error_message(
      "Argument 'column_names' is invalid. Must be of type 'character', not",
      " 'logical'."
    )
})

test_that("returns a function with the parameter 'adam'", {
  check_has_any_cols(column_names = c("col_a", "col_b")) |>
    checkmate::expect_function(args = "adam")
})

test_that("fails if input isn't a dataframe or tibble", {
  check_has_any_cols(column_names = "col_a")(1) |>
    expect_error_message(
      "Argument 'adam' is invalid. Must be of type 'data.frame', not 'double'."
    )

  check_has_any_cols(column_names = "col_a")(TRUE) |>
    expect_error_message(
      "Argument 'adam' is invalid. Must be of type 'data.frame', not 'logical'."
    )

  check_has_any_cols(column_names = "col_a")("a") |>
    expect_error_message(stringr::str_c(
      "Argument 'adam' is invalid.",
      " Must be of type 'data.frame', not 'character'.")
    )

  check_has_any_cols(column_names = "col_a")(list(USUBJID = 1)) |>
    expect_error_message(
      "Argument 'adam' is invalid. Must be of type 'data.frame', not 'list'."
    )
})

test_that("returns 'FALSE' if a tibble doesn't have specified columns", {
  dplyr::tibble(col_a = 1) |>
    check_has_any_cols(column_names = "col_b")() |>
    expect_false()

  dplyr::tibble(col_a = 1, col_b = 2) |>
    check_has_any_cols(column_names = c("col_c", "col_d"))() |>
    expect_false()

  dplyr::tibble(col_a = 1, col_b = 2, col_c = 3) |>
    check_has_any_cols(column_names = c("col_d", "col_e", "col_f"))() |>
    expect_false()
})

test_that("'msg' describes which columns are missing when check fails", {

  expect_checker_false(
    data = dplyr::tibble(col_a = 1),
    checker = check_has_any_cols(column_names = "col_b"),
    msg = stringr::str_c("Dataset must have at least one of",
                         " the columns 'col_b', but is missing all of them.")
  )

  expect_checker_false(
    data = dplyr::tibble(col_A = 1, col_B = 2),
    checker = check_has_any_cols(column_names = c("col_a", "col_b")),
    msg = stringr::str_c("Dataset must have at least one of",
                         " the columns 'col_a', 'col_b',",
                         " but is missing all of them.")
  )

  expect_checker_false(
    data = dplyr::tibble(col_A = 1, col_B = 2),
    checker = check_has_any_cols(column_names = c("col_a", "col_b", "col_c")),
    msg = stringr::str_c("Dataset must have at least one of",
                         " the columns 'col_a', 'col_b', 'col_c',",
                         " but is missing all of them.")
  )
})

test_that("returns 'TRUE' if a tibble has specified columns", {
  dplyr::tibble(col_a = 1) |>
    check_has_any_cols(column_names = "col_a")() |>
    expect_true()

  dplyr::tibble(col_a = 1) |>
    check_has_any_cols(column_names = c("col_a", "col_b"))() |>
    expect_true()

  dplyr::tibble(col_a = 1, col_b = 2) |>
    check_has_any_cols(column_names = c("col_c", "col_b"))() |>
    expect_true()

  dplyr::tibble(col_a = 1, col_b = 2) |>
    check_has_any_cols(column_names = c("col_a", "col_b"))() |>
    expect_true()

  dplyr::tibble(col_a = 1, col_b = 2, col_3 = 3) |>
    check_has_any_cols(column_names = c("col_a", "col_b"))() |>
    expect_true()

  dplyr::tibble(col_a = 1, col_b = 2, col_c = 3) |>
    check_has_any_cols(column_names = c("col_c", "col_b"))() |>
    expect_true()

  dplyr::tibble(col_a = 1, col_b = 2, col_c = 3) |>
    check_has_any_cols(column_names = c("col_d", "col_c"))() |>
    expect_true()
})

test_that("'msg' attribute is 'NULL' when check passes", {

  expect_checker_true(
    data = dplyr::tibble(col_a = 1),
    check_has_any_cols(column_names = "col_a")
  )

  expect_checker_true(
    data = dplyr::tibble(col_a = 1, col_b = 2),
    check_has_any_cols(column_names = c("col_a", "col_b"))
  )

  expect_checker_true(
    data = dplyr::tibble(col_a = 1),
    checker = check_has_any_cols(column_names = c("col_a", "col_b", "col_c"))
  )

  expect_checker_true(
    data = dplyr::tibble(col_a = 1, col_b = 2, col_3 = 3),
    check_has_any_cols(column_names = c("col_a", "col_b", "col_3"))
  )

  expect_checker_true(
    data = dplyr::tibble(col_a = 1, col_b = 2, col_c = 3),
    check_has_any_cols(column_names = c("col_c", "col_b"))
  )
})
