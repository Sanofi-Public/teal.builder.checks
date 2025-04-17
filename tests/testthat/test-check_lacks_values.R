test_that("fails with invalid 'column_name'", {
  check_lacks_values(column_name = "", values = 1) |>
    expect_error_message(
      "Argument 'column_name' is invalid. All elements must have at least 1",
      " characters, but element 1 has 0 characters."
    )

  check_lacks_values(column_name = character(), values = 1) |>
    expect_error_message(
      "Argument 'column_name' is invalid. Must have length 1, but has length 0."
    )

  check_lacks_values(column_name = c("col_a", "col_b"), values = 1) |>
    expect_error_message(
      "Argument 'column_name' is invalid. Must have length 1, but has length 2."
    )

  check_lacks_values(column_name = NA_character_, values = 1) |>
    expect_error_message(
      "Argument 'column_name' is invalid. Contains missing values (element 1)."
    )

  check_lacks_values(column_name = 1, values = 1) |>
    expect_error_message(
      "Argument 'column_name' is invalid. Must be of type 'character', not",
      " 'double'."
    )

  check_lacks_values(column_name = TRUE, values = 1) |>
    expect_error_message(
      "Argument 'column_name' is invalid. Must be of type 'character', not",
      " 'logical'."
    )
})

test_that("fails with invalid 'values'", {
  check_lacks_values(column_name = "col_a", values = list(1)) |>
    expect_error_message(
      "Argument 'values' is invalid. Must be of type 'atomic', not 'list'."
    )

  check_lacks_values(column_name = "col_a", values = integer()) |>
    expect_error_message(
      "Argument 'values' is invalid. Must have length >= 1, but has length 0."
    )

  check_lacks_values(column_name = "col_a", values = c(1, 1)) |>
    expect_error_message(
      "Argument 'values' is invalid. Contains duplicated values, position 2."
    )
})

test_that("returns a function with the parameter 'adam'", {
  check_lacks_values(column_name = "col_a", values = 1) |>
    checkmate::expect_function(args = "adam")
})

test_that("fails if input isn't a dataframe or tibble", {
  check_lacks_values(column_name = "col_a", values = 1)(1) |>
    expect_error_message(
      "Argument 'adam' is invalid. Must be of type 'data.frame', not 'double'."
    )

  check_lacks_values(column_name = "col_a", values = 1)(TRUE) |>
    expect_error_message(
      "Argument 'adam' is invalid. Must be of type 'data.frame', not 'logical'."
    )

  check_lacks_values(column_name = "col_a", values = 1)("a") |>
    expect_error_message(
      "Argument 'adam' is invalid. Must be of type 'data.frame', not 'character'."
    )

  check_lacks_values(column_name = "col_a", values = 1)(list(USUBJID = 1)) |>
    expect_error_message(
      "Argument 'adam' is invalid. Must be of type 'data.frame', not 'list'."
    )
})

test_that("returns 'FALSE' if a column has specified values", {
  dplyr::tibble(col_a = 1) |>
    check_lacks_values(column_name = "col_a", values = 1)() |>
    expect_false()

  dplyr::tibble(col_a = 1:10) |>
    check_lacks_values(column_name = "col_a", values = 4:6)() |>
    expect_false()

  dplyr::tibble(col_a = c("a", "b", "c")) |>
    check_lacks_values(column_name = "col_a", values = c("a", "b"))() |>
     expect_false()

   dplyr::tibble(col_a = c(FALSE)) |>
     check_lacks_values(column_name = "col_a", values = FALSE)() |>
     expect_false()
})

test_that("round doubles and integers are equivalent", {
  dplyr::tibble(col_a = 1L) |>
    check_lacks_values(column_name = "col_a", values = 1.0)() |>
    expect_false()

  dplyr::tibble(col_a = 1.0) |>
    check_lacks_values(column_name = "col_a", values = 1L)() |>
    expect_false()
})

test_that("character and factors without levels are equivalent to factors", {
  dplyr::tibble(col_a = factor(letters)) |>
    check_lacks_values(column_name = "col_a", values = "a")() |>
    expect_false()

  dplyr::tibble(col_a = factor(letters)) |>
    check_lacks_values(column_name = "col_a", values = factor("a"))() |>
    expect_false()

  dplyr::tibble(col_a = letters) |>
    check_lacks_values(column_name = "col_a", values = factor("a"))() |>
    expect_false()
})

test_that("'msg' describes which values are present when check fails", {

  expect_checker_false(
    data = dplyr::tibble(col_a = 1),
    checker = check_lacks_values(column_name = "col_a", values = c(1, 2)),
    msg = "Column 'col_a' can't have values '1', '2', but has values '1'."
  )

   expect_checker_false(
     data = dplyr::tibble(col_a = 1),
     checker = check_lacks_values(column_name = "col_a", values = c(1, 3)),
     msg = "Column 'col_a' can't have values '1', '3', but has values '1'."
   )

   expect_checker_false(
     data = dplyr::tibble(col_a = "text"),
     checker = check_lacks_values(column_name = "col_a", values = c("text", "mext")),
     msg = "Column 'col_a' can't have values 'text', 'mext', but has values 'text'."
   )

   expect_checker_false(
      data = dplyr::tibble(col_a = test_date),
      checker = check_lacks_values(column_name = "col_a", values = c(test_date, as.Date("2003-02-28"))),
      msg = "Column 'col_a' can't have values '2003-02-27', '2003-02-28', but has values '2003-02-27'."
   )

   expect_checker_false(
     data = dplyr::tibble(col_a = test_datetime),
     checker = check_lacks_values(column_name = "col_a", values = c(as.POSIXct("2020/01/01 22:22:23"), test_datetime)),
     msg = "Column 'col_a' can't have values '2020-01-01 22:22:23', '2020-01-01 22:22:22', but has values '2020-01-01 22:22:22'."
   )

   expect_checker_false(
     data = dplyr::tibble(col_a = test_sex),
     checker = check_lacks_values(column_name = "col_a", values = c(factor(c("tall, short")), test_sex)),
     msg = "Column 'col_a' can't have values 'tall, short', 'male, female', but has values 'male, female'."
   )

 })

test_that("returns 'TRUE' if a column doesn't have specified values", {
  dplyr::tibble(col_a = 1) |>
    check_lacks_values(column_name = "col_b", values = 1)() |>
    expect_true()

  dplyr::tibble(col_a = 1) |>
    check_lacks_values(column_name = "col_a", values = 2)() |>
    expect_true()

  dplyr::tibble(col_a = c("a", "b", "c")) |>
    check_lacks_values(column_name = "col_a", values = c("e", "d"))() |>
    expect_true()

  dplyr::tibble(col_a = c(FALSE, TRUE)) |>
    check_lacks_values(column_name = "col_a", values = 1)() |>
    expect_true()
})

test_that("numeric and logicals are not equivalent", {
   dplyr::tibble(col_a = TRUE) |>
     check_lacks_values(column_name = "col_a", values = 1L)() |>
     expect_true()

   dplyr::tibble(col_a = TRUE) |>
     check_lacks_values(column_name = "col_a", values = 1.0)() |>
     expect_true()

   dplyr::tibble(col_a = 0L) |>
     check_lacks_values(column_name = "col_a", values = FALSE)() |>
     expect_true()

   dplyr::tibble(col_a = 0) |>
     check_lacks_values(column_name = "col_a", values = FALSE)() |>
     expect_true()
})

test_that("round doubles with extra decimal places and integers are not equivalent", {
  dplyr::tibble(col_a = 1L) |>
    check_lacks_values(column_name = "col_a", values = 1.00001)() |>
    expect_true()

  dplyr::tibble(col_a = 1.00001) |>
    check_lacks_values(column_name = "col_a", values = 1L)() |>
    expect_true()
})

test_that("'msg' attribute is 'NULL' when check passes", {

  expect_checker_true(
    data = dplyr::tibble(col_a = TRUE),
    check_lacks_values(column_name = "col_a", values = 1L)
  )

  expect_checker_true(
    data = dplyr::tibble(col_a = TRUE),
    check_lacks_values(column_name = "col_a", values = 1.0)
  )

  expect_checker_true(
    data = dplyr::tibble(col_a = TRUE),
    check_lacks_values(column_name = "col_a", values = "adam")
  )

  expect_checker_true(
    data = dplyr::tibble(col_a = TRUE),
    check_lacks_values(column_name = "col_a", values = test_date)
  )

  expect_checker_true(
    data = dplyr::tibble(col_a = TRUE),
    check_lacks_values(column_name = "col_a", values = test_datetime)
  )

  expect_checker_true(
    data = dplyr::tibble(col_a = TRUE),
    check_lacks_values(column_name = "col_a", values = test_sex)
  )

  expect_checker_true(
    data = dplyr::tibble(col_a = 0L),
    check_lacks_values(column_name = "col_a", values = FALSE)
  )

  expect_checker_true(
    data = dplyr::tibble(col_a = 0L),
    check_lacks_values(column_name = "col_a", values = "adam")
  )

  expect_checker_true(
    data = dplyr::tibble(col_a = 0L),
    check_lacks_values(column_name = "col_a", values = test_date)
  )

  expect_checker_true(
    data = dplyr::tibble(col_a = 0L),
    check_lacks_values(column_name = "col_a", values = test_datetime)
  )

  expect_checker_true(
    data = dplyr::tibble(col_a = 0L),
    check_lacks_values(column_name = "col_a", values = test_sex)
  )

  expect_checker_true(
    data = dplyr::tibble(col_a = 0),
    check_lacks_values(column_name = "col_a", values = FALSE)
  )

  expect_checker_true(
    data = dplyr::tibble(col_a = 0),
    check_lacks_values(column_name = "col_a", values = "adam")
  )

  expect_checker_true(
    data = dplyr::tibble(col_a = 0),
    check_lacks_values(column_name = "col_a", values = test_date)
  )

  expect_checker_true(
    data = dplyr::tibble(col_a = 0),
    check_lacks_values(column_name = "col_a", values = test_datetime)
  )

  expect_checker_true(
    data = dplyr::tibble(col_a = 0),
    check_lacks_values(column_name = "col_a", values = test_sex)
  )

  expect_checker_true(
    data = dplyr::tibble(col_a = 1L),
    check_lacks_values(column_name = "col_a", values = 1.00001)
  )

  expect_checker_true(
    data = dplyr::tibble(col_a = 1L),
    check_lacks_values(column_name = "col_a", values = "adam")
  )

  expect_checker_true(
    data = dplyr::tibble(col_a = 1L),
    check_lacks_values(column_name = "col_a", values = test_date)
  )

  expect_checker_true(
    data = dplyr::tibble(col_a = 1L),
    check_lacks_values(column_name = "col_a", values = test_datetime)
  )

  expect_checker_true(
    data = dplyr::tibble(col_a = 1L),
    check_lacks_values(column_name = "col_a", values = test_sex)
  )

  expect_checker_true(
    data = dplyr::tibble(col_a = 1.00001),
    check_lacks_values(column_name = "col_a", values = 1L)
  )

  expect_checker_true(
    data = dplyr::tibble(col_a = 1.00001),
    check_lacks_values(column_name = "col_a", values = "adam")
  )

  expect_checker_true(
    data = dplyr::tibble(col_a = 1.00001),
    check_lacks_values(column_name = "col_a", values = test_date)
  )

  expect_checker_true(
    data = dplyr::tibble(col_a = 1.00001),
    check_lacks_values(column_name = "col_a", values = test_datetime)
  )

  expect_checker_true(
    data = dplyr::tibble(col_a = 1.00001),
    check_lacks_values(column_name = "col_a", values = test_sex)
  )

  expect_checker_true(
    data = dplyr::tibble(col_a = 1),
    check_lacks_values(column_name = "col_a", values = 2)
  )

  expect_checker_true(
    data = dplyr::tibble(col_a = 1),
    check_lacks_values(column_name = "col_a", values = "adam")
  )

  expect_checker_true(
    data = dplyr::tibble(col_a = 1),
    check_lacks_values(column_name = "col_a", values = test_date)
  )

  expect_checker_true(
    data = dplyr::tibble(col_a = 1),
    check_lacks_values(column_name = "col_a", values = test_datetime)
  )

  expect_checker_true(
    data = dplyr::tibble(col_a = 1),
    check_lacks_values(column_name = "col_a", values = test_sex)
  )

  expect_checker_true(
    data = dplyr::tibble(col_a = 1),
    check_lacks_values(column_name = "col_a", values = c(2, 3))
  )

  expect_checker_true(
    data = dplyr::tibble(col_a = "text"),
    check_lacks_values(column_name = "col_a", values = "frext")
  )

  expect_checker_true(
    data = dplyr::tibble(col_a = "text"),
    check_lacks_values(column_name = "col_a", values = c("frext", "zext"))
  )

  expect_checker_true(
    data = dplyr::tibble(col_a = "text"),
    check_lacks_values(column_name = "col_a", values = 1L)
  )

  expect_checker_true(
    data = dplyr::tibble(col_a = "text"),
    check_lacks_values(column_name = "col_a", values = 1.00001)
  )

  expect_checker_true(
    data = dplyr::tibble(col_a = "text"),
    check_lacks_values(column_name = "col_a", values = test_date)
  )

  expect_checker_true(
    data = dplyr::tibble(col_a = "text"),
    check_lacks_values(column_name = "col_a", values = test_datetime)
  )

  expect_checker_true(
    data = dplyr::tibble(col_a = "text"),
    check_lacks_values(column_name = "col_a", values = test_sex)
  )
})
