test_that("fails with invalid 'column_name'", {
  check_is_categorical(column_name = c("col_a", "col_b")) |>
    expect_error(
      "Argument 'column_name' is invalid. Must have length 1, but has length 2."
    )

  check_is_categorical(column_name = factor("col_a")) |>
    expect_error(
      "Argument 'column_name' is invalid. Must be of type 'character', not 'factor'."
    )

  # test if the column_name parameter is missing
  check_is_categorical(column_name = "") |>
    expect_error(
      "Argument 'column_name' is invalid. All elements must have at least 1 characters, but element 1 has 0 characters."
    )

  # test integer
  check_is_categorical(column_name = 1L) |>
    expect_error(
      "Argument 'column_name' is invalid. Must be of type 'character', not 'integer'."
    )

  # test double
  check_is_categorical(column_name = 1.1) |>
    expect_error(
      "Argument 'column_name' is invalid. Must be of type 'character', not 'double'."
    )

  # test logical
  check_is_categorical(column_name = TRUE) |>
    expect_error(
      "Argument 'column_name' is invalid. Must be of type 'character', not 'logical'."
    )
})

test_that("returned function fails with something other than ADaM dataframe", {
  check_is_categorical(column_name = "col_a")() |>
    expect_error('argument "adam" is missing, with no default')

  dplyr::tibble(col_a = integer()) |>
    check_is_categorical(column_name = "col_a")() |>
    expect_error(
      "Argument 'adam' is invalid. Must have at least 1 rows, but has 0 rows."
    )

  list(col_a = 1) |>
    check_is_categorical(column_name = "col_a")() |>
    expect_error(
      "Argument 'adam' is invalid. Must be of type 'data.frame', not 'list'."
    )

  NULL |>
    check_is_categorical(column_name = "col_a")() |>
    expect_error(
      "Argument 'adam' is invalid. Must be of type 'data.frame', not 'NULL'."
    )
})

test_that("returned function provides correct 'msg' for invalid columns", {
  expect_checker_false(
    data = dplyr::tibble(col_a = 1L),
    checker = check_is_categorical(column_name = "col_a"),
    msg = "Column 'col_a' must have data type 'factor', but is data type 'integer'."
  )

  expect_checker_false(
    data = dplyr::tibble(col_a = unclass(as.Date("2020-01-01"))),
    checker = check_is_categorical(column_name = "col_a"),
    msg = "Column 'col_a' must have data type 'factor', but is data type 'numeric'."
  )

  expect_checker_false(
    data = dplyr::tibble(col_a = as.POSIXct("2020-01-01 00:00:00")),
    checker = check_is_categorical(column_name = "col_a"),
    msg = "Column 'col_a' must have data type 'factor', but is data type 'POSIXct', 'POSIXt'."
  )

  dplyr::tibble(col_a = as.factor(NULL)) |>
    check_is_logical(column_name = "col_a")() |>
    expect_error_message(
      "Argument 'adam' is invalid. Must have at least 1 rows, but has 0 rows."
    )
})

test_that("a message is returned if the column doesn't exist", {
  expect_checker_false(
    data = dplyr::tibble(col_a = as.Date("2020-01-01")),
    checker = check_is_categorical(column_name = "col_b"),
    msg = "Column 'col_b' must have data type 'factor', but is data type 'NULL'."
  )
})

test_that("the function returns 'TRUE' with valid column", {
  expect_checker_true(
    data = dplyr::tibble(col_a = factor(c("male, female"))),
    checker = check_is_categorical(column_name = "col_a")
  )

  expect_checker_true(
    data = dplyr::tibble(col_a = c(factor(c("male, female")), factor(c("arm 1, arm 2")))),
    checker = check_is_categorical(column_name = "col_a")
  )

  expect_checker_true(
    data = dplyr::tibble(col_a = "2020-01-01"),
    checker = check_is_categorical(column_name = "col_a")
  )
})

test_that("return 'FALSE' for 1 invalid NA datatype", {
  dplyr::tibble(col_a = NA) |>
    check_is_categorical(column_name = "col_a")() |>
    expect_false()

  dplyr::tibble(col_a = NA_integer_) |>
    check_is_categorical(column_name = "col_a")() |>
    expect_false()

  dplyr::tibble(col_a = NA_real_) |>
    check_is_categorical(column_name = "col_a")() |>
    expect_false()

  dplyr::tibble(col_a = NA_complex_) |>
    check_is_categorical(column_name = "col_a")() |>
    expect_false()

  dplyr::tibble(col_a = NaN) |>
    check_is_categorical(column_name = "col_a")() |>
    expect_false()
})

test_that("return 'FALSE' for 1 invalid number datatype", {
  dplyr::tibble(col_a = test_integer) |>
    check_is_categorical(column_name = "col_a")() |>
    expect_false()

  dplyr::tibble(col_a = test_double) |>
    check_is_categorical(column_name = "col_a")() |>
    expect_false()

  dplyr::tibble(col_a = 1L) |>
    check_is_categorical(column_name = "col_a")() |>
    expect_false()

  dplyr::tibble(col_a = 1.0) |>
    check_is_categorical(column_name = "col_a")() |>
    expect_false()

  dplyr::tibble(col_a = 1.1) |>
    check_is_categorical(column_name = "col_a")() |>
    expect_false()
})

test_that("return 'FALSE' for 1 invalid date datatype", {
  dplyr::tibble(col_a = test_date) |>
    check_is_categorical(column_name = "col_a")() |>
    expect_false()
})

test_that("return 'FALSE' for 1 invalid logical datatype", {
  dplyr::tibble(col_a = TRUE) |>
    check_is_categorical(column_name = "col_a")() |>
    expect_false()

  dplyr::tibble(col_a = FALSE) |>
    check_is_categorical(column_name = "col_a")() |>
    expect_false()
})

test_that("return 'FALSE' for 1 invalid datetime datatype", {
  dplyr::tibble(col_a = test_datetime) |>
    check_is_categorical(column_name = "col_a")() |>
    expect_false()
})

test_that("an appropriate message is returned for 1 invalid number datatype", {
  expect_checker_false(
    data = dplyr::tibble(col_a = test_integer),
    checker = check_is_categorical(column_name = "col_a"),
    msg = "Column 'col_a' must have data type 'factor', but is data type 'integer'."
  )

  expect_checker_false(
    data = dplyr::tibble(col_a = test_double),
    checker = check_is_categorical(column_name = "col_a"),
    msg = "Column 'col_a' must have data type 'factor', but is data type 'numeric'."
  )

  expect_checker_false(
    data = dplyr::tibble(col_a = 1L),
    checker = check_is_categorical(column_name = "col_a"),
    msg = "Column 'col_a' must have data type 'factor', but is data type 'integer'."
  )

  expect_checker_false(
    data = dplyr::tibble(col_a = 1.0),
    checker = check_is_categorical(column_name = "col_a"),
    msg = "Column 'col_a' must have data type 'factor', but is data type 'numeric'."
  )

  expect_checker_false(
    data = dplyr::tibble(col_a = 1.1),
    checker = check_is_categorical(column_name = "col_a"),
    msg = "Column 'col_a' must have data type 'factor', but is data type 'numeric'."
  )
})

test_that("an appropriate message is returned for 1 invalid date datatype", {
  expect_checker_false(
    data = dplyr::tibble(col_a = test_date),
    checker = check_is_categorical(column_name = "col_a"),
    msg = "Column 'col_a' must have data type 'factor', but is data type 'Date'."
  )
})

test_that("an appropriate message is returned for 1 invalid logical datatype", {
  expect_checker_false(
    data = dplyr::tibble(col_a = TRUE),
    checker = check_is_categorical(column_name = "col_a"),
    msg = "Column 'col_a' must have data type 'factor', but is data type 'logical'."
  )

  expect_checker_false(
    data = dplyr::tibble(col_a = FALSE),
    checker = check_is_categorical(column_name = "col_a"),
    msg = "Column 'col_a' must have data type 'factor', but is data type 'logical'."
  )
})

test_that("an appropriate message is returned for 1 invalid datetime datatype", {
  expect_checker_false(
    data = dplyr::tibble(col_a = test_datetime),
    checker = check_is_categorical(column_name = "col_a"),
    msg = "Column 'col_a' must have data type 'factor', but is data type 'POSIXct', 'POSIXt'."
  )
})

test_that("return 'FALSE' for 2 invalid datatypes", {
  dplyr::tibble(col_a = c(1.0, 2.1)) |>
    check_is_categorical(column_name = "col_a")() |>
    expect_false()

  dplyr::tibble(col_a = c(1.0, 3.0)) |>
    check_is_categorical(column_name = "col_a")() |>
    expect_false()

  dplyr::tibble(col_a = c(1L, 3L)) |>
    check_is_categorical(column_name = "col_a")() |>
    expect_false()

  dplyr::tibble(col_a = c(test_integer, test_integer)) |>
    check_is_categorical(column_name = "col_a")() |>
    expect_false()

  dplyr::tibble(col_a = c(test_integer, test_double)) |>
    check_is_categorical(column_name = "col_a")() |>
    expect_false()

  dplyr::tibble(col_a = c(test_double, test_integer)) |>
    check_is_categorical(column_name = "col_a")() |>
    expect_false()

  dplyr::tibble(col_a = c(test_double, test_double)) |>
    check_is_categorical(column_name = "col_a")() |>
    expect_false()

  dplyr::tibble(col_a = c(test_datetime, test_date)) |>
    check_is_categorical(column_name = "col_a")() |>
    expect_false()

  dplyr::tibble(col_a = c(test_datetime, test_datetime)) |>
    check_is_categorical(column_name = "col_a")() |>
    expect_false()

  dplyr::tibble(col_a = c(test_date, test_date)) |>
    check_is_categorical(column_name = "col_a")() |>
    expect_false()

  dplyr::tibble(col_a = c(test_sex, test_date)) |>
    check_is_categorical(column_name = "col_a")() |>
    expect_false()

  dplyr::tibble(col_a = c(TRUE, TRUE)) |>
    check_is_categorical(column_name = "col_a")() |>
    expect_false()

  dplyr::tibble(col_a = c(TRUE, FALSE)) |>
    check_is_categorical(column_name = "col_a")() |>
    expect_false()

  dplyr::tibble(col_a = c(FALSE, TRUE)) |>
    check_is_categorical(column_name = "col_a")() |>
    expect_false()

  dplyr::tibble(col_a = c(FALSE, FALSE)) |>
    check_is_categorical(column_name = "col_a")() |>
    expect_false()
})

test_that("an appropriate message is returned for 2 invalid datatypes", {
  expect_checker_false(
    data = dplyr::tibble(col_a = c(1.0, 3.0)),
    checker = check_is_categorical(column_name = "col_a"),
    msg = "Column 'col_a' must have data type 'factor', but is data type 'numeric'."
  )

  expect_checker_false(
    data = dplyr::tibble(col_a = c(1L, 3L)),
    checker = check_is_categorical(column_name = "col_a"),
    msg = "Column 'col_a' must have data type 'factor', but is data type 'integer'."
  )

  expect_checker_false(
    data = dplyr::tibble(col_a = c(test_integer, test_integer)),
    checker = check_is_categorical(column_name = "col_a"),
    msg = "Column 'col_a' must have data type 'factor', but is data type 'integer'."
  )

  expect_checker_false(
    data = dplyr::tibble(col_a = c(test_double, test_integer)),
    checker = check_is_categorical(column_name = "col_a"),
    msg = "Column 'col_a' must have data type 'factor', but is data type 'numeric'."
  )

  expect_checker_false(
    data = dplyr::tibble(col_a = c(test_double, test_double)),
    checker = check_is_categorical(column_name = "col_a"),
    msg = "Column 'col_a' must have data type 'factor', but is data type 'numeric'."
  )

  expect_checker_false(
    data = dplyr::tibble(col_a = c(test_datetime, test_date)),
    checker = check_is_categorical(column_name = "col_a"),
    msg = "Column 'col_a' must have data type 'factor', but is data type 'POSIXct', 'POSIXt'."
  )

  expect_checker_false(
    data = dplyr::tibble(col_a = c(test_datetime, test_datetime)),
    checker = check_is_categorical(column_name = "col_a"),
    msg = "Column 'col_a' must have data type 'factor', but is data type 'POSIXct', 'POSIXt'."
  )

  expect_checker_false(
    data = dplyr::tibble(col_a = c(test_integer, test_sex)),
    checker = check_is_categorical(column_name = "col_a"),
    msg = "Column 'col_a' must have data type 'factor', but is data type 'integer'."
  )

  expect_checker_false(
    data = dplyr::tibble(col_a = c(test_sex, test_date)),
    checker = check_is_categorical(column_name = "col_a"),
    msg = "Column 'col_a' must have data type 'factor', but is data type 'numeric'."
  )

  expect_checker_false(
    data = dplyr::tibble(col_a = c(test_sex, TRUE)),
    checker = check_is_categorical(column_name = "col_a"),
    msg = "Column 'col_a' must have data type 'factor', but is data type 'integer'."
  )

  expect_checker_false(
    data = dplyr::tibble(col_a = c(TRUE, TRUE)),
    checker = check_is_categorical(column_name = "col_a"),
    msg = "Column 'col_a' must have data type 'factor', but is data type 'logical'."
  )

  expect_checker_false(
    data = dplyr::tibble(col_a = c(TRUE, FALSE)),
    checker = check_is_categorical(column_name = "col_a"),
    msg = "Column 'col_a' must have data type 'factor', but is data type 'logical'."
  )

  expect_checker_false(
    data = dplyr::tibble(col_a = c(FALSE, TRUE)),
    checker = check_is_categorical(column_name = "col_a"),
    msg = "Column 'col_a' must have data type 'factor', but is data type 'logical'."
  )

  expect_checker_false(
    data = dplyr::tibble(col_a = c(FALSE, FALSE)),
    checker = check_is_categorical(column_name = "col_a"),
    msg = "Column 'col_a' must have data type 'factor', but is data type 'logical'."
  )
})

test_that("passes with 1 valid 'column_name'", {
  check_is_categorical(column_name = "col_a") |>
    expect_silent()
})

test_that("function returns a function that accepts ADaM dataset", {
  check_is_categorical(column_name = "col_a") |>
    checkmate::expect_function(args = "adam")
})

test_that("return 'TRUE' for 1 valid categorical datatype", {
  dplyr::tibble(col_a = test_sex) |>
    check_is_categorical(column_name = "col_a")() |>
    expect_true()

  dplyr::tibble(col_a = "text") |>
    check_is_categorical(column_name = "col_a")() |>
    expect_true()

  dplyr::tibble(col_a = NA_character_) |>
    check_is_categorical(column_name = "col_a")() |>
    expect_true()
})

test_that("return 'TRUE' for 2 valid categorical datatype", {
  dplyr::tibble(col_a = c("a", TRUE)) |>
    check_is_categorical(column_name = "col_a")() |>
    expect_true()

  dplyr::tibble(col_a = c(TRUE, "a")) |>
    check_is_categorical(column_name = "col_a")() |>
    expect_true()

  dplyr::tibble(col_a = c("a", FALSE)) |>
    check_is_categorical(column_name = "col_a")() |>
    expect_true()

  dplyr::tibble(col_a = c(FALSE, "a")) |>
    check_is_categorical(column_name = "col_a")() |>
    expect_true()

  dplyr::tibble(col_a = c("1234", "text")) |>
    check_is_categorical(column_name = "col_a")() |>
    expect_true()

  dplyr::tibble(col_a = c("text", "1234")) |>
    check_is_categorical(column_name = "col_a")() |>
    expect_true()

  dplyr::tibble(col_a = c("text", "1234")) |>
    check_is_categorical(column_name = "col_a")() |>
    expect_true()

  dplyr::tibble(col_a = c("1234", "text")) |>
    check_is_categorical(column_name = "col_a")() |>
    expect_true()
})

test_that("no message is returned for 1 valid datatype", {
  expect_checker_true(
    data = dplyr::tibble(col_a = test_sex),
    checker = check_is_categorical(column_name = "col_a")
  )

  expect_checker_true(
    data = dplyr::tibble(col_a = "text"),
    checker = check_is_categorical(column_name = "col_a")
  )

  expect_checker_true(
    data = dplyr::tibble(col_a = NA_character_),
    checker = check_is_categorical(column_name = "col_a")
  )
})

test_that("return 'TRUE' for 2 valid factor datatypes", {
  dplyr::tibble(col_a = c(test_sex, test_sex)) |>
    check_is_categorical(column_name = "col_a")() |>
    expect_true()
})

test_that("no message is returned for 2 valid datatypes", {
  expect_checker_true(
    data = dplyr::tibble(col_a = c("text", "text")),
    checker = check_is_categorical(column_name = "col_a")
  )

  expect_checker_true(
    data = dplyr::tibble(col_a = c(NA_character_, NA_character_)),
    checker = check_is_categorical(column_name = "col_a")
  )
})
