test_that("returned function fails with something other than ADaM dataframe", {
  dplyr::tibble(col_a = double()) |>
    check_adam() |>
    expect_error(
      "Argument 'adam' is invalid. Must have at least 1 rows, but has 0 rows."
    )

  dplyr::tibble(col_a = factor()) |>
    check_adam() |>
    expect_error(
      "Argument 'adam' is invalid. Must have at least 1 rows, but has 0 rows."
    )

  dplyr::tibble(col_a = logical()) |>
    check_adam() |>
    expect_error(
      "Argument 'adam' is invalid. Must have at least 1 rows, but has 0 rows."
    )

  dplyr::tibble(col_a = integer()) |>
    check_adam() |>
    expect_error(
      "Argument 'adam' is invalid. Must have at least 1 rows, but has 0 rows."
    )

  list(col_a = 1) |>
    check_adam() |>
    expect_error(
      "Argument 'adam' is invalid. Must be of type 'data.frame', not 'list'."
    )

  NULL |>
    check_adam() |>
    expect_error(
      "Argument 'adam' is invalid. Must be of type 'data.frame', not 'NULL'."
    )
})

test_that("returns 'FALSE' with invalid column headings", {
  dplyr::tibble(
    STUDYID = character("1"),
    ID = character("1")
  ) |>
    check_adam() |>
    expect_false()

  dplyr::tibble(
    ID = character("1"),
    USUBJID = character("1")
  ) |>
    check_adam() |>
    expect_false()
})

test_that("returns appropriate message with invalid column headings", {
  expect_checker_false(
    data = dplyr::tibble(
      STUDYID = character("1"),
      ID = character("1")
    ),
    checker = check_adam,
    msg = stringr::str_c(
      "Dataset must have columns 'STUDYID', 'USUBJID',",
      " but is missing 'USUBJID'."
    )
  )

  expect_checker_false(
    data = dplyr::tibble(
      ID = character("1"),
      USUBJID = character("1")
    ),
    checker = check_adam,
    msg = stringr::str_c(
      "Dataset must have columns 'STUDYID', 'USUBJID',",
      " but is missing 'STUDYID'."
    )
  )

  expect_checker_false(
    data = dplyr::tibble(
      ID = character("1"),
      UID = character("1")
    ),
    checker = check_adam,
    msg = stringr::str_c(
      "Dataset must have columns 'STUDYID', 'USUBJID',",
      " but is missing 'STUDYID', 'USUBJID'."
    )
  )
})

test_that("returns 'TRUE' with valid tibble", {
  dplyr::tibble(
    STUDYID = character("1"),
    USUBJID = character("1")
  ) |>
    check_adam() |>
    expect_true()
})

test_that("'msg' attribute is 'NULL' when check passes", {
  expect_checker_true(
    data = dplyr::tibble(
      STUDYID = character("1"),
      USUBJID = character("1")
    ),
    checker = check_adam
  )

  expect_checker_true(
    data = dplyr::tibble(
      "STUDYID" = 1L,
      "USUBJID" = 1L
    ),
    checker = check_adam
  )
})
