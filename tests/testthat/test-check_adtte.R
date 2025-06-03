test_that("returned function fails with something other than ADaM dataframe", {
  dplyr::tibble(col_a = double()) |>
    check_adtte() |>
    expect_error(
      "Argument 'adam' is invalid. Must have at least 1 rows, but has 0 rows."
    )

  dplyr::tibble(col_a = factor()) |>
    check_adtte() |>
    expect_error(
      "Argument 'adam' is invalid. Must have at least 1 rows, but has 0 rows."
    )

  dplyr::tibble(col_a = logical()) |>
    check_adtte() |>
    expect_error(
      "Argument 'adam' is invalid. Must have at least 1 rows, but has 0 rows."
    )

  dplyr::tibble(col_a = integer()) |>
    check_adtte() |>
    expect_error(
      "Argument 'adam' is invalid. Must have at least 1 rows, but has 0 rows."
    )

  list(col_a = 1) |>
    check_adtte() |>
    expect_error(
      "Argument 'adam' is invalid. Must be of type 'data.frame', not 'list'."
    )

  NULL |>
    check_adtte() |>
    expect_error(
      "Argument 'adam' is invalid. Must be of type 'data.frame', not 'NULL'."
    )
})

test_that("returns 'FALSE' with invalid column headings", {
  dplyr::tibble(
    STUDYID = character("1"),
    ID = character("1")
  ) |>
    check_adtte() |>
    expect_false()

  dplyr::tibble(
    ID = character("1"),
    USUBJID = character("1")
  ) |>
    check_adtte() |>
    expect_false()

  dplyr::tibble(
    STUDYID = character("1"),
    USUBJID = character("1"),
    PARAM = character("1")
  ) |>
    check_adtte() |>
    expect_false()
})

test_that("returns appropriate message/s with invalid column headings", {

  msg_1 <- paste0(
    "Dataset must have columns 'STUDYID', 'USUBJID',",
    " but is missing 'USUBJID'."
  )

  msg_2 <- paste0(
    "Column 'USUBJID' must have data type 'factor',",
    " but is data type 'NULL'."
  )

  msg_3 <- paste0(
    "Dataset must have one record per subject per param,",
    " but is missing column 'USUBJID' or 'PARAMCD'."
  )
  msg_ref <- c(
    msg_1,
    msg_2,
    msg_3
  )

  expect_checker_false(
    data = dplyr::tibble(
      STUDYID = character("1"),
      ID = character("1")
    ),
    checker = check_adtte,
    msg = msg_ref
  )

  msg_1 <- paste0(
    "Dataset must have columns 'STUDYID', 'USUBJID',",
    " but is missing 'STUDYID'."
  )

  msg_2 <- paste0(
    "Column 'STUDYID' must have data type 'factor',",
    " but is data type 'NULL'."
  )

  msg_3 <- paste0(
    "Dataset must have one record per subject per param,",
    " but is missing column 'USUBJID' or 'PARAMCD'."
  )
  msg_ref <- c(
    msg_1,
    msg_2,
    msg_3
  )

  expect_checker_false(
    data = dplyr::tibble(
      ID = character("1"),
      USUBJID = character("1")
    ),
    checker = check_adtte,
    msg = msg_ref
  )

  msg_1 <- paste0(
    "Dataset must have one record per subject per param,",
    " but is missing column 'USUBJID' or 'PARAMCD'."
  )
  msg_ref <- c(
    msg_1
  )

  expect_checker_false(
    data = dplyr::tibble(
      STUDYID = character("1"),
      USUBJID = character("1"),
      PARAM = character("1")
    ),
    checker = check_adtte,
    msg = msg_ref
  )
})


test_that("returns 'TRUE' with valid tibble", {

  dplyr::tibble(
    STUDYID = character("1"),
    USUBJID = character("1"),
    PARAMCD = character("1")
  ) |>
    check_adtte() |>
    expect_true()

  dplyr::tibble(
    STUDYID = character("1"),
    USUBJID = character("1"),
    PARAM  = character("1"),
    PARAMCD = character("1"),
    PARAMN = integer("1")
    ) |>
    check_adtte() |>
    expect_true()
})

test_that("'msg' attribute is 'NULL' when check passes", {

  expect_checker_true(
    data = dplyr::tibble(
      STUDYID = character("1"),
      USUBJID = character("1"),
      PARAMCD = character("1")
    ),
    checker = check_adtte
  )

  expect_checker_true(
    data = dplyr::tibble(
      STUDYID = character("1"),
      USUBJID = character("1"),
      PARAMCD = character("1"),
      PARAM = character("1"),
      PARAMN = integer("1")
    ),
    checker = check_adtte
  )

  expect_checker_true(
    data = dplyr::tibble(
        STUDYID = ("1"),
        USUBJID = c("1", "2"),
        PARAMCD = c("a", "b"),
        PARAM = c("a", "b"),
        PARAMN = c(1, 2)
    ),
    checker = check_adtte
  )

  expect_checker_true(
    data = dplyr::tibble(
      STUDYID = ("1"),
      USUBJID = c("1", "2", "3"),
      PARAMCD = c("a", "b", "c"),
      PARAM = c("a", "b", "c"),
      PARAMN = c(1, 2, 3)
    ),
    checker = check_adtte
  )
})
