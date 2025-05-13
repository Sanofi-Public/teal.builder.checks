test_that("returned function fails with something other than ADaM dataframe", {
  dplyr::tibble(col_a = double()) |>
    check_adqs() |>
    expect_error(
      "Argument 'adam' is invalid. Must have at least 1 rows, but has 0 rows."
    )

  dplyr::tibble(col_a = factor()) |>
    check_adqs() |>
    expect_error(
      "Argument 'adam' is invalid. Must have at least 1 rows, but has 0 rows."
    )

  dplyr::tibble(col_a = logical()) |>
    check_adqs() |>
    expect_error(
      "Argument 'adam' is invalid. Must have at least 1 rows, but has 0 rows."
    )

  dplyr::tibble(col_a = integer()) |>
    check_adqs() |>
    expect_error(
      "Argument 'adam' is invalid. Must have at least 1 rows, but has 0 rows."
    )

  list(col_a = 1) |>
    check_adqs() |>
    expect_error(
      "Argument 'adam' is invalid. Must be of type 'data.frame', not 'list'."
    )

  NULL |>
    check_adqs() |>
    expect_error(
      "Argument 'adam' is invalid. Must be of type 'data.frame', not 'NULL'."
    )
})

test_that("returns 'FALSE' with invalid column headings", {
  dplyr::tibble(
    STUDYID = character("1"),
    ID = character("1")
  ) |>
    check_adqs() |>
    expect_false()

  dplyr::tibble(
    ID = character("1"),
    USUBJID = character("1")
  ) |>
    check_adqs() |>
    expect_false()

  dplyr::tibble(
    USUBJID = character("1"),
    PARAMCD = character("1"),
    AVISIT = character("1")
  ) |>
    check_adqs() |>
    expect_false()

  dplyr::tibble(
    STUDYID = character("1"),
    PARAMCD = character("1"),
    AVISIT = character("1")
  ) |>
    check_adqs() |>
    expect_false()

  dplyr::tibble(
    PARAMCD = character("1"),
    AVISIT = character("1")
  ) |>
    check_adqs() |>
    expect_false()
})

test_that("returns appropriate message with invalid column headings", {

  msg_1 <- paste0(
    "Dataset must have columns 'STUDYID', 'USUBJID',",
    " but is missing 'USUBJID'."
  )

  msg_2 <- paste0(
    "Column 'USUBJID' must have data type 'factor',",
    " but is data type 'NULL'."
  )

  msg_ref <- c(
    msg_1,
    msg_2
  )

  expect_checker_false(
    data = dplyr::tibble(
      STUDYID = character("1"),
      ID = character("1")
    ),
    checker = check_adqs,
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

  msg_ref <- c(
    msg_1,
    msg_2
  )

  expect_checker_false(
    data = dplyr::tibble(
      ID = character("1"),
      USUBJID = character("1")
    ),
    checker = check_adqs,
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

  msg_ref <- c(
    msg_1,
    msg_2
  )

  expect_checker_false(
    data = dplyr::tibble(
      USUBJID = character("1"),
      PARAMCD = character("1"),
      AVISIT = character("1")
    ),
    checker = check_adqs,
    msg = msg_ref
  )

  msg_1 <- paste0(
    "Dataset must have columns 'STUDYID', 'USUBJID',",
    " but is missing 'STUDYID', 'USUBJID'."
  )

  msg_2 <- paste0(
    "Column 'STUDYID' must have data type 'factor',",
    " but is data type 'NULL'."
  )

  msg_3 <- paste0(
    "Column 'USUBJID' must have data type 'factor',",
    " but is data type 'NULL'."
  )

  msg_ref <- c(
    msg_1,
    msg_2,
    msg_3
  )

  expect_checker_false(
    data = dplyr::tibble(
      PARAMCD = character("1"),
      AVISIT = character("1")
    ),
    checker = check_adqs,
    msg = msg_ref
  )

  msg_1 <- paste0(
    "Column 'STUDYID' must have data type 'factor',",
    " but is data type 'integer'."
  )

  msg_2 <- paste0(
    "Column 'USUBJID' must have data type 'factor',",
    " but is data type 'integer'."
  )

  msg_ref <- c(
    msg_1,
    msg_2
  )

  expect_checker_false(
    data = dplyr::tibble(
      "STUDYID" = 1L,
      "USUBJID" = 1L,
      "PARAMCD" = 1L,
      "AVISIT" = 1L
    ),
    checker = check_adqs,
    msg = msg_ref
  )
})

test_that("returns 'TRUE' with valid tibble", {

  dplyr::tibble(
    STUDYID = character("1"),
    USUBJID = character("1")
  ) |>
    check_adqs() |>
    expect_true()

  dplyr::tibble(
    STUDYID = character("1"),
    USUBJID = character("1"),
    PARAMCD = character("1")
  ) |>
    check_adqs() |>
    expect_true()

  dplyr::tibble(
    STUDYID = character("1"),
    USUBJID = character("1"),
    AVISIT = character("1")
  ) |>
    check_adqs() |>
    expect_true()

  dplyr::tibble(
    STUDYID = character("1"),
    USUBJID = character("1"),
    PARAMCD = character("1"),
    AVISIT = character("1")
  ) |>
    check_adqs() |>
    expect_true()
})

test_that("'msg' attribute is 'NULL' when check passes", {

  expect_checker_true(
    data = dplyr::tibble(
      STUDYID = character("1"),
      USUBJID = character("1")
    ),
    checker = check_adqs
  )

  expect_checker_true(
    data = dplyr::tibble(
      STUDYID = character("1"),
      USUBJID = character("1"),
      PARAMCD = character("1")
    ),
    checker = check_adqs
  )

  expect_checker_true(
    data = dplyr::tibble(
      STUDYID = character("1"),
      USUBJID = character("1"),
      AVISIT = character("1")
    ),
    checker = check_adqs
  )

  expect_checker_true(
    data = dplyr::tibble(
      STUDYID = character("1"),
      USUBJID = character("1"),
      PARAMCD = character("1"),
      AVISIT = character("1")
    ),
    checker = check_adqs
  )
})
