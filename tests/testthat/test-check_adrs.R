test_that("returned function fails with something other than ADaM dataframe", {
  dplyr::tibble(col_a = double()) |>
    check_adrs() |>
    expect_error(
      "Argument 'adam' is invalid. Must have at least 1 rows, but has 0 rows."
    )

  dplyr::tibble(col_a = factor()) |>
    check_adrs() |>
    expect_error(
      "Argument 'adam' is invalid. Must have at least 1 rows, but has 0 rows."
    )

  dplyr::tibble(col_a = logical()) |>
    check_adrs() |>
    expect_error(
      "Argument 'adam' is invalid. Must have at least 1 rows, but has 0 rows."
    )

  dplyr::tibble(col_a = integer()) |>
    check_adrs() |>
    expect_error(
      "Argument 'adam' is invalid. Must have at least 1 rows, but has 0 rows."
    )

  list(col_a = 1) |>
    check_adrs() |>
    expect_error(
      "Argument 'adam' is invalid. Must be of type 'data.frame', not 'list'."
    )

  NULL |>
    check_adrs() |>
    expect_error(
      "Argument 'adam' is invalid. Must be of type 'data.frame', not 'NULL'."
    )
})

test_that("returns 'FALSE' with invalid column headings", {
  dplyr::tibble(
    STUDYID = character("1"),
    ID = character("1")
  ) |>
    check_adrs() |>
    expect_false()

  dplyr::tibble(
    ID = character("1"),
    USUBJID = character("1")
  ) |>
    check_adrs() |>
    expect_false()

  dplyr::tibble(
    USUBJID = character("1"),
    PARAMCD = character("1"),
    AVISIT = character("1")
  ) |>
    check_adrs() |>
    expect_false()

  dplyr::tibble(
    STUDYID = character("1"),
    PARAMCD = character("1"),
    AVISIT = character("1")
  ) |>
    check_adrs() |>
    expect_false()

  dplyr::tibble(
    PARAMCD = character("1"),
    AVISIT = character("1")
  ) |>
    check_adrs() |>
    expect_false()
})

test_that("returns appropriate message with invalid column headings", {

  msg_1 <- paste0("Dataset must have columns 'STUDYID', 'USUBJID',",
                  " but is missing 'USUBJID'."
                  )

  msg_2 <- paste0("Column 'USUBJID' must have data type 'factor',",
                  " but is data type 'NULL'."
                  )

  msg_3 <- paste0("Dataset must have one record per subject,",
                  " but has 1 records (rows) and 0 subjects (USUBJID).")

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
    checker = check_adrs,
    msg = msg_ref
  )

  msg_1 <- paste0("Dataset must have columns 'STUDYID', 'USUBJID',",
                  " but is missing 'STUDYID'."
                 )

  msg_2 <- paste0("Column 'STUDYID' must have data type 'factor',",
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
    checker = check_adrs,
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

  msg_3 <- paste0("Column 'USUBJID' must have data type 'factor',",
                  " but is data type 'NULL'.")

  msg_4 <- paste0("Dataset must have one record per subject,",
                  " but has 1 records (rows) and 0 subjects (USUBJID)."
  )

  msg_ref <- c(
    msg_1,
    msg_2,
    msg_3,
    msg_4
  )

  expect_checker_false(
    data = dplyr::tibble(
      PARAMCD = character("1"),
      AVISIT = character("1")
    ),
    checker = check_adrs,
    msg = msg_ref
  )

  msg_1 <- paste0("Dataset must have columns 'STUDYID', 'USUBJID',",
                  " but is missing 'USUBJID'.")

  msg_2 <- paste0("Column 'USUBJID' must have data type 'factor',",
                  " but is data type 'NULL'.")

  msg_3 <- paste0("Dataset must have one record per subject,",
                  " but has 1 records (rows) and 0 subjects (USUBJID).")

  msg_ref <- c(
    msg_1,
    msg_2,
    msg_3
  )

  expect_checker_false(
    data = dplyr::tibble(
      STUDYID = "1",
      PARAMCD = character("1"),
      AVISIT = character("1")
    ),
    checker = check_adrs,
    msg = msg_ref
  )

  msg_1 <- paste0("Dataset must have columns 'STUDYID', 'USUBJID',",
                  " but is missing 'STUDYID'.")

  msg_2 <- paste0("Column 'STUDYID' must have data type 'factor',",
                  " but is data type 'NULL'.")

  msg_ref <- c(
    msg_1,
    msg_2
  )

  expect_checker_false(
    data = dplyr::tibble(
      USUBJID = "1",
      PARAMCD = character("1"),
      AVISIT = character("1")
    ),
    checker = check_adrs,
    msg = msg_ref
  )

  msg_1 <- paste0("Dataset must have columns 'STUDYID', 'USUBJID',",
                  " but is missing 'STUDYID'.")

  msg_2 <- paste0("Column 'STUDYID' must have data type 'factor',",
                  " but is data type 'NULL'.")

  msg_ref <- c(
    msg_1,
    msg_2
  )

  expect_checker_false(
    data = dplyr::tibble(
      USUBJID = test_sex,
      PARAMCD = character("1"),
      AVISIT = character("1")
    ),
    checker = check_adrs,
    msg = msg_ref
  )

  msg_1 <- paste0("Dataset must have columns 'STUDYID', 'USUBJID',",
                  " but is missing 'USUBJID'."
                  )

  msg_2 <- paste0("Column 'USUBJID' must have data type 'factor',",
                  " but is data type 'NULL'."
                  )

  msg_3 <- paste0("Dataset must have one record per subject,",
                  " but has 1 records (rows) and 0 subjects (USUBJID)."
                  )

  msg_ref <- c(
    msg_1,
    msg_2,
    msg_3
  )

  expect_checker_false(
    data = dplyr::tibble(
      STUDYID = test_sex,
      PARAMCD = character("1"),
      AVISIT = character("1")
    ),
    checker = check_adrs,
    msg = msg_ref
  )

  msg_1 <- paste0("Dataset must have columns 'STUDYID', 'USUBJID',",
                  " but is missing 'USUBJID'."
  )

  msg_2 <- paste0("Column 'STUDYID' must have data type 'factor',",
                  " but is data type 'Date'."
  )

  msg_3 <- paste0("Column 'USUBJID' must have data type 'factor',",
                  " but is data type 'NULL'."
  )

  msg_4 <- paste0("Dataset must have one record per subject,",
                  " but has 1 records (rows) and 0 subjects (USUBJID)."
  )

  msg_ref <- c(
    msg_1,
    msg_2,
    msg_3,
    msg_4
  )

  expect_checker_false(
    data = dplyr::tibble(
      STUDYID = test_date,
      PARAMCD = character("1"),
      AVISIT = character("1")
    ),
    checker = check_adrs,
    msg = msg_ref
  )

  msg_1 <- paste0("Dataset must have columns 'STUDYID', 'USUBJID',",
                  " but is missing 'USUBJID'."
  )

  msg_2 <- paste0("Column 'STUDYID' must have data type 'factor',",
                  " but is data type 'POSIXct', 'POSIXt'."
  )

  msg_3 <- paste0("Column 'USUBJID' must have data type 'factor',",
                  " but is data type 'NULL'."
  )

  msg_4 <- paste0("Dataset must have one record per subject,",
                  " but has 1 records (rows) and 0 subjects (USUBJID)."
  )

  msg_ref <- c(
    msg_1,
    msg_2,
    msg_3,
    msg_4
  )

  expect_checker_false(
    data = dplyr::tibble(
      STUDYID = test_datetime,
      PARAMCD = character("1"),
      AVISIT = character("1")
    ),
    checker = check_adrs,
    msg = msg_ref
  )

  msg_1 <- paste0("Dataset must have columns 'STUDYID', 'USUBJID',",
                  " but is missing 'USUBJID'."
  )

  msg_2 <- paste0("Column 'STUDYID' must have data type 'factor',",
                  " but is data type 'integer'."
  )

  msg_3 <- paste0("Column 'USUBJID' must have data type 'factor',",
                  " but is data type 'NULL'."
  )

  msg_4 <- paste0("Dataset must have one record per subject,",
                  " but has 1 records (rows) and 0 subjects (USUBJID)."
  )

  msg_ref <- c(
    msg_1,
    msg_2,
    msg_3,
    msg_4
  )

  expect_checker_false(
    data = dplyr::tibble(
      STUDYID = test_integer,
      PARAMCD = character("1"),
      AVISIT = character("1")
    ),
    checker = check_adrs,
    msg = msg_ref
  )

  msg_1 <- paste0("Dataset must have columns 'STUDYID', 'USUBJID',",
                  " but is missing 'USUBJID'."
  )

  msg_2 <- paste0("Column 'STUDYID' must have data type 'factor',",
                  " but is data type 'numeric'."
  )

  msg_3 <- paste0("Column 'USUBJID' must have data type 'factor',",
                  " but is data type 'NULL'."
  )

  msg_4 <- paste0("Dataset must have one record per subject,",
                  " but has 1 records (rows) and 0 subjects (USUBJID)."
  )

  msg_ref <- c(
    msg_1,
    msg_2,
    msg_3,
    msg_4
  )

  expect_checker_false(
    data = dplyr::tibble(
      STUDYID = test_double,
      PARAMCD = character("1"),
      AVISIT = character("1")
    ),
    checker = check_adrs,
    msg = msg_ref
  )

  msg_1 <- paste0("Dataset must have columns 'STUDYID', 'USUBJID',",
                  " but is missing 'USUBJID'."
  )

  msg_2 <- paste0("Column 'STUDYID' must have data type 'factor',",
                  " but is data type 'logical'."
  )

  msg_3 <- paste0("Column 'USUBJID' must have data type 'factor',",
                  " but is data type 'NULL'."
  )

  msg_4 <- paste0("Dataset must have one record per subject,",
                  " but has 1 records (rows) and 0 subjects (USUBJID)."
  )

  msg_ref <- c(
    msg_1,
    msg_2,
    msg_3,
    msg_4
  )

  expect_checker_false(
    data = dplyr::tibble(
      STUDYID = NA,
      PARAMCD = character("1"),
      AVISIT = character("1")
    ),
    checker = check_adrs,
    msg = msg_ref
  )

  msg_1 <- paste0("Dataset must have columns 'STUDYID', 'USUBJID',",
                  " but is missing 'STUDYID'."
  )

  msg_2 <- paste0("Column 'STUDYID' must have data type 'factor',",
                  " but is data type 'NULL'.")

  msg_ref <- c(
    msg_1,
    msg_2
  )

  expect_checker_false(
    data = dplyr::tibble(
      USUBJID = test_sex,
      PARAMCD = character("1"),
      AVISIT = character("1")
    ),
    checker = check_adrs,
    msg = msg_ref
  )

  msg_1 <- paste0("Dataset must have columns 'STUDYID', 'USUBJID',",
                  " but is missing 'STUDYID'."
  )

  msg_2 <- paste0("Column 'STUDYID' must have data type 'factor',",
                  " but is data type 'NULL'."
  )

  msg_ref <- c(
    msg_1,
    msg_2
  )

  expect_checker_false(
    data = dplyr::tibble(
      USUBJID = test_sex,
      PARAMCD = character("1"),
      AVISIT = character("1")
    ),
    checker = check_adrs,
    msg = msg_ref
  )

  msg_1 <- paste0("Dataset must have columns 'STUDYID', 'USUBJID',",
                  " but is missing 'STUDYID'."
  )

  msg_2 <- paste0("Column 'STUDYID' must have data type 'factor',",
                  " but is data type 'NULL'."
  )

  msg_3 <- paste0("Column 'USUBJID' must have data type 'factor',",
                  " but is data type 'Date'."
  )

  msg_ref <- c(
    msg_1,
    msg_2,
    msg_3
  )

  expect_checker_false(
    data = dplyr::tibble(
      USUBJID = test_date,
      PARAMCD = character("1"),
      AVISIT = character("1")
    ),
    checker = check_adrs,
    msg = msg_ref
  )

  msg_1 <- paste0("Dataset must have columns 'STUDYID', 'USUBJID',",
                  " but is missing 'STUDYID'."
  )

  msg_2 <- paste0("Column 'STUDYID' must have data type 'factor',",
                  " but is data type 'NULL'."
  )

  msg_3 <- paste0("Column 'USUBJID' must have data type 'factor',",
                  " but is data type 'POSIXct', 'POSIXt'."
  )

  msg_ref <- c(
    msg_1,
    msg_2,
    msg_3
  )

  expect_checker_false(
    data = dplyr::tibble(
      USUBJID = test_datetime,
      PARAMCD = character("1"),
      AVISIT = character("1")
    ),
    checker = check_adrs,
    msg = msg_ref
  )

  msg_1 <- paste0("Dataset must have columns 'STUDYID', 'USUBJID',",
                  " but is missing 'STUDYID'."
  )

  msg_2 <- paste0("Column 'STUDYID' must have data type 'factor',",
                  " but is data type 'NULL'."
  )

  msg_3 <- paste0("Column 'USUBJID' must have data type 'factor',",
                  " but is data type 'integer'."
  )

  msg_ref <- c(
    msg_1,
    msg_2,
    msg_3
  )

  expect_checker_false(
    data = dplyr::tibble(
      USUBJID = test_integer,
      PARAMCD = character("1"),
      AVISIT = character("1")
    ),
    checker = check_adrs,
    msg = msg_ref
  )

  msg_1 <- paste0("Dataset must have columns 'STUDYID', 'USUBJID',",
                  " but is missing 'STUDYID'."
  )

  msg_2 <- paste0("Column 'STUDYID' must have data type 'factor',",
                  " but is data type 'NULL'."
  )

  msg_3 <- paste0("Column 'USUBJID' must have data type 'factor',",
                  " but is data type 'numeric'."
  )

  msg_ref <- c(
    msg_1,
    msg_2,
    msg_3
  )

  expect_checker_false(
    data = dplyr::tibble(
      USUBJID = test_double,
      PARAMCD = character("1"),
      AVISIT = character("1")
    ),
    checker = check_adrs,
    msg = msg_ref
  )

  msg_1 <- paste0("Dataset must have columns 'STUDYID', 'USUBJID',",
                  " but is missing 'STUDYID'."
  )

  msg_2 <- paste0("Column 'STUDYID' must have data type 'factor',",
                  " but is data type 'NULL'."
  )

  msg_3 <- paste0("Column 'USUBJID' must have data type 'factor',",
                  " but is data type 'logical'."
  )

  msg_ref <- c(
    msg_1,
    msg_2,
    msg_3
  )

  expect_checker_false(
    data = dplyr::tibble(
      USUBJID = NA,
      PARAMCD = character("1"),
      AVISIT = character("1")
    ),
    checker = check_adrs,
    msg = msg_ref
  )

})

test_that("'msg' describes record and subject counts when check fails", {

  adam <- dplyr::tibble(
    STUDYID = "1",
    USUBJID = c("1", "1"),
    col_a = c("a", "b")
    )
  expect_checker_false(
    data = adam,
    checker = check_adrs,
    msg = stringr::str_c("Dataset must have one record per subject,",
                         " but has 2 records (rows) and 1 subjects (USUBJID).")
  )

  adam <- dplyr::tibble(
    STUDYID = "1",
    USUBJID = c("1", "1", "2"),
    col_a = c("a", "b", "c"))

  expect_checker_false(
    data = adam,
    checker = check_adrs,
    msg = stringr::str_c("Dataset must have one record per subject,",
                         " but has 3 records (rows) and 2 subjects (USUBJID).")
  )
})

test_that("returns 'TRUE' with valid tibble", {
  dplyr::tibble(
    STUDYID = character("1"),
    USUBJID = character("1"),
    PARAMCD = character("1"),
    AVISIT = character("1")
  ) |>
    check_adrs() |>
    expect_true()

  dplyr::tibble(
    STUDYID = character("1"),
    USUBJID = character("1")
  ) |>
    check_adrs() |>
    expect_true()

  dplyr::tibble(
    STUDYID = character("1"),
    USUBJID = character("1"),
    PARAMCD = character("1")
  ) |>
    check_adrs() |>
    expect_true()

  dplyr::tibble(
    STUDYID = character("1"),
    USUBJID = character("1"),
    AVISIT = character("1")
  ) |>
    check_adrs() |>
    expect_true()


  dplyr::tibble(
      STUDYID = test_sex,
      USUBJID = test_sex,
      PARAMCD = character("1"),
      AVISIT = character("1")
    ) |>
      check_adrs() |>
      expect_true()

  dplyr::tibble(
    STUDYID = "1",
    USUBJID = c("1", "2"),
    col_a = c("a", "b")) |>
    check_adrs() |>
    expect_true()

  dplyr::tibble(
    STUDYID = "1",
    USUBJID = c("1", "2", "3"),
    col_a = c("a", "b", "c")) |>
    check_adrs() |>
    expect_true()
})

test_that("'msg' attribute is 'NULL' when check passes", {
  expect_checker_true(
    data = dplyr::tibble(
      STUDYID = character("1"),
      USUBJID = character("1"),
      PARAMCD = character("1"),
      AVISIT = character("1")
    ),
    checker = check_adrs
  )

  expect_checker_true(
    data = dplyr::tibble(
      STUDYID = character("1"),
      USUBJID = character("1"),
      AVISIT = character("1")
    ),
    checker = check_adrs
  )

  expect_checker_true(
    data = dplyr::tibble(
      STUDYID = test_sex,
      USUBJID = test_sex,
      PARAMCD = character("1"),
      AVISIT = character("1")
    ),
    checker = check_adrs
  )

  expect_checker_true(
    data = dplyr::tibble(
      "STUDYID" = "1",
      "USUBJID" = "1",
      "PARAMCD" = "1",
      "AVISIT" = "1"
    ),
    checker = check_adrs
  )

  expect_checker_true(
    data = dplyr::tibble(
      "STUDYID" = "STUDY1",
      "USUBJID" = "SUBJECT01",
      "PARAMCD" = "1",
      "AVISIT" = "1"
    ),
    checker = check_adrs
  )

  expect_checker_true(
   data = dplyr::tibble(
    STUDYID = "1",
    USUBJID = c("1", "2"),
    col_a = c("a", "b")
    ),
   checker = check_adrs
  )

  expect_checker_true(
    data = dplyr::tibble(
      STUDYID = "1",
      USUBJID = c("1", "2", "3"),
      col_a = c("a", "b", "c")
    ),
    checker = check_adrs
  )

})
