test_that("returned function fails with something other than ADaM dataframe", {
  dplyr::tibble(col_a = double()) |>
    check_adlb() |>
    expect_error(
      "Argument 'adam' is invalid. Must have at least 1 rows, but has 0 rows."
    )

  dplyr::tibble(col_a = factor()) |>
    check_adlb() |>
    expect_error(
      "Argument 'adam' is invalid. Must have at least 1 rows, but has 0 rows."
    )

  adam <- dplyr::tibble(col_a = logical()) |>
    check_adlb() |>
    expect_error(
      "Argument 'adam' is invalid. Must have at least 1 rows, but has 0 rows."
    )

  dplyr::tibble(col_a = integer()) |>
    check_adlb() |>
    expect_error(
      "Argument 'adam' is invalid. Must have at least 1 rows, but has 0 rows."
    )

  list(col_a = 1) |>
    check_adlb() |>
    expect_error(
      "Argument 'adam' is invalid. Must be of type 'data.frame', not 'list'."
    )

  NULL |>
    check_adlb() |>
    expect_error(
      "Argument 'adam' is invalid. Must be of type 'data.frame', not 'NULL'."
    )
})

test_that("returns 'FALSE' with invalid column headings", {
  dplyr::tibble(
    STUDYID = character("1"),
    ID = character("1")
  ) |>
    check_adlb() |>
    expect_false()

  dplyr::tibble(
    ID = character("1"),
    USUBJID = character("1")
  ) |>
    check_adlb() |>
    expect_false()

  dplyr::tibble(
    STUDYID = character("1"),
    USUBJID = character("1")
  ) |>
    check_adlb() |>
    expect_false()


  dplyr::tibble(
    USUBJID = character("1"),
    PARAMCD = character("1"),
    AVISIT = character("1"),
    ASEQ = character("1")
  ) |>
    check_adlb() |>
    expect_false()

  dplyr::tibble(
    STUDYID = character("1"),
    PARAMCD = character("1"),
    AVISIT = character("1"),
    ASEQ = character("1")
  ) |>
    check_adlb() |>
    expect_false()

  dplyr::tibble(
    PARAMCD = character("1"),
    AVISIT = character("1"),
    ASEQ = character("1")
  ) |>
    check_adlb() |>
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
    checker = check_adlb,
    msg = msg_ref
  )

  msg_1 <- paste0(
    "Dataset must have columns 'STUDYID', 'USUBJID',",
    " but is missing 'STUDYID'."
  )

  msg_2 <- paste0("Column 'STUDYID' must have data type 'factor',",
                  " but is data type 'NULL'.")

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
    checker = check_adlb,
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


  msg_3 <- paste0("Dataset must have one record per subject per param,",
                  " but is missing column 'USUBJID' or 'PARAMCD'."
                  )

  msg_ref <- c(
    msg_1,
    msg_2,
    msg_3
  )

  expect_checker_false(
    data = dplyr::tibble(
      "STUDYID" = 1L,
      "USUBJID" = 1L
    ),
    checker = check_adlb,
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

  msg_4 <- paste0(
    "Dataset must have one record per subject per param,",
    " but is missing column 'USUBJID' or 'PARAMCD'."
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
      AVISITN = character("1")
    ),
    checker = check_adlb,
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
      USUBJID = character("1")
    ),
    checker = check_adlb,
    msg = msg_ref
  )

  msg_1 <- paste0(
    "Dataset must have columns 'STUDYID', 'USUBJID',",
    " but is missing 'USUBJID'."
    )

  msg_2 <- paste0(
    "Column 'USUBJID' must have data type 'factor',",
                  " but is data type 'NULL'."
    )

  msg_3 <- paste0("Dataset must have one record per subject per param,",
                  " but is missing column 'USUBJID' or 'PARAMCD'.")

  msg_ref <- c(
    msg_1,
    msg_2,
    msg_3
  )

  expect_checker_false(
    data = dplyr::tibble(
      STUDYID = "1",
      PARAMCD = character("1"),
      AVISITN = character("1")
    ),
    checker = check_adlb,
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
      USUBJID = test_sex,
      PARAMCD = character("1"),
      AVISIT = character("1")
    ),
    checker = check_adlb,
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
    "Column 'PARAM' must have data type 'character',",
    " but is data type 'NULL'."
  )

  msg_4 <- paste0(
    "Column 'PARAMN' must have data type 'integer'",
    " or 'whole number double',",
    " but is data type 'NULL'."
  )

  msg_5 <- paste0(
    "Column 'AVISITN' must have data type 'double',",
    " but is data type 'character'."
  )

  msg_6 <- paste0(
    "Column 'ADTM' must have data type 'integer'",
    " or 'whole number double',",
    " but is data type 'NULL'."
  )

  msg_7 <- paste0(
    "Column 'ADT' must have data type 'integer'",
    " or 'whole number double',",
    " but is data type 'NULL'."
  )

  msg_ref <- c(
    msg_1,
    msg_2
  )

  expect_checker_false(
    data = dplyr::tibble(
      USUBJID = "1",
      PARAMCD = character("1"),
      AVISITN = character("1")
    ),
    checker = check_adlb,
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
      STUDYID = test_sex,
      USUBJID = test_sex
    ),
    checker = check_adlb,
    msg = msg_ref
  )

  msg_1 <- paste0(
    "Column 'USUBJID' must have data type 'factor',",
    " but is data type 'Date'."
  )

  msg_2 <- paste0(
    "Dataset must have one record per subject per param,",
    " but is missing column 'USUBJID' or 'PARAMCD'."
  )
  msg_ref <- c(
    msg_1,
    msg_2
  )

  expect_checker_false(
    data = dplyr::tibble(
      "STUDYID" = test_sex,
      "USUBJID" = test_date
    ),
    checker = check_adlb,
    msg = msg_ref
  )

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
      STUDYID = test_sex,
      PARAMCD = character("1"),
      AVISITN = character("1")
    ),
    checker = check_adlb,
    msg = msg_ref
  )

  msg_1 <- paste0(
    "Dataset must have columns 'STUDYID', 'USUBJID',",
    " but is missing 'USUBJID'."
  )

  msg_2 <- paste0(
    "Column 'STUDYID' must have data type 'factor',",
    " but is data type 'Date'."
  )

  msg_3 <- paste0(
    "Column 'USUBJID' must have data type 'factor',",
    " but is data type 'NULL'."
  )

  msg_4 <- paste0(
    "Dataset must have one record per subject per param,",
    " but is missing column 'USUBJID' or 'PARAMCD'."
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
      AVISITN = character("1")
    ),
    checker = check_adlb,
    msg = msg_ref
  )

  msg_1 <- paste0(
    "Dataset must have columns 'STUDYID', 'USUBJID',",
    " but is missing 'USUBJID'."
  )

  msg_2 <- paste0(
    "Column 'STUDYID' must have data type 'factor',",
    " but is data type 'POSIXct', 'POSIXt'."
  )

  msg_3 <- paste0(
    "Column 'USUBJID' must have data type 'factor',",
    " but is data type 'NULL'."
  )

  msg_4 <- paste0(
    "Dataset must have one record per subject per param,",
    " but is missing column 'USUBJID' or 'PARAMCD'."
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
      AVISITN = character("1")
    ),
    checker = check_adlb,
    msg = msg_ref
  )

  msg_1 <- paste0(
    "Dataset must have columns 'STUDYID', 'USUBJID',",
    " but is missing 'USUBJID'."
  )

  msg_2 <- paste0(
    "Column 'STUDYID' must have data type 'factor',",
    " but is data type 'integer'."
  )

  msg_3 <- paste0(
    "Column 'USUBJID' must have data type 'factor',",
    " but is data type 'NULL'."
  )

  msg_4 <- paste0(
    "Dataset must have one record per subject per param,",
    " but is missing column 'USUBJID' or 'PARAMCD'."
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
      AVISITN = character("1")
    ),
    checker = check_adlb,
    msg = msg_ref
  )

  msg_1 <- paste0(
    "Dataset must have columns 'STUDYID', 'USUBJID',",
    " but is missing 'USUBJID'."
  )

  msg_2 <- paste0(
    "Column 'STUDYID' must have data type 'factor',",
    " but is data type 'numeric'."
  )

  msg_3 <- paste0(
    "Column 'USUBJID' must have data type 'factor',",
    " but is data type 'NULL'."
  )

  msg_4 <- paste0(
    "Dataset must have one record per subject per param,",
    " but is missing column 'USUBJID' or 'PARAMCD'."
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
      AVISITN = character("1")
    ),
    checker = check_adlb,
    msg = msg_ref
  )

  msg_1 <- paste0(
    "Dataset must have columns 'STUDYID', 'USUBJID',",
    " but is missing 'USUBJID'."
  )

  msg_2 <- paste0(
    "Column 'STUDYID' must have data type 'factor',",
    " but is data type 'logical'."
  )

  msg_3 <- paste0(
    "Column 'USUBJID' must have data type 'factor',",
    " but is data type 'NULL'."
  )

  msg_4 <- paste0(
    "Dataset must have one record per subject per param,",
    " but is missing column 'USUBJID' or 'PARAMCD'."
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
      AVISITN = character("1")
    ),
    checker = check_adlb,
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
      USUBJID = test_sex,
      PARAMCD = character("1"),
      AVISITN = character("1")
    ),
    checker = check_adlb,
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
    "Column 'USUBJID' must have data type 'factor',",
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
      AVISITN = character("1")
    ),
    checker = check_adlb,
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
    "Column 'USUBJID' must have data type 'factor',",
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
      AVISITN = character("1")
    ),
    checker = check_adlb,
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
    "Column 'USUBJID' must have data type 'factor',",
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
      AVISITN = character("1")
    ),
    checker = check_adlb,
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
    "Column 'USUBJID' must have data type 'factor',",
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
      AVISITN = character("1")
    ),
    checker = check_adlb,
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
    "Column 'USUBJID' must have data type 'factor',",
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
      AVISITN = character("1")
    ),
    checker = check_adlb,
    msg = msg_ref
  )

  msg_1 <- paste0(
    "Column 'STUDYID' must have data type 'factor',",
    " but is data type 'POSIXct', 'POSIXt'."
  )

  msg_2 <- paste0(
    "Column 'USUBJID' must have data type 'factor',",
    " but is data type 'Date'."
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
      "STUDYID" = test_datetime,
      "USUBJID" = test_date
    ),
    checker = check_adlb,
    msg = msg_ref
  )

  msg_1 <- paste0(
    "Column 'STUDYID' must have data type 'factor',",
    " but is data type 'integer'."
  )

  msg_2 <- paste0(
    "Column 'USUBJID' must have data type 'factor',",
    " but is data type 'numeric'."
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
      "STUDYID" = test_integer,
      "USUBJID" = test_double
    ),
    checker = check_adlb,
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
      AVISITN = character("1"),
      ADTM = character("1")
    ),
    checker = check_adlb,
    msg = msg_ref
  )
})

test_that("'msg' describes record and subject counts when check fails", {

  adam <- dplyr::tibble(
    STUDYID = "1",
    USUBJID = c("1", "1"),
    PARAMCD = c("a", "a"),
    PARAM = c("a", "b"),
    PARAMN = c(1, 2),
    AVISITN = c(1, 1),
    ADTM = c(1, 2),
    ADT = c(1, 2)
  )

  msg_ref <- paste0(
    "Dataset must have one record per subject per param,",
    " but param 'PARAMCD' has 2 records (rows) and 1 subjects (USUBJID)."
  )

  expect_checker_false(
    data = adam,
    checker = check_adlb,
    msg = msg_ref
  )

  adam <- dplyr::tibble(
    STUDYID = "1",
    USUBJID = c("1", "1", "2"),
    PARAMCD = c("a", "a", "a"),
    PARAM = c("a", "b", "c"),
    PARAMN = c(1, 2, 3),
    AVISITN = c(1, 2, 3),
    ADTM = c(1, 2, 3),
    ADT = c(1, 2, 3)
  )

  msg_ref <- paste0(
    "Dataset must have one record per subject per param,",
    " but param 'PARAMCD' has 3 records (rows) and 2 subjects (USUBJID)."
  )

  expect_checker_false(
    data = adam,
    checker = check_adlb,
    msg = msg_ref
  )
})


test_that("returns 'TRUE' with valid tibble", {

  dplyr::tibble(
    STUDYID = character("1"),
    USUBJID = character("1"),
    PARAMCD = character("1")
  ) |>
    check_adlb() |>
    expect_true()


  dplyr::tibble(
    STUDYID = character("1"),
    USUBJID = character("1"),
    PARAMCD = character("1"),
    AVISIT = character("1")
  ) |>
    check_adlb() |>
    expect_true()

  dplyr::tibble(
      STUDYID = character("1"),
      USUBJID = character("1"),
      PARAMCD = character("1"),
      AVISITN = character("1")
    ) |>
     check_adlb() |>
     expect_true()

  dplyr::tibble(
    STUDYID = character("1"),
    USUBJID = character("1"),
    PARAMCD = character("1"),
    AVISIT = character("1"),
    ASEQ = character("1")
  ) |>
    check_adlb() |>
    expect_true()

  dplyr::tibble(
    STUDYID = character("1"),
    USUBJID = character("1"),
    PARAMCD = character("1"),
    PARAM = character("1"),
    PARAMN = integer("1"),
    AVISITN = numeric("1"),
    ADTM = integer("1"),
    ADT = integer("1")
  ) |>
    check_adlb() |>
    expect_true()

  dplyr::tibble(
    STUDYID = "1",
    USUBJID = "1",
    PARAMCD = "1",
    PARAM = "1",
    PARAMN = 1,
    AVISITN = 1,
    ADTM = 1,
    ADT = 1
  ) |>
    check_adlb() |>
    expect_true()

  dplyr::tibble(
    STUDYID = "1",
    USUBJID = c("1", "2"),
    PARAMCD = c("a", "b"),
    PARAM = c("a", "b"),
    PARAMN = c(1, 2),
    AVISITN = c(1, 2),
    ADTM = c(1, 2),
    ADT = c(1, 2)
  ) |>
    check_adlb() |>
    expect_true()

  dplyr::tibble(
    STUDYID = "1",
    USUBJID = c("1", "2", "3"),
    PARAMCD = c("a", "b", "c"),
    PARAM = c("a", "b", "c"),
    PARAMN = c(1, 2, 3),
    AVISITN = c(1, 2, 3),
    ADTM = c(1, 2, 3),
    ADT = c(1, 2, 3)
  ) |>
    check_adlb() |>
    expect_true()

})

test_that("'msg' attribute is 'NULL' when check passes", {

  expect_checker_true(
    data = dplyr::tibble(
      STUDYID = character("1"),
      USUBJID = character("1"),
      PARAMCD = character("1")
    ),
    checker = check_adlb
  )

  expect_checker_true(
    data = dplyr::tibble(
      STUDYID = character("1"),
      USUBJID = character("1"),
      PARAMCD = character("1"),
      AVISIT = character("1")
    ),
    checker = check_adlb
  )

  expect_checker_true(
    data = dplyr::tibble(
      STUDYID = character("1"),
      USUBJID = character("1"),
      PARAMCD = character("1"),
      AVISITN = character("1")
    ),
    checker = check_adlb
  )

  expect_checker_true(
    data = dplyr::tibble(
      STUDYID = character("1"),
      USUBJID = character("1"),
      PARAMCD = character("1"),
      AVISIT = character("1"),
      ASEQ = character("1")
    ),
    checker = check_adlb
  )

  expect_checker_true(
    data = dplyr::tibble(
      STUDYID = character("1"),
      USUBJID = character("1"),
      PARAMCD = character("1"),
      PARAM = character("1"),
      PARAMN = integer("1"),
      AVISITN = numeric("1"),
      ADTM = integer("1"),
      ADT = integer("1")
    ),
    checker = check_adlb
  )

  expect_checker_true(
    data = dplyr::tibble(
      STUDYID = "1",
      USUBJID = c("1", "2"),
      PARAMCD = c("a", "b"),
      PARAM = c("a", "b"),
      PARAMN = c(1, 2),
      AVISITN = c(1, 2),
      ADTM = c(1, 2),
      ADT = c(1, 2)
    ),
    checker = check_adlb
  )

  expect_checker_true(
    data = dplyr::tibble(
      STUDYID = "1",
      USUBJID = c("1", "2", "3"),
      PARAMCD = c("a", "b", "c"),
      PARAM = c("a", "b", "c"),
      PARAMN = c(1, 2, 3),
      AVISITN = c(1, 2, 3),
      ADTM = c(1, 2, 3),
      ADT = c(1, 2, 3)
    ),
    checker = check_adlb
  )
})
