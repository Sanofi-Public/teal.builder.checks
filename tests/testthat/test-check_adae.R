test_that("returned function fails with something other than ADaM dataframe", {
  dplyr::tibble(col_a = double()) |>
    check_adae() |>
    expect_error(
      "Argument 'adam' is invalid. Must have at least 1 rows, but has 0 rows."
    )

  dplyr::tibble(col_a = factor()) |>
    check_adae() |>
    expect_error(
      "Argument 'adam' is invalid. Must have at least 1 rows, but has 0 rows."
    )

  dplyr::tibble(col_a = logical()) |>
    check_adae() |>
    expect_error(
      "Argument 'adam' is invalid. Must have at least 1 rows, but has 0 rows."
    )

  dplyr::tibble(col_a = integer()) |>
    check_adae() |>
    expect_error(
      "Argument 'adam' is invalid. Must have at least 1 rows, but has 0 rows."
    )

  list(col_a = 1) |>
    check_adae() |>
    expect_error(
      "Argument 'adam' is invalid. Must be of type 'data.frame', not 'list'."
    )

  NULL |>
    check_adae() |>
    expect_error(
      "Argument 'adam' is invalid. Must be of type 'data.frame', not 'NULL'."
    )
})

test_that("returns 'FALSE' with invalid column headings", {
  dplyr::tibble(
    STUDYID = character("1"),
    ID = character("1")
  ) |>
    check_adae() |>
    expect_false()

  dplyr::tibble(
    ID = character("1"),
    USUBJID = character("1")
  ) |>
    check_adae() |>
    expect_false()

  dplyr::tibble(
    STUDYID = "1",
    USUBJID = c("1", "1"),
    AEDECOD = c("1", "1"),
    AEBODSYS = c("1", "1"),
    AEREFID = c("1", "1"),
    AECYCVID = c("1", "1"),
    AESER = c("1", "1"),
    ASTDT = c(1L, 1L)
  ) |>
    check_adae() |>
    expect_false()

  dplyr::tibble(
    STUDYID = "1",
    USUBJID = c("1", "2", "2"),
    AEDECOD = c("1", "1", "2"),
    AEBODSYS = c("1", "1", "2"),
    AEREFID = c("1", "1", "2"),
    AECYCVID = c("1", "1", "2"),
    AESER = c("1", "1", "2"),
    ASTDT = c(1L, 1L, 2L)
  ) |>
    check_adae() |>
    expect_false()
})

test_that("returns appropriate message with invalid column headings", {
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
      STUDYID = character("1"),
      ID = character("1")
    ),
    checker = check_adae,
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
      ID = character("1"),
      USUBJID = character("1")
    ),
    checker = check_adae,
    msg = msg_ref
  )


  msg_1 <- paste0("Column 'STUDYID' must have data type 'factor',",
                  " but is data type 'integer'.")
  msg_2 <- paste0("Column 'USUBJID' must have data type 'factor',",
                  " but is data type 'integer'.")

  msg_ref <- c(
    msg_1,
    msg_2
  )

  expect_checker_false(
    data = dplyr::tibble(
      "STUDYID" = 1L,
      "USUBJID" = 1L,
      "AESEQ" = 1L,
      "ASEQ" = 1L
    ),
    checker = check_adae,
    msg = msg_ref
  )
})

test_that("returns 'FALSE' if a tibble has more than one record per subject", {

  dplyr::tibble(
    STUDYID = "1",
    USUBJID = c("1", "1"),
    AEDECOD = c("1", "1"),
    AEBODSYS = c("1", "1"),
    AEREFID = c("1", "1"),
    AECYCVID = c("1", "1"),
    AESER = c("1", "1"),
    ASTDT = c(1L, 1L)
  ) |>
  check_adae() |>
  expect_false()

  dplyr::tibble(
    STUDYID = "1",
    USUBJID = c("1", "2", "2"),
    AEDECOD = c("1", "1", "2"),
    AEBODSYS = c("1", "1", "2"),
    AEREFID = c("1", "1", "2"),
    AECYCVID = c("1", "1", "2"),
    AESER = c("1", "1", "2"),
    ASTDT = c(1L, 1L, 2L)
  ) |>
  check_adae() |>
  expect_false()

})

test_that("'msg' describes record and subject counts when check fails", {

  expect_checker_false(
    data = dplyr::tibble(
      STUDYID = "1",
      USUBJID = c("1", "1"),
      AEDECOD = c("1", "1"),
      AEBODSYS = c("1", "1"),
      AEREFID = c("1", "1"),
      AECYCVID = c("1", "1"),
      AESER = c("1", "1"),
      ASTDT = c(1L, 1L)
    ),
    checker = check_adae,
    msg = stringr::str_c("Dataset must have one record per subject,",
                         " but has 2 records (rows) and 1 subjects (USUBJID).")
  )

  expect_checker_false(
    data = dplyr::tibble(
      STUDYID = "1",
      USUBJID = c("1", "2", "2"),
      AEDECOD = c("1", "1", "2"),
      AEBODSYS = c("1", "1", "2"),
      AEREFID = c("1", "1", "2"),
      AECYCVID = c("1", "1", "2"),
      AESER = c("1", "1", "2"),
      ASTDT = c(1L, 1L, 2L)
    ),
    checker = check_adae,
    msg = stringr::str_c("Dataset must have one record per subject,",
                         " but has 3 records (rows) and 2 subjects (USUBJID).")
  )
})

test_that("returns 'TRUE' with valid tibble", {

  dplyr::tibble(
    STUDYID = character("1"),
    USUBJID = character("1")
  ) |>
    check_adae() |>
    expect_true()

  dplyr::tibble(
    STUDYID = character("1"),
    USUBJID = character("1"),
    AEDECOD = character("1")
  ) |>
    check_adae() |>
    expect_true()

  expect_checker_true(
    data = dplyr::tibble(
      STUDYID = character("1"),
      USUBJID = character("1"),
      AEDECOD = character("1"),
      AEBODSYS = character("1")
    ),
    checker = check_adae
  )

  dplyr::tibble(
    STUDYID = character("1"),
    USUBJID = character("1"),
    AEDECOD = character("1"),
    AEBODSYS = character("1"),
    AEREFID = character("1")
  ) |>
    check_adae() |>
    expect_true()

  dplyr::tibble(
    STUDYID = character("1"),
    USUBJID = character("1"),
    AEDECOD = character("1"),
    AEBODSYS = character("1"),
    AEREFID = character("1"),
    AECYCVID = character("1")
  ) |>
    check_adae() |>
    expect_true()

  dplyr::tibble(
    STUDYID = character("1"),
    USUBJID = character("1"),
    AEDECOD = character("1"),
    AEBODSYS = character("1"),
    AEREFID = character("1"),
    AECYCVID = character("1"),
    AESER = character("1")
  ) |>
    check_adae() |>
    expect_true()

  dplyr::tibble(
    STUDYID = character("1"),
    USUBJID = character("1"),
    AEDECOD = character("1"),
    AEBODSYS = character("1"),
    AEREFID = character("1"),
    AECYCVID = character("1"),
    AESER = character("1"),
    ASTDT = integer(1L)
  ) |>
    check_adae() |>
    expect_true()

  dplyr::tibble(
    STUDYID = "1",
    USUBJID = "1",
    AEDECOD = "1",
    AEBODSYS = "1",
    AEREFID = "1",
    AECYCVID = "1",
    AESER = "1",
    ASTDT = 1L
    ) |>
    check_adae() |>
    expect_true()

  dplyr::tibble(
    STUDYID = "1",
    USUBJID = c("1", "2"),
    AEDECOD = c("1", "1"),
    AEBODSYS = c("1", "1"),
    AEREFID = c("1", "1"),
    AECYCVID = c("1", "1"),
    AESER = c("1", "1"),
    ASTDT = c(1L, 1L)
    ) |>
    check_adae() |>
    expect_true()

  dplyr::tibble(
    STUDYID = "1",
    USUBJID = c("1", "2", "3"),
    AEDECOD = c("1", "1", "2"),
    AEBODSYS = c("1", "1", "2"),
    AEREFID = c("1", "1", "2"),
    AECYCVID = c("1", "1", "2"),
    AESER = c("1", "1", "2"),
    ASTDT = c(1L, 1L, 2L)
    ) |>
    check_adae() |>
    expect_true()
})

test_that("'msg' attribute is 'NULL' when check passes", {

  expect_checker_true(
    data = dplyr::tibble(
      STUDYID = character("1"),
      USUBJID = character("1")
    ),
    checker = check_adae
  )

  expect_checker_true(
    data = dplyr::tibble(
      STUDYID = character("1"),
      USUBJID = character("1"),
      AEDECOD = character("1"),
      AEBODSYS = character("1"),
      AEREFID = character("1"),
      AECYCVID = character("1"),
      AESER = character("1"),
      ASTDT = integer(1L)
    ),
    checker = check_adae
  )

  expect_checker_true(
    data = dplyr::tibble(
      STUDYID = "1",
      USUBJID = "1",
      AEDECOD = "1",
      AEBODSYS = "1",
      AEREFID = "1",
      AECYCVID = "1",
      AESER = "1",
      ASTDT = 1L
    ),
    checker = check_adae
  )

  expect_checker_true(
    data = dplyr::tibble(
      STUDYID = "1",
      USUBJID = c("1", "2"),
      AEDECOD = c("1", "1"),
      AEBODSYS = c("1", "1"),
      AEREFID = c("1", "1"),
      AECYCVID = c("1", "1"),
      AESER = c("1", "1"),
      ASTDT = c(1L, 1L)
    ),
    checker = check_adae
  )

  expect_checker_true(
    data = dplyr::tibble(
      STUDYID = "1",
      USUBJID = c("1", "2", "3"),
      AEDECOD = c("1", "1", "2"),
      AEBODSYS = c("1", "1", "2"),
      AEREFID = c("1", "1", "2"),
      AECYCVID = c("1", "1", "2"),
      AESER = c("1", "1", "2"),
      ASTDT = c(1L, 1L, 2L)
    ),
    checker = check_adae
  )
})
