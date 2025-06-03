test_that("fails if input isn't a dataframe or tibble", {
  check_has_one_record_per_subject(1) |>
    expect_error_message(
      "Argument 'adam' is invalid. Must be of type 'data.frame', not 'double'."
    )

  check_has_one_record_per_subject(TRUE) |>
    expect_error_message(
      "Argument 'adam' is invalid. Must be of type 'data.frame', not 'logical'."
    )

  check_has_one_record_per_subject("a") |>
    expect_error_message(
      "Argument 'adam' is invalid. Must be of type 'data.frame', not 'character'."
    )

  check_has_one_record_per_subject(NULL) |>
    expect_error_message(
      "Argument 'adam' is invalid. Must be of type 'data.frame', not 'NULL'."
    )

  check_has_one_record_per_subject(list(USUBJID = 1)) |>
    expect_error_message(
      "Argument 'adam' is invalid. Must be of type 'data.frame', not 'list'."
    )
})

test_that("returns 'error' if USUBJID is missing", {
  dplyr::tibble(USUBJID = NULL) |>
    check_has_one_record_per_subject() |>
    expect_error()
})

test_that("returns 'FALSE' if a tibble has more than one record per subject", {
  dplyr::tibble(USUBJID = c(1, 1)) |>
    check_has_one_record_per_subject() |>
    expect_false()

  dplyr::tibble(USUBJID = c(1, 1), col_a = c("a", "b")) |>
    check_has_one_record_per_subject() |>
    expect_false()
})

test_that("'msg' describes record and subject counts when check fails", {

  adam <- dplyr::tibble(USUBJID = c(1, 1), col_a = c("a", "b"))
  expect_checker_false(
    data = adam,
    checker = check_has_one_record_per_subject,
    msg = stringr::str_c("Dataset must have one record per subject,",
                " but has 2 records (rows) and 1 subjects (USUBJID).")
  )

  adam <- dplyr::tibble(USUBJID = c(1, 1, 2), col_a = c("a", "b", "c"))
  expect_checker_false(
    data = adam,
    checker = check_has_one_record_per_subject,
    msg = stringr::str_c("Dataset must have one record per subject,",
                         " but has 3 records (rows) and 2 subjects (USUBJID).")
  )

  adam <- dplyr::tibble(USUBJID = c(1, 1, 2, 2), col_a = c("a", "b", "c", "d"))
  expect_checker_false(
    data = adam,
    checker = check_has_one_record_per_subject,
    msg = stringr::str_c("Dataset must have one record per subject,",
                         " but has 4 records (rows) and 2 subjects (USUBJID).")
  )
})

test_that("returns 'TRUE' if a tibble has one record per subject", {
  dplyr::tibble(USUBJID = 1) |>
    check_has_one_record_per_subject() |>
    expect_true()

  dplyr::tibble(USUBJID = c(1, 2), col_a = c("a", "b")) |>
    check_has_one_record_per_subject() |>
    expect_true()
})

test_that("'msg' attribute is 'NULL' when check passes", {

  adam <- dplyr::tibble(USUBJID = 1)
  expect_checker_true(
    data = adam,
    checker = check_has_one_record_per_subject
  )

  adam <- dplyr::tibble(USUBJID = c(1, 2), col_a = c("a", "b"))
  expect_checker_true(
    data = adam,
    checker = check_has_one_record_per_subject
  )

  adam <- dplyr::tibble(USUBJID = c(1, 2, 3), col_a = c("a", "b", "c"))
  expect_checker_true(
    data = adam,
    checker = check_has_one_record_per_subject
  )
})
