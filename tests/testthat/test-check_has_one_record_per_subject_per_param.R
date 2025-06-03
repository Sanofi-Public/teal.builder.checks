test_that("fails if input isn't a dataframe or tibble", {
  check_has_one_record_per_subject_per_param(1) |>
    expect_error_message(
      "Argument 'adam' is invalid. Must be of type 'data.frame', not 'double'."
    )

  check_has_one_record_per_subject_per_param(TRUE) |>
    expect_error_message(
      "Argument 'adam' is invalid. Must be of type 'data.frame', not 'logical'."
    )

  check_has_one_record_per_subject_per_param("a") |>
    expect_error_message(
      "Argument 'adam' is invalid. Must be of type 'data.frame', not 'character'."
    )

  check_has_one_record_per_subject_per_param(list(USUBJID = 1)) |>
    expect_error_message(
      "Argument 'adam' is invalid. Must be of type 'data.frame', not 'list'."
    )
})

test_that("returns appropriate error message if tibble is missing 'USUBJID' or 'PARAMCD'", {
  expect_checker_false(
    data = dplyr::tibble(col_a = 1),
    checker = check_has_one_record_per_subject_per_param,
    msg = str_c(
      "Dataset must have one record per subject per param, but is missing",
      " column 'USUBJID' or 'PARAMCD'."
    )
  )

  expect_checker_false(
    data = dplyr::tibble(STUDYID = 1, USUBJID = 1),
    checker = check_has_one_record_per_subject_per_param,
    msg = str_c(
      "Dataset must have one record per subject per param, but is missing",
      " column 'USUBJID' or 'PARAMCD'."
    )
  )

  expect_checker_false(
    data = dplyr::tibble(STUDYID = 1, PARAMCD = 1),
    checker = check_has_one_record_per_subject_per_param,
    msg = str_c(
      "Dataset must have one record per subject per param, but is missing",
      " column 'USUBJID' or 'PARAMCD'."
    )
  )
})

test_that("returns 'FALSE' if a tibble has more than one record per subject per param", {
  dplyr::tibble(USUBJID = c(1, 1)) |>
    check_has_one_record_per_subject_per_param() |>
    expect_false()

  dplyr::tibble(STUDYID = c(1, 1), USUBJID = c(1, 1), PARAMCD = c(1, 1)) |>
    check_has_one_record_per_subject_per_param() |>
    expect_false()

  dplyr::tibble(STUDYID = c(1, 1, 1), USUBJID = c(1, 1, 1), PARAMCD = c(1, 1, 1)) |>
    check_has_one_record_per_subject_per_param() |>
    expect_false()

  dplyr::tibble(STUDYID = c(1, 1, 1), USUBJID = c(1, 1, 1), PARAMCD = c(1, 1, 2)) |>
    check_has_one_record_per_subject_per_param() |>
    expect_false()
})

test_that("returns appropriate error message if a tibble has more than one record per subject per param", {
  expect_checker_false(
    data = dplyr::tibble(USUBJID = c(1, 1)),
    checker = check_has_one_record_per_subject_per_param,
    msg = str_c(
      "Dataset must have one record per subject per param, but is missing",
      " column 'USUBJID' or 'PARAMCD'."
    )
  )

  expect_checker_false(
    data = dplyr::tibble(USUBJID = c(1, 1), PARAMCD = c(1, 1)),
    checker = check_has_one_record_per_subject_per_param,
    msg = str_c(
      "Dataset must have one record per subject per param,",
      " but param 'PARAMCD' has 2 records (rows) and 1 subjects (USUBJID)."
    )
  )
})
test_that("returns 'TRUE' if a tibble has one record per subject per param", {
  dplyr::tibble(STUDYID = 1, USUBJID = 1, PARAMCD = 1) |>
    check_has_one_record_per_subject_per_param() |>
    expect_true()

  dplyr::tibble(USUBJID = c(1, 1), PARAMCD = c(1, 2)) |>
    check_has_one_record_per_subject_per_param() |>
    expect_true()

  dplyr::tibble(STUDYID = c(1, 1), USUBJID = c(1, 1), PARAMCD = c(1, 2)) |>
    check_has_one_record_per_subject_per_param() |>
    expect_true()

  dplyr::tibble(
    STUDYID = c(1, 1, 1),
    USUBJID = c(1, 2, 1),
    PARAMCD = c(1, 1, 2)
  ) |>
    check_has_one_record_per_subject_per_param() |>
    expect_true()

  dplyr::tibble(STUDYID = c(1, 2), USUBJID = c(1, 2), PARAMCD = c(1, 2)) |>
    check_has_one_record_per_subject_per_param() |>
    expect_true()

  dplyr::tibble(STUDYID = c(1, 1), USUBJID = c(1, 2), PARAMCD = c(1, 2)) |>
    check_has_one_record_per_subject_per_param() |>
    expect_true()

  dplyr::tibble(STUDYID = c(1, 1), USUBJID = c(1, 2), PARAMCD = c(1, 1)) |>
    check_has_one_record_per_subject_per_param() |>
    expect_true()

  dplyr::tibble(
    STUDYID = c(1, 1, 1),
    USUBJID = c(1, 2, 3),
    PARAMCD = c(1, 1, 2)
  ) |>
    check_has_one_record_per_subject_per_param() |>
    expect_true()
})

test_that("'msg' attribute is 'NULL' when check passes", {
  expect_checker_true(
    data = dplyr::tibble(STUDYID = 1, USUBJID = 1, PARAMCD = 1),
    checker = check_has_one_record_per_subject_per_param
  )

  expect_checker_true(
    data = dplyr::tibble(USUBJID = c(1, 1), PARAMCD = c(1, 2)),
    checker = check_has_one_record_per_subject_per_param
  )

  expect_checker_true(
    data = dplyr::tibble(
      STUDYID = c(1, 2),
      USUBJID = c(1, 2),
      PARAMCD = c(1, 2)
    ),
    checker = check_has_one_record_per_subject_per_param
  )

  expect_checker_true(
    data = dplyr::tibble(
      STUDYID = c(1, 1),
      USUBJID = c(1, 2),
      PARAMCD = c(1, 2)
    ),
    checker = check_has_one_record_per_subject_per_param
  )

  expect_checker_true(
    data = dplyr::tibble(
      STUDYID = c(1, 1),
      USUBJID = c(1, 2),
      PARAMCD = c(1, 1)
    ),
    checker = check_has_one_record_per_subject_per_param
  )

  expect_checker_true(
    data = dplyr::tibble(
      STUDYID = c(1, 1, 1),
      USUBJID = c(1, 2, 1),
      PARAMCD = c(1, 1, 2)
    ),
    checker = check_has_one_record_per_subject_per_param
  )

  expect_checker_true(
    data = dplyr::tibble(
      STUDYID = c(1, 1, 1),
      USUBJID = c(1, 2, 3),
      PARAMCD = c(1, 1, 2)
    ),
    checker = check_has_one_record_per_subject_per_param
  )
})
