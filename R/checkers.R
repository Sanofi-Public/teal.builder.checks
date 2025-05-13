#' ADaM dataset checker
#'
#' @description checks the ADaM dataset
#'
#' @template adam
#'
#' @template checker_return
#'
#' @export
#'
#' @examples
#' dplyr::tibble(
#'   STUDYID = character("1"),
#'   USUBJID = character("1")
#' ) |>
#'   check_adam()
#'
check_adam <- function(adam) {
  check_has_cols(c("STUDYID", "USUBJID"))(adam)
}

#' ADSL dataset checker
#'
#' @description checks the ADSL dataset
#'
#' @template adam
#'
#' @template checker_return
#'
#' @export
#'
#' @examples
#' dplyr::tibble(
#'   STUDYID = character("1"),
#'   USUBJID = character("1")
#' ) |>
#'   check_adsl()
#'
check_adsl <- function(adam) {
  make_multi_checker(
    check_has_cols(c("STUDYID", "USUBJID")),
    check_is_categorical("STUDYID"),
    check_is_categorical("USUBJID"),
    check_has_one_record_per_subject
  )(adam)
}

#' ADRS dataset checker
#'
#' @template adam
#'
#' @template checker_return
#'
#' @export
#'
#' @examples
#' dplyr::tibble(
#'   STUDYID = character("1"),
#'   USUBJID = character("1")
#' ) |>
#'   check_adrs()
#'
check_adrs <- function(adam) {
  make_multi_checker(
    check_has_cols(c("STUDYID", "USUBJID")),
    check_is_categorical("STUDYID"),
    check_is_categorical("USUBJID"),
    check_has_one_record_per_subject
  )(adam)
}

#' ADTTE dataset checker
#'
#' @template adam
#'
#' @template checker_return
#'
#' @export
#'
#' @examples
#' dplyr::tibble(
#'   STUDYID = character("1"),
#'   USUBJID = character("1"),
#'   PARAM  = character("1"),
#'   PARAMCD = character("1"),
#'   PARAMN = integer("1")
#' ) |>
#'   check_adtte()
#'
check_adtte <- function(adam) {
  make_multi_checker(
    check_has_cols(c("STUDYID", "USUBJID")),
    check_is_categorical("STUDYID"),
    check_is_categorical("USUBJID"),
    check_has_one_record_per_subject_per_param
  )(adam)
}

#' ADAE dataset checker
#'
#' @template adam
#'
#' @template checker_return
#'
#' @export
#'
#' @examples
#' dplyr::tibble(
#'   STUDYID = character("1"),
#'   USUBJID = character("1"),
#'   AEDECOD = character("1"),
#'   AEBODSYS = character("1"),
#'   AEREFID = character("1"),
#'   AECYCVID = character("1"),
#'   AESER = character("1"),
#'   ASTDT = integer(1L)
#' ) |>
#'   check_adae()
#'
check_adae <- function(adam) {
  make_multi_checker(
    check_has_cols(c("STUDYID", "USUBJID")),
    check_is_categorical("STUDYID"),
    check_is_categorical("USUBJID"),
    check_has_one_record_per_subject
  )(adam)
}

#' ADLB dataset checker
#'
#' @template adam
#'
#' @template checker_return
#'
#' @export
#'
#' @examples
#' dplyr::tibble(
#'   STUDYID = character("1"),
#'   USUBJID = character("1"),
#'   PARAM  = character("1"),
#'   PARAMCD = character("1"),
#'   PARAMN = integer("1"),
#'   AVISITN = numeric(1),
#'   ADTM = integer("1"),
#'   ADT = integer("1")
#' ) |>
#'   check_adlb()
#'
check_adlb <- function(adam) {
  make_multi_checker(
    check_has_cols(c("STUDYID", "USUBJID")),
    check_is_categorical("STUDYID"),
    check_is_categorical("USUBJID"),
    check_has_one_record_per_subject_per_param
  )(adam)
}

#' ADQS dataset checker
#'
#' @template adam
#'
#' @template checker_return
#'
#' @export
#'
#' @examples
#' dplyr::tibble(
#'   STUDYID = character("1"),
#'   USUBJID = character("1"),
#'   PARAMCD = character("1"),
#'   AVISIT = character("1")
#' ) |>
#'   check_adqs()
#'
check_adqs <- function(adam) {
  make_multi_checker(
    check_has_cols(c("STUDYID", "USUBJID")),
    check_is_categorical("STUDYID"),
    check_is_categorical("USUBJID")
  )(adam)
}

#' Check if columns exist in a dataset
#'
#' @param column_names A character vector of column names expected in the
#' dataset.
#'
#' @template checker
#'
#' @export
#'
#' @examples
#' dplyr::tibble(col_a = 1, col_b = 2) |>
#'   check_has_cols(column_names = c("col_a", "col_b"))()
#'
check_has_cols <- function(column_names) {
  assert_column_names(column_names, "column_names")

  make_checker(
    \(adam) {
      adam_col_names <-
        colnames(adam)

      check <-
        every(column_names, \(x) has_element(adam_col_names, x))

      missing_col_names <-
        discard(column_names, \(x) has_element(adam_col_names, x))

      msg <-
        str_c(
          "Dataset must have columns ",
          str_c("'", column_names, "'", collapse = ", "),
          ", but is missing ",
          str_c("'", missing_col_names, "'", collapse = ", "),
          "."
        )

      list(
        check = check,
        msg = msg
      )
    }
  )
}

#' Check if a group of columns exist in a dataset
#'
#' @param column_names A character vector of column names expected in the
#' dataset.
#'
#' @template checker
#'
#' @export
#'
#' @examples
#' param_col_names <- c("PARAMCD", "PARAM", "PARAMN")
#' dplyr::tibble(
#'   PARAMCD = "A", PARAM = "B") |>
#'   check_has_any_cols(column_names = param_col_names)()
#'
#' param_col_names <- c("PARAMCD", "PARAM", "PARAMN")
#' dplyr::tibble(
#'   A = "A") |>
#'   check_has_any_cols(column_names = param_col_names)()
#'
check_has_any_cols <- function(column_names) {
  assert_column_names(column_names, "column_names")

  make_checker(
    \(adam) {
      adam_col_names <-
        colnames(adam)

      check <-
        some(column_names, \(x) has_element(adam_col_names, x))

      missing_col_names <-
        none(column_names, \(x) has_element(adam_col_names, x))

      if (check == TRUE && missing_col_names == FALSE) {
        msg <-
          str_c(
            "Dataset must have at least one of the columns ",
            str_c("'", column_names, "'", collapse = ", "),
            ", and has at least one."
          )
      } else {
        msg <-
          str_c(
            "Dataset must have at least one of the columns ",
            str_c("'", column_names, "'", collapse = ", "),
            ", but is missing all of them."
          )
      }

      list(
        check = check,
        msg = msg
      )
    }
  )
}

#' Check if a column contains specific values
#'
#' @param column_name The column name to check values of.
#' @param values A vector of values expected to be present in the specified
#' column.
#'
#' @template checker
#'
#' @export
#'
#' @examples
#' dplyr::tibble(col_a = factor(letters)) |>
#'   check_has_values(column_name = "col_a", values = "a")()
#'
check_has_values <- function(column_name, values) {
  assert_column_name(column_name, "column_name")
  assert_values(values, "values")

  make_checker(
    \(adam) {
      column_values <-
        pluck(adam, column_name)

      arg_is_logical <-
        is.logical(column_values) ||
          (is.logical(values) && !is.na(values))

      check <-
        if (arg_is_logical) {
          every(values, \(x) has_element(column_values, x))
        } else {
          all(values %in% column_values)
        }

      missing_values <-
        if (arg_is_logical) {
          discard(values, \(x) x %in% column_values)
        } else {
          values[!values %in% column_values]
        }

      msg <-
        str_c(
          "Column '",
          column_name,
          "' must have values ",
          str_c("'", values, "'", collapse = ", "),
          ", but is missing values ",
          str_c("'", missing_values, "'", collapse = ", "),
          "."
        )

      list(
        check = check,
        msg = msg
      )
    }
  )
}

#' Check if a column lacks specific values
#'
#' @param column_name The column name to check values of.
#' @param values A vector of values expected to not be present in the specified
#' column.
#'
#' @template checker
#'
#' @export
#'
#' @examples
#' dplyr::tibble(col_a = 1) |>
#' check_lacks_values(column_name = "col_a", values = 2)()
#'
check_lacks_values <- function(column_name, values) {
  assert_column_name(column_name, "column_name")
  assert_values(values, "values")

  make_checker(
    \(adam) {
      column_values <-
        pluck(adam, column_name)

      arg_is_logical <-
        is.logical(column_values) ||
          (is.logical(values) && !is.na(values))

      check <-
        if (arg_is_logical) {
          none(values, \(x) has_element(column_values, x))
        } else {
          !any(values %in% column_values)
        }

      present_values <-
        if (arg_is_logical) {
          keep(values, \(x) has_element(column_values, x))
        } else {
          values[values %in% column_values]
        }

      msg <-
        str_c(
          "Column '",
          column_name,
          "' can't have values ",
          str_c("'", values, "'", collapse = ", "),
          ", but has values ",
          str_c("'", present_values, "'", collapse = ", "),
          "."
        )

      list(
        check = check,
        msg = msg
      )
    }
  )
}

#' Check if dataset has one record per subject
#'
#' @template adam
#'
#' @template checker_return
#'
#' @export
#'
#' @examples
#' dplyr::tibble(USUBJID = 1) |>
#'   check_has_one_record_per_subject()
#'
check_has_one_record_per_subject <- function(adam) {
  make_checker(
    \(adam) {
      records <-
        nrow(adam)

      subjects <-
        pluck(adam, "USUBJID") |>
        dplyr::n_distinct()

      check <-
        identical(records, subjects) &&
          records > 0L

      msg <-
        str_c(
          "Dataset must have one record per subject, but has ",
          records,
          " records (rows) and ",
          subjects,
          " subjects (USUBJID)."
        )

      list(
        check = check,
        msg = msg
      )
    }
  )(adam)
}

#' Check if dataset has one record per subject per parameter
#'
#' @template adam
#'
#' @template checker_return
#'
#' @export
#'
#' @examples
#' dplyr::tibble(STUDYID = 1, USUBJID = 1, PARAMCD = 1) |>
#'   check_has_one_record_per_subject_per_param()
#'
check_has_one_record_per_subject_per_param <- function(adam) {
  make_checker(
    \(adam) {
      check_with_columns <- function(adam) {
        param_counts <-
          adam |>
          dplyr::group_by(PARAMCD) |>
          dplyr::summarise(
            subjects = dplyr::n_distinct(USUBJID),
            records = dplyr::n()
          )

        invalid_params <-
          dplyr::filter(param_counts, subjects != records)

        check <-
          identical(
            param_counts$subjects,
            param_counts$records
          )

        if (check == FALSE) {
          invalid_params <- invalid_params %>%
            dplyr::mutate(PARAMCD = "PARAMCD")
        }

        msg <-
          str_c(
            "Dataset must have one record per subject per param, but param ",
            str_c(
              "'",
              invalid_params$PARAMCD,
              "' has ",
              invalid_params$records,
              " records (rows) and ",
              invalid_params$subjects,
              " subjects (USUBJID)",
              collapse = ", "
            ),
            "."
          )

        list(
          check = check,
          msg = msg
        )
      }

      is_missing_column <- function(adam, column_name) {
        colnames(adam) |>
          negate(has_element)(column_name)
      }

      usubjid_is_missing <-
        is_missing_column(adam, "USUBJID")

      paramcd_is_missing <-
        is_missing_column(adam, "PARAMCD")

      if (usubjid_is_missing || paramcd_is_missing) {
        list(
          check = FALSE,
          msg = str_c(
            "Dataset must have one record per subject per param, but is",
            " missing column 'USUBJID' or 'PARAMCD'."
          )
        )
      } else {
        check_with_columns(adam)
      }
    }
  )(adam)
}

#' Check if a column is logical
#'
#' @param column_name The column name to check datatype.
#'
#' @template checker
#'
#' @export
#'
#' @examples
#' status <- TRUE
#'
#' dplyr::tibble(
#'   status
#' ) |> check_is_logical("status")()
#'
#' x <- c("male, female")
#' status <- factor(x)
#'
#' dplyr::tibble(
#'   status
#' ) |> check_is_logical("status")()
#'
check_is_logical <- function(column_name) {
  assert_column_name(column_name, "column_name")

  make_checker(
    \(adam) {
      column_datatype <-
        pluck(adam, column_name)
      datatype <- class(column_datatype)

      arg_is_logical <-
        is.logical(column_datatype)

      check <- arg_is_logical
      values <- "logical"

      missing_data_type <-
        if (arg_is_logical) {
          discard(column_name, \(x) has_element(column_name, x))
        } else {
          datatype
        }

      msg <-
        str_c(
          "Column '",
          column_name,
          "' must have data type ",
          str_c("'", values, "'", collapse = ", "),
          ", but is data type ",
          str_c("'", missing_data_type, "'", collapse = ", "),
          "."
        )

      list(
        check = check,
        msg = msg
      )
    }
  )
}

#' Check if a column is a factor category
#'
#' @param column_name The column name to check datatype.
#'
#' @template checker
#'
#' @export
#'
#' @examples
#' sex <- factor(c("male, female"))
#'
#' dplyr::tibble(
#'   sex
#' ) |> check_is_categorical("sex")()
#'
#' sex <- c("male, female")
#'
#' dplyr::tibble(
#'   sex
#' ) |> check_is_categorical("sex")()
#'
check_is_categorical <- function(column_name) {
  assert_column_name(column_name, "column_name")

  make_checker(
    \(adam) {
      column_datatype <-
        pluck(adam, column_name)
      datatype <- class(column_datatype)

      arg_is_categorical <-
        is.factor(column_datatype) ||
        is.character(column_datatype)

      check <- arg_is_categorical
      values <- "factor"

      missing_data_type <-
        if (arg_is_categorical) {
          discard(column_name, \(x) has_element(column_name, x))
        } else {
          datatype
        }

      msg <-
        str_c(
          "Column '",
          column_name,
          "' must have data type ",
          str_c("'", values, "'", collapse = ", "),
          ", but is data type ",
          str_c("'", missing_data_type, "'", collapse = ", "),
          "."
        )

      list(
        check = check,
        msg = msg
      )
    }
  )
}

#' Check if a column is character
#'
#' @param column_name The column name to check datatype.
#'
#' @template checker
#'
#' @export
#'
#' @examples
#' comments <- "text"
#'
#' dplyr::tibble(
#'   comments
#' ) |> check_is_character("comments")()
#'
#' comments <- factor(c("male, female"))
#'
#' dplyr::tibble(
#'   comments
#' ) |> check_is_character("comments")()
#'
check_is_character <- function(column_name) {
  assert_column_name(column_name, "column_name")

  make_checker(
    \(adam) {
      column_datatype <-
        pluck(adam, column_name)
      datatype <- class(column_datatype)

      arg_is_character <-
        is.character(column_datatype)

      check <- arg_is_character
      values <- "character"

      missing_data_type <-
        if (arg_is_character) {
          discard(column_name, \(x) has_element(column_name, x))
        } else {
          datatype
        }


      msg <-
        str_c(
          "Column '",
          column_name,
          "' must have data type ",
          str_c("'", values, "'", collapse = ", "),
          ", but is data type ",
          str_c("'", missing_data_type, "'", collapse = ", "),
          "."
        )

      list(
        check = check,
        msg = msg
      )
    }
  )
}

#' Check if a column is date
#'
#' @param column_name The column name to check datatype.
#'
#' @template checker
#'
#' @export
#'
#' @examples
#' library(lubridate)
#' test_date <- as.Date("2003-02-27")
#' dplyr::tibble(
#'   test_date
#' ) |> check_is_date("test_date")()
#'
#' test_date <- as.POSIXct("2003-02-27 22:22:22")
#'
#' dplyr::tibble(
#'   test_date
#' ) |> check_is_date("test_date")()
#'
check_is_date <- function(column_name) {
  assert_column_name(column_name, "column_name")

  make_checker(
    \(adam) {
      column_datatype <-
        pluck(adam, column_name)
      datatype <- class(column_datatype)

      arg_is_date <-
        lubridate::is.Date(datatype) ||
          lubridate::is.Date(column_datatype)

      check <- arg_is_date
      values <- "date"

      missing_data_type <-
        if (arg_is_date) {
          discard(column_name, \(x) has_element(column_name, x))
        } else {
          datatype
        }
      msg <-
        str_c(
          "Column '",
          column_name,
          "' must have data type ",
          str_c("'", values, "'", collapse = ", "),
          ", but is data type ",
          str_c("'", missing_data_type, "'", collapse = ", "),
          "."
        )

      list(
        check = check,
        msg = msg
      )
    }
  )
}

#' Check if a column is datetime
#'
#' @param column_name The column name to check datatype.
#'
#' @template checker
#'
#' @export
#'
#' @examples
#' library(lubridate)
#' test_datetime <- as.POSIXct("2020/01/01 22:22:22")
#' dplyr::tibble(
#'   test_datetime
#' ) |> check_is_datetime("test_datetime")()
#'
#' test_datetime <- as.Date("2020/01/01")
#'
#' dplyr::tibble(
#'   test_datetime
#' ) |> check_is_datetime("test_datetime")()
#'
check_is_datetime <- function(column_name) {
  assert_column_name(column_name, "column_name")

  make_checker(
    \(adam) {
      column_datatype <-
        pluck(adam, column_name)
      datatype <- class(column_datatype)

      arg_is_datetime <-
        lubridate::is.POSIXct(datatype) ||
          lubridate::is.POSIXct(column_datatype)

      check <- arg_is_datetime
      values <- "datetime"

      missing_data_type <-
        if (arg_is_datetime) {
          discard(column_name, \(x) has_element(column_name, x))
        } else {
          datatype
        }



      msg <-
        str_c(
          "Column '",
          column_name,
          "' must have data type ",
          str_c("'", values, "'", collapse = ", "),
          ", but is data type ",
          str_c("'", missing_data_type, "'", collapse = ", "),
          "."
        )

      list(
        check = check,
        msg = msg
      )
    }
  )
}

#' Check if a column is integerish
#'
#' @param column_name The column name to check datatype.
#'
#' @template checker
#'
#' @export
#'
#' @examples
#' USUBJID <- as.integer(1.1)
#' dplyr::tibble(
#'   USUBJID
#' ) |> check_is_integerish("USUBJID")()
#'
#' USUBJID <- as.double(1)
#'
#' dplyr::tibble(
#'   USUBJID
#' ) |> check_is_integerish("USUBJID")()
#'
#' USUBJID <- as.double(1.1)
#'
#' dplyr::tibble(
#'   USUBJID
#' ) |> check_is_integerish("USUBJID")()
#'
check_is_integerish <- function(column_name) {
  assert_column_name(column_name, "column_name")

  make_checker(
    \(adam) {
      column_datatype <-
        pluck(adam, column_name)
      datatype <- class(column_datatype)

      arg_is_integerish <-
        checkmate::test_integerish(datatype) ||
          checkmate::test_integerish(column_datatype)

      check <- arg_is_integerish
      value_1 <- "integer"
      value_2 <- "whole number double"

      missing_data_type <-
        if (arg_is_integerish) {
          discard(column_name, \(x) has_element(column_name, x))
        } else {
          datatype
        }

      msg <-
        str_c(
          "Column '",
          column_name,
          "' must have data type ",
          str_c("'", value_1, "'", " or " , "'", value_2, "'", collapse = ", "),
          ", but is data type ",
          str_c("'", missing_data_type, "'", collapse = ", "),
          "."
        )

      list(
        check = check,
        msg = msg
      )
    }
  )
}

#' Check if a column is double
#'
#' @param column_name The column name to check datatype.
#'
#' @template checker
#'
#' @export
#'
#' @examples
#' USUBJID <- 1.1
#' dplyr::tibble(
#'   USUBJID
#' ) |> check_is_double("USUBJID")()
#'
#' test_datetime <- as.POSIXct("2020/01/01 22:22:22")
#'
#' dplyr::tibble(
#'   test_datetime
#' ) |> check_is_double("test_datetime")()
#'
check_is_double <- function(column_name) {
  assert_column_name(column_name, "column_name")

  make_checker(
    \(adam) {
      column_datatype <-
        pluck(adam, column_name)
      datatype <- class(column_datatype)

      arg_is_double <-
        vctrs::vec_is(datatype, double()) ||
          vctrs::vec_is(column_datatype, double())

      check <- arg_is_double
      values <- "double"

      missing_data_type <-
        if (arg_is_double) {
          discard(column_name, \(x) has_element(column_name, x))
        } else {
          datatype
        }

      msg <-
        str_c(
          "Column '",
          column_name,
          "' must have data type ",
          str_c("'", values, "'", collapse = ", "),
          ", but is data type ",
          str_c("'", missing_data_type, "'", collapse = ", "),
          "."
        )

      list(
        check = check,
        msg = msg
      )
    }
  )
}
