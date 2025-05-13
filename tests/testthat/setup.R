test_sex <- factor(c("male, female"))

test_date <- as.Date("2003-02-27")
test_datetime <- as.POSIXct("2020/01/01 22:22:22")

test_integer <- as.integer(4.1)
test_double <- 4.1

# expect checker true
expect_checker_true <- function(data, checker) {
  check_list <- checker(data)
  testthat::expect_true(check_list)
  testthat::expect_null(attr(check_list, "msg"))
  testthat::expect_identical(attr(check_list, "data"), data)
}

# expect checker false
expect_checker_false <- function(data, checker, msg) {
  check_list <- checker(data)
  testthat::expect_false(check_list)
  testthat::expect_identical(attr(check_list, "msg"), msg)
  testthat::expect_identical(attr(check_list, "data"), data)
}

# expect error message
expect_error_message <- function(x, ...) {
  testthat::expect_error(
    object = x,
    regexp = str_c(list(...), collapse = ""),
    fixed = TRUE
  )
}

