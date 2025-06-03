### --- DEVELOPER NOTE --- ###
# This template is to be used with:
# - data type checker, e.g. `check_double`
# - column checkers, e.g. `check_has_cols`
# - column value checkers, e.g. `check_lacks_values`
# - functions that use `make_checker` or `make_multi_checker`, e.g. `check_adsl`
### --- END DEVELOPER NOTE --- ###

#' @return
#' A single `logical`, `TRUE` if the check passes and `FALSE` if not, with attributes:
#'
#' - `msg`: `character` vector of reasons for check failure
#' - `data`: `adam`

