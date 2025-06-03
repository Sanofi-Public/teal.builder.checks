
<!-- README.md is generated from README.Rmd. Please edit that file -->

# teal.builder.checks

# Overview

**teal.builder.checks** is a package for checking the data integrity of
**ADaM** datasets

- \[Package website\] -
  (<https://github.com/teal.builder.checks/index.html>)

## Installation

You can install the development version from
[Github](https://%3Cgithub.com%3E/%3Cteal.builder.checks%3E) with:

``` r

# remotes::install_github(
#  "https://<github.com>/teal.builder.checks",
#  auth_token = Sys.getenv("GITHUB_PAT"),
# upgrade = "never"
# )
```

# Usage

**teal.builder.checks** checks the following **ADaM** datasets:

| ADaM datasets | ADaM datasets contd. |
|:-------------:|:--------------------:|
|     adam      |         adrs         |
|     adsl      |         adqs         |
|     adae      |         adlb         |
|     adtte     |                      |

**teal.builder.checks** can perform the following data integrity checks
by column on those datasets:

| Column Checks | Column Checks contd.                 |
|:--------------|:-------------------------------------|
| existence     | is date                              |
| is character  | is datetime                          |
| is integerish | has values                           |
| is double     | lacks values                         |
| is logical    | has one record per subject           |
| is factor     | has one record per subject per param |

# Example usage

``` r
library(purrr)
library(dplyr)    
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
library(lubridate)
#> 
#> Attaching package: 'lubridate'
#> The following objects are masked from 'package:base':
#> 
#>     date, intersect, setdiff, union
library(stringr)
library(checkmate)
library(vctrs)
#> 
#> Attaching package: 'vctrs'
#> The following object is masked from 'package:dplyr':
#> 
#>     data_frame
library(random.cdisc.data)

# Load adam datasets for checking
adsl <- random.cdisc.data::radsl(N = 10, seed = 1, na_percentage = .1)
adtte <- random.cdisc.data::radtte(adsl, seed = 1, na_percentage = .1)
```

## Successful check of adsl

This check returns a logical **TRUE** with an attribute **data** which
contains the original ADaM dataset checked

``` r
library(teal.builder.checks)
check_adsl(adsl)
#> [1] TRUE
#> attr(,"data")
#> # A data frame: 10 × 55
#>    STUDYID USUBJID    SUBJID SITEID   AGE AGEU  SEX   RACE  ETHNIC COUNTRY DTHFL
#>    <chr>   <chr>      <chr>  <chr>  <int> <fct> <fct> <fct> <fct>  <fct>   <fct>
#>  1 AB12345 AB12345-C… id-10  CHN-3     NA YEARS M     <NA>  NOT H… CHN     N    
#>  2 AB12345 AB12345-J… id-7   JPN-4     30 YEARS <NA>  ASIAN NOT H… JPN     N    
#>  3 AB12345 AB12345-U… id-3   USA-13    35 YEARS F     AMER… NOT H… USA     N    
#>  4 AB12345 AB12345-B… id-8   BRA-9     31 YEARS F     ASIAN NOT H… BRA     Y    
#>  5 AB12345 AB12345-C… id-2   CHN-11    35 YEARS M     BLAC… NOT H… CHN     N    
#>  6 AB12345 AB12345-C… id-1   CHN-11    35 YEARS F     WHITE NOT H… CHN     Y    
#>  7 AB12345 AB12345-C… id-5   CHN-3     36 YEARS F     ASIAN NOT H… CHN     N    
#>  8 AB12345 AB12345-R… id-4   RUS-1     36 YEARS M     BLAC… NOT H… RUS     N    
#>  9 AB12345 AB12345-R… id-6   RUS-1     36 YEARS F     ASIAN NOT H… RUS     N    
#> 10 AB12345 AB12345-B… id-9   BRA-1     35 YEARS F     BLAC… UNKNO… BRA     N    
#> # ℹ 44 more variables: INVID <chr>, INVNAM <chr>, ARM <fct>, ARMCD <fct>,
#> #   ACTARM <fct>, ACTARMCD <fct>, TRT01P <fct>, TRT01A <fct>, TRT02P <fct>,
#> #   TRT02A <fct>, REGION1 <fct>, STRATA1 <fct>, STRATA2 <fct>, BMRKR1 <dbl>,
#> #   BMRKR2 <fct>, ITTFL <fct>, SAFFL <fct>, BMEASIFL <fct>, BEP01FL <fct>,
#> #   AEWITHFL <fct>, RANDDT <date>, TRTSDTM <dttm>, TRTEDTM <dttm>,
#> #   TRT01SDTM <dttm>, TRT01EDTM <dttm>, TRT02SDTM <dttm>, TRT02EDTM <dttm>,
#> #   AP01SDTM <dttm>, AP01EDTM <dttm>, AP02SDTM <dttm>, AP02EDTM <dttm>, …
```

## Unsuccessful check of adsl

### check for non-existence of columns

This check returns a logical **FALSE** with two attributes

1)  **msg** contains a character vector describing the reason for
    failure, if the check failed

2)  **data** which contains the ADaM dataset checked

``` r
adsl |>
  dplyr::select(-STUDYID) |> 
  check_adsl()
#> [1] FALSE
#> attr(,"msg")
#> [1] "Dataset must have columns 'STUDYID', 'USUBJID', but is missing 'STUDYID'."
#> [2] "Column 'STUDYID' must have data type 'factor', but is data type 'NULL'."  
#> attr(,"data")
#> # A data frame: 10 × 54
#>    USUBJID      SUBJID SITEID   AGE AGEU  SEX   RACE  ETHNIC COUNTRY DTHFL INVID
#>    <chr>        <chr>  <chr>  <int> <fct> <fct> <fct> <fct>  <fct>   <fct> <chr>
#>  1 AB12345-CHN… id-10  CHN-3     NA YEARS M     <NA>  NOT H… CHN     N     INV …
#>  2 AB12345-JPN… id-7   JPN-4     30 YEARS <NA>  ASIAN NOT H… JPN     N     INV …
#>  3 AB12345-USA… id-3   USA-13    35 YEARS F     AMER… NOT H… USA     N     INV …
#>  4 AB12345-BRA… id-8   BRA-9     31 YEARS F     ASIAN NOT H… BRA     Y     INV …
#>  5 AB12345-CHN… id-2   CHN-11    35 YEARS M     BLAC… NOT H… CHN     N     INV …
#>  6 AB12345-CHN… id-1   CHN-11    35 YEARS F     WHITE NOT H… CHN     Y     INV …
#>  7 AB12345-CHN… id-5   CHN-3     36 YEARS F     ASIAN NOT H… CHN     N     INV …
#>  8 AB12345-RUS… id-4   RUS-1     36 YEARS M     BLAC… NOT H… RUS     N     INV …
#>  9 AB12345-RUS… id-6   RUS-1     36 YEARS F     ASIAN NOT H… RUS     N     INV …
#> 10 AB12345-BRA… id-9   BRA-1     35 YEARS F     BLAC… UNKNO… BRA     N     INV …
#> # ℹ 43 more variables: INVNAM <chr>, ARM <fct>, ARMCD <fct>, ACTARM <fct>,
#> #   ACTARMCD <fct>, TRT01P <fct>, TRT01A <fct>, TRT02P <fct>, TRT02A <fct>,
#> #   REGION1 <fct>, STRATA1 <fct>, STRATA2 <fct>, BMRKR1 <dbl>, BMRKR2 <fct>,
#> #   ITTFL <fct>, SAFFL <fct>, BMEASIFL <fct>, BEP01FL <fct>, AEWITHFL <fct>,
#> #   RANDDT <date>, TRTSDTM <dttm>, TRTEDTM <dttm>, TRT01SDTM <dttm>,
#> #   TRT01EDTM <dttm>, TRT02SDTM <dttm>, TRT02EDTM <dttm>, AP01SDTM <dttm>,
#> #   AP01EDTM <dttm>, AP02SDTM <dttm>, AP02EDTM <dttm>, EOSSTT <fct>, …
```

## Creation of your own multichecker

### Successful check of adtte

In the following example, the checks are performed:

1)  the existence of certain columns
2)  specific values in one column
3)  one record per subject per parameter

``` r
my_adtte_checker <- make_multi_checker(
  check_has_cols(
    column_names = c("STUDYID", "USUBJID", "PARAMCD")
  ),
  check_has_values(
    column_name = "ARM",
    values = c("A: Drug X", "B: Placebo", "C: Combination")
  ),
  check_has_one_record_per_subject_per_param
)
```

``` r
my_adtte_checker(adtte) 
#> [1] TRUE
#> attr(,"data")
#> # A data frame: 50 × 67
#>    STUDYID USUBJID    SUBJID SITEID   AGE AGEU  SEX   RACE  ETHNIC COUNTRY DTHFL
#>    <chr>   <chr>      <chr>  <chr>  <int> <fct> <fct> <fct> <fct>  <fct>   <fct>
#>  1 AB12345 AB12345-B… id-9   BRA-1     35 YEARS F     BLAC… UNKNO… BRA     N    
#>  2 AB12345 AB12345-B… id-9   BRA-1     35 YEARS F     BLAC… UNKNO… BRA     N    
#>  3 AB12345 AB12345-B… id-9   BRA-1     35 YEARS F     BLAC… UNKNO… BRA     N    
#>  4 AB12345 AB12345-B… id-9   BRA-1     35 YEARS F     BLAC… UNKNO… BRA     N    
#>  5 AB12345 AB12345-B… id-9   BRA-1     35 YEARS F     BLAC… UNKNO… BRA     N    
#>  6 AB12345 AB12345-B… id-8   BRA-9     31 YEARS F     ASIAN NOT H… BRA     Y    
#>  7 AB12345 AB12345-B… id-8   BRA-9     31 YEARS F     ASIAN NOT H… BRA     Y    
#>  8 AB12345 AB12345-B… id-8   BRA-9     31 YEARS F     ASIAN NOT H… BRA     Y    
#>  9 AB12345 AB12345-B… id-8   BRA-9     31 YEARS F     ASIAN NOT H… BRA     Y    
#> 10 AB12345 AB12345-B… id-8   BRA-9     31 YEARS F     ASIAN NOT H… BRA     Y    
#> # ℹ 40 more rows
#> # ℹ 56 more variables: INVID <chr>, INVNAM <chr>, ARM <fct>, ARMCD <fct>,
#> #   ACTARM <fct>, ACTARMCD <fct>, TRT01P <fct>, TRT01A <fct>, TRT02P <fct>,
#> #   TRT02A <fct>, REGION1 <fct>, STRATA1 <fct>, STRATA2 <fct>, BMRKR1 <dbl>,
#> #   BMRKR2 <fct>, ITTFL <fct>, SAFFL <fct>, BMEASIFL <fct>, BEP01FL <fct>,
#> #   AEWITHFL <fct>, RANDDT <date>, TRTSDTM <dttm>, TRTEDTM <dttm>,
#> #   TRT01SDTM <dttm>, TRT01EDTM <dttm>, TRT02SDTM <dttm>, TRT02EDTM <dttm>, …
```

## Unsuccessful check of adtte

This check returns a **msg** attribute with multiple error messages.

``` r
adtte |>
  dplyr::slice_head(n = 12) |>
  dplyr::select(-STUDYID) |>
  dplyr::filter(ARM != "A: Drug X") |>
  my_adtte_checker() 
#> [1] FALSE
#> attr(,"msg")
#> [1] "Dataset must have columns 'STUDYID', 'USUBJID', 'PARAMCD', but is missing 'STUDYID'."                                       
#> [2] "Column 'ARM' must have values 'A: Drug X', 'B: Placebo', 'C: Combination', but is missing values 'A: Drug X', 'B: Placebo'."
#> attr(,"data")
#> # A data frame: 7 × 66
#>   USUBJID       SUBJID SITEID   AGE AGEU  SEX   RACE  ETHNIC COUNTRY DTHFL INVID
#>   <chr>         <chr>  <chr>  <int> <fct> <fct> <fct> <fct>  <fct>   <fct> <chr>
#> 1 AB12345-BRA-… id-9   BRA-1     35 YEARS F     BLAC… UNKNO… BRA     N     INV …
#> 2 AB12345-BRA-… id-9   BRA-1     35 YEARS F     BLAC… UNKNO… BRA     N     INV …
#> 3 AB12345-BRA-… id-9   BRA-1     35 YEARS F     BLAC… UNKNO… BRA     N     INV …
#> 4 AB12345-BRA-… id-9   BRA-1     35 YEARS F     BLAC… UNKNO… BRA     N     INV …
#> 5 AB12345-BRA-… id-9   BRA-1     35 YEARS F     BLAC… UNKNO… BRA     N     INV …
#> 6 AB12345-CHN-… id-1   CHN-11    35 YEARS F     WHITE NOT H… CHN     Y     INV …
#> 7 AB12345-CHN-… id-1   CHN-11    35 YEARS F     WHITE NOT H… CHN     Y     INV …
#> # ℹ 55 more variables: INVNAM <chr>, ARM <fct>, ARMCD <fct>, ACTARM <fct>,
#> #   ACTARMCD <fct>, TRT01P <fct>, TRT01A <fct>, TRT02P <fct>, TRT02A <fct>,
#> #   REGION1 <fct>, STRATA1 <fct>, STRATA2 <fct>, BMRKR1 <dbl>, BMRKR2 <fct>,
#> #   ITTFL <fct>, SAFFL <fct>, BMEASIFL <fct>, BEP01FL <fct>, AEWITHFL <fct>,
#> #   RANDDT <date>, TRTSDTM <dttm>, TRTEDTM <dttm>, TRT01SDTM <dttm>,
#> #   TRT01EDTM <dttm>, TRT02SDTM <dttm>, TRT02EDTM <dttm>, AP01SDTM <dttm>,
#> #   AP01EDTM <dttm>, AP02SDTM <dttm>, AP02EDTM <dttm>, EOSSTT <fct>, …
```
