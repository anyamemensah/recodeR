## Introduction

Welcome to the `recodeR` package. `recodeR` is an R package for recoding
values in data.frames and tibbles. If you notice a bug or have trouble
using a function, please contact Ama Nyame-Mensah
<ama@anyamemensah.com>, the package’s maintainer.

## Table of Contents

- [Installation](#download-and-installation)
- [A Note on data dictionaries](#a-note-on-data-dictionaries)
- [Creating a ‘recoding’ data
  dictionary](#creating-a-recoding-data-dictionary)
- [Recoding columns in a dataset](#recoding-columns-in-a-dataset)

## Download and Installation

To begin, download, install, and load `recodeR`.

``` r
# devtools::install_github("anyamemensah/recodeR")

library(recodeR)
```

## A Note on data dictionaries

A data dictionary is a helpful guide for changing the values in a
dataset. It lists unique column/variable names, the original
column/variable values/labels, the new recoded values/labels, and other
useful details for working with the data, such as column/variable
descriptions. The `recodeR` package includes tools that make it easy to
turn ‘regular’ data dictionaries into ones that R can use to quickly
update variable values/labels.

## Creating a ‘recoding’ data dictionary

You can convert your standard data dictionary into a recoding data
dictionary in R using the `create_recode_dict()` function. Before
running the function, make sure your data dictionary includes the
following:

1.  A `var_col` column with the unique variable/column names from your
    dataset.
2.  A `from_col` column with the original values for each variable.
3.  A `to_col` column with the new values that will replace the old ones
    for each variable.
4.  A `default_col` column contains a fallback value for any unmatched
    original values. NOTE: To keep the original value as the default,
    use `.x`. Blank entries will default to `NA`.
5.  A `string_col` column indicates whether the new values to be mapped
    onto the variable are string.

The `grad_app_dict` dataset, provided with the `recodeR` package, shows
how to set up a sample data dictionary in a flat file format, where each
unique column in your dataset is associated with multiple rows. To learn
more about this dataset, use `?grad_app_dict`.

The `create_recode_dict()` function creates a list of ‘recoding’
dictionaries: one for string variables and another for numeric variables
in your dataset. To create only a `string` or `numeric` recoding
dictionary, set the `dict_type` argument to `string` or `numeric`,
respectively. By default, this argument is set to `all`. Each ‘recoding’
dictionary contains the unique column names to be recoded, a set of
two-sided formulas that map old values to new one, default values for
any unmatched original values, and a column indicating whether the new
values to be applied are strings (1) or not (0).

``` r
recoding_dict <- create_recode_dict(df = grad_app_dict,
                                    var_col = "column_name",
                                    from_col = "old_values",
                                    to_col = "new_labels",
                                    default_col = "default",
                                    string_col = "string")

recoding_dict
```

    ## $string
    ## # A tibble: 7 × 4
    ##   column_name      formula_pairs default  string
    ##   <chr>            <list>        <chr>     <dbl>
    ## 1 applied_dual_prg <list [2]>    No Entry      1
    ## 2 ethnicity        <list [6]>    No Entry      1
    ## 3 ft_pt            <list [3]>    No Entry      1
    ## 4 gender           <list [3]>    No Entry      1
    ## 5 low_income       <list [2]>    No Entry      1
    ## 6 school_decision  <list [3]>    No Entry      1
    ## 7 student_decision <list [2]>    No Entry      1
    ## 
    ## $numeric
    ## # A tibble: 3 × 4
    ##   column_name    formula_pairs default string
    ##   <chr>          <list>          <dbl>  <dbl>
    ## 1 gre_analytical <list [13]>      -999      0
    ## 2 gre_quant      <list [41]>      -999      0
    ## 3 gre_verbal     <list [41]>      -999      0

``` r
recoding_dict$string$formula_pairs[[6]]
```

    ## [[1]]
    ## "A" ~ "Accepted"
    ## <environment: 0x103a2ce40>
    ## 
    ## [[2]]
    ## "W" ~ "Waitlisted"
    ## <environment: 0x103a2d818>
    ## 
    ## [[3]]
    ## "R" ~ "Rejected"
    ## <environment: 0x103a2aa50>

Sometimes, data dictionaries are created with one row of data for each
unique column/variable in the dataset, where the old and new values to
be mapped for each variable are separated by delimiters like commas or
semicolons.

The `grad_app_dict_delim` dataset, provided with the `recodeR` package,
shows how to set up a sample data dictionary in a delimited format,
where each unique column in your dataset is associated with exactly one
row To learn more about this dataset, use `?grad_app_dict_delim`.

If your data dictionary is in a delimited format, you can use the
`expand_delim_dict()` function to convert it into the traditional flat
format, where each unique column in your dataset is associated with
multiple rows. By default, the function also verifies that the expanded
data dictionary meets the required criteria for use with the
`create_recode_dict()` function.

``` r
expanded_dict <- expand_delim_dict(df = grad_app_dict_delim,
                                   var_col = "column_name",
                                   from_col = "old_values",
                                   to_col = "new_labels",
                                   default_col = "default",
                                   string_col = "string",
                                   from_delim = ";",
                                   to_delim = ";")
```

    ## 'the_expanded_df' is a valid data.frame.'. ✔

    ## 'column_name' is a valid column in 'the_expanded_df'. ✔

    ## 'old_values' is a valid column in 'the_expanded_df'. ✔

    ## 'new_labels' is a valid column in 'the_expanded_df'. ✔

    ## 'default' is a valid column in 'the_expanded_df'. ✔

    ## string is a valid column in 'the_expanded_df'. ✔

    ## Each unique variable in 'the_expanded_df' is associated with more than one row of data. ✔

    ## Each unique variable in 'the_expanded_df' is associated with a single default value. ✔

    ## Each unique variable in 'the_expanded_df' is associated with a single string flag ✔

    ## The value and label pairs for each unique variable in 'the_expanded_df' have the same length. ✔

``` r
expanded_dict
```

    ## # A tibble: 115 × 5
    ##    column_name      old_values new_labels     default  string
    ##    <chr>            <chr>      <chr>          <chr>     <dbl>
    ##  1 school_decision  A          Accepted       No Entry      1
    ##  2 school_decision  W          Waitlisted     No Entry      1
    ##  3 school_decision  R          Rejected       No Entry      1
    ##  4 student_decision A          Accepted Offer No Entry      1
    ##  5 student_decision D          Declined Offer No Entry      1
    ##  6 ft_pt            pt         Part-time      No Entry      1
    ##  7 ft_pt            ft         Full-time      No Entry      1
    ##  8 ft_pt            sub        Submatriculate No Entry      1
    ##  9 applied_dual_prg NODUAL     No             No Entry      1
    ## 10 applied_dual_prg YES        Yes            No Entry      1
    ## # ℹ 105 more rows

After the data dictionary passes all checks, use the
`create_recode_dict()` function to convert it into a ‘recoding’
dictionary.

``` r
expanded_recoding_dict <- create_recode_dict(df = expanded_dict,
                                             var_col = "column_name",
                                             from_col = "old_values",
                                             to_col = "new_labels",
                                             default_col = "default",
                                             string_col = "string")

expanded_recoding_dict
```

    ## $string
    ## # A tibble: 7 × 4
    ##   column_name      formula_pairs default  string
    ##   <chr>            <list>        <chr>     <dbl>
    ## 1 applied_dual_prg <list [2]>    No Entry      1
    ## 2 ethnicity        <list [5]>    No Entry      1
    ## 3 ft_pt            <list [3]>    No Entry      1
    ## 4 gender           <list [3]>    No Entry      1
    ## 5 low_income       <list [2]>    No Entry      1
    ## 6 school_decision  <list [3]>    No Entry      1
    ## 7 student_decision <list [2]>    No Entry      1
    ## 
    ## $numeric
    ## # A tibble: 3 × 4
    ##   column_name    formula_pairs default string
    ##   <chr>          <list>          <dbl>  <dbl>
    ## 1 gre_analytical <list [13]>      -999      0
    ## 2 gre_quant      <list [41]>      -999      0
    ## 3 gre_verbal     <list [41]>      -999      0

``` r
expanded_recoding_dict$string$formula_pairs[[6]]
```

    ## [[1]]
    ## "A" ~ "Accepted"
    ## <environment: 0x105de2e20>
    ## 
    ## [[2]]
    ## "W" ~ "Waitlisted"
    ## <environment: 0x105de2480>
    ## 
    ## [[3]]
    ## "R" ~ "Rejected"
    ## <environment: 0x105de1b18>

## Recoding columns in a dataset

After creating the ‘recoding’ dictionary, use the `recode_cols()`
function to apply the recoding rules to your dataset.

``` r
grad_app_recoded <-
  recode_cols(df = grad_app,
              list_of_dicts = recoding_dict,
              var_col = "column_name",
              formula_pairs_col = "formula_pairs",
              default_col = "default",
              string_col = "string")

grad_app_recoded
```

    ## # A tibble: 5,000 × 14
    ##    applicant_id email     email_preferred school_decision student_decision ft_pt
    ##           <dbl> <chr>     <chr>           <chr>           <chr>            <chr>
    ##  1          401 karin.wa… walborn_karin@… Accepted        Accepted Offer   Part…
    ##  2          386 yareli.g… <NA>            Accepted        Accepted Offer   Full…
    ##  3         2905 sulaimaa… al-demian_sula… Waitlisted      No Entry         Full…
    ##  4         3043 ryan.bur… burt_ryan@Outl… Accepted        Declined Offer   Part…
    ##  5         1546 ryan.vil… villalobos_rya… Waitlisted      No Entry         Full…
    ##  6         3861 aayid.el… el-jamal_aayid… Accepted        Accepted Offer   Full…
    ##  7          934 austin.p… poveromo_austi… Waitlisted      No Entry         Full…
    ##  8         1763 sapphire… quick_sapphire… Accepted        Accepted Offer   Full…
    ##  9         2839 franco.r… reyes_franco@y… Accepted        Declined Offer   Full…
    ## 10         2388 hamdoona… al-mohammad_ha… Rejected        No Entry         Part…
    ## # ℹ 4,990 more rows
    ## # ℹ 8 more variables: gre_verbal <dbl>, gre_quant <dbl>, gre_analytical <dbl>,
    ## #   applied_dual_prg <chr>, birth_date <chr>, gender <chr>, ethnicity <chr>,
    ## #   low_income <chr>
