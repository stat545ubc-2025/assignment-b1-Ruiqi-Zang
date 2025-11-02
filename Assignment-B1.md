Assignment-B1
================
Ruiqi Zang
2025-11-01

Load into the library, which is include;

`tidyverse`: For data import, cleansing, visualization and modeling

`roxygen2`: Automatic generation of function documentation

`datateachr`: datagram

`testthat`: Unit testing framework

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.2
    ## ✔ ggplot2   4.0.0     ✔ tibble    3.3.0
    ## ✔ lubridate 1.9.4     ✔ tidyr     1.3.1
    ## ✔ purrr     1.1.0     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(roxygen2)
```

``` r
library(datateachr)
library(testthat)
```

    ## 
    ## Attaching package: 'testthat'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     matches

    ## The following object is masked from 'package:purrr':
    ## 
    ##     is_null

    ## The following objects are masked from 'package:readr':
    ## 
    ##     edition_get, local_edition

    ## The following object is masked from 'package:tidyr':
    ## 
    ##     matches

## Exercise 1: Make a Function (25 points)

## Exercise 2: Document your Function (20 points)

> In this exercise, I’ll be making a function and fortifying it.

> In the same code chunk where you made your function, document the
> function using roxygen2 tags.

When practicing with different datasets, I found by calculating the
percentage of each category in a categorical variable, it is possible to
visualize the distribution of that variable in the population. For
example, calculating the proportion of benign versus malignant tumors in
the cancer sample dataset, or calculating the proportion of different
types in the game dataset. In order to perform such analyses more
efficiently, I plan to write a function that takes as input a data frame
and a specified categorical variable, first calculates the total number
of observations in the dataset, then counts the number of observations
in each category, and further calculates its proportion relative to the
total, thus helping to quickly understand the distribution of different
categories in the data.

``` r
#' @title Summarize Counts and Proportions by Group
#'
#' @param data ; A data frame containing the data to summarize.
#'
#' @param group ; A string specifying the name of the categorical variable for which frequencies and proportions are to be calculated. 
#'
#' @param na.rm Logical; if TRUE (default), missing values (NA) are removed before counting.
#'
#' @return A tibble with one row per group, containing:
#'   - The group variable
#'   - The count of each category
#'   - The proportion of each category relative to the total number of 
#'     non-missing observations.
#'
summarize_by_group <- function(data, group, na.rm = TRUE) {
  
  # ---- Input validation ----
  if (!is.data.frame(data)) {
    stop("'data' must be a frame or tibble")
  }
  
  if (!group %in% names(data)) {
    stop(paste("Grouping variable '", group, "' not found in data", sep = ""))
  }
  
  # ---- Handle missing values ----
  if (na.rm) {
    data <- data %>% dplyr::filter(!is.na(.data[[group]]))
  }
  
  # ---- Calculate total number of observations ----
  total_n <- nrow(data)
  
  # ---- Group and summarize counts and proportions ----
  result <- data %>%
    dplyr::group_by(.data[[group]]) %>%
    dplyr::summarise(
      count = dplyr::n(),
      proportion = count / total_n,
      .groups = "drop"
    )
}
```

## Exercise 3: Include examples (15 points)

> Demonstrate the usage of your function with a few examples. Use one or
> more new code chunks, describing what you’re doing.

> Note: If you want to deliberately show an error, you can use error =
> TRUE in your code chunk option.

1.  **`cancer_sample`**: Breast cancer diagnostic data

Statistics about the number of benign and malignant tumors in the breast
cancer dataset and the ratio.

``` r
# Explore the cancer_sample dataset
glimpse(cancer_sample)
```

    ## Rows: 569
    ## Columns: 32
    ## $ ID                      <dbl> 842302, 842517, 84300903, 84348301, 84358402, …
    ## $ diagnosis               <chr> "M", "M", "M", "M", "M", "M", "M", "M", "M", "…
    ## $ radius_mean             <dbl> 17.990, 20.570, 19.690, 11.420, 20.290, 12.450…
    ## $ texture_mean            <dbl> 10.38, 17.77, 21.25, 20.38, 14.34, 15.70, 19.9…
    ## $ perimeter_mean          <dbl> 122.80, 132.90, 130.00, 77.58, 135.10, 82.57, …
    ## $ area_mean               <dbl> 1001.0, 1326.0, 1203.0, 386.1, 1297.0, 477.1, …
    ## $ smoothness_mean         <dbl> 0.11840, 0.08474, 0.10960, 0.14250, 0.10030, 0…
    ## $ compactness_mean        <dbl> 0.27760, 0.07864, 0.15990, 0.28390, 0.13280, 0…
    ## $ concavity_mean          <dbl> 0.30010, 0.08690, 0.19740, 0.24140, 0.19800, 0…
    ## $ concave_points_mean     <dbl> 0.14710, 0.07017, 0.12790, 0.10520, 0.10430, 0…
    ## $ symmetry_mean           <dbl> 0.2419, 0.1812, 0.2069, 0.2597, 0.1809, 0.2087…
    ## $ fractal_dimension_mean  <dbl> 0.07871, 0.05667, 0.05999, 0.09744, 0.05883, 0…
    ## $ radius_se               <dbl> 1.0950, 0.5435, 0.7456, 0.4956, 0.7572, 0.3345…
    ## $ texture_se              <dbl> 0.9053, 0.7339, 0.7869, 1.1560, 0.7813, 0.8902…
    ## $ perimeter_se            <dbl> 8.589, 3.398, 4.585, 3.445, 5.438, 2.217, 3.18…
    ## $ area_se                 <dbl> 153.40, 74.08, 94.03, 27.23, 94.44, 27.19, 53.…
    ## $ smoothness_se           <dbl> 0.006399, 0.005225, 0.006150, 0.009110, 0.0114…
    ## $ compactness_se          <dbl> 0.049040, 0.013080, 0.040060, 0.074580, 0.0246…
    ## $ concavity_se            <dbl> 0.05373, 0.01860, 0.03832, 0.05661, 0.05688, 0…
    ## $ concave_points_se       <dbl> 0.015870, 0.013400, 0.020580, 0.018670, 0.0188…
    ## $ symmetry_se             <dbl> 0.03003, 0.01389, 0.02250, 0.05963, 0.01756, 0…
    ## $ fractal_dimension_se    <dbl> 0.006193, 0.003532, 0.004571, 0.009208, 0.0051…
    ## $ radius_worst            <dbl> 25.38, 24.99, 23.57, 14.91, 22.54, 15.47, 22.8…
    ## $ texture_worst           <dbl> 17.33, 23.41, 25.53, 26.50, 16.67, 23.75, 27.6…
    ## $ perimeter_worst         <dbl> 184.60, 158.80, 152.50, 98.87, 152.20, 103.40,…
    ## $ area_worst              <dbl> 2019.0, 1956.0, 1709.0, 567.7, 1575.0, 741.6, …
    ## $ smoothness_worst        <dbl> 0.1622, 0.1238, 0.1444, 0.2098, 0.1374, 0.1791…
    ## $ compactness_worst       <dbl> 0.6656, 0.1866, 0.4245, 0.8663, 0.2050, 0.5249…
    ## $ concavity_worst         <dbl> 0.71190, 0.24160, 0.45040, 0.68690, 0.40000, 0…
    ## $ concave_points_worst    <dbl> 0.26540, 0.18600, 0.24300, 0.25750, 0.16250, 0…
    ## $ symmetry_worst          <dbl> 0.4601, 0.2750, 0.3613, 0.6638, 0.2364, 0.3985…
    ## $ fractal_dimension_worst <dbl> 0.11890, 0.08902, 0.08758, 0.17300, 0.07678, 0…

``` r
# Calculating diagnosis group statistics and percentages using the `summarize_by_group()` function
result <- summarize_by_group(cancer_sample, "diagnosis")

# View Results
print(result)
```

    ## # A tibble: 2 × 3
    ##   diagnosis count proportion
    ##   <chr>     <int>      <dbl>
    ## 1 B           357      0.627
    ## 2 M           212      0.373

The total number of benign tumors obtained was 357, representing 62.7%,
and 212 malignant tumors, representing 37.3%.

2.  **`steam_games`**: Game information on Steam platform

Statistics on the number and percentage of game types in steam game data

``` r
# Explore the steam_games dataset
glimpse(steam_games)
```

    ## Rows: 40,833
    ## Columns: 21
    ## $ id                       <dbl> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14…
    ## $ url                      <chr> "https://store.steampowered.com/app/379720/DO…
    ## $ types                    <chr> "app", "app", "app", "app", "app", "bundle", …
    ## $ name                     <chr> "DOOM", "PLAYERUNKNOWN'S BATTLEGROUNDS", "BAT…
    ## $ desc_snippet             <chr> "Now includes all three premium DLC packs (Un…
    ## $ recent_reviews           <chr> "Very Positive,(554),- 89% of the 554 user re…
    ## $ all_reviews              <chr> "Very Positive,(42,550),- 92% of the 42,550 u…
    ## $ release_date             <chr> "May 12, 2016", "Dec 21, 2017", "Apr 24, 2018…
    ## $ developer                <chr> "id Software", "PUBG Corporation", "Harebrain…
    ## $ publisher                <chr> "Bethesda Softworks,Bethesda Softworks", "PUB…
    ## $ popular_tags             <chr> "FPS,Gore,Action,Demons,Shooter,First-Person,…
    ## $ game_details             <chr> "Single-player,Multi-player,Co-op,Steam Achie…
    ## $ languages                <chr> "English,French,Italian,German,Spanish - Spai…
    ## $ achievements             <dbl> 54, 37, 128, NA, NA, NA, 51, 55, 34, 43, 72, …
    ## $ genre                    <chr> "Action", "Action,Adventure,Massively Multipl…
    ## $ game_description         <chr> "About This Game Developed by id software, th…
    ## $ mature_content           <chr> NA, "Mature Content Description  The develope…
    ## $ minimum_requirements     <chr> "Minimum:,OS:,Windows 7/8.1/10 (64-bit versio…
    ## $ recommended_requirements <chr> "Recommended:,OS:,Windows 7/8.1/10 (64-bit ve…
    ## $ original_price           <dbl> 19.99, 29.99, 39.99, 44.99, 0.00, NA, 59.99, …
    ## $ discount_price           <dbl> 14.99, NA, NA, NA, NA, 35.18, 70.42, 17.58, N…

``` r
# Calculating the number and proportion of types using `summarize_by_group()` function
result <- summarize_by_group(steam_games, "types")

# View Results
print(result)
```

    ## # A tibble: 3 × 3
    ##   types  count proportion
    ##   <chr>  <int>      <dbl>
    ## 1 app    38021    0.931  
    ## 2 bundle  2572    0.0630 
    ## 3 sub      238    0.00583

There are three game types, app, bundle, and sub, which account for
93.11%, 6.3%, and 0.6% respectively.

## Exercise 4: Test the Function (25 points)

> Running examples is a good way of checking by-eye whether your
> function is working as expected. But, having a formal “yes or no”
> check is useful when you move on to other parts of your analysis.

``` r
library(tibble)
```

Test 1: Vector with no NA’s

``` r
# Create a tibble for testing summarize_by_group()
test_data1 <- tibble(
  ID = 1:6,
  group = c("A", "A", "B", "B", "B", "C"),
  value = c(10, 15, 20, 25, 30, 35)
)

test_data1
```

    ## # A tibble: 6 × 3
    ##      ID group value
    ##   <int> <chr> <dbl>
    ## 1     1 A        10
    ## 2     2 A        15
    ## 3     3 B        20
    ## 4     4 B        25
    ## 5     5 B        30
    ## 6     6 C        35

``` r
# ---- Run test ----
test_that("`summarize_by_group` works correctly with no NAs", {
  
  result <- summarize_by_group(test_data1, "group")
  
  # Check that returned column names are correct
  expect_true(all(c("group", "count", "proportion") %in% names(result)))
  
  # Check that counts per group are correct
  expect_equal(result$count[result$group == "A"], 2)
  expect_equal(result$count[result$group == "B"], 3)
  expect_equal(result$count[result$group == "C"], 1)
  
  # Check that proportions are correct (2/6, 3/6, 1/6)
  expect_equal(result$proportion[result$group == "A"], 2/6)
  expect_equal(result$proportion[result$group == "B"], 3/6)
  expect_equal(result$proportion[result$group == "C"], 1/6)
})
```

    ## Test passed 🎊

Test 2: Vector that has NA’s

``` r
# Create a tibble for testing summarize_by_group()
test_data2 <- tibble(
  ID = 1:8,
  group = c("A", "A", "B", "B", NA, "C", "C", NA),
  value = c(10, 20, 30, 40, 50, 60, 70, 80)
)

test_data2
```

    ## # A tibble: 8 × 3
    ##      ID group value
    ##   <int> <chr> <dbl>
    ## 1     1 A        10
    ## 2     2 A        20
    ## 3     3 B        30
    ## 4     4 B        40
    ## 5     5 <NA>     50
    ## 6     6 C        60
    ## 7     7 C        70
    ## 8     8 <NA>     80

``` r
# ---- Run test ----
test_that("summarize_by_group handles NA values correctly", {
  
  result <- summarize_by_group(test_data2, "group", na.rm = TRUE)
  
  # 1️⃣ Check that NA groups are excluded
  expect_false(any(is.na(result$group)))
  
  # 2️⃣ Check that counts are correct (A=2, B=2, C=2; 2 NAs removed)
  expect_equal(result$count[result$group == "A"], 2)
  expect_equal(result$count[result$group == "B"], 2)
  expect_equal(result$count[result$group == "C"], 2)
  
  # 3️⃣ Check that proportions are calculated based on non-NA groups (2/6 each)
  expect_equal(result$proportion[result$group == "A"], 2/6)
  expect_equal(result$proportion[result$group == "B"], 2/6)
  expect_equal(result$proportion[result$group == "C"], 2/6)
})
```

    ## Test passed 🌈

Test 3: Vector of length 0, like numeric(0)

``` r
# This tibble has the correct column name 'group' but contains 0 rows.
test_data3 <- tibble(group = character(0))

test_data3
```

    ## # A tibble: 0 × 1
    ## # ℹ 1 variable: group <chr>

``` r
# ---- Run test ----
test_result_empty <- test_that("summarize_by_group handles empty data correctly", {
  
  result <- summarize_by_group(test_data3, "group")
  
  # 1️⃣ Check that the result is a tibble (even if empty)
  expect_s3_class(result, "tbl_df")
  
  # 2️⃣ Check that the tibble has 0 rows
  expect_equal(nrow(result), 0)
  
  # 3️⃣ Check that the column names are correct
  expect_equal(names(result), c("group", "count", "proportion"))
})
```

    ## Test passed 😸
