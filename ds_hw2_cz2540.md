P8105 Homework 2
================
Connie Zhang

## Problem 1

``` r
trash_wheel = 
  read_excel("./data/Trash-Wheel.xlsx", sheet = 1) %>%
  janitor::clean_names() %>%
  drop_na (dumpster) %>%
  select(-15, -16, -17) %>%
  mutate(sports_balls = as.integer(sports_balls))
```

    ## New names:
    ## * `` -> ...15
    ## * `` -> ...16
    ## * `` -> ...17

``` r
  trash_wheel
```

    ## # A tibble: 344 x 14
    ##    dumpster month  year date                weight_tons volume_cubic_ya…
    ##       <dbl> <chr> <dbl> <dttm>                    <dbl>            <dbl>
    ##  1        1 May    2014 2014-05-16 00:00:00        4.31               18
    ##  2        2 May    2014 2014-05-16 00:00:00        2.74               13
    ##  3        3 May    2014 2014-05-16 00:00:00        3.45               15
    ##  4        4 May    2014 2014-05-17 00:00:00        3.1                15
    ##  5        5 May    2014 2014-05-17 00:00:00        4.06               18
    ##  6        6 May    2014 2014-05-20 00:00:00        2.71               13
    ##  7        7 May    2014 2014-05-21 00:00:00        1.91                8
    ##  8        8 May    2014 2014-05-28 00:00:00        3.7                16
    ##  9        9 June   2014 2014-06-05 00:00:00        2.52               14
    ## 10       10 June   2014 2014-06-11 00:00:00        3.76               18
    ## # … with 334 more rows, and 8 more variables: plastic_bottles <dbl>,
    ## #   polystyrene <dbl>, cigarette_butts <dbl>, glass_bottles <dbl>,
    ## #   grocery_bags <dbl>, chip_bags <dbl>, sports_balls <int>,
    ## #   homes_powered <dbl>

  - Read and clean precipitation data for 2017
  - Omitting rows without data and adding variable 2017

<!-- end list -->

``` r
precip_2017 =
  read_excel("./data/Trash-Wheel.xlsx", sheet = 6, range = "A2:B14") %>%
  janitor::clean_names() %>%
  mutate (year = 2017) %>%
  select(year,month,total)
precip_2017
```

    ## # A tibble: 12 x 3
    ##     year month total
    ##    <dbl> <dbl> <dbl>
    ##  1  2017     1  2.34
    ##  2  2017     2  1.46
    ##  3  2017     3  3.57
    ##  4  2017     4  3.99
    ##  5  2017     5  5.64
    ##  6  2017     6  1.4 
    ##  7  2017     7  7.09
    ##  8  2017     8  4.44
    ##  9  2017     9  1.95
    ## 10  2017    10  0   
    ## 11  2017    11  0.11
    ## 12  2017    12  0.94

  - Read and clean precipitation data for 2018
  - Omitting rows without data and adding variable 2018

<!-- end list -->

``` r
precip_2018 =
  read_excel("./Data/Trash-Wheel.xlsx", sheet = 5, range = "A2:B14") %>%
  janitor::clean_names() %>%
  mutate (year = 2018) %>%
  select (year,month,total)
precip_2018
```

    ## # A tibble: 12 x 3
    ##     year month total
    ##    <dbl> <dbl> <dbl>
    ##  1  2018     1  0.94
    ##  2  2018     2  4.8 
    ##  3  2018     3  2.69
    ##  4  2018     4  4.69
    ##  5  2018     5  9.27
    ##  6  2018     6  4.77
    ##  7  2018     7 10.2 
    ##  8  2018     8  6.45
    ##  9  2018     9 10.5 
    ## 10  2018    10  2.12
    ## 11  2018    11  7.82
    ## 12  2018    12  6.11

  - Combining precipation datasets, converting month to character
    variable

<!-- end list -->

``` r
combine_precip = 
  full_join(precip_2017,precip_2018) %>%
  mutate(month= month.abb[month])
```

    ## Joining, by = c("year", "month", "total")

``` r
combine_precip
```

    ## # A tibble: 24 x 3
    ##     year month total
    ##    <dbl> <chr> <dbl>
    ##  1  2017 Jan    2.34
    ##  2  2017 Feb    1.46
    ##  3  2017 Mar    3.57
    ##  4  2017 Apr    3.99
    ##  5  2017 May    5.64
    ##  6  2017 Jun    1.4 
    ##  7  2017 Jul    7.09
    ##  8  2017 Aug    4.44
    ##  9  2017 Sep    1.95
    ## 10  2017 Oct    0   
    ## # … with 14 more rows

  - \*Description of data:
