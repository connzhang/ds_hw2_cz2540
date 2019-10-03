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
  bind_rows(precip_2017,precip_2018) %>%
  mutate(month= as.numeric(month), month = month.name[month]) 
combine_precip
```

    ## # A tibble: 24 x 3
    ##     year month     total
    ##    <dbl> <chr>     <dbl>
    ##  1  2017 January    2.34
    ##  2  2017 February   1.46
    ##  3  2017 March      3.57
    ##  4  2017 April      3.99
    ##  5  2017 May        5.64
    ##  6  2017 June       1.4 
    ##  7  2017 July       7.09
    ##  8  2017 August     4.44
    ##  9  2017 September  1.95
    ## 10  2017 October    0   
    ## # … with 14 more rows

**Description of data:**

  - The Mr. Trash Wheel dataset contains information on dumpster number,
    date of trash collection, amount of total litter collected and the
    type of litter collected.The combined precipitation dataset contains
    information on total precipitation in inches for each month of 2017
    and 2018 periods. The number of observations in both resulting
    datasets are 344 and 24 respectively.The total amount of
    precipitation in 2018 was 70.33. The median number of sports balls
    in a dumpster in 2017 is 8.
