P8105 Homework 2
================
Connie Zhang

## Problem 1

  - Importing trash wheel data and tidying/cleaning the data
  - Omitting non-data entries and rows that do not include dumpster
    specific data

<!-- end list -->

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
  mutate(month= as.numeric(month), month = month.name[month]) 
```

    ## Joining, by = c("year", "month", "total")

``` r
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

  - The `trash_wheel` dataset contains information inlcuding amount of
    total litter collected and the type of litter collected, such as
    sports balls, plastic bags, and glass bottles .The `combine_precip`
    dataset contains information on total precipitation for each month
    of 2017 and 2018 periods, measured in inches. There are 344
    observations in the `trash_wheel` dataset and 24 observations in the
    `combine_precip` dataset. The total amount of precipitation in 2018
    was 70.33. The median number of sports balls in a dumpster in 2017
    is 8.

## Problem 2

  - Clean pols\_month data and create a president variable

<!-- end list -->

``` r
pols_month = 
  read_csv("./data/pols-month.csv") %>%
  janitor::clean_names() %>%
  separate(mon, into= c("year", "month", "day")) %>%
  mutate (month = as.numeric(month), month = month.abb[month], year= as.numeric(year), president = ifelse(prez_gop, "gop", "dem")) %>%
  select(-prez_gop, -prez_dem, -day)
```

    ## Parsed with column specification:
    ## cols(
    ##   mon = col_date(format = ""),
    ##   prez_gop = col_double(),
    ##   gov_gop = col_double(),
    ##   sen_gop = col_double(),
    ##   rep_gop = col_double(),
    ##   prez_dem = col_double(),
    ##   gov_dem = col_double(),
    ##   sen_dem = col_double(),
    ##   rep_dem = col_double()
    ## )

``` r
pols_month
```

    ## # A tibble: 822 x 9
    ##     year month gov_gop sen_gop rep_gop gov_dem sen_dem rep_dem president
    ##    <dbl> <chr>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl> <chr>    
    ##  1  1947 Jan        23      51     253      23      45     198 dem      
    ##  2  1947 Feb        23      51     253      23      45     198 dem      
    ##  3  1947 Mar        23      51     253      23      45     198 dem      
    ##  4  1947 Apr        23      51     253      23      45     198 dem      
    ##  5  1947 May        23      51     253      23      45     198 dem      
    ##  6  1947 Jun        23      51     253      23      45     198 dem      
    ##  7  1947 Jul        23      51     253      23      45     198 dem      
    ##  8  1947 Aug        23      51     253      23      45     198 dem      
    ##  9  1947 Sep        23      51     253      23      45     198 dem      
    ## 10  1947 Oct        23      51     253      23      45     198 dem      
    ## # … with 812 more rows

  - Clean snp.csv data by arranging according to year and month

<!-- end list -->

``` r
snp_data = 
  read_csv("./data/snp.csv") %>%
  janitor::clean_names() %>%
  separate(date, c("month", "day", "year")) %>%
  mutate(year = as.numeric(year), month = as.numeric(month), month = month.name[month]) %>%
  select(year, month, -day, close) %>%
  arrange (year, month)
```

    ## Parsed with column specification:
    ## cols(
    ##   date = col_character(),
    ##   close = col_double()
    ## )

``` r
snp_data
```

    ## # A tibble: 787 x 3
    ##     year month    close
    ##    <dbl> <chr>    <dbl>
    ##  1  1950 April     18.0
    ##  2  1950 August    18.4
    ##  3  1950 December  20.4
    ##  4  1950 February  17.2
    ##  5  1950 January   17.0
    ##  6  1950 July      17.8
    ##  7  1950 June      17.7
    ##  8  1950 March     17.3
    ##  9  1950 May       18.8
    ## 10  1950 November  19.5
    ## # … with 777 more rows

  - Tidy unemployment.csv which will then be merged with snp.csv and
    pols-month.csv

<!-- end list -->

``` r
unemployment_data = 
  read_csv("./data/unemployment.csv") %>%
  pivot_longer((Jan:Dec), 
               names_to = "month", 
               values_to = "percent") %>%
  janitor::clean_names()
```

    ## Parsed with column specification:
    ## cols(
    ##   Year = col_double(),
    ##   Jan = col_double(),
    ##   Feb = col_double(),
    ##   Mar = col_double(),
    ##   Apr = col_double(),
    ##   May = col_double(),
    ##   Jun = col_double(),
    ##   Jul = col_double(),
    ##   Aug = col_double(),
    ##   Sep = col_double(),
    ##   Oct = col_double(),
    ##   Nov = col_double(),
    ##   Dec = col_double()
    ## )

``` r
unemployment_data
```

    ## # A tibble: 816 x 3
    ##     year month percent
    ##    <dbl> <chr>   <dbl>
    ##  1  1948 Jan       3.4
    ##  2  1948 Feb       3.8
    ##  3  1948 Mar       4  
    ##  4  1948 Apr       3.9
    ##  5  1948 May       3.5
    ##  6  1948 Jun       3.6
    ##  7  1948 Jul       3.6
    ##  8  1948 Aug       3.9
    ##  9  1948 Sep       3.8
    ## 10  1948 Oct       3.7
    ## # … with 806 more rows

  - Merge snp.csv with pols-month.csv, and then with unemployment.csv

<!-- end list -->

``` r
polsnp_data =
 left_join(pols_month, snp_data, by = c("year", "month"))

merged_data = 
  left_join(polsnp_data, unemployment_data, by = c("year", "month"))
merged_data
```

    ## # A tibble: 822 x 11
    ##     year month gov_gop sen_gop rep_gop gov_dem sen_dem rep_dem president
    ##    <dbl> <chr>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl> <chr>    
    ##  1  1947 Jan        23      51     253      23      45     198 dem      
    ##  2  1947 Feb        23      51     253      23      45     198 dem      
    ##  3  1947 Mar        23      51     253      23      45     198 dem      
    ##  4  1947 Apr        23      51     253      23      45     198 dem      
    ##  5  1947 May        23      51     253      23      45     198 dem      
    ##  6  1947 Jun        23      51     253      23      45     198 dem      
    ##  7  1947 Jul        23      51     253      23      45     198 dem      
    ##  8  1947 Aug        23      51     253      23      45     198 dem      
    ##  9  1947 Sep        23      51     253      23      45     198 dem      
    ## 10  1947 Oct        23      51     253      23      45     198 dem      
    ## # … with 812 more rows, and 2 more variables: close <dbl>, percent <dbl>

**Description of data:**

  - The

The resulting dataset, merged\_data, has the following range of years: a
minimum year of 1947 to a maximum year of 2015 Explain briefly what each
dataset contained, and describe the resulting dataset (e.g. give the
dimension, range of years, and names of key variables

## Problem 3

  - Importing, loading, and tidying NYC Open data on popularity of baby
    names

<!-- end list -->

``` r
baby_data =
  read_csv ("./data/popular_baby_names.csv") %>%
  janitor::clean_names() %>%
  distinct() %>%
  mutate(ethnicity =recode(ethnicity, "ASIAN AND PACI" = "ASIAN AND PACIFIC ISLANDER", "BLACK NON HISP" = "BLACK NON HISPANIC", "WHITE NON HISP" = "WHITE NON HISPANIC"), ethnicity = str_to_lower(ethnicity), gender = str_to_lower(gender), childs_first_name = str_to_lower (childs_first_name))
```

    ## Parsed with column specification:
    ## cols(
    ##   `Year of Birth` = col_double(),
    ##   Gender = col_character(),
    ##   Ethnicity = col_character(),
    ##   `Child's First Name` = col_character(),
    ##   Count = col_double(),
    ##   Rank = col_double()
    ## )

``` r
baby_data
```

    ## # A tibble: 12,181 x 6
    ##    year_of_birth gender ethnicity              childs_first_na… count  rank
    ##            <dbl> <chr>  <chr>                  <chr>            <dbl> <dbl>
    ##  1          2016 female asian and pacific isl… olivia             172     1
    ##  2          2016 female asian and pacific isl… chloe              112     2
    ##  3          2016 female asian and pacific isl… sophia             104     3
    ##  4          2016 female asian and pacific isl… emily               99     4
    ##  5          2016 female asian and pacific isl… emma                99     4
    ##  6          2016 female asian and pacific isl… mia                 79     5
    ##  7          2016 female asian and pacific isl… charlotte           59     6
    ##  8          2016 female asian and pacific isl… sarah               57     7
    ##  9          2016 female asian and pacific isl… isabella            56     8
    ## 10          2016 female asian and pacific isl… hannah              56     8
    ## # … with 12,171 more rows

  - Creating table showing the name Olivia and its rank in popularity
    for female babies over time

<!-- end list -->

``` r
baby_data %>%
  filter (childs_first_name == "olivia") %>%
  select (-count) %>%
  pivot_wider(names_from = "year_of_birth", values_from = "rank") %>%
  knitr::kable()
```

| gender | ethnicity                  | childs\_first\_name | 2016 | 2015 | 2014 | 2013 | 2012 | 2011 |
| :----- | :------------------------- | :------------------ | ---: | ---: | ---: | ---: | ---: | ---: |
| female | asian and pacific islander | olivia              |    1 |    1 |    1 |    3 |    3 |    4 |
| female | black non hispanic         | olivia              |    8 |    4 |    8 |    6 |    8 |   10 |
| female | hispanic                   | olivia              |   13 |   16 |   16 |   22 |   22 |   18 |
| female | white non hispanic         | olivia              |    1 |    1 |    1 |    1 |    4 |    2 |

``` r
baby_data
```

    ## # A tibble: 12,181 x 6
    ##    year_of_birth gender ethnicity              childs_first_na… count  rank
    ##            <dbl> <chr>  <chr>                  <chr>            <dbl> <dbl>
    ##  1          2016 female asian and pacific isl… olivia             172     1
    ##  2          2016 female asian and pacific isl… chloe              112     2
    ##  3          2016 female asian and pacific isl… sophia             104     3
    ##  4          2016 female asian and pacific isl… emily               99     4
    ##  5          2016 female asian and pacific isl… emma                99     4
    ##  6          2016 female asian and pacific isl… mia                 79     5
    ##  7          2016 female asian and pacific isl… charlotte           59     6
    ##  8          2016 female asian and pacific isl… sarah               57     7
    ##  9          2016 female asian and pacific isl… isabella            56     8
    ## 10          2016 female asian and pacific isl… hannah              56     8
    ## # … with 12,171 more rows

  - Creating table showing the most popular male child name over time

<!-- end list -->

``` r
baby_data %>%
  filter (gender == "male", rank == 1) %>%
  select (-count) %>%
  pivot_wider(names_from = "year_of_birth", values_from = "childs_first_name") %>%
  knitr::kable()
```

| gender | ethnicity                  | rank | 2016   | 2015   | 2014   | 2013   | 2012   | 2011    |
| :----- | :------------------------- | ---: | :----- | :----- | :----- | :----- | :----- | :------ |
| male   | asian and pacific islander |    1 | ethan  | jayden | jayden | jayden | ryan   | ethan   |
| male   | black non hispanic         |    1 | noah   | noah   | ethan  | ethan  | jayden | jayden  |
| male   | hispanic                   |    1 | liam   | liam   | liam   | jayden | jayden | jayden  |
| male   | white non hispanic         |    1 | joseph | david  | joseph | david  | joseph | michael |

``` r
baby_data
```

    ## # A tibble: 12,181 x 6
    ##    year_of_birth gender ethnicity              childs_first_na… count  rank
    ##            <dbl> <chr>  <chr>                  <chr>            <dbl> <dbl>
    ##  1          2016 female asian and pacific isl… olivia             172     1
    ##  2          2016 female asian and pacific isl… chloe              112     2
    ##  3          2016 female asian and pacific isl… sophia             104     3
    ##  4          2016 female asian and pacific isl… emily               99     4
    ##  5          2016 female asian and pacific isl… emma                99     4
    ##  6          2016 female asian and pacific isl… mia                 79     5
    ##  7          2016 female asian and pacific isl… charlotte           59     6
    ##  8          2016 female asian and pacific isl… sarah               57     7
    ##  9          2016 female asian and pacific isl… isabella            56     8
    ## 10          2016 female asian and pacific isl… hannah              56     8
    ## # … with 12,171 more rows

  - Creating scatter plot with popularity of name (x axis) againgst
    number of children with the name (y axis) for male, white
    non-hispanic children born in 2016.

<!-- end list -->

``` r
scatterplot_child = baby_data %>% 
  filter(gender == "male", ethnicity == "white non hispanic", year_of_birth == "2016")
ggplot(scatterplot_child, aes(x = rank, y = count )) + geom_point()
```

![](ds_hw2_cz2540_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->
