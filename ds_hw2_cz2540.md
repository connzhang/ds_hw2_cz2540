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
  separate(date, into= c("year", "month", "day")) %>%
  mutate(month = as.numeric(month), month =month.abb[month], year= as.numeric(year))
```

    ## Parsed with column specification:
    ## cols(
    ##   date = col_character(),
    ##   close = col_double()
    ## )

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

  - Creating table showing the most popular male child name over time

<!-- end list -->

``` r
baby_data %>%
  filter (gender == "male") %>%
  select (-count) %>%
  pivot_wider(names_from = "year_of_birth", values_from = "rank") %>%
  knitr::kable()
```

| gender | ethnicity                  | childs\_first\_name | 2016 | 2015 | 2014 | 2013 | 2012 | 2011 |
| :----- | :------------------------- | :------------------ | ---: | ---: | ---: | ---: | ---: | ---: |
| male   | asian and pacific islander | ethan               |    1 |    2 |    2 |    2 |    2 |    1 |
| male   | asian and pacific islander | ryan                |    2 |    3 |    3 |    3 |    1 |    3 |
| male   | asian and pacific islander | muhammad            |    3 |    4 |    6 |    6 |    7 |   11 |
| male   | asian and pacific islander | lucas               |    4 |    6 |    4 |    4 |    3 |    5 |
| male   | asian and pacific islander | jayden              |    5 |    1 |    1 |    1 |    2 |    2 |
| male   | asian and pacific islander | aiden               |    6 |    5 |    5 |    5 |    6 |    7 |
| male   | asian and pacific islander | daniel              |    7 |   16 |    7 |    7 |    9 |   10 |
| male   | asian and pacific islander | evan                |    8 |    8 |   11 |   14 |   13 |   19 |
| male   | asian and pacific islander | jason               |    9 |    8 |   10 |    9 |    8 |    6 |
| male   | asian and pacific islander | liam                |    9 |    9 |   16 |   10 |   29 |   41 |
| male   | asian and pacific islander | matthew             |   10 |   10 |   12 |   12 |   12 |   12 |
| male   | asian and pacific islander | austin              |   11 |   15 |   14 |   19 |   18 |   16 |
| male   | asian and pacific islander | eric                |   12 |   12 |    9 |    8 |    5 |    9 |
| male   | asian and pacific islander | kevin               |   13 |   23 |   15 |   13 |   10 |    8 |
| male   | asian and pacific islander | william             |   14 |    7 |    8 |   15 |   11 |   15 |
| male   | asian and pacific islander | justin              |   15 |   22 |   17 |   11 |    4 |    4 |
| male   | asian and pacific islander | owen                |   16 |   24 |   24 |   18 |   24 |   39 |
| male   | asian and pacific islander | mohammad            |   17 |   11 |   14 |   24 |   23 |   23 |
| male   | asian and pacific islander | oliver              |   17 |   18 |   18 |   36 |   44 |   56 |
| male   | asian and pacific islander | aaron               |   18 |   17 |   14 |   13 |   19 |   19 |
| male   | asian and pacific islander | noah                |   18 |   17 |   29 |   20 |   25 |   47 |
| male   | asian and pacific islander | henry               |   19 |   23 |   38 |   34 |   39 |   45 |
| male   | asian and pacific islander | benjamin            |   20 |   22 |   24 |   37 |   28 |   28 |
| male   | asian and pacific islander | mason               |   21 |   13 |   13 |   17 |   21 |   33 |
| male   | asian and pacific islander | nathan              |   21 |   19 |   23 |   22 |   36 |   26 |
| male   | asian and pacific islander | alexander           |   22 |   14 |   21 |   16 |   16 |   22 |
| male   | asian and pacific islander | leo                 |   23 |   25 |   24 |   23 |   30 |   27 |
| male   | asian and pacific islander | vincent             |   23 |   24 |   25 |   23 |   15 |   13 |
| male   | asian and pacific islander | ian                 |   24 |   37 |   28 |   38 |   30 |   44 |
| male   | asian and pacific islander | dylan               |   25 |   20 |   22 |   24 |   14 |   17 |
| male   | asian and pacific islander | jacob               |   26 |   23 |   24 |   13 |   27 |   21 |
| male   | asian and pacific islander | andrew              |   26 |   21 |   14 |   25 |   19 |   14 |
| male   | asian and pacific islander | kyle                |   27 |   25 |   19 |   31 |   20 |   24 |
| male   | asian and pacific islander | jonathan            |   27 |   27 |   28 |   28 |   26 |   20 |
| male   | asian and pacific islander | ivan                |   28 |   27 |   28 |   24 |   17 |   15 |
| male   | asian and pacific islander | james               |   29 |   18 |   20 |   22 |   30 |   31 |
| male   | asian and pacific islander | michael             |   30 |   30 |   27 |   34 |   36 |   29 |
| male   | asian and pacific islander | samuel              |   31 |   38 |   46 |   34 |   37 |   36 |
| male   | asian and pacific islander | syed                |   32 |   37 |   48 |   44 |   43 |   50 |
| male   | asian and pacific islander | brian               |   32 |   39 |   34 |   21 |   32 |   27 |
| male   | asian and pacific islander | ibrahim             |   33 |   33 |   44 |   42 |   42 |   49 |
| male   | asian and pacific islander | brandon             |   33 |   29 |   26 |   33 |   35 |   37 |
| male   | asian and pacific islander | alex                |   33 |   26 |   26 |   24 |   24 |   18 |
| male   | asian and pacific islander | alvin               |   34 |   38 |   36 |   30 |   29 |   25 |
| male   | asian and pacific islander | abdullah            |   34 |   38 |   38 |   49 |   43 |   36 |
| male   | asian and pacific islander | max                 |   35 |   39 |   46 |   40 |   39 |   52 |
| male   | asian and pacific islander | jasper              |   35 |   35 |   50 |   52 |   57 |   NA |
| male   | asian and pacific islander | eason               |   35 |   34 |   30 |   35 |   41 |   56 |
| male   | asian and pacific islander | kai                 |   36 |   42 |   47 |   48 |   57 |   54 |
| male   | asian and pacific islander | tenzin              |   36 |   39 |   29 |   42 |   41 |   42 |
| male   | asian and pacific islander | logan               |   37 |   42 |   44 |   39 |   46 |   47 |
| male   | asian and pacific islander | jeremy              |   37 |   36 |   39 |   45 |   22 |   49 |
| male   | asian and pacific islander | ayaan               |   37 |   31 |   18 |   24 |   32 |   35 |
| male   | asian and pacific islander | darren              |   38 |   41 |   53 |   40 |   45 |   45 |
| male   | asian and pacific islander | andy                |   38 |   28 |   29 |   32 |   23 |   25 |
| male   | asian and pacific islander | gavin               |   38 |   45 |   48 |   43 |   46 |   39 |
| male   | asian and pacific islander | joshua              |   39 |   38 |   32 |   26 |   29 |   31 |
| male   | asian and pacific islander | joseph              |   40 |   43 |   50 |   44 |   42 |   45 |
| male   | asian and pacific islander | mohammed            |   40 |   32 |   33 |   49 |   37 |   26 |
| male   | asian and pacific islander | theodore            |   41 |   45 |   46 |   50 |   59 |   NA |
| male   | asian and pacific islander | julian              |   41 |   43 |   51 |   48 |   50 |   55 |
| male   | asian and pacific islander | adam                |   41 |   26 |   33 |   32 |   32 |   38 |
| male   | asian and pacific islander | jaden               |   42 |   30 |   38 |   29 |   31 |   43 |
| male   | asian and pacific islander | rayan               |   42 |   39 |   48 |   57 |   51 |   56 |
| male   | asian and pacific islander | marcus              |   42 |   47 |   40 |   43 |   36 |   46 |
| male   | asian and pacific islander | arham               |   42 |   44 |   56 |   NA |   NA |   NA |
| male   | asian and pacific islander | nicholas            |   42 |   27 |   38 |   38 |   38 |   40 |
| male   | asian and pacific islander | david               |   43 |   31 |   38 |   27 |   33 |   38 |
| male   | asian and pacific islander | alan                |   44 |   38 |   40 |   43 |   38 |   30 |
| male   | asian and pacific islander | zachary             |   44 |   40 |   43 |   46 |   38 |   54 |
| male   | asian and pacific islander | aidan               |   44 |   48 |   39 |   42 |   34 |   34 |
| male   | asian and pacific islander | sean                |   44 |   47 |   56 |   46 |   48 |   52 |
| male   | asian and pacific islander | raymond             |   44 |   30 |   31 |   49 |   40 |   39 |
| male   | asian and pacific islander | anthony             |   44 |   39 |   37 |   32 |   29 |   23 |
| male   | asian and pacific islander | derek               |   45 |   40 |   36 |   43 |   38 |   34 |
| male   | asian and pacific islander | kyson               |   45 |   NA |   NA |   NA |   NA |   NA |
| male   | asian and pacific islander | rohan               |   45 |   51 |   47 |   53 |   51 |   51 |
| male   | asian and pacific islander | connor              |   45 |   41 |   47 |   44 |   54 |   55 |
| male   | asian and pacific islander | oscar               |   45 |   38 |   41 |   31 |   54 |   41 |
| male   | asian and pacific islander | luke                |   46 |   42 |   42 |   41 |   46 |   55 |
| male   | asian and pacific islander | jackson             |   46 |   48 |   NA |   50 |   54 |   53 |
| male   | asian and pacific islander | ayden               |   46 |   43 |   53 |   51 |   52 |   42 |
| male   | asian and pacific islander | tyler               |   46 |   37 |   35 |   31 |   33 |   35 |
| male   | asian and pacific islander | christopher         |   47 |   41 |   50 |   38 |   56 |   45 |
| male   | asian and pacific islander | nathaniel           |   47 |   39 |   45 |   46 |   47 |   50 |
| male   | asian and pacific islander | aryan               |   47 |   50 |   50 |   43 |   57 |   48 |
| male   | asian and pacific islander | caleb               |   47 |   46 |   45 |   36 |   56 |   50 |
| male   | asian and pacific islander | zayn                |   48 |   NA |   NA |   NA |   NA |   NA |
| male   | asian and pacific islander | tristan             |   48 |   47 |   49 |   53 |   48 |   NA |
| male   | asian and pacific islander | christian           |   48 |   48 |   53 |   49 |   48 |   45 |
| male   | asian and pacific islander | winston             |   48 |   NA |   58 |   51 |   59 |   NA |
| male   | asian and pacific islander | isaac               |   48 |   44 |   35 |   42 |   46 |   53 |
| male   | asian and pacific islander | steven              |   48 |   45 |   50 |   34 |   43 |   32 |
| male   | asian and pacific islander | adrian              |   49 |   43 |   42 |   44 |   44 |   47 |
| male   | asian and pacific islander | arjun               |   49 |   53 |   48 |   47 |   50 |   39 |
| male   | asian and pacific islander | hudson              |   49 |   52 |   51 |   52 |   59 |   NA |
| male   | asian and pacific islander | allen               |   49 |   45 |   42 |   43 |   38 |   52 |
| male   | asian and pacific islander | wesley              |   49 |   46 |   50 |   53 |   53 |   NA |
| male   | asian and pacific islander | sebastian           |   49 |   44 |   51 |   54 |   54 |   52 |
| male   | asian and pacific islander | george              |   50 |   49 |   53 |   53 |   NA |   54 |
| male   | asian and pacific islander | hamza               |   50 |   NA |   NA |   NA |   60 |   NA |
| male   | asian and pacific islander | xavier              |   50 |   52 |   58 |   55 |   NA |   56 |
| male   | asian and pacific islander | elijah              |   50 |   51 |   55 |   52 |   52 |   54 |
| male   | asian and pacific islander | louis               |   50 |   50 |   50 |   51 |   54 |   56 |
| male   | asian and pacific islander | rayyan              |   50 |   49 |   44 |   NA |   59 |   NA |
| male   | asian and pacific islander | yusuf               |   50 |   48 |   NA |   NA |   56 |   NA |
| male   | asian and pacific islander | jordan              |   51 |   NA |   51 |   48 |   55 |   43 |
| male   | asian and pacific islander | anson               |   51 |   40 |   45 |   45 |   46 |   48 |
| male   | asian and pacific islander | jack                |   51 |   51 |   47 |   46 |   45 |   45 |
| male   | asian and pacific islander | ayan                |   51 |   48 |   52 |   42 |   59 |   NA |
| male   | asian and pacific islander | gabriel             |   51 |   49 |   51 |   50 |   44 |   53 |
| male   | asian and pacific islander | caden               |   51 |   NA |   NA |   NA |   NA |   NA |
| male   | asian and pacific islander | miles               |   51 |   49 |   48 |   56 |   53 |   56 |
| male   | asian and pacific islander | harrison            |   51 |   52 |   52 |   49 |   58 |   53 |
| male   | asian and pacific islander | kingsley            |   51 |   49 |   47 |   44 |   43 |   31 |
| male   | asian and pacific islander | jerry               |   51 |   47 |   53 |   49 |   40 |   41 |
| male   | asian and pacific islander | benson              |   51 |   47 |   45 |   55 |   56 |   47 |
| male   | asian and pacific islander | charles             |   52 |   43 |   47 |   43 |   49 |   49 |
| male   | asian and pacific islander | bryan               |   52 |   41 |   53 |   43 |   53 |   51 |
| male   | asian and pacific islander | john                |   52 |   48 |   42 |   42 |   45 |   45 |
| male   | asian and pacific islander | musa                |   52 |   49 |   57 |   NA |   58 |   NA |
| male   | asian and pacific islander | thomas              |   52 |   48 |   51 |   55 |   46 |   48 |
| male   | asian and pacific islander | kayden              |   52 |   49 |   52 |   55 |   NA |   NA |
| male   | asian and pacific islander | roy                 |   53 |   54 |   45 |   52 |   56 |   NA |
| male   | asian and pacific islander | felix               |   53 |   51 |   35 |   44 |   49 |   44 |
| male   | asian and pacific islander | lawrence            |   53 |   50 |   42 |   48 |   58 |   52 |
| male   | asian and pacific islander | eli                 |   54 |   NA |   58 |   56 |   59 |   NA |
| male   | asian and pacific islander | leon                |   54 |   51 |   53 |   52 |   57 |   49 |
| male   | asian and pacific islander | aarav               |   54 |   50 |   NA |   50 |   61 |   51 |
| male   | asian and pacific islander | ahnaf               |   54 |   NA |   57 |   56 |   NA |   NA |
| male   | asian and pacific islander | carson              |   54 |   46 |   30 |   57 |   61 |   46 |
| male   | asian and pacific islander | jeffrey             |   54 |   40 |   51 |   55 |   42 |   41 |
| male   | asian and pacific islander | preston             |   54 |   53 |   NA |   NA |   NA |   NA |
| male   | asian and pacific islander | krish               |   55 |   NA |   NA |   NA |   56 |   NA |
| male   | asian and pacific islander | edison              |   55 |   53 |   54 |   NA |   55 |   52 |
| male   | asian and pacific islander | mustafa             |   55 |   54 |   NA |   NA |   59 |   NA |
| male   | asian and pacific islander | calvin              |   55 |   NA |   58 |   53 |   61 |   53 |
| male   | asian and pacific islander | chris               |   55 |   52 |   54 |   56 |   NA |   NA |
| male   | asian and pacific islander | ahmed               |   55 |   43 |   56 |   49 |   56 |   45 |
| male   | asian and pacific islander | hunter              |   55 |   43 |   50 |   51 |   54 |   NA |
| male   | asian and pacific islander | kabir               |   55 |   NA |   58 |   NA |   NA |   NA |
| male   | asian and pacific islander | abdul               |   55 |   54 |   NA |   47 |   55 |   46 |
| male   | asian and pacific islander | richard             |   55 |   40 |   50 |   45 |   37 |   40 |
| male   | asian and pacific islander | zayan               |   55 |   54 |   50 |   56 |   61 |   56 |
| male   | asian and pacific islander | md                  |   55 |   NA |   NA |   56 |   NA |   NA |
| male   | asian and pacific islander | aayan               |   55 |   51 |   58 |   54 |   58 |   NA |
| male   | asian and pacific islander | cameron             |   56 |   53 |   57 |   NA |   NA |   NA |
| male   | asian and pacific islander | rehan               |   56 |   NA |   55 |   56 |   60 |   NA |
| male   | asian and pacific islander | azaan               |   56 |   NA |   58 |   NA |   NA |   NA |
| male   | asian and pacific islander | armaan              |   56 |   52 |   55 |   NA |   NA |   51 |
| male   | asian and pacific islander | hashim              |   56 |   NA |   NA |   NA |   NA |   NA |
| male   | asian and pacific islander | johnny              |   56 |   55 |   NA |   55 |   57 |   48 |
| male   | asian and pacific islander | aaryan              |   56 |   55 |   NA |   NA |   NA |   NA |
| male   | asian and pacific islander | adyan               |   56 |   NA |   51 |   54 |   NA |   NA |
| male   | asian and pacific islander | maxwell             |   56 |   49 |   48 |   53 |   55 |   NA |
| male   | asian and pacific islander | brayden             |   56 |   47 |   58 |   55 |   56 |   NA |
| male   | asian and pacific islander | cyrus               |   56 |   NA |   NA |   NA |   60 |   NA |
| male   | asian and pacific islander | kian                |   56 |   NA |   NA |   NA |   NA |   NA |
| male   | asian and pacific islander | shawn               |   57 |   NA |   57 |   54 |   NA |   56 |
| male   | asian and pacific islander | aden                |   57 |   NA |   NA |   NA |   NA |   NA |
| male   | asian and pacific islander | damon               |   57 |   55 |   51 |   57 |   NA |   NA |
| male   | asian and pacific islander | ashton              |   57 |   NA |   NA |   NA |   NA |   NA |
| male   | asian and pacific islander | eden                |   57 |   54 |   51 |   57 |   NA |   NA |
| male   | asian and pacific islander | ahmad               |   57 |   50 |   NA |   NA |   NA |   NA |
| male   | asian and pacific islander | elvis               |   57 |   NA |   55 |   56 |   61 |   55 |
| male   | asian and pacific islander | carter              |   57 |   NA |   56 |   56 |   61 |   NA |
| male   | asian and pacific islander | stanley             |   57 |   54 |   54 |   51 |   53 |   43 |
| male   | asian and pacific islander | lincoln             |   58 |   NA |   NA |   NA |   NA |   NA |
| male   | asian and pacific islander | hayden              |   58 |   NA |   52 |   50 |   60 |   56 |
| male   | asian and pacific islander | farhan              |   58 |   NA |   NA |   56 |   NA |   49 |
| male   | asian and pacific islander | edwin               |   58 |   52 |   57 |   54 |   NA |   51 |
| male   | asian and pacific islander | edward              |   58 |   53 |   55 |   53 |   61 |   NA |
| male   | asian and pacific islander | aahil               |   58 |   52 |   54 |   NA |   NA |   NA |
| male   | asian and pacific islander | albert              |   58 |   NA |   48 |   45 |   53 |   NA |
| male   | asian and pacific islander | jace                |   58 |   NA |   NA |   NA |   NA |   NA |
| male   | asian and pacific islander | asher               |   58 |   52 |   53 |   57 |   59 |   NA |
| male   | asian and pacific islander | simon               |   58 |   NA |   58 |   NA |   NA |   52 |
| male   | asian and pacific islander | aydin               |   58 |   52 |   NA |   NA |   NA |   NA |
| male   | asian and pacific islander | cody                |   58 |   NA |   NA |   NA |   61 |   56 |
| male   | asian and pacific islander | hugo                |   58 |   NA |   NA |   NA |   NA |   NA |
| male   | asian and pacific islander | victor              |   59 |   55 |   56 |   54 |   49 |   49 |
| male   | asian and pacific islander | cayden              |   59 |   NA |   NA |   NA |   NA |   NA |
| male   | asian and pacific islander | timothy             |   59 |   49 |   49 |   50 |   55 |   50 |
| male   | asian and pacific islander | wilson              |   59 |   49 |   57 |   49 |   56 |   43 |
| male   | asian and pacific islander | sulaiman            |   59 |   NA |   NA |   NA |   NA |   NA |
| male   | asian and pacific islander | nelson              |   59 |   50 |   NA |   NA |   59 |   54 |
| male   | asian and pacific islander | kimi                |   59 |   43 |   44 |   55 |   NA |   NA |
| male   | asian and pacific islander | ryder               |   59 |   52 |   53 |   NA |   NA |   NA |
| male   | asian and pacific islander | ali                 |   59 |   52 |   47 |   45 |   54 |   53 |
| male   | asian and pacific islander | robert              |   59 |   NA |   NA |   NA |   NA |   NA |
| male   | asian and pacific islander | alston              |   59 |   55 |   55 |   NA |   57 |   NA |
| male   | asian and pacific islander | devin               |   59 |   46 |   46 |   57 |   55 |   45 |
| male   | asian and pacific islander | terry               |   59 |   NA |   NA |   NA |   NA |   55 |
| male   | asian and pacific islander | mikael              |   59 |   NA |   NA |   NA |   NA |   NA |
| male   | asian and pacific islander | grayson             |   59 |   51 |   56 |   NA |   NA |   NA |
| male   | black non hispanic         | noah                |    1 |    1 |    2 |    3 |    9 |   11 |
| male   | black non hispanic         | aiden               |    2 |    3 |    3 |    3 |    2 |    2 |
| male   | black non hispanic         | elijah              |    3 |    6 |    4 |    4 |    6 |    3 |
| male   | black non hispanic         | liam                |    4 |    2 |    6 |   10 |   11 |   24 |
| male   | black non hispanic         | ethan               |    5 |    5 |    1 |    1 |    3 |    6 |
| male   | black non hispanic         | jeremiah            |    6 |    4 |    5 |    6 |    4 |    4 |
| male   | black non hispanic         | amir                |    7 |   11 |   10 |    7 |   17 |   12 |
| male   | black non hispanic         | joshua              |    8 |    8 |    7 |    5 |    5 |    5 |
| male   | black non hispanic         | mason               |    9 |    7 |    9 |    8 |    8 |   16 |
| male   | black non hispanic         | josiah              |    9 |    5 |   11 |    9 |    8 |    7 |
| male   | black non hispanic         | carter              |   10 |    9 |    8 |   17 |   25 |   39 |
| male   | black non hispanic         | jayden              |   11 |   10 |    3 |    2 |    1 |    1 |
| male   | black non hispanic         | michael             |   12 |   12 |   12 |   12 |    6 |   10 |
| male   | black non hispanic         | david               |   13 |   19 |   28 |   26 |   16 |   15 |
| male   | black non hispanic         | logan               |   14 |   10 |   16 |   25 |   33 |   49 |
| male   | black non hispanic         | chase               |   14 |   13 |   16 |   23 |   21 |   33 |
| male   | black non hispanic         | king                |   15 |   18 |   18 |   19 |   29 |   60 |
| male   | black non hispanic         | christian           |   16 |   16 |   14 |   13 |    7 |   11 |
| male   | black non hispanic         | cameron             |   17 |   23 |   34 |   27 |   23 |   29 |
| male   | black non hispanic         | tyler               |   17 |   17 |   13 |   11 |   10 |    9 |
| male   | black non hispanic         | isaiah              |   18 |   15 |   22 |   16 |    9 |    8 |
| male   | black non hispanic         | jacob               |   18 |   14 |   15 |   14 |   20 |   17 |
| male   | black non hispanic         | malachi             |   19 |   32 |   28 |   34 |   22 |   21 |
| male   | black non hispanic         | zion                |   20 |   32 |   33 |   24 |   31 |   39 |
| male   | black non hispanic         | daniel              |   21 |   35 |   23 |   18 |   16 |   18 |
| male   | black non hispanic         | kayden              |   22 |   22 |   17 |   25 |   25 |   37 |
| male   | black non hispanic         | ayden               |   23 |   22 |   21 |   27 |   14 |   13 |
| male   | black non hispanic         | messiah             |   23 |   21 |   26 |   32 |   41 |   51 |
| male   | black non hispanic         | kaiden              |   23 |   26 |   19 |   18 |   39 |   46 |
| male   | black non hispanic         | samuel              |   24 |   36 |   46 |   28 |   36 |   44 |
| male   | black non hispanic         | mohamed             |   24 |   28 |   31 |   36 |   28 |   37 |
| male   | black non hispanic         | kyrie               |   25 |   38 |   47 |   51 |   NA |   NA |
| male   | black non hispanic         | christopher         |   25 |   39 |   23 |   22 |   15 |   19 |
| male   | black non hispanic         | ryan                |   26 |   33 |   28 |   23 |   13 |   33 |
| male   | black non hispanic         | jayce               |   27 |   46 |   39 |   59 |   NA |   NA |
| male   | black non hispanic         | justin              |   27 |   36 |   31 |   15 |   12 |   14 |
| male   | black non hispanic         | caleb               |   28 |   29 |   23 |   20 |   33 |   31 |
| male   | black non hispanic         | nathan              |   28 |   25 |   20 |   24 |   27 |   42 |
| male   | black non hispanic         | aaron               |   29 |   20 |   38 |   37 |   19 |   28 |
| male   | black non hispanic         | xavier              |   29 |   31 |   29 |   24 |   33 |   26 |
| male   | black non hispanic         | tristan             |   30 |   36 |   25 |   23 |   23 |   26 |
| male   | black non hispanic         | amari               |   31 |   30 |   42 |   40 |   30 |   27 |
| male   | black non hispanic         | jace                |   31 |   27 |   30 |   38 |   51 |   53 |
| male   | black non hispanic         | nathaniel           |   32 |   27 |   27 |   30 |   21 |   23 |
| male   | black non hispanic         | jonathan            |   33 |   43 |   28 |   36 |   25 |   28 |
| male   | black non hispanic         | prince              |   33 |   33 |   49 |   32 |   26 |   42 |
| male   | black non hispanic         | matthew             |   33 |   21 |   25 |   21 |   18 |   25 |
| male   | black non hispanic         | ashton              |   33 |   51 |   42 |   49 |   55 |   56 |
| male   | black non hispanic         | jaden               |   33 |   44 |   44 |   31 |   25 |   20 |
| male   | black non hispanic         | joel                |   33 |   53 |   43 |   52 |   45 |   37 |
| male   | black non hispanic         | hunter              |   34 |   37 |   39 |   52 |   50 |   62 |
| male   | black non hispanic         | alexander           |   34 |   28 |   26 |   39 |   32 |   35 |
| male   | black non hispanic         | anthony             |   34 |   31 |   35 |   28 |   20 |   22 |
| male   | black non hispanic         | jason               |   35 |   52 |   38 |   38 |   22 |   31 |
| male   | black non hispanic         | dylan               |   35 |   34 |   24 |   29 |   20 |   32 |
| male   | black non hispanic         | jayceon             |   36 |   50 |   36 |   54 |   NA |   NA |
| male   | black non hispanic         | jordan              |   36 |   34 |   33 |   29 |   18 |   28 |
| male   | black non hispanic         | adonis              |   37 |   61 |   59 |   61 |   56 |   63 |
| male   | black non hispanic         | gabriel             |   37 |   24 |   31 |   32 |   35 |   38 |
| male   | black non hispanic         | brandon             |   37 |   44 |   41 |   33 |   25 |   27 |
| male   | black non hispanic         | bryson              |   37 |   60 |   53 |   59 |   61 |   60 |
| male   | black non hispanic         | nasir               |   38 |   42 |   39 |   45 |   39 |   41 |
| male   | black non hispanic         | bryce               |   39 |   48 |   40 |   39 |   39 |   39 |
| male   | black non hispanic         | emmanuel            |   39 |   38 |   35 |   33 |   38 |   30 |
| male   | black non hispanic         | zachary             |   39 |   53 |   36 |   49 |   45 |   52 |
| male   | black non hispanic         | kyle                |   39 |   40 |   39 |   52 |   30 |   34 |
| male   | black non hispanic         | karter              |   39 |   33 |   37 |   55 |   NA |   NA |
| male   | black non hispanic         | lucas               |   40 |   52 |   50 |   49 |   56 |   62 |
| male   | black non hispanic         | austin              |   40 |   47 |   48 |   44 |   44 |   61 |
| male   | black non hispanic         | avery               |   40 |   52 |   47 |   53 |   51 |   55 |
| male   | black non hispanic         | kai                 |   40 |   51 |   42 |   52 |   57 |   60 |
| male   | black non hispanic         | micah               |   40 |   38 |   40 |   40 |   42 |   45 |
| male   | black non hispanic         | mamadou             |   40 |   50 |   47 |   42 |   38 |   47 |
| male   | black non hispanic         | omari               |   40 |   49 |   57 |   NA |   54 |   55 |
| male   | black non hispanic         | chance              |   41 |   53 |   44 |   49 |   46 |   59 |
| male   | black non hispanic         | ibrahim             |   41 |   55 |   48 |   39 |   43 |   49 |
| male   | black non hispanic         | sebastian           |   41 |   45 |   49 |   51 |   53 |   63 |
| male   | black non hispanic         | joseph              |   42 |   37 |   45 |   35 |   24 |   39 |
| male   | black non hispanic         | nicholas            |   43 |   41 |   41 |   43 |   34 |   43 |
| male   | black non hispanic         | james               |   44 |   45 |   32 |   41 |   37 |   41 |
| male   | black non hispanic         | maison              |   45 |   NA |   NA |   NA |   NA |   NA |
| male   | black non hispanic         | kaleb               |   46 |   57 |   50 |   56 |   53 |   56 |
| male   | black non hispanic         | eli                 |   46 |   51 |   50 |   46 |   37 |   52 |
| male   | black non hispanic         | adam                |   47 |   47 |   46 |   54 |   51 |   46 |
| male   | black non hispanic         | andrew              |   47 |   43 |   32 |   49 |   25 |   38 |
| male   | black non hispanic         | major               |   48 |   58 |   54 |   62 |   NA |   NA |
| male   | black non hispanic         | isaac               |   48 |   44 |   44 |   45 |   54 |   56 |
| male   | black non hispanic         | malik               |   48 |   63 |   45 |   49 |   46 |   50 |
| male   | black non hispanic         | benjamin            |   48 |   52 |   42 |   42 |   47 |   61 |
| male   | black non hispanic         | ezra                |   48 |   62 |   60 |   NA |   NA |   NA |
| male   | black non hispanic         | julian              |   48 |   54 |   52 |   45 |   45 |   55 |
| male   | black non hispanic         | richard             |   49 |   54 |   54 |   53 |   56 |   59 |
| male   | black non hispanic         | jaiden              |   49 |   58 |   49 |   53 |   42 |   39 |
| male   | black non hispanic         | william             |   49 |   54 |   49 |   37 |   49 |   47 |
| male   | black non hispanic         | robert              |   49 |   55 |   57 |   54 |   48 |   45 |
| male   | black non hispanic         | royal               |   50 |   59 |   NA |   NA |   NA |   NA |
| male   | black non hispanic         | adrian              |   50 |   57 |   55 |   55 |   47 |   56 |
| male   | black non hispanic         | elias               |   50 |   57 |   55 |   NA |   61 |   60 |
| male   | black non hispanic         | john                |   50 |   53 |   50 |   56 |   54 |   40 |
| male   | black non hispanic         | legend              |   50 |   62 |   NA |   NA |   NA |   NA |
| male   | black non hispanic         | nolan               |   50 |   NA |   54 |   59 |   NA |   NA |
| male   | black non hispanic         | justice             |   51 |   59 |   60 |   50 |   56 |   63 |
| male   | black non hispanic         | grayson             |   51 |   62 |   NA |   NA |   NA |   NA |
| male   | black non hispanic         | kevin               |   51 |   48 |   45 |   38 |   40 |   46 |
| male   | black non hispanic         | zaire               |   51 |   53 |   50 |   45 |   48 |   55 |
| male   | black non hispanic         | jaxon               |   51 |   62 |   58 |   NA |   NA |   NA |
| male   | black non hispanic         | alpha               |   51 |   60 |   54 |   61 |   57 |   NA |
| male   | black non hispanic         | andre               |   52 |   58 |   57 |   57 |   57 |   52 |
| male   | black non hispanic         | muhammad            |   52 |   59 |   NA |   58 |   54 |   NA |
| male   | black non hispanic         | princeton           |   52 |   61 |   58 |   NA |   NA |   NA |
| male   | black non hispanic         | mekhi               |   52 |   59 |   47 |   55 |   50 |   41 |
| male   | black non hispanic         | levi                |   52 |   58 |   57 |   53 |   56 |   59 |
| male   | black non hispanic         | sean                |   52 |   61 |   57 |   60 |   53 |   45 |
| male   | black non hispanic         | giovanni            |   52 |   59 |   55 |   48 |   59 |   60 |
| male   | black non hispanic         | khalil              |   52 |   59 |   57 |   NA |   53 |   55 |
| male   | black non hispanic         | ousmane             |   52 |   63 |   NA |   59 |   NA |   60 |
| male   | black non hispanic         | alex                |   53 |   NA |   NA |   56 |   59 |   NA |
| male   | black non hispanic         | maximus             |   53 |   NA |   NA |   NA |   NA |   NA |
| male   | black non hispanic         | aidan               |   53 |   NA |   NA |   42 |   57 |   54 |
| male   | black non hispanic         | shane               |   53 |   NA |   NA |   56 |   55 |   NA |
| male   | black non hispanic         | mouhamed            |   53 |   NA |   58 |   62 |   NA |   NA |
| male   | black non hispanic         | kaden               |   53 |   53 |   50 |   44 |   55 |   52 |
| male   | black non hispanic         | jaylen              |   53 |   56 |   51 |   47 |   42 |   36 |
| male   | black non hispanic         | asher               |   53 |   59 |   57 |   NA |   NA |   NA |
| male   | black non hispanic         | ezekiel             |   53 |   NA |   55 |   NA |   NA |   NA |
| male   | black non hispanic         | ibrahima            |   54 |   62 |   57 |   56 |   54 |   63 |
| male   | black non hispanic         | sincere             |   54 |   54 |   51 |   55 |   61 |   50 |
| male   | black non hispanic         | jonas               |   54 |   NA |   NA |   NA |   NA |   NA |
| male   | black non hispanic         | kameron             |   54 |   62 |   NA |   62 |   NA |   60 |
| male   | black non hispanic         | judah               |   54 |   NA |   NA |   NA |   55 |   NA |
| male   | black non hispanic         | jamari              |   54 |   NA |   NA |   62 |   55 |   NA |
| male   | black non hispanic         | israel              |   54 |   57 |   NA |   55 |   60 |   NA |
| male   | black non hispanic         | darius              |   54 |   NA |   NA |   60 |   57 |   62 |
| male   | black non hispanic         | timothy             |   54 |   NA |   59 |   62 |   61 |   60 |
| male   | black non hispanic         | jaxson              |   54 |   NA |   NA |   NA |   NA |   NA |
| male   | black non hispanic         | victor              |   55 |   61 |   NA |   NA |   NA |   62 |
| male   | black non hispanic         | bryan               |   55 |   56 |   55 |   NA |   54 |   64 |
| male   | black non hispanic         | preston             |   55 |   52 |   54 |   50 |   59 |   55 |
| male   | black non hispanic         | blake               |   55 |   55 |   53 |   47 |   47 |   55 |
| male   | black non hispanic         | kamari              |   55 |   63 |   58 |   60 |   61 |   57 |
| male   | black non hispanic         | cairo               |   55 |   NA |   NA |   60 |   NA |   NA |
| male   | black non hispanic         | charles             |   55 |   NA |   59 |   52 |   53 |   59 |
| male   | black non hispanic         | greyson             |   55 |   NA |   59 |   NA |   NA |   NA |
| male   | black non hispanic         | peyton              |   55 |   63 |   56 |   56 |   59 |   NA |
| male   | black non hispanic         | amadou              |   55 |   55 |   55 |   56 |   50 |   59 |
| male   | black non hispanic         | caiden              |   56 |   54 |   52 |   54 |   NA |   NA |
| male   | black non hispanic         | george              |   56 |   NA |   NA |   NA |   NA |   64 |
| male   | black non hispanic         | moussa              |   56 |   63 |   54 |   53 |   59 |   63 |
| male   | black non hispanic         | nehemiah            |   56 |   NA |   NA |   61 |   NA |   61 |
| male   | black non hispanic         | abdoul              |   56 |   63 |   59 |   NA |   61 |   58 |
| male   | black non hispanic         | marcus              |   56 |   53 |   52 |   54 |   42 |   50 |
| male   | black non hispanic         | kingston            |   56 |   NA |   54 |   NA |   NA |   NA |
| male   | black non hispanic         | jonah               |   56 |   NA |   NA |   NA |   58 |   NA |
| male   | black non hispanic         | derrick             |   56 |   NA |   NA |   62 |   60 |   64 |
| male   | black non hispanic         | connor              |   56 |   59 |   NA |   55 |   61 |   NA |
| male   | black non hispanic         | abraham             |   57 |   NA |   57 |   62 |   NA |   NA |
| male   | black non hispanic         | wyatt               |   57 |   61 |   60 |   NA |   NA |   NA |
| male   | black non hispanic         | miles               |   57 |   62 |   57 |   NA |   52 |   64 |
| male   | black non hispanic         | ameer               |   57 |   NA |   NA |   NA |   NA |   NA |
| male   | black non hispanic         | omar                |   57 |   60 |   49 |   53 |   57 |   60 |
| male   | black non hispanic         | brayden             |   57 |   NA |   NA |   NA |   NA |   NA |
| male   | black non hispanic         | camren              |   57 |   NA |   NA |   61 |   60 |   NA |
| male   | black non hispanic         | oumar               |   57 |   NA |   56 |   54 |   54 |   NA |
| male   | black non hispanic         | damari              |   57 |   NA |   NA |   NA |   NA |   NA |
| male   | black non hispanic         | ali                 |   57 |   63 |   60 |   NA |   57 |   63 |
| male   | black non hispanic         | henry               |   57 |   NA |   NA |   NA |   NA |   NA |
| male   | black non hispanic         | jamal               |   57 |   63 |   NA |   60 |   NA |   61 |
| male   | black non hispanic         | cayden              |   57 |   58 |   43 |   49 |   52 |   63 |
| male   | black non hispanic         | myles               |   57 |   60 |   59 |   61 |   56 |   59 |
| male   | black non hispanic         | kyree               |   57 |   NA |   NA |   NA |   NA |   NA |
| male   | black non hispanic         | zayden              |   57 |   60 |   57 |   59 |   NA |   NA |
| male   | hispanic                   | liam                |    1 |    1 |    1 |   11 |   24 |   32 |
| male   | hispanic                   | jacob               |    2 |    6 |    2 |    2 |    2 |    3 |
| male   | hispanic                   | dylan               |    3 |    2 |    2 |    3 |   15 |   23 |
| male   | hispanic                   | matthew             |    4 |    4 |    6 |    4 |    3 |    4 |
| male   | hispanic                   | noah                |    5 |    5 |    3 |    9 |   20 |   19 |
| male   | hispanic                   | sebastian           |    6 |    8 |    7 |   13 |   10 |   14 |
| male   | hispanic                   | ethan               |    7 |    3 |    5 |    5 |    4 |    6 |
| male   | hispanic                   | jayden              |    8 |    7 |    4 |    1 |    1 |    1 |
| male   | hispanic                   | lucas               |    9 |   11 |   11 |   16 |   29 |   26 |
| male   | hispanic                   | aiden               |   10 |   12 |   15 |   14 |    6 |   12 |
| male   | hispanic                   | daniel              |   11 |    9 |    9 |    6 |    7 |    8 |
| male   | hispanic                   | angel               |   12 |   10 |   10 |    8 |    5 |    5 |
| male   | hispanic                   | alexander           |   13 |   13 |    8 |    7 |    9 |    9 |
| male   | hispanic                   | mason               |   14 |   14 |   12 |   10 |   13 |   17 |
| male   | hispanic                   | christopher         |   15 |   17 |   16 |   15 |   11 |    7 |
| male   | hispanic                   | aaron               |   16 |   22 |   21 |   25 |   28 |   33 |
| male   | hispanic                   | mateo               |   17 |   27 |   49 |   54 |   44 |   62 |
| male   | hispanic                   | joshua              |   18 |   24 |   17 |   17 |   17 |   11 |
| male   | hispanic                   | adrian              |   19 |   19 |   18 |   20 |   19 |   19 |
| male   | hispanic                   | anthony             |   20 |   15 |   17 |   13 |   12 |   10 |
| male   | hispanic                   | david               |   21 |   16 |   14 |   12 |   18 |   15 |
| male   | hispanic                   | michael             |   22 |   18 |   13 |   16 |   14 |   16 |
| male   | hispanic                   | logan               |   23 |   21 |   29 |   38 |   51 |   61 |
| male   | hispanic                   | gabriel             |   23 |   23 |   19 |   21 |   26 |   18 |
| male   | hispanic                   | isaac               |   24 |   23 |   38 |   35 |   37 |   42 |
| male   | hispanic                   | ian                 |   25 |   30 |   27 |   34 |   42 |   43 |
| male   | hispanic                   | joseph              |   26 |   20 |   25 |   19 |   23 |   22 |
| male   | hispanic                   | ryan                |   27 |   33 |   32 |   26 |   28 |   26 |
| male   | hispanic                   | julian              |   27 |   35 |   30 |   30 |   33 |   35 |
| male   | hispanic                   | josiah              |   28 |   44 |   58 |   49 |   58 |   58 |
| male   | hispanic                   | justin              |   29 |   28 |   22 |   15 |    8 |    2 |
| male   | hispanic                   | kevin               |   30 |   34 |   24 |   18 |   16 |   14 |
| male   | hispanic                   | jose                |   30 |   29 |   33 |   24 |   25 |   21 |
| male   | hispanic                   | elijah              |   31 |   39 |   23 |   29 |   35 |   29 |
| male   | hispanic                   | luis                |   32 |   36 |   35 |   27 |   27 |   24 |
| male   | hispanic                   | jonathan            |   33 |   25 |   26 |   23 |   21 |   14 |
| male   | hispanic                   | nathan              |   34 |   49 |   42 |   46 |   45 |   60 |
| male   | hispanic                   | james               |   34 |   26 |   34 |   39 |   47 |   45 |
| male   | hispanic                   | benjamin            |   34 |   38 |   47 |   52 |   54 |   57 |
| male   | hispanic                   | samuel              |   34 |   41 |   43 |   48 |   42 |   41 |
| male   | hispanic                   | jeremiah            |   35 |   31 |   35 |   32 |   30 |   27 |
| male   | hispanic                   | john                |   36 |   48 |   49 |   51 |   46 |   37 |
| male   | hispanic                   | jeremy              |   37 |   27 |   28 |   24 |   31 |   28 |
| male   | hispanic                   | nicholas            |   38 |   46 |   31 |   31 |   41 |   32 |
| male   | hispanic                   | carlos              |   38 |   43 |   40 |   37 |   34 |   30 |
| male   | hispanic                   | christian           |   39 |   31 |   37 |   28 |   25 |   20 |
| male   | hispanic                   | andrew              |   40 |   32 |   39 |   40 |   32 |   25 |
| male   | hispanic                   | ayden               |   41 |   42 |   36 |   43 |   50 |   31 |
| male   | hispanic                   | brandon             |   41 |   31 |   20 |   22 |   22 |   13 |
| male   | hispanic                   | elias               |   42 |   49 |   38 |   44 |   46 |   48 |
| male   | hispanic                   | xavier              |   42 |   50 |   41 |   42 |   40 |   38 |
| male   | hispanic                   | leonardo            |   43 |   55 |   53 |   58 |   59 |   54 |
| male   | hispanic                   | diego               |   44 |   58 |   52 |   58 |   56 |   55 |
| male   | hispanic                   | axel                |   45 |   63 |   58 |   55 |   61 |   46 |
| male   | hispanic                   | erick               |   45 |   54 |   55 |   53 |   45 |   51 |
| male   | hispanic                   | jesus               |   46 |   54 |   48 |   49 |   43 |   45 |
| male   | hispanic                   | oliver              |   47 |   59 |   63 |   81 |   68 |   64 |
| male   | hispanic                   | alan                |   48 |   47 |   52 |   50 |   66 |   66 |
| male   | hispanic                   | kayden              |   49 |   60 |   54 |   64 |   71 |   79 |
| male   | hispanic                   | emmanuel            |   50 |   40 |   50 |   56 |   42 |   53 |
| male   | hispanic                   | steven              |   51 |   53 |   45 |   41 |   48 |   31 |
| male   | hispanic                   | gael                |   51 |   72 |   70 |   52 |   49 |   86 |
| male   | hispanic                   | thiago              |   51 |   62 |   71 |   67 |   NA |   NA |
| male   | hispanic                   | joel                |   51 |   45 |   44 |   33 |   51 |   39 |
| male   | hispanic                   | jason               |   52 |   59 |   46 |   45 |   39 |   38 |
| male   | hispanic                   | santiago            |   52 |   65 |   54 |   64 |   58 |   58 |
| male   | hispanic                   | alex                |   53 |   62 |   49 |   57 |   60 |   47 |
| male   | hispanic                   | alejandro           |   54 |   61 |   55 |   45 |   53 |   47 |
| male   | hispanic                   | nicolas             |   54 |   60 |   60 |   50 |   55 |   56 |
| male   | hispanic                   | william             |   55 |   48 |   54 |   48 |   47 |   36 |
| male   | hispanic                   | damian              |   56 |   75 |   60 |   61 |   67 |   78 |
| male   | hispanic                   | austin              |   57 |   68 |   69 |   67 |   94 |   89 |
| male   | hispanic                   | juan                |   58 |   57 |   49 |   47 |   36 |   35 |
| male   | hispanic                   | adam                |   58 |   66 |   59 |   56 |   65 |   72 |
| male   | hispanic                   | isaiah              |   59 |   51 |   61 |   50 |   43 |   45 |
| male   | hispanic                   | jace                |   59 |   62 |   51 |   59 |   76 |   67 |
| male   | hispanic                   | bryan               |   60 |   56 |   41 |   40 |   38 |   35 |
| male   | hispanic                   | jayce               |   61 |   71 |   73 |   82 |   89 |   92 |
| male   | hispanic                   | matteo              |   62 |   70 |   91 |   91 |   99 |   NA |
| male   | hispanic                   | nathaniel           |   62 |   50 |   57 |   58 |   52 |   49 |
| male   | hispanic                   | abraham             |   62 |   73 |   75 |   82 |   91 |   82 |
| male   | hispanic                   | evan                |   63 |   49 |   60 |   52 |   57 |   40 |
| male   | hispanic                   | jordan              |   63 |   63 |   52 |   52 |   58 |   43 |
| male   | hispanic                   | johan               |   64 |   79 |   85 |   73 |   75 |   76 |
| male   | hispanic                   | miguel              |   64 |   52 |   50 |   36 |   38 |   34 |
| male   | hispanic                   | derek               |   65 |   60 |   56 |   61 |   63 |   50 |
| male   | hispanic                   | jake                |   65 |   68 |   67 |   69 |   73 |   77 |
| male   | hispanic                   | iker                |   65 |   37 |   60 |   69 |   81 |   91 |
| male   | hispanic                   | abel                |   65 |   88 |   90 |   79 |   86 |   90 |
| male   | hispanic                   | adriel              |   66 |   72 |   74 |   79 |   86 |   70 |
| male   | hispanic                   | hunter              |   66 |   78 |   75 |   87 |   98 |   89 |
| male   | hispanic                   | edward              |   67 |   71 |   73 |   58 |   66 |   74 |
| male   | hispanic                   | ivan                |   67 |   75 |   64 |   64 |   69 |   64 |
| male   | hispanic                   | leo                 |   67 |   68 |   86 |   88 |   94 |   93 |
| male   | hispanic                   | caleb               |   68 |   56 |   66 |   63 |   68 |   64 |
| male   | hispanic                   | alexis              |   69 |   72 |   68 |   65 |   65 |   54 |
| male   | hispanic                   | javier              |   69 |   85 |   66 |   69 |   80 |   66 |
| male   | hispanic                   | eli                 |   69 |   66 |   69 |   62 |   65 |   66 |
| male   | hispanic                   | oscar               |   69 |   74 |   82 |   68 |   73 |   77 |
| male   | hispanic                   | jadiel              |   70 |   64 |   64 |   67 |   64 |   57 |
| male   | hispanic                   | matias              |   70 |   87 |   76 |   85 |   90 |   84 |
| male   | hispanic                   | tyler               |   70 |   64 |   68 |   66 |   47 |   59 |
| male   | hispanic                   | josue               |   70 |   80 |   73 |   70 |   66 |   69 |
| male   | hispanic                   | henry               |   70 |   82 |   79 |   71 |   75 |   63 |
| male   | hispanic                   | messiah             |   70 |   93 |   91 |   NA |   NA |   NA |
| male   | hispanic                   | edwin               |   70 |   77 |   67 |   69 |   60 |   44 |
| male   | hispanic                   | brayden             |   71 |   85 |   91 |   95 |   95 |   88 |
| male   | hispanic                   | victor              |   72 |   67 |   58 |   60 |   59 |   59 |
| male   | hispanic                   | bradley             |   72 |   77 |   80 |   84 |   94 |   80 |
| male   | hispanic                   | andy                |   73 |   75 |   75 |   74 |   62 |   67 |
| male   | hispanic                   | jackson             |   73 |   91 |   87 |   88 |   93 |   NA |
| male   | hispanic                   | mathias             |   73 |   94 |   92 |   91 |   97 |   NA |
| male   | hispanic                   | king                |   74 |   80 |   82 |   90 |   NA |   NA |
| male   | hispanic                   | elian               |   74 |   84 |   89 |   84 |   88 |   87 |
| male   | hispanic                   | zachary             |   74 |   82 |   81 |   80 |   87 |   88 |
| male   | hispanic                   | jonah               |   74 |   87 |   96 |   NA |   93 |   89 |
| male   | hispanic                   | giovanni            |   75 |   79 |   66 |   67 |   71 |   47 |
| male   | hispanic                   | dominic             |   75 |   80 |   75 |   71 |   82 |   82 |
| male   | hispanic                   | jaden               |   75 |   67 |   65 |   52 |   81 |   42 |
| male   | hispanic                   | jaxon               |   75 |   87 |   88 |   91 |   NA |   NA |
| male   | hispanic                   | cristian            |   75 |   76 |   75 |   61 |   73 |   56 |
| male   | hispanic                   | rafael              |   75 |   82 |   88 |   72 |   78 |   73 |
| male   | hispanic                   | emanuel             |   76 |   86 |   84 |   85 |   80 |   78 |
| male   | hispanic                   | max                 |   76 |   80 |   95 |   70 |   72 |   71 |
| male   | hispanic                   | jorge               |   76 |   71 |   79 |   66 |   74 |   65 |
| male   | hispanic                   | julius              |   76 |   67 |   62 |   80 |   84 |   78 |
| male   | hispanic                   | jayson              |   77 |   96 |   92 |   87 |   82 |   80 |
| male   | hispanic                   | carter              |   77 |   85 |   89 |   88 |   96 |   NA |
| male   | hispanic                   | cameron             |   77 |   89 |   89 |   86 |   92 |   85 |
| male   | hispanic                   | emilio              |   77 |   88 |   86 |   83 |   85 |   83 |
| male   | hispanic                   | pablo               |   77 |   95 |   92 |   84 |   94 |   79 |
| male   | hispanic                   | jack                |   77 |   81 |   83 |   73 |   82 |   83 |
| male   | hispanic                   | kenneth             |   77 |   88 |   84 |   76 |   76 |   71 |
| male   | hispanic                   | thomas              |   77 |   84 |   78 |   92 |   90 |   78 |
| male   | hispanic                   | owen                |   77 |   86 |   97 |   91 |   NA |   92 |
| male   | hispanic                   | andres              |   77 |   77 |   78 |   76 |   79 |   57 |
| male   | hispanic                   | robert              |   78 |   86 |   78 |   62 |   85 |   69 |
| male   | hispanic                   | allen               |   78 |   88 |   96 |   NA |   88 |   82 |
| male   | hispanic                   | maximiliano         |   79 |   92 |   94 |   93 |   93 |   91 |
| male   | hispanic                   | eric                |   79 |   78 |   81 |   66 |   77 |   51 |
| male   | hispanic                   | hector              |   79 |   86 |   90 |   80 |   82 |   73 |
| male   | hispanic                   | julio               |   79 |   98 |   93 |   82 |   83 |   77 |
| male   | hispanic                   | jaiden              |   79 |   90 |   94 |   85 |   90 |   64 |
| male   | hispanic                   | brian               |   79 |   80 |   82 |   67 |   73 |   52 |
| male   | hispanic                   | noel                |   79 |   85 |   77 |   78 |   87 |   80 |
| male   | hispanic                   | lorenzo             |   79 |   85 |   91 |   85 |   95 |   90 |
| male   | hispanic                   | chase               |   79 |   85 |   87 |   88 |   89 |   82 |
| male   | hispanic                   | avery               |   80 |   90 |   85 |   92 |   98 |   91 |
| male   | hispanic                   | greyson             |   80 |   NA |   NA |   NA |   NA |   NA |
| male   | hispanic                   | aidan               |   80 |   91 |   84 |   73 |   85 |   68 |
| male   | hispanic                   | francisco           |   80 |   94 |   75 |   75 |   90 |   72 |
| male   | hispanic                   | vincent             |   80 |   88 |   87 |   83 |   96 |   87 |
| male   | hispanic                   | peter               |   80 |   95 |   91 |   89 |   97 |   89 |
| male   | hispanic                   | eduardo             |   80 |   97 |   89 |   83 |   71 |   61 |
| male   | hispanic                   | richard             |   81 |   82 |   81 |   83 |   70 |   66 |
| male   | hispanic                   | dariel              |   81 |   81 |   80 |   76 |   81 |   73 |
| male   | hispanic                   | anderson            |   81 |   88 |   91 |   69 |   74 |   71 |
| male   | hispanic                   | cesar               |   81 |   79 |   85 |   76 |   77 |   75 |
| male   | hispanic                   | bryce               |   81 |   96 |   95 |   NA |   99 |   94 |
| male   | hispanic                   | jonas               |   81 |   97 |   95 |   90 |  100 |   NA |
| male   | hispanic                   | kaiden              |   81 |   77 |   88 |   72 |  100 |   NA |
| male   | hispanic                   | ismael              |   81 |   89 |   82 |   80 |   81 |   87 |
| male   | hispanic                   | manuel              |   82 |   82 |   83 |   73 |   67 |   73 |
| male   | hispanic                   | zayden              |   82 |   93 |   95 |   NA |   97 |   NA |
| male   | hispanic                   | george              |   82 |   96 |   88 |   93 |   91 |   74 |
| male   | hispanic                   | ezra                |   82 |   NA |   96 |   94 |   NA |   NA |
| male   | hispanic                   | jefferson           |   82 |   92 |   91 |   92 |   97 |   90 |
| male   | hispanic                   | damien              |   82 |   96 |   92 |   93 |   99 |   89 |
| male   | hispanic                   | grayson             |   82 |   90 |   98 |   95 |   NA |   NA |
| male   | hispanic                   | moises              |   82 |   96 |   98 |   87 |   97 |   86 |
| male   | hispanic                   | amir                |   82 |   69 |   89 |   81 |   95 |   83 |
| male   | hispanic                   | zion                |   82 |   99 |   NA |   95 |   92 |   91 |
| male   | hispanic                   | jaxson              |   82 |   96 |   97 |   NA |   NA |   NA |
| male   | hispanic                   | prince              |   82 |   96 |   89 |   87 |   88 |   90 |
| male   | hispanic                   | uriel               |   83 |   97 |   97 |   87 |   97 |   83 |
| male   | hispanic                   | leandro             |   83 |   93 |   97 |   91 |   95 |   88 |
| male   | hispanic                   | nolan               |   83 |   NA |   NA |   NA |   NA |   NA |
| male   | hispanic                   | levi                |   83 |   86 |   94 |   90 |   NA |   NA |
| male   | hispanic                   | mathew              |   83 |   92 |   90 |   88 |   88 |   85 |
| male   | hispanic                   | tristan             |   83 |   78 |   73 |   84 |   79 |   70 |
| male   | hispanic                   | ricardo             |   83 |   86 |   86 |   86 |   82 |   79 |
| male   | hispanic                   | ean                 |   84 |   92 |   NA |   NA |   NA |   NA |
| male   | hispanic                   | milan               |   84 |   74 |   76 |   91 |   NA |   NA |
| male   | hispanic                   | chris               |   84 |   88 |   91 |   85 |   79 |   77 |
| male   | hispanic                   | maximus             |   84 |   80 |   80 |   85 |   84 |   89 |
| male   | hispanic                   | maison              |   84 |   NA |   NA |   93 |   NA |   NA |
| male   | hispanic                   | dominick            |   84 |   92 |   92 |   88 |   99 |   88 |
| male   | hispanic                   | bryson              |   84 |   NA |   NA |   NA |   NA |   NA |
| male   | hispanic                   | luke                |   84 |   92 |   91 |   94 |  100 |   87 |
| male   | hispanic                   | mark                |   84 |   NA |   95 |   92 |   95 |   85 |
| male   | hispanic                   | joaquin             |   84 |   93 |   87 |   92 |   95 |   81 |
| male   | hispanic                   | martin              |   85 |   92 |   88 |   92 |   94 |   88 |
| male   | hispanic                   | charles             |   85 |   98 |   87 |   NA |   98 |   84 |
| male   | hispanic                   | pedro               |   85 |   87 |   96 |   81 |   91 |   75 |
| male   | hispanic                   | jeffrey             |   85 |   88 |   90 |   74 |   79 |   85 |
| male   | hispanic                   | abdiel              |   85 |   93 |   NA |   NA |   99 |   92 |
| male   | hispanic                   | israel              |   85 |   91 |   90 |   87 |   90 |   82 |
| male   | hispanic                   | jay                 |   85 |   NA |   87 |   95 |   95 |   86 |
| male   | hispanic                   | jaycob              |   85 |   NA |   NA |   91 |   NA |   NA |
| male   | hispanic                   | ariel               |   85 |   89 |   92 |   92 |   92 |   78 |
| male   | hispanic                   | adonis              |   85 |   NA |   88 |   90 |   94 |   88 |
| male   | hispanic                   | luca                |   85 |   88 |   90 |   87 |   97 |   NA |
| male   | hispanic                   | marcos              |   85 |   83 |   94 |   91 |   85 |   75 |
| male   | hispanic                   | louis               |   86 |   98 |   97 |   90 |   96 |   90 |
| male   | hispanic                   | brayan              |   86 |   86 |   89 |   83 |   87 |   81 |
| male   | hispanic                   | lukas               |   86 |   86 |   90 |   NA |   99 |   93 |
| male   | hispanic                   | edgar               |   86 |   90 |   84 |   84 |   82 |   75 |
| male   | hispanic                   | andre               |   86 |   NA |   NA |   92 |   92 |   94 |
| male   | hispanic                   | marcus              |   86 |   96 |   89 |   87 |   89 |   71 |
| male   | hispanic                   | angelo              |   86 |   94 |   NA |   85 |   96 |   76 |
| male   | hispanic                   | enzo                |   86 |   92 |   91 |   NA |   NA |   NA |
| male   | hispanic                   | roman               |   86 |   88 |   NA |   NA |  100 |   NA |
| male   | hispanic                   | kenny               |   86 |   98 |   94 |   89 |   95 |   84 |
| male   | hispanic                   | kai                 |   87 |   NA |   95 |   NA |   NA |   NA |
| male   | hispanic                   | yadiel              |   87 |   87 |   93 |   85 |   84 |   79 |
| male   | hispanic                   | maxwell             |   87 |   92 |   92 |   89 |   92 |   NA |
| male   | hispanic                   | valentino           |   87 |   99 |   NA |   93 |   NA |   NA |
| male   | hispanic                   | kaden               |   87 |   NA |   98 |   NA |   NA |   91 |
| male   | hispanic                   | ezekiel             |   87 |   99 |   98 |   93 |  100 |   93 |
| male   | hispanic                   | jean                |   87 |   98 |   86 |   91 |   78 |   66 |
| male   | hispanic                   | raymond             |   87 |   NA |   91 |   95 |   82 |   80 |
| male   | hispanic                   | kyrie               |   87 |   97 |   NA |   NA |   NA |   NA |
| male   | hispanic                   | isaias              |   87 |   97 |   NA |   92 |   NA |   NA |
| male   | hispanic                   | roberto             |   87 |   94 |   86 |   90 |   92 |   93 |
| male   | hispanic                   | landon              |   88 |   89 |   93 |   87 |   92 |   85 |
| male   | hispanic                   | armani              |   88 |   94 |   95 |   87 |   93 |   84 |
| male   | hispanic                   | franklin            |   88 |   97 |   NA |   NA |   97 |   90 |
| male   | hispanic                   | fernando            |   88 |   92 |   82 |   68 |   81 |   76 |
| male   | hispanic                   | randy               |   88 |   95 |   96 |   69 |   67 |   59 |
| male   | hispanic                   | sergio              |   88 |   96 |   92 |   88 |   98 |   91 |
| male   | hispanic                   | marco               |   88 |   88 |   88 |   79 |   81 |   82 |
| male   | hispanic                   | mario               |   89 |   91 |   89 |   89 |   92 |   81 |
| male   | hispanic                   | jimmy               |   89 |   NA |   NA |   94 |   NA |   90 |
| male   | hispanic                   | johann              |   89 |   NA |   NA |   NA |   NA |   92 |
| male   | hispanic                   | jariel              |   89 |   99 |   88 |   80 |   88 |   79 |
| male   | hispanic                   | august              |   89 |   NA |   NA |   NA |   NA |   NA |
| male   | hispanic                   | enrique             |   89 |   95 |   90 |   NA |   99 |   90 |
| male   | hispanic                   | alessandro          |   89 |   NA |   NA |   NA |   NA |   NA |
| male   | hispanic                   | eliel               |   89 |   NA |   NA |   NA |   NA |   NA |
| male   | hispanic                   | rodrigo             |   89 |   NA |   96 |   89 |   98 |   89 |
| male   | hispanic                   | byron               |   89 |   93 |   NA |   84 |   NA |   85 |
| male   | hispanic                   | hudson              |   89 |   99 |   94 |   NA |   NA |   NA |
| male   | hispanic                   | adiel               |   89 |   NA |   NA |   NA |   NA |   NA |
| male   | hispanic                   | omar                |   89 |   91 |   85 |   77 |   85 |   65 |
| male   | hispanic                   | frank               |   89 |   94 |   97 |   92 |   97 |   87 |
| male   | hispanic                   | kaleb               |   89 |   95 |   92 |   NA |   96 |   NA |
| male   | hispanic                   | kyle                |   89 |   86 |   94 |   91 |   91 |   NA |
| male   | hispanic                   | jael                |   89 |   97 |   95 |   82 |   88 |   NA |
| male   | hispanic                   | harrison            |   89 |   NA |   NA |   NA |   NA |   NA |
| male   | hispanic                   | felipe              |   89 |   NA |   NA |   93 |   NA |   NA |
| male   | hispanic                   | jared               |   90 |   83 |   83 |   80 |   91 |   87 |
| male   | hispanic                   | paul                |   90 |   95 |   NA |   93 |   98 |   88 |
| male   | hispanic                   | nathanael           |   90 |   NA |   NA |   NA |   NA |   NA |
| male   | hispanic                   | jasiah              |   90 |   NA |   NA |   NA |   NA |   NA |
| male   | hispanic                   | kelvin              |   90 |   99 |   NA |   88 |   86 |   82 |
| male   | hispanic                   | zabdiel             |   90 |   NA |   NA |   NA |   NA |   NA |
| male   | hispanic                   | leonidas            |   90 |   96 |   97 |   NA |   NA |   NA |
| male   | hispanic                   | timothy             |   90 |   NA |   92 |   93 |   NA |   88 |
| male   | hispanic                   | lenny               |   90 |   99 |   NA |   91 |   90 |   NA |
| male   | hispanic                   | jax                 |   90 |   NA |   NA |   NA |   NA |   NA |
| male   | hispanic                   | allan               |   90 |   95 |   95 |   94 |   NA |   94 |
| male   | hispanic                   | ezequiel            |   90 |   99 |   94 |   94 |   92 |   93 |
| male   | hispanic                   | johnny              |   90 |   NA |   91 |   90 |   86 |   86 |
| male   | hispanic                   | elliot              |   90 |   97 |   NA |   NA |   NA |   NA |
| male   | hispanic                   | miles               |   90 |   NA |   95 |   NA |   NA |   NA |
| male   | hispanic                   | darwin              |   91 |   98 |   97 |   84 |   89 |   86 |
| male   | hispanic                   | antonio             |   91 |   77 |   72 |   76 |   79 |   72 |
| male   | hispanic                   | nelson              |   91 |   97 |   96 |   89 |   95 |   88 |
| male   | hispanic                   | arturo              |   91 |   NA |   NA |   NA |   NA |   94 |
| male   | hispanic                   | lionel              |   91 |   NA |   NA |   NA |   NA |   NA |
| male   | hispanic                   | kendrick            |   91 |   NA |   96 |   85 |   NA |   NA |
| male   | hispanic                   | felix               |   91 |   91 |   95 |   85 |   92 |   92 |
| male   | hispanic                   | gavin               |   91 |   93 |   90 |   78 |   86 |   85 |
| male   | hispanic                   | asher               |   91 |   NA |   NA |   NA |   NA |   NA |
| male   | hispanic                   | marlon              |   91 |   NA |   97 |   95 |   97 |   94 |
| male   | hispanic                   | edison              |   91 |   97 |   NA |   90 |   NA |   93 |
| male   | hispanic                   | eliam               |   91 |   96 |   NA |   95 |   NA |   NA |
| male   | hispanic                   | albert              |   91 |   83 |   93 |   85 |   84 |   91 |
| male   | hispanic                   | mike                |   91 |   NA |   97 |   87 |   98 |   87 |
| male   | hispanic                   | fabian              |   91 |   86 |   98 |   93 |   93 |   NA |
| male   | hispanic                   | sean                |   91 |   NA |   98 |   91 |   91 |   74 |
| male   | hispanic                   | caiden              |   91 |   NA |   NA |   NA |   NA |   NA |
| male   | hispanic                   | matthias            |   91 |   NA |   NA |   NA |   NA |   NA |
| male   | hispanic                   | jayceon             |   91 |   NA |   95 |   NA |   NA |   NA |
| male   | hispanic                   | maximilian          |   91 |   NA |   97 |   93 |   NA |   NA |
| male   | hispanic                   | dante               |   91 |   97 |   92 |   92 |   94 |   83 |
| male   | white non hispanic         | joseph              |    1 |    2 |    1 |    2 |    1 |    2 |
| male   | white non hispanic         | michael             |    2 |    6 |    3 |    3 |    3 |    1 |
| male   | white non hispanic         | david               |    3 |    1 |    2 |    1 |    2 |    4 |
| male   | white non hispanic         | moshe               |    4 |    3 |    4 |    4 |    5 |    6 |
| male   | white non hispanic         | jacob               |    5 |    4 |    5 |    8 |    4 |    3 |
| male   | white non hispanic         | james               |    6 |    9 |   11 |    7 |   10 |   12 |
| male   | white non hispanic         | benjamin            |    7 |    5 |    6 |    6 |    8 |    5 |
| male   | white non hispanic         | alexander           |    8 |   10 |    7 |   10 |    7 |    8 |
| male   | white non hispanic         | daniel              |    9 |    7 |    8 |    5 |    6 |    7 |
| male   | white non hispanic         | henry               |    9 |   13 |   14 |   13 |   16 |   16 |
| male   | white non hispanic         | adam                |   10 |   15 |   12 |   18 |   17 |   21 |
| male   | white non hispanic         | jack                |   10 |   11 |   10 |    9 |   11 |   10 |
| male   | white non hispanic         | william             |   11 |   12 |   13 |   11 |   16 |   13 |
| male   | white non hispanic         | abraham             |   12 |   17 |   15 |   20 |   20 |   12 |
| male   | white non hispanic         | samuel              |   13 |    8 |    9 |   15 |    9 |   11 |
| male   | white non hispanic         | noah                |   14 |   20 |   21 |   14 |   21 |   31 |
| male   | white non hispanic         | john                |   15 |   18 |   17 |   12 |   15 |   14 |
| male   | white non hispanic         | isaac               |   16 |   29 |   25 |   25 |   25 |   36 |
| male   | white non hispanic         | oliver              |   17 |   14 |   30 |   26 |   26 |   30 |
| male   | white non hispanic         | chaim               |   18 |   15 |   23 |   19 |   12 |   17 |
| male   | white non hispanic         | nicholas            |   19 |   18 |   16 |   16 |   13 |   15 |
| male   | white non hispanic         | ethan               |   20 |   19 |   18 |   23 |   21 |   26 |
| male   | white non hispanic         | ryan                |   21 |   25 |   20 |   22 |   14 |   24 |
| male   | white non hispanic         | liam                |   22 |   20 |   19 |   21 |   20 |   20 |
| male   | white non hispanic         | matthew             |   23 |   21 |   22 |   17 |   13 |    9 |
| male   | white non hispanic         | gabriel             |   24 |   24 |   32 |   28 |   27 |   33 |
| male   | white non hispanic         | leo                 |   25 |   26 |   31 |   30 |   28 |   39 |
| male   | white non hispanic         | theodore            |   26 |   16 |   24 |   35 |   35 |   58 |
| male   | white non hispanic         | eli                 |   26 |   32 |   33 |   40 |   31 |   32 |
| male   | white non hispanic         | lucas               |   27 |   22 |   26 |   23 |   23 |   22 |
| male   | white non hispanic         | yosef               |   28 |   31 |   28 |   29 |   25 |   28 |
| male   | white non hispanic         | thomas              |   29 |   30 |   27 |   31 |   22 |   25 |
| male   | white non hispanic         | charles             |   30 |   23 |   22 |   22 |   18 |   21 |
| male   | white non hispanic         | andrew              |   31 |   34 |   38 |   27 |   24 |   19 |
| male   | white non hispanic         | dylan               |   32 |   35 |   36 |   22 |   23 |   23 |
| male   | white non hispanic         | anthony             |   33 |   33 |   29 |   24 |   19 |   18 |
| male   | white non hispanic         | shimon              |   33 |   27 |   34 |   34 |   33 |   35 |
| male   | white non hispanic         | max                 |   34 |   31 |   31 |   31 |   27 |   34 |
| male   | white non hispanic         | yisroel             |   35 |   37 |   43 |   52 |   36 |   44 |
| male   | white non hispanic         | luke                |   36 |   36 |   35 |   32 |   29 |   29 |
| male   | white non hispanic         | mordechai           |   37 |   43 |   37 |   43 |   34 |   43 |
| male   | white non hispanic         | yehuda              |   38 |   39 |   41 |   46 |   37 |   45 |
| male   | white non hispanic         | menachem            |   39 |   36 |   27 |   49 |   40 |   27 |
| male   | white non hispanic         | luca                |   40 |   28 |   36 |   45 |   44 |   46 |
| male   | white non hispanic         | julian              |   40 |   47 |   40 |   50 |   45 |   40 |
| male   | white non hispanic         | sebastian           |   41 |   34 |   34 |   38 |   42 |   46 |
| male   | white non hispanic         | zachary             |   42 |   40 |   39 |   43 |   28 |   38 |
| male   | white non hispanic         | jackson             |   43 |   43 |   42 |   33 |   35 |   42 |
| male   | white non hispanic         | george              |   44 |   41 |   52 |   59 |   41 |   50 |
| male   | white non hispanic         | mason               |   45 |   48 |   36 |   39 |   30 |   44 |
| male   | white non hispanic         | owen                |   46 |   49 |   40 |   48 |   46 |   51 |
| male   | white non hispanic         | christopher         |   46 |   43 |   46 |   37 |   42 |   36 |
| male   | white non hispanic         | jonathan            |   47 |   51 |   47 |   39 |   43 |   46 |
| male   | white non hispanic         | asher               |   48 |   41 |   48 |   61 |   50 |   57 |
| male   | white non hispanic         | robert              |   48 |   37 |   50 |   38 |   39 |   48 |
| male   | white non hispanic         | aiden               |   49 |   42 |   45 |   51 |   43 |   41 |
| male   | white non hispanic         | nathan              |   49 |   39 |   44 |   41 |   40 |   46 |
| male   | white non hispanic         | yitzchok            |   49 |   40 |   51 |   47 |   47 |   56 |
| male   | white non hispanic         | shlomo              |   49 |   47 |   59 |   52 |   54 |   53 |
| male   | white non hispanic         | omar                |   50 |   53 |   65 |   56 |   60 |   74 |
| male   | white non hispanic         | hudson              |   51 |   45 |   56 |   59 |   49 |   58 |
| male   | white non hispanic         | joshua              |   52 |   40 |   49 |   36 |   38 |   37 |
| male   | white non hispanic         | miles               |   52 |   46 |   58 |   60 |   54 |   62 |
| male   | white non hispanic         | mark                |   53 |   54 |   55 |   68 |   59 |   47 |
| male   | white non hispanic         | aron                |   53 |   44 |   54 |   44 |   45 |   54 |
| male   | white non hispanic         | maxwell             |   54 |   58 |   68 |   59 |   53 |   69 |
| male   | white non hispanic         | shmuel              |   54 |   51 |   57 |   59 |   57 |   64 |
| male   | white non hispanic         | nathaniel           |   55 |   52 |   66 |   65 |   50 |   62 |
| male   | white non hispanic         | logan               |   56 |   38 |   49 |   50 |   47 |   49 |
| male   | white non hispanic         | yakov               |   56 |   51 |   68 |   55 |   48 |   54 |
| male   | white non hispanic         | meir                |   56 |   61 |   51 |   70 |   56 |   61 |
| male   | white non hispanic         | connor              |   56 |   62 |   70 |   61 |   62 |   58 |
| male   | white non hispanic         | levi                |   57 |   56 |   56 |   57 |   51 |   67 |
| male   | white non hispanic         | aaron               |   58 |   45 |   39 |   42 |   32 |   49 |
| male   | white non hispanic         | ezra                |   59 |   56 |   68 |   63 |   54 |   74 |
| male   | white non hispanic         | shaya               |   60 |   70 |   81 |   89 |   78 |   81 |
| male   | white non hispanic         | ali                 |   60 |   60 |   60 |   73 |   63 |   74 |
| male   | white non hispanic         | dov                 |   61 |   65 |   74 |   83 |   72 |   62 |
| male   | white non hispanic         | simon               |   61 |   56 |   65 |   58 |   52 |   61 |
| male   | white non hispanic         | tzvi                |   62 |   54 |   65 |   64 |   66 |   66 |
| male   | white non hispanic         | ari                 |   62 |   57 |   71 |   79 |   65 |   77 |
| male   | white non hispanic         | oscar               |   63 |   58 |   74 |   80 |   77 |   77 |
| male   | white non hispanic         | edward              |   64 |   66 |   72 |   77 |   69 |   66 |
| male   | white non hispanic         | vincent             |   64 |   54 |   70 |   63 |   64 |   52 |
| male   | white non hispanic         | jonah               |   64 |   55 |   68 |   62 |   49 |   59 |
| male   | white non hispanic         | adrian              |   64 |   59 |   71 |   70 |   64 |   72 |
| male   | white non hispanic         | hunter              |   64 |   66 |   69 |   69 |   73 |   78 |
| male   | white non hispanic         | avraham             |   64 |   60 |   59 |   64 |   61 |   69 |
| male   | white non hispanic         | harrison            |   65 |   60 |   69 |   66 |   74 |   67 |
| male   | white non hispanic         | zev                 |   66 |   63 |   65 |   63 |   58 |   61 |
| male   | white non hispanic         | yaakov              |   67 |   50 |   67 |   54 |   57 |   63 |
| male   | white non hispanic         | solomon             |   67 |   53 |   63 |   75 |   68 |   60 |
| male   | white non hispanic         | christian           |   68 |   61 |   53 |   62 |   43 |   43 |
| male   | white non hispanic         | eliezer             |   69 |   65 |   62 |   69 |   75 |   69 |
| male   | white non hispanic         | leon                |   69 |   73 |   80 |   85 |   74 |   87 |
| male   | white non hispanic         | peter               |   69 |   64 |   60 |   53 |   60 |   59 |
| male   | white non hispanic         | leonardo            |   69 |   74 |   76 |   81 |   71 |   78 |
| male   | white non hispanic         | wyatt               |   70 |   75 |   79 |   78 |   82 |   82 |
| male   | white non hispanic         | jake                |   70 |   57 |   67 |   59 |   51 |   42 |
| male   | white non hispanic         | cole                |   71 |   79 |   81 |   88 |   84 |   70 |
| male   | white non hispanic         | evan                |   71 |   72 |   56 |   68 |   47 |   54 |
| male   | white non hispanic         | aidan               |   71 |   69 |   80 |   77 |   68 |   55 |
| male   | white non hispanic         | dovid               |   71 |   69 |   86 |   76 |   78 |   67 |
| male   | white non hispanic         | mohamed             |   72 |   64 |   65 |   70 |   69 |   69 |
| male   | white non hispanic         | nolan               |   72 |   80 |   92 |   91 |   86 |   96 |
| male   | white non hispanic         | mendel              |   72 |   64 |   74 |   72 |   56 |   71 |
| male   | white non hispanic         | moses               |   72 |   76 |   86 |   79 |   86 |   76 |
| male   | white non hispanic         | grayson             |   72 |   70 |   82 |   87 |   80 |   88 |
| male   | white non hispanic         | sam                 |   72 |   71 |   80 |   74 |   75 |   72 |
| male   | white non hispanic         | patrick             |   73 |   68 |   65 |   60 |   72 |   56 |
| male   | white non hispanic         | judah               |   74 |   71 |   97 |   86 |   83 |   73 |
| male   | white non hispanic         | gavin               |   74 |   70 |   64 |   60 |   58 |   58 |
| male   | white non hispanic         | finn                |   75 |   67 |   91 |   82 |   78 |   84 |
| male   | white non hispanic         | israel              |   75 |   80 |   77 |   76 |   74 |   79 |
| male   | white non hispanic         | arthur              |   75 |   75 |   87 |   77 |   75 |   78 |
| male   | white non hispanic         | dean                |   75 |   77 |   93 |   87 |   79 |   83 |
| male   | white non hispanic         | elijah              |   76 |   65 |   84 |   75 |   75 |   72 |
| male   | white non hispanic         | ahmed               |   76 |   76 |   80 |   81 |   70 |   81 |
| male   | white non hispanic         | shulem              |   76 |   73 |   68 |   75 |   68 |   65 |
| male   | white non hispanic         | nicolas             |   77 |   79 |   80 |   82 |   75 |   79 |
| male   | white non hispanic         | elias               |   77 |   82 |   80 |   87 |   70 |   86 |
| male   | white non hispanic         | raphael             |   78 |   85 |   83 |   88 |   86 |   86 |
| male   | white non hispanic         | chase               |   78 |   69 |   70 |   84 |   71 |   82 |
| male   | white non hispanic         | paul                |   78 |   78 |   87 |   80 |   76 |   81 |
| male   | white non hispanic         | elliot              |   78 |   63 |   79 |   81 |   75 |   74 |
| male   | white non hispanic         | shmiel              |   78 |   67 |   93 |   92 |   81 |   90 |
| male   | white non hispanic         | rocco               |   79 |   85 |   97 |   99 |   86 |   88 |
| male   | white non hispanic         | antonio             |   79 |   73 |   91 |   81 |   79 |   79 |
| male   | white non hispanic         | aryeh               |   79 |   73 |   90 |   85 |   77 |   83 |
| male   | white non hispanic         | colin               |   79 |   73 |   76 |   73 |   61 |   75 |
| male   | white non hispanic         | lipa                |   80 |   76 |   73 |   71 |   78 |   88 |
| male   | white non hispanic         | ben                 |   80 |   87 |   99 |   84 |   86 |   85 |
| male   | white non hispanic         | frank               |   80 |   85 |   98 |   95 |   71 |   78 |
| male   | white non hispanic         | simcha              |   80 |   76 |   89 |   80 |   69 |   75 |
| male   | white non hispanic         | eliyahu             |   80 |   63 |   87 |   83 |   75 |   71 |
| male   | white non hispanic         | wesley              |   80 |   77 |  101 |   93 |   92 |   97 |
| male   | white non hispanic         | tyler               |   80 |   57 |   78 |   67 |   55 |   53 |
| male   | white non hispanic         | kai                 |   80 |   81 |   89 |   90 |   86 |   92 |
| male   | white non hispanic         | cooper              |   80 |   72 |   72 |   85 |   69 |   77 |
| male   | white non hispanic         | amir                |   80 |   69 |   85 |   79 |   79 |   92 |
| male   | white non hispanic         | matteo              |   81 |   74 |   90 |   80 |   76 |   77 |
| male   | white non hispanic         | roman               |   81 |   78 |   77 |   84 |   88 |   95 |
| male   | white non hispanic         | hershy              |   81 |   75 |   91 |   80 |   79 |   86 |
| male   | white non hispanic         | mayer               |   81 |   82 |   83 |   82 |   81 |   83 |
| male   | white non hispanic         | yechiel             |   81 |   75 |   73 |   81 |   76 |   80 |
| male   | white non hispanic         | lukas               |   82 |   81 |   85 |   95 |   79 |   91 |
| male   | white non hispanic         | richard             |   82 |   84 |   92 |   81 |   75 |   81 |
| male   | white non hispanic         | naftali             |   82 |   73 |   90 |   82 |   80 |   89 |
| male   | white non hispanic         | naftuli             |   82 |   87 |   95 |   98 |   NA |   89 |
| male   | white non hispanic         | philip              |   82 |   92 |   84 |   87 |   81 |   81 |
| male   | white non hispanic         | parker              |   82 |   78 |   85 |   79 |   82 |   91 |
| male   | white non hispanic         | lazer               |   82 |   84 |   86 |   92 |   90 |   87 |
| male   | white non hispanic         | milo                |   82 |   76 |   84 |   90 |   84 |   87 |
| male   | white non hispanic         | rayan               |   82 |   83 |   91 |   90 |   88 |   91 |
| male   | white non hispanic         | alan                |   82 |   80 |  100 |   85 |   77 |   86 |
| male   | white non hispanic         | rafael              |   82 |   71 |   92 |   86 |   68 |   80 |
| male   | white non hispanic         | luka                |   82 |   86 |   88 |   89 |   81 |   86 |
| male   | white non hispanic         | declan              |   83 |   63 |   70 |   70 |   67 |   75 |
| male   | white non hispanic         | graham              |   83 |   84 |   85 |   90 |   83 |   86 |
| male   | white non hispanic         | alex                |   83 |   81 |   85 |   75 |   81 |   73 |
| male   | white non hispanic         | jude                |   83 |   74 |   87 |   87 |   82 |   94 |
| male   | white non hispanic         | charlie             |   83 |   87 |   96 |   86 |   75 |   87 |
| male   | white non hispanic         | yousef              |   83 |   91 |   94 |   96 |   91 |   95 |
| male   | white non hispanic         | august              |   83 |   77 |   84 |   83 |   73 |   91 |
| male   | white non hispanic         | jordan              |   84 |   67 |   78 |   85 |   79 |   75 |
| male   | white non hispanic         | yusuf               |   84 |   82 |   96 |   87 |   90 |   NA |
| male   | white non hispanic         | elliott             |   84 |   86 |  100 |   98 |   94 |   95 |
| male   | white non hispanic         | giovanni            |   84 |   65 |   80 |   78 |   83 |   72 |
| male   | white non hispanic         | theo                |   84 |   77 |  100 |   96 |   92 |   NA |
| male   | white non hispanic         | justin              |   84 |   71 |   88 |   80 |   58 |   62 |
| male   | white non hispanic         | hamza               |   84 |   79 |   96 |   91 |   90 |   NA |
| male   | white non hispanic         | austin              |   85 |   93 |   72 |   81 |   85 |   81 |
| male   | white non hispanic         | victor              |   85 |   71 |   77 |   88 |   73 |   73 |
| male   | white non hispanic         | carter              |   85 |   78 |   85 |   81 |   85 |   88 |
| male   | white non hispanic         | maxim               |   85 |   85 |   86 |   84 |   80 |   80 |
| male   | white non hispanic         | maximilian          |   85 |   76 |   92 |   82 |   74 |   78 |
| male   | white non hispanic         | jason               |   85 |   76 |   65 |   73 |   68 |   72 |
| male   | white non hispanic         | conor               |   85 |   94 |   93 |   93 |   NA |   94 |
| male   | white non hispanic         | hershel             |   85 |   78 |   94 |   87 |   83 |   83 |
| male   | white non hispanic         | emmett              |   85 |   83 |  100 |   94 |   91 |   NA |
| male   | white non hispanic         | ariel               |   85 |   81 |   88 |   95 |   84 |   77 |
| male   | white non hispanic         | quinn               |   85 |   90 |   98 |  100 |   92 |   94 |
| male   | white non hispanic         | cameron             |   85 |   80 |   88 |   83 |   83 |   80 |
| male   | white non hispanic         | shia                |   85 |   74 |   75 |   78 |   79 |   77 |
| male   | white non hispanic         | maximus             |   86 |   92 |   96 |   NA |   90 |   93 |
| male   | white non hispanic         | martin              |   86 |   83 |   86 |   76 |   71 |   86 |
| male   | white non hispanic         | felix               |   86 |   67 |   80 |   88 |   83 |   84 |
| male   | white non hispanic         | marco               |   86 |   89 |   97 |   92 |   74 |   91 |
| male   | white non hispanic         | lorenzo             |   86 |   80 |   92 |   85 |   83 |   85 |
| male   | white non hispanic         | lev                 |   87 |   84 |   94 |   96 |   NA |   97 |
| male   | white non hispanic         | louis               |   87 |   73 |   94 |   83 |   80 |   84 |
| male   | white non hispanic         | rowan               |   87 |   87 |   97 |   96 |   92 |   95 |
| male   | white non hispanic         | timothy             |   87 |   90 |   85 |   83 |   80 |   83 |
| male   | white non hispanic         | ian                 |   87 |   88 |   82 |   80 |   73 |   79 |
| male   | white non hispanic         | caleb               |   87 |   63 |   84 |   79 |   69 |   76 |
| male   | white non hispanic         | sean                |   87 |   77 |   61 |   73 |   64 |   71 |
| male   | white non hispanic         | spencer             |   87 |   83 |   94 |   84 |   86 |   92 |
| male   | white non hispanic         | grant               |   87 |   79 |  101 |   95 |   89 |   93 |
| male   | white non hispanic         | landon              |   87 |   92 |   89 |   99 |   87 |   94 |
| male   | white non hispanic         | avrohom             |   87 |   74 |   85 |   79 |   75 |   80 |
| male   | white non hispanic         | brandon             |   88 |   71 |   79 |   74 |   61 |   64 |
| male   | white non hispanic         | noam                |   88 |   81 |   89 |   89 |   90 |   96 |
| male   | white non hispanic         | ellis               |   88 |   92 |  100 |   NA |   93 |   97 |
| male   | white non hispanic         | reid                |   88 |   92 |   97 |   94 |   90 |   94 |
| male   | white non hispanic         | usher               |   88 |   80 |   92 |   85 |   77 |   78 |
| male   | white non hispanic         | youssef             |   88 |   85 |   88 |   87 |   93 |   96 |
| male   | white non hispanic         | yehoshua            |   88 |   82 |   86 |   88 |   82 |   85 |
| male   | white non hispanic         | akiva               |   88 |   89 |   90 |   80 |   84 |   87 |
| male   | white non hispanic         | dominic             |   88 |   74 |   85 |   75 |   76 |   92 |
| male   | white non hispanic         | cheskel             |   89 |   89 |   97 |   98 |   94 |   93 |
| male   | white non hispanic         | eric                |   89 |   85 |   83 |   84 |   69 |   80 |
| male   | white non hispanic         | shlome              |   89 |   78 |   93 |   91 |   85 |   90 |
| male   | white non hispanic         | gavriel             |   89 |   93 |   95 |   94 |   84 |   92 |
| male   | white non hispanic         | marcus              |   89 |   89 |   92 |   95 |   93 |   93 |
| male   | white non hispanic         | baruch              |   89 |   84 |   96 |   95 |   86 |   86 |
| male   | white non hispanic         | albert              |   89 |   86 |   95 |   NA |   83 |   92 |
| male   | white non hispanic         | meshilem            |   89 |   92 |   NA |   NA |   NA |   NA |
| male   | white non hispanic         | eitan               |   89 |   90 |   99 |   94 |   83 |   93 |
| male   | white non hispanic         | arlo                |   89 |   93 |   96 |   NA |   NA |   NA |
| male   | white non hispanic         | nico                |   90 |   92 |  102 |   97 |   92 |   94 |
| male   | white non hispanic         | avi                 |   90 |   87 |  101 |   99 |   NA |   95 |
| male   | white non hispanic         | abdullah            |   90 |   NA |   NA |   96 |   NA |   NA |
| male   | white non hispanic         | jasper              |   90 |   79 |   88 |   91 |   77 |   87 |
| male   | white non hispanic         | jaxon               |   90 |   86 |   98 |   88 |   92 |   NA |
| male   | white non hispanic         | ibrahim             |   90 |   90 |   94 |   90 |   86 |   NA |
| male   | white non hispanic         | mendy               |   90 |   80 |   99 |   95 |   80 |   96 |
| male   | white non hispanic         | everett             |   90 |   88 |   96 |   90 |   89 |   NA |
| male   | white non hispanic         | bennett             |   90 |   76 |   92 |   96 |   94 |   97 |
| male   | white non hispanic         | shloimy             |   90 |   90 |   96 |  100 |   88 |   94 |
| male   | white non hispanic         | shloime             |   90 |   86 |  101 |   96 |   92 |   92 |
| male   | white non hispanic         | salvatore           |   90 |   79 |   92 |   83 |   79 |   71 |
| male   | white non hispanic         | hugo                |   91 |   82 |   90 |   89 |   89 |   96 |
| male   | white non hispanic         | efraim              |   91 |   80 |   99 |   92 |   85 |   82 |
| male   | white non hispanic         | elimelech           |   91 |   85 |   96 |   92 |   86 |   91 |
| male   | white non hispanic         | jayden              |   91 |   78 |   77 |   78 |   74 |   68 |
| male   | white non hispanic         | morris              |   91 |   85 |   99 |   99 |   NA |   96 |
| male   | white non hispanic         | aharon              |   92 |   89 |   91 |   91 |   NA |   88 |
| male   | white non hispanic         | muhammad            |   92 |   84 |   96 |   89 |   91 |   NA |
| male   | white non hispanic         | kevin               |   92 |   74 |   95 |   71 |   67 |   78 |
| male   | white non hispanic         | hersh               |   92 |   91 |   90 |   95 |   90 |   93 |
| male   | white non hispanic         | kyle                |   92 |   93 |  100 |   95 |   89 |   83 |
| male   | white non hispanic         | yossi               |   92 |   92 |   97 |   92 |   93 |   NA |
| male   | white non hispanic         | samir               |   92 |   91 |   99 |   97 |   94 |   97 |
| male   | white non hispanic         | shane               |   92 |   88 |   93 |   98 |   80 |   86 |
| male   | white non hispanic         | anas                |   93 |   NA |   NA |   NA |   NA |   NA |
| male   | white non hispanic         | joel                |   93 |   72 |   81 |   92 |   76 |   77 |
| male   | white non hispanic         | angelo              |   93 |   89 |  100 |   90 |   85 |   91 |
| male   | white non hispanic         | moishe              |   93 |   76 |   91 |   95 |   82 |   89 |
| male   | white non hispanic         | pinchas             |   93 |   88 |   96 |  100 |   91 |   92 |
| male   | white non hispanic         | francesco           |   93 |   92 |   97 |   98 |   92 |   88 |
| male   | white non hispanic         | mateo               |   93 |   87 |   91 |   91 |   87 |   90 |
| male   | white non hispanic         | brian               |   93 |   84 |   94 |   88 |   84 |   77 |
| male   | white non hispanic         | mohammed            |   93 |   79 |   90 |   80 |   85 |   97 |
| male   | white non hispanic         | vincenzo            |   94 |   92 |  101 |   NA |   NA |   94 |
| male   | white non hispanic         | binyamin            |   94 |   NA |  101 |   NA |   NA |   NA |
| male   | white non hispanic         | axel                |   94 |   86 |  100 |  100 |   NA |   NA |
| male   | white non hispanic         | donovan             |   94 |   NA |   NA |   NA |   NA |   NA |
| male   | white non hispanic         | chesky              |   94 |   NA |   NA |   97 |   88 |   NA |
| male   | white non hispanic         | blake               |   94 |   78 |   94 |   83 |   80 |   93 |
| male   | white non hispanic         | damian              |   94 |   NA |   98 |   98 |   89 |   NA |
| male   | white non hispanic         | calvin              |   94 |   82 |   98 |   NA |   93 |   97 |
| male   | white non hispanic         | raymond             |   94 |   93 |   99 |   93 |   84 |   NA |
| male   | white non hispanic         | ronan               |   94 |   87 |   91 |  100 |   92 |   NA |
| male   | white non hispanic         | zalmen              |   94 |   89 |   NA |   98 |   92 |   93 |
| male   | white non hispanic         | ryder               |   94 |   80 |  100 |   96 |   NA |   91 |
| male   | white non hispanic         | benzion             |   94 |   78 |   94 |   90 |   91 |   94 |
| male   | white non hispanic         | bodhi               |   94 |   NA |   NA |   NA |   NA |   NA |
| male   | white non hispanic         | brody               |   95 |   87 |   92 |   88 |   89 |   88 |
| male   | white non hispanic         | beckett             |   95 |   87 |   99 |   97 |   93 |   95 |
| male   | white non hispanic         | frederick           |   95 |   79 |   96 |   98 |   94 |   NA |
| male   | white non hispanic         | mohammad            |   95 |   91 |   NA |   89 |   93 |   NA |
| male   | white non hispanic         | yisrael             |   95 |   92 |   98 |   NA |   93 |   93 |
| male   | white non hispanic         | pinchus             |   95 |   91 |  101 |   NA |   91 |   94 |
| male   | white non hispanic         | avery               |   95 |   92 |   NA |   93 |   92 |   NA |
| male   | white non hispanic         | harry               |   95 |   87 |  100 |   93 |   91 |   91 |
| male   | white non hispanic         | yoel                |   95 |   89 |  100 |   94 |   81 |   87 |
| male   | white non hispanic         | silas               |   95 |   91 |  101 |   99 |   92 |   97 |
| male   | white non hispanic         | reuven              |   95 |   93 |   NA |   NA |   90 |   NA |
| male   | white non hispanic         | griffin             |   95 |   86 |   96 |   94 |   85 |   86 |
| male   | white non hispanic         | timur               |   95 |   92 |   NA |   NA |   NA |   NA |
| male   | white non hispanic         | leonard             |   95 |   NA |   NA |   99 |   NA |   NA |
| male   | white non hispanic         | greyson             |   96 |   92 |   98 |   99 |   NA |   96 |
| male   | white non hispanic         | jace                |   96 |   NA |   99 |  100 |   94 |   NA |
| male   | white non hispanic         | boruch              |   96 |   90 |  100 |   97 |   93 |   91 |
| male   | white non hispanic         | river               |   96 |   NA |   NA |   NA |   NA |   NA |
| male   | white non hispanic         | brayden             |   96 |   93 |   99 |   92 |   82 |   92 |
| male   | white non hispanic         | steven              |   96 |   79 |   85 |   80 |   73 |   75 |
| male   | white non hispanic         | dominick            |   96 |   90 |   98 |   96 |   84 |   86 |
| male   | white non hispanic         | elya                |   96 |   NA |   NA |   NA |   NA |   NA |
| male   | white non hispanic         | jaxson              |   96 |   94 |   99 |   NA |   NA |   NA |
| male   | white non hispanic         | tristan             |   96 |   82 |   86 |   91 |   75 |   83 |
| male   | white non hispanic         | francis             |   96 |   89 |   99 |   97 |   NA |   NA |
| male   | white non hispanic         | stephen             |   96 |   NA |   94 |   97 |   90 |   97 |
| male   | white non hispanic         | pierce              |   96 |   89 |   NA |   NA |   NA |   NA |
| male   | white non hispanic         | xavier              |   96 |   92 |   87 |   92 |   89 |   89 |
| male   | white non hispanic         | yitzchak            |   96 |   NA |   97 |  100 |   90 |   90 |
| male   | white non hispanic         | malik               |   96 |   93 |   NA |   NA |   92 |   NA |
| male   | white non hispanic         | jad                 |   96 |   NA |   NA |   NA |   94 |   NA |
| male   | white non hispanic         | micah               |   96 |   85 |   96 |   92 |   81 |   95 |
| male   | white non hispanic         | otto                |   97 |   91 |   NA |   94 |   NA |   NA |
| male   | white non hispanic         | ivan                |   97 |   79 |   97 |   94 |   88 |   94 |
| male   | white non hispanic         | zayn                |   97 |   NA |   NA |   NA |   NA |   NA |
| male   | white non hispanic         | sholom              |   97 |   83 |   96 |   91 |   92 |   86 |
| male   | white non hispanic         | selim               |   97 |   91 |   NA |   NA |   NA |   NA |
| male   | white non hispanic         | brendan             |   97 |   89 |  100 |   91 |   80 |   84 |
| male   | white non hispanic         | fishel              |   97 |   NA |   NA |   NA |   NA |   NA |
| male   | white non hispanic         | enzo                |   97 |   NA |   NA |   NA |   NA |   NA |
| male   | white non hispanic         | ameer               |   97 |   NA |   NA |   NA |   NA |   NA |
| male   | white non hispanic         | desmond             |   97 |   NA |   NA |   91 |   NA |   NA |
| male   | white non hispanic         | lucian              |   97 |   NA |   NA |   NA |   NA |   NA |
| male   | white non hispanic         | imran               |   97 |   NA |  102 |   NA |   NA |   NA |
| male   | white non hispanic         | giuseppe            |   97 |   NA |  102 |   NA |   NA |   NA |
| male   | white non hispanic         | yonatan             |   97 |   NA |   NA |   NA |   NA |   NA |
| male   | white non hispanic         | avrum               |   97 |   91 |  100 |   93 |   86 |   96 |
| male   | white non hispanic         | julien              |   97 |   NA |   NA |   99 |   94 |   NA |
| male   | white non hispanic         | sami                |   97 |   NA |   NA |   NA |   94 |   NA |
| male   | white non hispanic         | cyrus               |   97 |   NA |   NA |   NA |   NA |   NA |
| male   | white non hispanic         | jay                 |   97 |   NA |   NA |   NA |   NA |   NA |
| male   | white non hispanic         | ayden               |   97 |   87 |  100 |   NA |   85 |   97 |
| male   | white non hispanic         | shalom              |   98 |   86 |   98 |  100 |   91 |   96 |
| male   | white non hispanic         | jeremy              |   98 |   88 |   99 |   99 |   86 |   NA |
| male   | white non hispanic         | ahron               |   98 |   93 |   NA |   NA |   93 |   90 |
| male   | white non hispanic         | motty               |   98 |   NA |   NA |   NA |   NA |   NA |
| male   | white non hispanic         | russell             |   98 |   NA |   NA |   NA |   NA |   NA |
| male   | white non hispanic         | gianluca            |   98 |   NA |   NA |   97 |   NA |   NA |
| male   | white non hispanic         | yassin              |   98 |   NA |   NA |   96 |   NA |   NA |
| male   | white non hispanic         | walter              |   98 |   93 |   NA |   97 |   NA |   NA |
| male   | white non hispanic         | ahmad               |   98 |   NA |   97 |  100 |   89 |   NA |
| male   | white non hispanic         | gregory             |   98 |   88 |   97 |   92 |   91 |   91 |
| male   | white non hispanic         | nikolai             |   98 |   NA |   NA |   NA |   NA |   NA |
| male   | white non hispanic         | zain                |   98 |   92 |   NA |   NA |   94 |   NA |
| male   | white non hispanic         | jesse               |   98 |   90 |   98 |   86 |   85 |   95 |
| male   | white non hispanic         | marcel              |   98 |   NA |  102 |   NA |   NA |   NA |
| male   | white non hispanic         | santino             |   98 |   NA |   98 |   96 |   NA |   96 |
| male   | white non hispanic         | josef               |   98 |   94 |   NA |   NA |   NA |   NA |
| male   | white non hispanic         | isaiah              |   98 |   94 |   NA |   99 |   94 |   91 |
| male   | white non hispanic         | shraga              |   98 |   NA |  102 |  100 |   NA |   NA |
| male   | white non hispanic         | milan               |   98 |   NA |  100 |   NA |   NA |   NA |
| male   | white non hispanic         | wolf                |   98 |   89 |  100 |   98 |   NA |   NA |
| male   | white non hispanic         | peretz              |   98 |   92 |   NA |   NA |   NA |   NA |
| male   | white non hispanic         | dillon              |   99 |   NA |   NA |   NA |   NA |   NA |
| male   | white non hispanic         | malek               |   99 |   NA |   NA |   NA |   NA |   NA |
| male   | white non hispanic         | joaquin             |   99 |   NA |   NA |   99 |   NA |   NA |
| male   | white non hispanic         | umar                |   99 |   NA |   NA |   NA |   NA |   NA |
| male   | white non hispanic         | lincoln             |   99 |   86 |  101 |   96 |   NA |   NA |
| male   | white non hispanic         | eyad                |   99 |   NA |   NA |   NA |   NA |   NA |
| male   | white non hispanic         | meyer               |   99 |   NA |   NA |   95 |   94 |   NA |
| male   | white non hispanic         | nosson              |   99 |   NA |  101 |   NA |   91 |   97 |
| male   | white non hispanic         | bradley             |   99 |   92 |   NA |  100 |   92 |   NA |
| male   | white non hispanic         | meilech             |   99 |   NA |  100 |   97 |   NA |   96 |
| male   | white non hispanic         | winston             |   99 |   93 |  101 |   NA |   NA |   NA |
| male   | white non hispanic         | walker              |   99 |   NA |   NA |   NA |   NA |   NA |
| male   | white non hispanic         | augustus            |   99 |   NA |   NA |   NA |   NA |   97 |
| male   | white non hispanic         | leonidas            |   99 |   NA |   NA |   NA |   NA |   NA |
| male   | white non hispanic         | camden              |   99 |   NA |   NA |   NA |   NA |   NA |
| male   | white non hispanic         | herman              |   99 |   NA |   NA |   NA |   NA |   NA |
| male   | white non hispanic         | noach               |   99 |   93 |   NA |   NA |   NA |   NA |
| male   | white non hispanic         | colton              |   99 |   94 |   NA |   96 |   NA |   NA |
| male   | white non hispanic         | nikolas             |   99 |   NA |   NA |   NA |   94 |   97 |
| male   | white non hispanic         | aviel               |   99 |   NA |   NA |   NA |   NA |   NA |
| male   | white non hispanic         | berl                |   99 |   88 |  100 |   NA |   NA |   NA |
| male   | white non hispanic         | mordche             |   99 |   NA |  102 |   NA |   NA |   NA |
| male   | white non hispanic         | elan                |   99 |   NA |   NA |   98 |   92 |   NA |
| male   | white non hispanic         | leibish             |   99 |   NA |  100 |   NA |   NA |   NA |
| male   | asian and pacific islander | jimmy               |   NA |   47 |   49 |   55 |   61 |   NA |
| male   | asian and pacific islander | luca                |   NA |   50 |   54 |   NA |   NA |   NA |
| male   | asian and pacific islander | gary                |   NA |   50 |   58 |   NA |   NA |   NA |
| male   | asian and pacific islander | feynman             |   NA |   51 |   NA |   NA |   NA |   NA |
| male   | asian and pacific islander | jacky               |   NA |   52 |   NA |   NA |   57 |   46 |
| male   | asian and pacific islander | anderson            |   NA |   52 |   56 |   NA |   NA |   NA |
| male   | asian and pacific islander | josiah              |   NA |   53 |   NA |   NA |   NA |   NA |
| male   | asian and pacific islander | saifan              |   NA |   53 |   NA |   NA |   NA |   NA |
| male   | asian and pacific islander | landon              |   NA |   53 |   NA |   NA |   NA |   NA |
| male   | asian and pacific islander | nolan               |   NA |   53 |   NA |   57 |   NA |   NA |
| male   | asian and pacific islander | dean                |   NA |   53 |   NA |   NA |   NA |   NA |
| male   | asian and pacific islander | derrick             |   NA |   54 |   NA |   NA |   NA |   NA |
| male   | asian and pacific islander | chase               |   NA |   54 |   57 |   NA |   58 |   NA |
| male   | asian and pacific islander | arthur              |   NA |   54 |   NA |   NA |   NA |   NA |
| male   | asian and pacific islander | ricky               |   NA |   54 |   58 |   54 |   59 |   50 |
| male   | asian and pacific islander | jay                 |   NA |   54 |   NA |   NA |   53 |   52 |
| male   | asian and pacific islander | abrar               |   NA |   55 |   NA |   NA |   NA |   NA |
| male   | asian and pacific islander | harry               |   NA |   55 |   NA |   56 |   NA |   NA |
| male   | asian and pacific islander | mohamed             |   NA |   55 |   NA |   NA |   59 |   53 |
| male   | asian and pacific islander | kenny               |   NA |   55 |   NA |   NA |   NA |   NA |
| male   | asian and pacific islander | bradley             |   NA |   55 |   NA |   NA |   NA |   NA |
| male   | asian and pacific islander | parker              |   NA |   55 |   NA |   NA |   NA |   NA |
| male   | asian and pacific islander | kaden               |   NA |   55 |   56 |   NA |   NA |   NA |
| male   | asian and pacific islander | kazi                |   NA |   55 |   NA |   NA |   NA |   NA |
| male   | asian and pacific islander | colin               |   NA |   55 |   53 |   56 |   NA |   NA |
| male   | asian and pacific islander | neil                |   NA |   55 |   55 |   NA |   61 |   NA |
| male   | asian and pacific islander | eshan               |   NA |   55 |   NA |   NA |   NA |   NA |
| male   | asian and pacific islander | tony                |   NA |   55 |   57 |   52 |   NA |   50 |
| male   | black non hispanic         | ian                 |   NA |   52 |   NA |   50 |   47 |   53 |
| male   | black non hispanic         | angel               |   NA |   52 |   60 |   NA |   NA |   64 |
| male   | black non hispanic         | aden                |   NA |   54 |   53 |   45 |   NA |   55 |
| male   | black non hispanic         | luke                |   NA |   56 |   57 |   58 |   53 |   NA |
| male   | black non hispanic         | kenneth             |   NA |   56 |   58 |   61 |   58 |   58 |
| male   | black non hispanic         | shawn               |   NA |   56 |   NA |   58 |   50 |   56 |
| male   | black non hispanic         | steven              |   NA |   58 |   60 |   NA |   59 |   62 |
| male   | black non hispanic         | camden              |   NA |   58 |   NA |   61 |   NA |   NA |
| male   | black non hispanic         | dominic             |   NA |   59 |   56 |   54 |   53 |   57 |
| male   | black non hispanic         | maxwell             |   NA |   59 |   NA |   61 |   NA |   NA |
| male   | black non hispanic         | evan                |   NA |   59 |   NA |   51 |   50 |   48 |
| male   | black non hispanic         | noel                |   NA |   59 |   57 |   56 |   58 |   NA |
| male   | black non hispanic         | kareem              |   NA |   59 |   52 |   57 |   57 |   NA |
| male   | black non hispanic         | aboubacar           |   NA |   60 |   NA |   NA |   NA |   NA |
| male   | black non hispanic         | abdoulaye           |   NA |   60 |   56 |   54 |   56 |   61 |
| male   | black non hispanic         | eric                |   NA |   60 |   51 |   55 |   49 |   60 |
| male   | black non hispanic         | mark                |   NA |   60 |   NA |   NA |   NA |   NA |
| male   | black non hispanic         | roman               |   NA |   61 |   60 |   NA |   NA |   NA |
| male   | black non hispanic         | thomas              |   NA |   61 |   59 |   NA |   57 |   NA |
| male   | black non hispanic         | kymani              |   NA |   61 |   NA |   NA |   51 |   55 |
| male   | black non hispanic         | dallas              |   NA |   61 |   NA |   NA |   NA |   NA |
| male   | black non hispanic         | jahmir              |   NA |   61 |   NA |   NA |   59 |   60 |
| male   | black non hispanic         | boubacar            |   NA |   61 |   NA |   59 |   61 |   NA |
| male   | black non hispanic         | owen                |   NA |   61 |   NA |   62 |   NA |   NA |
| male   | black non hispanic         | august              |   NA |   61 |   NA |   NA |   NA |   NA |
| male   | black non hispanic         | caden               |   NA |   61 |   60 |   NA |   61 |   NA |
| male   | black non hispanic         | makai               |   NA |   62 |   NA |   NA |   61 |   64 |
| male   | black non hispanic         | chad                |   NA |   62 |   NA |   NA |   60 |   62 |
| male   | black non hispanic         | chace               |   NA |   62 |   NA |   NA |   NA |   NA |
| male   | black non hispanic         | jackson             |   NA |   62 |   NA |   NA |   NA |   63 |
| male   | black non hispanic         | jamir               |   NA |   62 |   NA |   NA |   NA |   62 |
| male   | black non hispanic         | ahmed               |   NA |   62 |   60 |   NA |   NA |   63 |
| male   | black non hispanic         | jaheim              |   NA |   63 |   NA |   NA |   NA |   64 |
| male   | black non hispanic         | corey               |   NA |   63 |   NA |   60 |   NA |   64 |
| male   | black non hispanic         | riley               |   NA |   63 |   56 |   57 |   57 |   NA |
| male   | black non hispanic         | stephen             |   NA |   63 |   NA |   60 |   NA |   64 |
| male   | black non hispanic         | jeremy              |   NA |   63 |   56 |   56 |   55 |   59 |
| male   | black non hispanic         | quincy              |   NA |   63 |   55 |   NA |   NA |   64 |
| male   | black non hispanic         | leo                 |   NA |   63 |   NA |   NA |   NA |   NA |
| male   | black non hispanic         | jasiah              |   NA |   63 |   58 |   57 |   55 |   58 |
| male   | black non hispanic         | jabari              |   NA |   63 |   NA |   NA |   NA |   NA |
| male   | black non hispanic         | leon                |   NA |   63 |   NA |   59 |   60 |   NA |
| male   | hispanic                   | danny               |   NA |   86 |   91 |   86 |   89 |   73 |
| male   | hispanic                   | alvin               |   NA |   89 |   NA |   NA |   98 |   94 |
| male   | hispanic                   | ashton              |   NA |   90 |   95 |   93 |  100 |   87 |
| male   | hispanic                   | emiliano            |   NA |   90 |   97 |   NA |  100 |   91 |
| male   | hispanic                   | esteban             |   NA |   92 |   95 |   NA |   93 |   91 |
| male   | hispanic                   | seth                |   NA |   93 |   NA |   NA |   NA |   NA |
| male   | hispanic                   | saul                |   NA |   94 |   92 |   NA |   98 |   NA |
| male   | hispanic                   | jaime               |   NA |   94 |   NA |   NA |   93 |   93 |
| male   | hispanic                   | yariel              |   NA |   95 |   96 |   95 |  100 |   92 |
| male   | hispanic                   | bruce               |   NA |   95 |   94 |   93 |   97 |   NA |
| male   | hispanic                   | ronald              |   NA |   95 |   NA |   NA |   97 |   NA |
| male   | hispanic                   | bryant              |   NA |   96 |   95 |   93 |   97 |   87 |
| male   | hispanic                   | amari               |   NA |   96 |   NA |   NA |   NA |   NA |
| male   | hispanic                   | xander              |   NA |   96 |   NA |   93 |   NA |   NA |
| male   | hispanic                   | romeo               |   NA |   96 |   94 |   95 |   95 |   90 |
| male   | hispanic                   | leon                |   NA |   96 |   95 |   NA |   NA |   NA |
| male   | hispanic                   | devin               |   NA |   96 |   96 |   81 |   88 |   79 |
| male   | hispanic                   | jonael              |   NA |   97 |   NA |   NA |   NA |   NA |
| male   | hispanic                   | cristopher          |   NA |   97 |   NA |   94 |   95 |   91 |
| male   | hispanic                   | neymar              |   NA |   97 |   89 |   89 |   96 |   NA |
| male   | hispanic                   | jordi               |   NA |   97 |   NA |   NA |   NA |   NA |
| male   | hispanic                   | charlie             |   NA |   98 |   NA |   89 |   97 |   NA |
| male   | hispanic                   | yahir               |   NA |   98 |   93 |   84 |   91 |   88 |
| male   | hispanic                   | leonel              |   NA |   99 |   NA |   92 |  100 |   92 |
| male   | hispanic                   | alberto             |   NA |   99 |   95 |   92 |   96 |   89 |
| male   | hispanic                   | joziah              |   NA |   99 |   NA |   NA |   NA |   NA |
| male   | hispanic                   | stephen             |   NA |   99 |   97 |   NA |   97 |   94 |
| male   | hispanic                   | marc                |   NA |   99 |   NA |   NA |   NA |   NA |
| male   | hispanic                   | karter              |   NA |   99 |   NA |   NA |   NA |   NA |
| male   | hispanic                   | preston             |   NA |   99 |   NA |   95 |   96 |   90 |
| male   | hispanic                   | gregory             |   NA |   99 |   98 |   94 |   96 |   88 |
| male   | hispanic                   | ronaldo             |   NA |   99 |   NA |   NA |   NA |   NA |
| male   | hispanic                   | julien              |   NA |   99 |   NA |   95 |   95 |   90 |
| male   | hispanic                   | mauricio            |   NA |   99 |   NA |   95 |   97 |   92 |
| male   | hispanic                   | wesley              |   NA |   99 |   NA |   94 |   98 |   NA |
| male   | hispanic                   | alonso              |   NA |   99 |   NA |   95 |   NA |   NA |
| male   | hispanic                   | bruno               |   NA |   99 |   NA |   NA |   NA |   NA |
| male   | hispanic                   | francis             |   NA |   99 |   NA |   NA |   NA |   NA |
| male   | hispanic                   | armando             |   NA |   99 |   NA |   NA |  100 |   93 |
| male   | white non hispanic         | yaseen              |   NA |   82 |  100 |   NA |   NA |   96 |
| male   | white non hispanic         | nachman             |   NA |   84 |   NA |   99 |   87 |   NA |
| male   | white non hispanic         | marc                |   NA |   85 |   99 |   94 |   91 |   91 |
| male   | white non hispanic         | shloma              |   NA |   85 |   NA |   NA |   93 |   NA |
| male   | white non hispanic         | sawyer              |   NA |   86 |   85 |   95 |   NA |   NA |
| male   | white non hispanic         | drew                |   NA |   87 |  100 |   NA |   89 |   NA |
| male   | white non hispanic         | phoenix             |   NA |   88 |   NA |   NA |   NA |   NA |
| male   | white non hispanic         | ezriel              |   NA |   90 |   NA |   NA |   94 |   NA |
| male   | white non hispanic         | binyomin            |   NA |   90 |  101 |  100 |   93 |   93 |
| male   | white non hispanic         | brady               |   NA |   90 |   99 |   94 |   82 |   94 |
| male   | white non hispanic         | archer              |   NA |   90 |  100 |   97 |   93 |   NA |
| male   | white non hispanic         | ephraim             |   NA |   90 |   NA |   NA |   89 |   NA |
| male   | white non hispanic         | jameson             |   NA |   90 |   NA |   NA |   NA |   NA |
| male   | white non hispanic         | clark               |   NA |   91 |   NA |   NA |   NA |   NA |
| male   | white non hispanic         | shea                |   NA |   91 |   99 |   91 |   93 |   91 |
| male   | white non hispanic         | brooks              |   NA |   91 |  100 |   NA |   NA |   NA |
| male   | white non hispanic         | eluzer              |   NA |   92 |   NA |   NA |   93 |   NA |
| male   | white non hispanic         | berish              |   NA |   93 |  102 |   NA |   NA |   97 |
| male   | white non hispanic         | kian                |   NA |   93 |   NA |   NA |   NA |   NA |
| male   | white non hispanic         | emmanuel            |   NA |   93 |   NA |   98 |   NA |   NA |
| male   | white non hispanic         | rory                |   NA |   93 |  101 |  100 |   89 |   NA |
| male   | white non hispanic         | karas               |   NA |   93 |  102 |   NA |   NA |   NA |
| male   | white non hispanic         | alessandro          |   NA |   93 |   NA |   NA |   NA |   NA |
| male   | white non hispanic         | bryan               |   NA |   93 |   NA |   NA |   NA |   93 |
| male   | white non hispanic         | oren                |   NA |   93 |   NA |   NA |   NA |   NA |
| male   | white non hispanic         | crosby              |   NA |   93 |   NA |   NA |   NA |   NA |
| male   | white non hispanic         | rhys                |   NA |   94 |  100 |  100 |   NA |   NA |
| male   | white non hispanic         | duvid               |   NA |   94 |   96 |  100 |   NA |   NA |
| male   | white non hispanic         | anton               |   NA |   94 |  101 |   99 |   NA |   NA |
| male   | white non hispanic         | saul                |   NA |   94 |   NA |   NA |   NA |   NA |
| male   | white non hispanic         | menashe             |   NA |   94 |  100 |   NA |   NA |   96 |
| male   | white non hispanic         | yahya               |   NA |   94 |  101 |   NA |   NA |   NA |
| male   | white non hispanic         | kieran              |   NA |   94 |   98 |   89 |   89 |   94 |
| male   | white non hispanic         | otis                |   NA |   94 |   NA |   NA |   NA |   NA |
| male   | white non hispanic         | bentzion            |   NA |   94 |   NA |   NA |   92 |   NA |
| male   | white non hispanic         | seth                |   NA |   94 |   NA |   NA |   NA |   NA |
| male   | white non hispanic         | eliot               |   NA |   94 |   NA |   NA |   NA |   NA |
| male   | asian and pacific islander | alfred              |   NA |   NA |   50 |   NA |   NA |   NA |
| male   | asian and pacific islander | jake                |   NA |   NA |   54 |   51 |   52 |   49 |
| male   | asian and pacific islander | leonardo            |   NA |   NA |   55 |   NA |   NA |   NA |
| male   | asian and pacific islander | zain                |   NA |   NA |   56 |   54 |   59 |   54 |
| male   | asian and pacific islander | isaiah              |   NA |   NA |   56 |   NA |   NA |   NA |
| male   | asian and pacific islander | arvin               |   NA |   NA |   57 |   NA |   NA |   NA |
| male   | asian and pacific islander | omar                |   NA |   NA |   58 |   NA |   NA |   NA |
| male   | asian and pacific islander | travis              |   NA |   NA |   58 |   NA |   NA |   55 |
| male   | asian and pacific islander | shayan              |   NA |   NA |   58 |   NA |   61 |   NA |
| male   | asian and pacific islander | roman               |   NA |   NA |   58 |   NA |   NA |   NA |
| male   | asian and pacific islander | riley               |   NA |   NA |   58 |   NA |   NA |   NA |
| male   | asian and pacific islander | mark                |   NA |   NA |   58 |   NA |   NA |   NA |
| male   | asian and pacific islander | hassan              |   NA |   NA |   58 |   NA |   NA |   NA |
| male   | asian and pacific islander | gordon              |   NA |   NA |   58 |   54 |   59 |   NA |
| male   | asian and pacific islander | frank               |   NA |   NA |   58 |   NA |   NA |   NA |
| male   | asian and pacific islander | emmanuel            |   NA |   NA |   58 |   NA |   NA |   NA |
| male   | black non hispanic         | brian               |   NA |   NA |   50 |   62 |   59 |   52 |
| male   | black non hispanic         | kristian            |   NA |   NA |   53 |   NA |   61 |   NA |
| male   | black non hispanic         | julius              |   NA |   NA |   53 |   NA |   NA |   NA |
| male   | black non hispanic         | bradley             |   NA |   NA |   55 |   59 |   61 |   NA |
| male   | black non hispanic         | zahir               |   NA |   NA |   56 |   NA |   NA |   NA |
| male   | black non hispanic         | zaiden              |   NA |   NA |   56 |   NA |   NA |   NA |
| male   | black non hispanic         | carmelo             |   NA |   NA |   57 |   NA |   60 |   61 |
| male   | black non hispanic         | gavin               |   NA |   NA |   58 |   59 |   52 |   56 |
| male   | black non hispanic         | mohammed            |   NA |   NA |   58 |   NA |   60 |   51 |
| male   | black non hispanic         | travis              |   NA |   NA |   58 |   59 |   NA |   57 |
| male   | black non hispanic         | jamar               |   NA |   NA |   58 |   NA |   NA |   NA |
| male   | black non hispanic         | jayson              |   NA |   NA |   59 |   NA |   NA |   60 |
| male   | black non hispanic         | jalen               |   NA |   NA |   59 |   58 |   54 |   59 |
| male   | black non hispanic         | landon              |   NA |   NA |   59 |   NA |   NA |   63 |
| male   | black non hispanic         | antonio             |   NA |   NA |   59 |   59 |   60 |   59 |
| male   | black non hispanic         | savion              |   NA |   NA |   60 |   NA |   NA |   NA |
| male   | black non hispanic         | zane                |   NA |   NA |   60 |   NA |   NA |   NA |
| male   | black non hispanic         | donovan             |   NA |   NA |   60 |   NA |   NA |   64 |
| male   | black non hispanic         | seth                |   NA |   NA |   60 |   NA |   NA |   62 |
| male   | black non hispanic         | zamir               |   NA |   NA |   60 |   NA |   NA |   NA |
| male   | black non hispanic         | devin               |   NA |   NA |   60 |   60 |   53 |   50 |
| male   | black non hispanic         | darren              |   NA |   NA |   60 |   NA |   58 |   62 |
| male   | black non hispanic         | calvin              |   NA |   NA |   60 |   NA |   NA |   NA |
| male   | black non hispanic         | lorenzo             |   NA |   NA |   60 |   NA |   NA |   NA |
| male   | black non hispanic         | amare               |   NA |   NA |   60 |   57 |   56 |   59 |
| male   | hispanic                   | riley               |   NA |   NA |   85 |   87 |   97 |   86 |
| male   | hispanic                   | yandel              |   NA |   NA |   93 |   NA |   92 |   92 |
| male   | hispanic                   | junior              |   NA |   NA |   94 |   95 |   NA |   86 |
| male   | hispanic                   | marcelo             |   NA |   NA |   96 |   NA |   NA |   NA |
| male   | hispanic                   | dean                |   NA |   NA |   96 |   93 |  100 |   NA |
| male   | hispanic                   | steve               |   NA |   NA |   96 |   NA |   97 |   94 |
| male   | hispanic                   | ruben               |   NA |   NA |   97 |   NA |   98 |   85 |
| male   | hispanic                   | melvin              |   NA |   NA |   97 |   92 |   91 |   92 |
| male   | hispanic                   | theodore            |   NA |   NA |   97 |   NA |   NA |   NA |
| male   | hispanic                   | gerardo             |   NA |   NA |   97 |   NA |   NA |   92 |
| male   | hispanic                   | skyler              |   NA |   NA |   97 |   NA |   99 |   NA |
| male   | hispanic                   | myles               |   NA |   NA |   97 |   NA |   NA |   NA |
| male   | hispanic                   | maddox              |   NA |   NA |   98 |   NA |   NA |   NA |
| male   | hispanic                   | walter              |   NA |   NA |   98 |   NA |   NA |   NA |
| male   | hispanic                   | donovan             |   NA |   NA |   98 |   NA |   98 |   NA |
| male   | hispanic                   | marvin              |   NA |   NA |   98 |   95 |   96 |   88 |
| male   | hispanic                   | dennis              |   NA |   NA |   98 |   NA |   NA |   89 |
| male   | hispanic                   | adan                |   NA |   NA |   98 |   95 |   99 |   NA |
| male   | hispanic                   | orlando             |   NA |   NA |   98 |   NA |   NA |   NA |
| male   | hispanic                   | eddy                |   NA |   NA |   98 |   NA |   98 |   NA |
| male   | hispanic                   | derick              |   NA |   NA |   98 |   NA |   NA |   86 |
| male   | hispanic                   | misael              |   NA |   NA |   98 |   NA |   NA |   NA |
| male   | white non hispanic         | derek               |   NA |   NA |   98 |   NA |   NA |   97 |
| male   | white non hispanic         | preston             |   NA |   NA |   99 |   92 |   91 |   NA |
| male   | white non hispanic         | devin               |   NA |   NA |   99 |   99 |   94 |   89 |
| male   | white non hispanic         | ashton              |   NA |   NA |  100 |   NA |   NA |   97 |
| male   | white non hispanic         | hillel              |   NA |   NA |  100 |   99 |   NA |   NA |
| male   | white non hispanic         | kareem              |   NA |   NA |  100 |   NA |   NA |   NA |
| male   | white non hispanic         | aydin               |   NA |   NA |  101 |   NA |   NA |   NA |
| male   | white non hispanic         | dante               |   NA |   NA |  101 |   96 |   NA |   NA |
| male   | white non hispanic         | weston              |   NA |   NA |  101 |   NA |   NA |   NA |
| male   | white non hispanic         | cormac              |   NA |   NA |  101 |   NA |   NA |   NA |
| male   | white non hispanic         | filip               |   NA |   NA |  101 |   99 |   89 |   94 |
| male   | white non hispanic         | jax                 |   NA |   NA |  101 |   97 |   NA |   NA |
| male   | white non hispanic         | reuben              |   NA |   NA |  101 |   NA |   NA |   NA |
| male   | white non hispanic         | riley               |   NA |   NA |  101 |   97 |   NA |   93 |
| male   | white non hispanic         | jakub               |   NA |   NA |  101 |   NA |   NA |   NA |
| male   | white non hispanic         | reed                |   NA |   NA |  101 |   NA |   93 |   97 |
| male   | white non hispanic         | nikita              |   NA |   NA |  101 |   NA |   93 |   NA |
| male   | white non hispanic         | yidel               |   NA |   NA |  102 |   NA |   NA |   96 |
| male   | white non hispanic         | issac               |   NA |   NA |  102 |   NA |   94 |   NA |
| male   | white non hispanic         | hayden              |   NA |   NA |  102 |   NA |   94 |   NA |
| male   | white non hispanic         | denis               |   NA |   NA |  102 |   NA |   NA |   NA |
| male   | white non hispanic         | lucien              |   NA |   NA |  102 |   92 |   NA |   NA |
| male   | white non hispanic         | erik                |   NA |   NA |  102 |   NA |   94 |   NA |
| male   | white non hispanic         | dashiell            |   NA |   NA |  102 |   NA |   NA |   95 |
| male   | white non hispanic         | myles               |   NA |   NA |  102 |  100 |   91 |   NA |
| male   | asian and pacific islander | vihaan              |   NA |   NA |   NA |   50 |   61 |   NA |
| male   | asian and pacific islander | danny               |   NA |   NA |   NA |   52 |   58 |   46 |
| male   | asian and pacific islander | aditya              |   NA |   NA |   NA |   54 |   60 |   52 |
| male   | asian and pacific islander | eddie               |   NA |   NA |   NA |   55 |   NA |   NA |
| male   | asian and pacific islander | ishaan              |   NA |   NA |   NA |   56 |   NA |   56 |
| male   | asian and pacific islander | avery               |   NA |   NA |   NA |   56 |   NA |   NA |
| male   | asian and pacific islander | bowen               |   NA |   NA |   NA |   57 |   NA |   NA |
| male   | asian and pacific islander | maximilian          |   NA |   NA |   NA |   57 |   NA |   NA |
| male   | asian and pacific islander | brendan             |   NA |   NA |   NA |   57 |   NA |   NA |
| male   | asian and pacific islander | martin              |   NA |   NA |   NA |   57 |   NA |   52 |
| male   | black non hispanic         | hayden              |   NA |   NA |   NA |   56 |   NA |   62 |
| male   | black non hispanic         | paul                |   NA |   NA |   NA |   58 |   58 |   NA |
| male   | black non hispanic         | malcolm             |   NA |   NA |   NA |   59 |   NA |   63 |
| male   | black non hispanic         | amar’e              |   NA |   NA |   NA |   60 |   55 |   62 |
| male   | black non hispanic         | chris               |   NA |   NA |   NA |   60 |   NA |   64 |
| male   | black non hispanic         | samir               |   NA |   NA |   NA |   61 |   59 |   NA |
| male   | black non hispanic         | isiah               |   NA |   NA |   NA |   61 |   NA |   63 |
| male   | black non hispanic         | jamel               |   NA |   NA |   NA |   61 |   NA |   61 |
| male   | black non hispanic         | taylor              |   NA |   NA |   NA |   61 |   NA |   NA |
| male   | black non hispanic         | ismael              |   NA |   NA |   NA |   61 |   NA |   NA |
| male   | black non hispanic         | patrick             |   NA |   NA |   NA |   62 |   NA |   60 |
| male   | black non hispanic         | nazir               |   NA |   NA |   NA |   62 |   NA |   NA |
| male   | black non hispanic         | gregory             |   NA |   NA |   NA |   62 |   NA |   NA |
| male   | black non hispanic         | skyler              |   NA |   NA |   NA |   62 |   NA |   NA |
| male   | black non hispanic         | jaylin              |   NA |   NA |   NA |   62 |   NA |   NA |
| male   | black non hispanic         | musa                |   NA |   NA |   NA |   62 |   NA |   NA |
| male   | black non hispanic         | zachariah           |   NA |   NA |   NA |   62 |   NA |   NA |
| male   | black non hispanic         | dominick            |   NA |   NA |   NA |   62 |   NA |   NA |
| male   | hispanic                   | raul                |   NA |   NA |   NA |   83 |   95 |   NA |
| male   | hispanic                   | tomas               |   NA |   NA |   NA |   87 |   NA |   94 |
| male   | hispanic                   | blake               |   NA |   NA |   NA |   88 |   98 |   NA |
| male   | hispanic                   | patrick             |   NA |   NA |   NA |   89 |   NA |   92 |
| male   | hispanic                   | dilan               |   NA |   NA |   NA |   90 |   NA |   NA |
| male   | hispanic                   | denzel              |   NA |   NA |   NA |   91 |   96 |   NA |
| male   | hispanic                   | carmelo             |   NA |   NA |   NA |   91 |  100 |   88 |
| male   | hispanic                   | gustavo             |   NA |   NA |   NA |   91 |   95 |   89 |
| male   | hispanic                   | cayden              |   NA |   NA |   NA |   92 |   NA |   NA |
| male   | hispanic                   | justice             |   NA |   NA |   NA |   93 |   NA |   NA |
| male   | hispanic                   | alfredo             |   NA |   NA |   NA |   93 |   NA |   89 |
| male   | hispanic                   | jeremias            |   NA |   NA |   NA |   93 |   NA |   90 |
| male   | hispanic                   | giovanny            |   NA |   NA |   NA |   93 |   NA |   91 |
| male   | hispanic                   | arian               |   NA |   NA |   NA |   94 |   NA |   NA |
| male   | hispanic                   | camilo              |   NA |   NA |   NA |   94 |   98 |   NA |
| male   | hispanic                   | dereck              |   NA |   NA |   NA |   94 |   NA |   93 |
| male   | hispanic                   | aden                |   NA |   NA |   NA |   94 |   99 |   91 |
| male   | hispanic                   | giovani             |   NA |   NA |   NA |   94 |   NA |   94 |
| male   | hispanic                   | mayson              |   NA |   NA |   NA |   94 |   NA |   NA |
| male   | hispanic                   | wilson              |   NA |   NA |   NA |   95 |   99 |   93 |
| male   | hispanic                   | lian                |   NA |   NA |   NA |   95 |   NA |   NA |
| male   | hispanic                   | hanley              |   NA |   NA |   NA |   95 |   NA |   NA |
| male   | hispanic                   | januel              |   NA |   NA |   NA |   95 |   NA |   93 |
| male   | hispanic                   | jair                |   NA |   NA |   NA |   95 |   NA |   NA |
| male   | hispanic                   | gerard              |   NA |   NA |   NA |   95 |   NA |   NA |
| male   | hispanic                   | jahir               |   NA |   NA |   NA |   95 |   NA |   NA |
| male   | hispanic                   | maximo              |   NA |   NA |   NA |   95 |   NA |   92 |
| male   | white non hispanic         | dennis              |   NA |   NA |   NA |   98 |   NA |   96 |
| male   | white non hispanic         | ilan                |   NA |   NA |   NA |   98 |   87 |   90 |
| male   | white non hispanic         | massimo             |   NA |   NA |   NA |   98 |   NA |   NA |
| male   | white non hispanic         | yanky               |   NA |   NA |   NA |   98 |   NA |   NA |
| male   | white non hispanic         | arye                |   NA |   NA |   NA |   98 |   NA |   NA |
| male   | white non hispanic         | ori                 |   NA |   NA |   NA |   99 |   NA |   NA |
| male   | white non hispanic         | shneur              |   NA |   NA |   NA |   99 |   94 |   96 |
| male   | white non hispanic         | yeshaya             |   NA |   NA |   NA |   99 |   NA |   NA |
| male   | white non hispanic         | alec                |   NA |   NA |   NA |   99 |   NA |   96 |
| male   | white non hispanic         | bryce               |   NA |   NA |   NA |   99 |   NA |   97 |
| male   | white non hispanic         | laith               |   NA |   NA |   NA |  100 |   NA |   NA |
| male   | white non hispanic         | stanley             |   NA |   NA |   NA |  100 |   NA |   NA |
| male   | white non hispanic         | orion               |   NA |   NA |   NA |  100 |   NA |   NA |
| male   | white non hispanic         | giancarlo           |   NA |   NA |   NA |  100 |   NA |   NA |
| male   | asian and pacific islander | patrick             |   NA |   NA |   NA |   NA |   55 |   52 |
| male   | asian and pacific islander | angus               |   NA |   NA |   NA |   NA |   55 |   NA |
| male   | asian and pacific islander | nikhil              |   NA |   NA |   NA |   NA |   56 |   NA |
| male   | asian and pacific islander | terrence            |   NA |   NA |   NA |   NA |   57 |   NA |
| male   | asian and pacific islander | shayaan             |   NA |   NA |   NA |   NA |   58 |   NA |
| male   | asian and pacific islander | kingston            |   NA |   NA |   NA |   NA |   58 |   NA |
| male   | asian and pacific islander | hao                 |   NA |   NA |   NA |   NA |   60 |   NA |
| male   | asian and pacific islander | kenneth             |   NA |   NA |   NA |   NA |   60 |   NA |
| male   | asian and pacific islander | jun                 |   NA |   NA |   NA |   NA |   61 |   NA |
| male   | asian and pacific islander | bruce               |   NA |   NA |   NA |   NA |   61 |   NA |
| male   | asian and pacific islander | subhan              |   NA |   NA |   NA |   NA |   61 |   NA |
| male   | black non hispanic         | kiyan               |   NA |   NA |   NA |   NA |   56 |   62 |
| male   | black non hispanic         | abdul               |   NA |   NA |   NA |   NA |   56 |   NA |
| male   | black non hispanic         | jermaine            |   NA |   NA |   NA |   NA |   57 |   58 |
| male   | black non hispanic         | troy                |   NA |   NA |   NA |   NA |   58 |   61 |
| male   | black non hispanic         | maurice             |   NA |   NA |   NA |   NA |   58 |   58 |
| male   | black non hispanic         | nathanael           |   NA |   NA |   NA |   NA |   58 |   NA |
| male   | black non hispanic         | makhi               |   NA |   NA |   NA |   NA |   60 |   NA |
| male   | black non hispanic         | keith               |   NA |   NA |   NA |   NA |   61 |   62 |
| male   | black non hispanic         | mahamadou           |   NA |   NA |   NA |   NA |   61 |   NA |
| male   | black non hispanic         | damani              |   NA |   NA |   NA |   NA |   61 |   NA |
| male   | black non hispanic         | armani              |   NA |   NA |   NA |   NA |   61 |   NA |
| male   | black non hispanic         | terrell             |   NA |   NA |   NA |   NA |   61 |   64 |
| male   | hispanic                   | erik                |   NA |   NA |   NA |   NA |   88 |   86 |
| male   | hispanic                   | yael                |   NA |   NA |   NA |   NA |   95 |   NA |
| male   | hispanic                   | aldo                |   NA |   NA |   NA |   NA |   96 |   94 |
| male   | hispanic                   | hugo                |   NA |   NA |   NA |   NA |   96 |   NA |
| male   | hispanic                   | freddy              |   NA |   NA |   NA |   NA |   98 |   91 |
| male   | hispanic                   | izaiah              |   NA |   NA |   NA |   NA |   99 |   NA |
| male   | hispanic                   | elvin               |   NA |   NA |   NA |   NA |  100 |   NA |
| male   | hispanic                   | jaylen              |   NA |   NA |   NA |   NA |  100 |   NA |
| male   | hispanic                   | jayren              |   NA |   NA |   NA |   NA |  100 |   NA |
| male   | hispanic                   | jencarlos           |   NA |   NA |   NA |   NA |  100 |   93 |
| male   | white non hispanic         | alter               |   NA |   NA |   NA |   NA |   89 |   NA |
| male   | white non hispanic         | chris               |   NA |   NA |   NA |   NA |   90 |   NA |
| male   | white non hispanic         | yechezkel           |   NA |   NA |   NA |   NA |   91 |   96 |
| male   | white non hispanic         | anderson            |   NA |   NA |   NA |   NA |   93 |   NA |
| male   | white non hispanic         | harvey              |   NA |   NA |   NA |   NA |   94 |   NA |
| male   | white non hispanic         | yona                |   NA |   NA |   NA |   NA |   94 |   NA |
| male   | white non hispanic         | aleksander          |   NA |   NA |   NA |   NA |   94 |   NA |
| male   | white non hispanic         | maksim              |   NA |   NA |   NA |   NA |   94 |   96 |
| male   | white non hispanic         | atticus             |   NA |   NA |   NA |   NA |   94 |   NA |
| male   | white non hispanic         | tomas               |   NA |   NA |   NA |   NA |   94 |   NA |
| male   | asian and pacific islander | jia                 |   NA |   NA |   NA |   NA |   NA |   51 |
| male   | asian and pacific islander | peter               |   NA |   NA |   NA |   NA |   NA |   55 |
| male   | black non hispanic         | lamar               |   NA |   NA |   NA |   NA |   NA |   59 |
| male   | black non hispanic         | sekou               |   NA |   NA |   NA |   NA |   NA |   59 |
| male   | black non hispanic         | zyaire              |   NA |   NA |   NA |   NA |   NA |   60 |
| male   | black non hispanic         | dwayne              |   NA |   NA |   NA |   NA |   NA |   60 |
| male   | black non hispanic         | jared               |   NA |   NA |   NA |   NA |   NA |   62 |
| male   | black non hispanic         | jesse               |   NA |   NA |   NA |   NA |   NA |   62 |
| male   | black non hispanic         | nicolas             |   NA |   NA |   NA |   NA |   NA |   63 |
| male   | black non hispanic         | marc                |   NA |   NA |   NA |   NA |   NA |   63 |
| male   | black non hispanic         | terrence            |   NA |   NA |   NA |   NA |   NA |   63 |
| male   | black non hispanic         | jelani              |   NA |   NA |   NA |   NA |   NA |   63 |
| male   | black non hispanic         | edward              |   NA |   NA |   NA |   NA |   NA |   64 |
| male   | black non hispanic         | ishmael             |   NA |   NA |   NA |   NA |   NA |   64 |
| male   | black non hispanic         | hassan              |   NA |   NA |   NA |   NA |   NA |   64 |
| male   | black non hispanic         | rodney              |   NA |   NA |   NA |   NA |   NA |   64 |
| male   | black non hispanic         | devon               |   NA |   NA |   NA |   NA |   NA |   64 |
| male   | black non hispanic         | marquis             |   NA |   NA |   NA |   NA |   NA |   64 |
| male   | black non hispanic         | nigel               |   NA |   NA |   NA |   NA |   NA |   64 |
| male   | black non hispanic         | alvin               |   NA |   NA |   NA |   NA |   NA |   64 |
| male   | black non hispanic         | alijah              |   NA |   NA |   NA |   NA |   NA |   64 |
| male   | black non hispanic         | ricardo             |   NA |   NA |   NA |   NA |   NA |   64 |
| male   | black non hispanic         | nana                |   NA |   NA |   NA |   NA |   NA |   64 |
| male   | black non hispanic         | cody                |   NA |   NA |   NA |   NA |   NA |   64 |
| male   | hispanic                   | johnathan           |   NA |   NA |   NA |   NA |   NA |   88 |
| male   | hispanic                   | shawn               |   NA |   NA |   NA |   NA |   NA |   89 |
| male   | hispanic                   | yerik               |   NA |   NA |   NA |   NA |   NA |   91 |
| male   | hispanic                   | travis              |   NA |   NA |   NA |   NA |   NA |   91 |
| male   | hispanic                   | eddie               |   NA |   NA |   NA |   NA |   NA |   92 |
| male   | hispanic                   | roger               |   NA |   NA |   NA |   NA |   NA |   93 |
| male   | hispanic                   | royce               |   NA |   NA |   NA |   NA |   NA |   93 |
| male   | hispanic                   | jerry               |   NA |   NA |   NA |   NA |   NA |   93 |
| male   | hispanic                   | cristofer           |   NA |   NA |   NA |   NA |   NA |   93 |
| male   | hispanic                   | yair                |   NA |   NA |   NA |   NA |   NA |   94 |
| male   | hispanic                   | yaniel              |   NA |   NA |   NA |   NA |   NA |   94 |
| male   | hispanic                   | taylor              |   NA |   NA |   NA |   NA |   NA |   94 |
| male   | hispanic                   | jeancarlos          |   NA |   NA |   NA |   NA |   NA |   94 |
| male   | hispanic                   | jesse               |   NA |   NA |   NA |   NA |   NA |   94 |
| male   | hispanic                   | elvis               |   NA |   NA |   NA |   NA |   NA |   94 |
| male   | hispanic                   | janiel              |   NA |   NA |   NA |   NA |   NA |   94 |
| male   | hispanic                   | shane               |   NA |   NA |   NA |   NA |   NA |   94 |
| male   | hispanic                   | larry               |   NA |   NA |   NA |   NA |   NA |   94 |
| male   | white non hispanic         | yida                |   NA |   NA |   NA |   NA |   NA |   94 |
| male   | white non hispanic         | amrom               |   NA |   NA |   NA |   NA |   NA |   94 |
| male   | white non hispanic         | kacper              |   NA |   NA |   NA |   NA |   NA |   96 |
| male   | white non hispanic         | carmine             |   NA |   NA |   NA |   NA |   NA |   96 |
| male   | white non hispanic         | allen               |   NA |   NA |   NA |   NA |   NA |   96 |
| male   | white non hispanic         | shaul               |   NA |   NA |   NA |   NA |   NA |   97 |
| male   | white non hispanic         | yonah               |   NA |   NA |   NA |   NA |   NA |   97 |
| male   | white non hispanic         | sidney              |   NA |   NA |   NA |   NA |   NA |   97 |
| male   | white non hispanic         | yehudah             |   NA |   NA |   NA |   NA |   NA |   97 |
| male   | white non hispanic         | kenneth             |   NA |   NA |   NA |   NA |   NA |   97 |
| male   | white non hispanic         | caden               |   NA |   NA |   NA |   NA |   NA |   97 |
