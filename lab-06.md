Lab 06 - Ugly charts and Simpson’s paradox
================
Hannah Crawley
2.17.25

### Load packages and data

``` r
library(tidyverse) 
library(dsbox)
library(mosaicData) 
```

### Exercise 1

``` r
staff <- read_csv("data/instructional-staff.csv")
```

    ## Rows: 5 Columns: 12
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (1): faculty_type
    ## dbl (11): 1975, 1989, 1993, 1995, 1999, 2001, 2003, 2005, 2007, 2009, 2011
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
staff_long <- staff %>%
  pivot_longer(cols = -faculty_type, names_to = "year") %>%
  mutate(value = as.numeric(value))
```

``` r
staff_long
```

    ## # A tibble: 55 × 3
    ##    faculty_type              year  value
    ##    <chr>                     <chr> <dbl>
    ##  1 Full-Time Tenured Faculty 1975   29  
    ##  2 Full-Time Tenured Faculty 1989   27.6
    ##  3 Full-Time Tenured Faculty 1993   25  
    ##  4 Full-Time Tenured Faculty 1995   24.8
    ##  5 Full-Time Tenured Faculty 1999   21.8
    ##  6 Full-Time Tenured Faculty 2001   20.3
    ##  7 Full-Time Tenured Faculty 2003   19.3
    ##  8 Full-Time Tenured Faculty 2005   17.8
    ##  9 Full-Time Tenured Faculty 2007   17.2
    ## 10 Full-Time Tenured Faculty 2009   16.8
    ## # ℹ 45 more rows

``` r
staff_long %>%
  ggplot(aes(x = year, y = value, group = faculty_type, color = faculty_type)) +
  geom_line() + 
  labs(Title = "Faculty Type by Year", y = "Count", x = "Year", color = "Faculty Type")
```

![](lab-06_files/figure-gfm/unnamed-chunk-4-1.png)<!-- --> \> Include
the line plot you made above in your report and make sure the figure
width is large enough to make it legible. Also fix the title, axis
labels, and legend label.

### Exercise 2

> Suppose the objective of this plot was to show that the proportion of
> part-time faculty have gone up over time compared to other
> instructional staff types. What changes would you propose making to
> this plot to tell this story?

- Can filter for part-time faculty then alter the plot so that the
  y-axis reflects change in percentage of faculty over time and the
  x-axis reflects year

### Exercise 3

> Can you help them make improve it? First, brainstorm how you would
> improve it. Then create the improved visualization and document your
> changes/decisions with bullet points. It’s ok if some of your
> improvements are aspirational, i.e. you don’t know how to implement
> it, but you think it’s a good idea. Implement what you can and leave
> notes identifying the aspirational improvements that could not be
> made. (You don’t need to recreate their plots in order to improve
> them)

Potential Improvements:  
- perhaps a bar graph would help to better visualize the smoothed line
graph - adjusting the scale of the graph so that you can see the
countries that have lower fishery production - pie plots are not
extremely informative, especially those with a hole in the middle
(cannot see the angles which is how one determines sizes of categories)

``` r
fisheries <- read_csv("data/fisheries.csv")
```

    ## Rows: 216 Columns: 4
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (1): country
    ## dbl (3): capture, aquaculture, total
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
fisheries
```

    ## # A tibble: 216 × 4
    ##    country             capture aquaculture  total
    ##    <chr>                 <dbl>       <dbl>  <dbl>
    ##  1 Afghanistan            1000        1200   2200
    ##  2 Albania                7886         950   8836
    ##  3 Algeria               95000        1361  96361
    ##  4 American Samoa         3047          20   3067
    ##  5 Andorra                   0           0      0
    ##  6 Angola               486490         655 487145
    ##  7 Antigua and Barbuda    3000          10   3010
    ##  8 Argentina            755226        3673 758899
    ##  9 Armenia                3758       16381  20139
    ## 10 Aruba                   142           0    142
    ## # ℹ 206 more rows

``` r
fishlong <- fisheries %>%
  pivot_longer(cols = c(aquaculture,capture, total), 
               names_to = "Type",
               values_to = "Counts")
```

``` r
fishlong
```

    ## # A tibble: 648 × 3
    ##    country        Type        Counts
    ##    <chr>          <chr>        <dbl>
    ##  1 Afghanistan    aquaculture   1200
    ##  2 Afghanistan    capture       1000
    ##  3 Afghanistan    total         2200
    ##  4 Albania        aquaculture    950
    ##  5 Albania        capture       7886
    ##  6 Albania        total         8836
    ##  7 Algeria        aquaculture   1361
    ##  8 Algeria        capture      95000
    ##  9 Algeria        total        96361
    ## 10 American Samoa aquaculture     20
    ## # ℹ 638 more rows
