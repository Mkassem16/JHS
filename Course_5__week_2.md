Loading libraries and Data
--------------------------

``` r
library(ggplot2)
library(lubridate)
library(dplyr)
df <- read.csv("/Users/ayman/Downloads/R/activity.csv", header = TRUE, stringsAsFactors = FALSE)
```

Cleaning data
-------------

``` r
df$date <- as.Date(as.character(df$date), "%Y-%m-%d")
```

Data overview
-------------

``` r
glimpse(df)
```

    ## Observations: 17,568
    ## Variables: 3
    ## $ steps    <int> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
    ## $ date     <date> 2012-10-01, 2012-10-01, 2012-10-01, 2012-10-01, 2012-1…
    ## $ interval <int> 0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 100, 105,…

``` r
head(df)
```

    ##   steps       date interval
    ## 1    NA 2012-10-01        0
    ## 2    NA 2012-10-01        5
    ## 3    NA 2012-10-01       10
    ## 4    NA 2012-10-01       15
    ## 5    NA 2012-10-01       20
    ## 6    NA 2012-10-01       25

Total number of steps taken per day histogram
---------------------------------------------

``` r
hist(df$steps, main = "Histogram including 0 and NA values")
```

![](Course_5__week_2_files/figure-markdown_github/Q2-1.png)

Mean and Median steps for each day
----------------------------------

``` r
df2 <- df %>% group_by(date) %>% summarize(mean_day = mean(steps, na.rm = TRUE), median_day = median(steps[!steps == 0], na.rm = TRUE))
head(df2)
```

    ## # A tibble: 6 x 3
    ##   date       mean_day median_day
    ##   <date>        <dbl>      <dbl>
    ## 1 2012-10-01  NaN           NA  
    ## 2 2012-10-02    0.438       63  
    ## 3 2012-10-03   39.4         61  
    ## 4 2012-10-04   42.1         56.5
    ## 5 2012-10-05   46.2         66  
    ## 6 2012-10-06   53.5         67

``` r
df2n <- na.omit(df2) # removing NaN 
head(df2n, 10)
```

    ## # A tibble: 10 x 3
    ##    date       mean_day median_day
    ##    <date>        <dbl>      <dbl>
    ##  1 2012-10-02    0.438       63  
    ##  2 2012-10-03   39.4         61  
    ##  3 2012-10-04   42.1         56.5
    ##  4 2012-10-05   46.2         66  
    ##  5 2012-10-06   53.5         67  
    ##  6 2012-10-07   38.2         52.5
    ##  7 2012-10-09   44.5         48  
    ##  8 2012-10-10   34.4         56.5
    ##  9 2012-10-11   35.8         35  
    ## 10 2012-10-12   60.4         46

Time series plot of the average number of steps taken
-----------------------------------------------------

``` r
plot(df2$date, df2$mean_day, data = df2, pch = 20, main = "AVG steps per day time series")
```

![](Course_5__week_2_files/figure-markdown_github/Q4-1.png)

The 5-minute interval that, on average, contains the maximum number of steps
----------------------------------------------------------------------------

``` r
df3 <- df %>% group_by(interval) %>% summarize(avg_steps_interval = mean(steps, na.rm = TRUE)) %>% arrange(desc(avg_steps_interval))
df3[1,]
```

    ## # A tibble: 1 x 2
    ##   interval avg_steps_interval
    ##      <int>              <dbl>
    ## 1      835               206.

Code to describe and show a strategy for imputing missing data
--------------------------------------------------------------

*Missing data is 13% of the total data Therfore I am going to omit them*

``` r
mean(is.na(df$steps))
```

    ## [1] 0.1311475

Histogram of the total number of steps taken each day after missing values are imputed
--------------------------------------------------------------------------------------

``` r
df1 <- df %>% group_by(date) %>% summarise(steps_day = sum(steps, na.rm = TRUE))
hist(df1$steps_day, main = "Steps per day frequency")
```

![](Course_5__week_2_files/figure-markdown_github/Q7-1.png)

Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends
---------------------------------------------------------------------------------------------------------

``` r
df4 <- df %>% group_by(week_day = wday(date, label = TRUE)) %>% summarize(mean_day = mean(steps, na.rm = TRUE))
df5 <- df4 %>% mutate(wdaylbl = case_when(week_day %in% c("Mon", "Tue", "Wed", "Thu", "Fri") ~ "Weekday",
                                          week_day %in% c("Sat", "Sun") ~ "Weekend")) %>%
        group_by(wdaylbl) %>% summarize(mean(mean_day))


as.data.frame(df5)
```

    ##   wdaylbl mean(mean_day)
    ## 1 Weekday       35.61641
    ## 2 Weekend       43.07837

``` r
barplot(df5$`mean(mean_day)`, names.arg = c("Weekdays", "weekends"), ylab = "AVG steps", main = " AVG steps / 5mins intervals ")
```

![](Course_5__week_2_files/figure-markdown_github/Q8-1.png)
