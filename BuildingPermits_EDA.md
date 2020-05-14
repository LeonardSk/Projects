US Building Permit data - EDA
================
Leonard Seok
4 May 2020

``` r
# setwd("C:\\Users\\Leonard\\Desktop\\Datascience for R\\Projects\\EDA_1")
library(tidyverse); library(gridExtra); library(ggplot2); library(reshape); library(caret)
```

    ## -- Attaching packages ------------------------------------------------------------------------------------------------------- tidyverse 1.2.1 --

    ## v ggplot2 3.0.0     v purrr   0.2.5
    ## v tibble  1.4.2     v dplyr   0.7.6
    ## v tidyr   0.8.1     v stringr 1.3.1
    ## v readr   1.1.1     v forcats 0.3.0

    ## -- Conflicts ---------------------------------------------------------------------------------------------------------- tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

    ## 
    ## Attaching package: 'gridExtra'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     combine

    ## 
    ## Attaching package: 'reshape'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     rename

    ## The following objects are masked from 'package:tidyr':
    ## 
    ##     expand, smiths

    ## Loading required package: lattice

    ## 
    ## Attaching package: 'caret'

    ## The following object is masked from 'package:purrr':
    ## 
    ##     lift

Import the data
---------------

Source : <https://www.recenter.tamu.edu/data/building-permits/>

``` r
perm <- read.csv("dataPermit_full.csv")
```

We can see that NAs are read in as nulls. We can also see that some fields are read in as factors where they really should be numerics (due to the "null" being NAs)

``` r
anyNA(perm)
```

    ## [1] FALSE

``` r
summary(perm$f1change)
```

    ##       0    null    -100     -50     100   -33.3      50     -25    33.3 
    ##    5687    4574    1228    1173    1098     874     831     708     629 
    ##      25   -66.7     -20      20   -16.7     -40     200    66.7    16.7 
    ##     518     500     481     423     394     389     389     367     355 
    ##   -14.3   -28.6   -12.5    14.3      40   -11.1     -60    12.5     150 
    ##     344     324     299     299     299     275     271     270     266 
    ##   -42.9    11.1     -75   -37.5      75    -9.1     -10    28.6      10 
    ##     253     245     244     238     231     226     224     223     222 
    ##      60   -22.2    -8.3     300   -18.2    -7.7     9.1    42.9    18.2 
    ##     213     207     207     204     202     192     192     191     183 
    ##   -44.4    22.2     8.3      30   -15.4     -30    -7.1   -27.3   -13.3 
    ##     180     178     178     177     176     175     175     172     170 
    ##    37.5   -23.1    15.4    -6.7     7.1     7.7     6.7   -18.8   -55.6 
    ##     169     164     162     161     159     158     157     156     156 
    ##   -57.1    27.3    -6.2    18.8     5.6     6.2      80   -36.4    23.1 
    ##     154     153     151     151     150     149     148     147     146 
    ##     -80     5.3    13.3    -5.6    -5.9   -31.2   133.3   -11.8   -21.4 
    ##     144     144     141     140     140     139     138     134     134 
    ##   -21.1    -4.5    44.4      -5    11.8     5.9   -38.5    21.4   -26.7 
    ##     132     132     132     131     131     129     128     128     127 
    ##    -5.3   -13.6   -41.7   -27.8   -17.6   -30.8   -45.5   -35.7   -15.8 
    ##     127     126     126     124     123     123     122     121     120 
    ## (Other) 
    ##   64810

``` r
str(perm)
```

    ## 'data.frame':    99880 obs. of  14 variables:
    ##  $ area        : Factor w/ 381 levels "Abilene, TX",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ date        : Factor w/ 411 levels "01/1980","01/1981",..: 1 36 71 106 140 174 208 242 276 310 ...
    ##  $ f1units     : int  24 39 38 29 29 42 48 67 53 80 ...
    ##  $ f1change    : Factor w/ 3027 levels "-0.1","-0.2",..: 3027 3027 3027 3027 3027 3027 3027 3027 3027 3027 ...
    ##  $ f1value     : int  67900 75900 78000 66500 77600 66500 67600 69000 60800 73000 ...
    ##  $ f1valchange : Factor w/ 2690 levels "-0.1","-0.2",..: 2690 2690 2690 2690 2690 2690 2690 2690 2690 2690 ...
    ##  $ f24units    : int  4 0 4 0 0 0 18 0 2 2 ...
    ##  $ f24change   : Factor w/ 2482 levels "-0.2","-0.3",..: 2482 2482 2482 2482 2482 2482 2482 2482 2482 2482 ...
    ##  $ f24value    : int  46200 0 37000 0 0 0 24400 0 31200 23800 ...
    ##  $ f24valchange: Factor w/ 3069 levels "-0.1","-0.2",..: 3069 3069 3069 3069 3069 3069 3069 3069 3069 3069 ...
    ##  $ f5units     : int  200 0 0 0 0 0 0 0 0 152 ...
    ##  $ f5change    : Factor w/ 5196 levels "-0.2","-0.3",..: 5196 5196 5196 5196 5196 5196 5196 5196 5196 5196 ...
    ##  $ f5value     : int  12800 0 0 0 0 0 0 0 0 22700 ...
    ##  $ f5valchange : Factor w/ 3273 levels "-0.1","-0.2",..: 3273 3273 3273 3273 3273 3273 3273 3273 3273 3273 ...

Fix the problematic nulls issue with an additional paramater na = "null"

``` r
perm <- read.csv("dataPermit_full.csv", na = "null")
str(perm)
```

    ## 'data.frame':    99880 obs. of  14 variables:
    ##  $ area        : Factor w/ 381 levels "Abilene, TX",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ date        : Factor w/ 411 levels "01/1980","01/1981",..: 1 36 71 106 140 174 208 242 276 310 ...
    ##  $ f1units     : int  24 39 38 29 29 42 48 67 53 80 ...
    ##  $ f1change    : num  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ f1value     : int  67900 75900 78000 66500 77600 66500 67600 69000 60800 73000 ...
    ##  $ f1valchange : num  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ f24units    : int  4 0 4 0 0 0 18 0 2 2 ...
    ##  $ f24change   : num  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ f24value    : int  46200 0 37000 0 0 0 24400 0 31200 23800 ...
    ##  $ f24valchange: num  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ f5units     : int  200 0 0 0 0 0 0 0 0 152 ...
    ##  $ f5change    : num  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ f5value     : int  12800 0 0 0 0 0 0 0 0 22700 ...
    ##  $ f5valchange : num  NA NA NA NA NA NA NA NA NA NA ...

-   Area = Metropolitan Standard Area
-   Date = Month/Year (Factor)
-   f1 = Single Family House
-   f24 = 2 - 4 Families House
-   f5 = 5+ Families House
-   units = Count of buildings
-   value = Average Building value

Lets Check how far back this data goes.

``` r
perm.1 <- perm %>% 
  mutate(year = as.numeric(str_sub(date,-4,-1))
         , month = as.numeric(str_sub(date,1,2))
         , yyyymm = year * 100 + month
         , yyyymm_G = year + (month-1)/12
         )

(year.dist <- ggplot(perm.1, aes(year)) +
  geom_histogram(binwidth = 1, color = 'black', fill = 'cyan') +
    ggtitle("Data points Count by Year")
)
```

![](BuildingPermits_EDA_files/figure-markdown_github/unnamed-chunk-4-1.png)

We can notice more consistency in volumes in the past 10 years, so lets cut the data.

``` r
perm.2 <- perm.1 %>% 
  filter(year >= 2000)
```

General EDA
-----------

Counts by Year

``` r
perm.2 %>% count(year) %>% arrange(desc(year))
```

    ## # A tibble: 21 x 2
    ##     year     n
    ##    <dbl> <int>
    ##  1  2020  1095
    ##  2  2019  4380
    ##  3  2018  4380
    ##  4  2017  4380
    ##  5  2016  4380
    ##  6  2015  4380
    ##  7  2014  4572
    ##  8  2013  4560
    ##  9  2012  4560
    ## 10  2011  4560
    ## # ... with 11 more rows

``` r
ggplot(perm.2, aes(year)) +
  geom_histogram(binwidth = 1, color='black', fill='cyan')
```

![](BuildingPermits_EDA_files/figure-markdown_github/unnamed-chunk-6-1.png)

Counts by Area

``` r
perm.2 %>% count(area) 
```

    ## # A tibble: 381 x 2
    ##    area                                  n
    ##    <fct>                             <int>
    ##  1 Abilene, TX                         243
    ##  2 Akron, OH                           243
    ##  3 Albany-Schenectady-Troy, NY         243
    ##  4 Albany, GA                          243
    ##  5 Albany, OR                          243
    ##  6 Albuquerque, NM                     243
    ##  7 Alexandria, LA                      243
    ##  8 Allentown-Bethlehem-Easton, PA-NJ   243
    ##  9 Altoona, PA                         243
    ## 10 Amarillo, TX                        243
    ## # ... with 371 more rows

Alot of areas seem to have the same counts, lets check with a double count

``` r
perm.2 %>% count(area) %>% count(n)
```

    ## # A tibble: 4 x 2
    ##       n    nn
    ##   <int> <int>
    ## 1   135     1
    ## 2   180    16
    ## 3   196     1
    ## 4   243   363

We can see that most areas have 243 records.

Now lets look at yearly figures across the different building profiles

``` r
counts.by.year <- perm.2 %>% group_by(year) %>% 
  summarise(f1.units = sum(f1units)
            , f24.units = sum(f24units)
            , f5.units = sum(f5units)) %>% 
  gather(key = "unit.type", value = "Counts", -year) %>% 
  ggplot(aes(x=year, y=Counts, fill = unit.type)) +
  geom_bar(aes(group = unit.type), color = 'black', stat = "Identity", position = "dodge") +
  theme_bw() +
  ggtitle("Counts by Building Unit Type Across Years")

Amounts.by.year <- perm.2 %>% 
  group_by(year) %>% 
  summarise(f1.value = sum(as.numeric(f1value)*as.numeric(f1units))/sum(as.numeric(f1units)) # as.numerics is required as INT types can overflow if too large
            , f24.value = sum(as.numeric(f24value)*as.numeric(f24units))/sum(as.numeric(f24units))
            , f5.value = sum(as.numeric(f5value)*as.numeric(f5units))/sum(as.numeric(f5units)) ) %>% 
  gather(key = "unit.type", value = "Amounts", -year) %>% 
  ggplot(aes(x=year, y=Amounts, color = unit.type)) +
  geom_line(aes(group = unit.type)) +
  theme_bw() +
  ggtitle("Weighted Average $ Amount by Building Unit Type Across Years")

grid.arrange(counts.by.year, Amounts.by.year, nrow =2)
```

![](BuildingPermits_EDA_files/figure-markdown_github/unnamed-chunk-9-1.png)

Single Family Building Units
----------------------------

Plot the number of single family units over time overall. We can see a noticable decrease starting 2007, which is when the GFC begun.

``` r
perm.2 %>% group_by(yyyymm_G) %>% 
  summarise(f1_tot = sum(f1units)) %>% 
  ggplot(aes(x=yyyymm_G,y=f1_tot)) +
  geom_line() +
  ggtitle("Number of F1 units Overall") + theme_bw()
```

![](BuildingPermits_EDA_files/figure-markdown_github/unnamed-chunk-10-1.png)

The effects of 2007 - 2009 GFC can be clearly seen here with the dips in that period.

Plot the number of single family units over time, this time by Area

``` r
ggplot(perm.2, aes(x=yyyymm_G, y=f1units)) + 
  geom_line(aes(group=area)) +
  ggtitle("Number of F1 Units by Area") +
  xlab("Period") +
  theme_bw()
```

![](BuildingPermits_EDA_files/figure-markdown_github/unnamed-chunk-11-1.png)

This looks bit messy... so lets focus on the areas with larger volumes of data.

``` r
# Lets take the top 25% areas by average monthly units 
length(unique(perm.2$area)) # we have 381 cities in total
```

    ## [1] 381

``` r
top.25 <- round(length(unique(perm.2$area)) * 0.25)

f1.by.area <- perm.2 %>% 
  group_by(area) %>% 
  summarise(average.unit.count = mean(f1units)) %>% 
  arrange(desc(average.unit.count)) %>% 
  head(top.25)

perm.large <- perm.2 %>% filter(area %in% f1.by.area$area)

ggplot(perm.large, aes(x=yyyymm_G, y=f1units)) + geom_line(aes(group = area)) + 
  scale_y_log10() + # a log transform can help when there is large scale discrepancies within the data
  geom_smooth() +
  ggtitle("F1 Units by all Areas") +
  xlab("Period")
```

    ## `geom_smooth()` using method = 'gam' and formula 'y ~ s(x, bs = "cs")'

![](BuildingPermits_EDA_files/figure-markdown_github/unnamed-chunk-12-1.png)

With the Log scale, we can see some seasonal patterns within a year here, and also a long term increasing trend post the GFC period.

Model to partition the seasonality pattern
------------------------------------------

Lets pick an area to build a model on - say we pick the one with largest average unit counts

``` r
area.name <- f1.by.area %>% filter(average.unit.count == max(average.unit.count)) %>% select(area)

area.1 <- perm.2 %>% filter(area == area.name$area) 

ggplot(area.1, aes(x=yyyymm_G, y = f1units)) + geom_line() + ggtitle("F1 Units for the Area with Largest Average Units (Houston)")
```

![](BuildingPermits_EDA_files/figure-markdown_github/unnamed-chunk-13-1.png)

``` r
# Highlight the 2019 and 2020 years in red (the later years)
ggplot(area.1, aes(x=factor(month), y = f1units)) + geom_line(aes(group = year, color = year)) + scale_color_gradient(low = "#56B1F7", high = "#132B43") +
  geom_line(data = area.1 %>% filter(year == 2020), color = 'red') +
  geom_line(data = area.1 %>% filter(year == 2019), color = 'red', linetype = 'longdash') +
  scale_y_log10() +
  ggtitle("Seasonality within a year (F1 in Houstons)") +
  xlab("Month") +
  theme_bw()
```

    ## geom_path: Each group consists of only one observation. Do you need to
    ## adjust the group aesthetic?
    ## geom_path: Each group consists of only one observation. Do you need to
    ## adjust the group aesthetic?

![](BuildingPermits_EDA_files/figure-markdown_github/unnamed-chunk-13-2.png)

We can see some trends here - some points to note:

-   Building permits activities start falling in Aug/Sep, and hits lowest in December. This could be due to the colder weather in these periods that cause a lower demands in general?
-   Permit activities peak around May/June - which is when its getting warm/hot
-   Is this trend the same for all areas other than Houston?
-   Is this trend same across all building unit types?

Seasonality model for Houston (for years 2010 onwards)
------------------------------------------------------

Lets build a simple linear model to quantify this signal just for Houston. Lets work on data 2010 onwards, since including GFC periods would simply cause more noise in our predictions.

``` r
area.2 <- area.1 %>% filter(year>=2010)

set.seed(13)
sig.mod.hous <- train(log(f1units) ~ factor(month), data = area.2, method = "lm")

area.2$f1unit.pred <- predict(sig.mod.hous, newdata = area.2)

area.2 %>% mutate(log.f1units = log(f1units)) %>% 
  select(yyyymm_G, log.f1units, f1unit.pred) %>% 
  gather(key = 'type', value = 'value', -yyyymm_G) %>% 
  ggplot(aes(x=yyyymm_G, y=value, color = type, linetype = type)) +
  geom_line(aes(group = type)) +
  scale_linetype_manual(labels = c("Pred", "Actual") ,values=c("longdash","solid")) +
  scale_color_manual(labels = c("Pred", "Actual") ,values = c("red", "black")) +
  ggtitle('Prediction VS Actual - Houston post 2010') + theme_bw()
```

![](BuildingPermits_EDA_files/figure-markdown_github/unnamed-chunk-14-1.png)

We can again see the strong monthly pattern since what we predict in our model is close to actuals.

Lets have a look at the residual plot. We would like to know what is the underlying trend IF we were to ignore the monthly pattern (basically the remaining signal that is not captured by the model.)

``` r
area.2 %>% mutate(log.f1units = log(f1units)) %>% 
  select(yyyymm_G, log.f1units, f1unit.pred) %>% 
  mutate(residuals = log.f1units - f1unit.pred) %>% 
  ggplot(aes(x=yyyymm_G, y=residuals)) +
  geom_line() +
  ggtitle('Residual Plot for F1 units Houston')
```

![](BuildingPermits_EDA_files/figure-markdown_github/unnamed-chunk-15-1.png)

Stripping out the monthly pattern, we can see the longer term trends more clearly.

Seasonality Model for all large areas (for years 2010 onwards)
--------------------------------------------------------------

``` r
# We split the dataset into individual datasets by area
nested.area <- perm.large %>% filter(year >= 2010) %>% group_by(area) %>% nest()

# Create a function for the model, this is so we can apply it to the nested datasets for each area
area.model <- function(df, res, pred){
  f <- as.formula(paste(res, paste(pred, collapse = '+'), sep = '~'))
  return(lm(f, data = df))
}

# For example , we run the model on just the Houston Area
model.Hous <- area.model(area.2, res="log(f1units+1)", pred="factor(month)")

# Now lets apply it to the whole nested sets using map - we can see more examples of nested models here https://drsimonj.svbtle.com/running-a-model-on-separate-groups
area.model.all <- nested.area %>% 
  mutate(
          # First step is to map the model to each nested set
          model = map(.x=data, .f=~area.model(df=.x,res="log(f1units+1)", pred="factor(month)"))
          # then we can manually obtain the predicted and the residuals
         , preds = map2(.x=model, .y=data, .f=~predict(.x, .y))
         , resi = map2(.x=data, .y=preds, .f=~log(.x$f1units) - .y)
         # another way of getting the predicted and residuals using what is stored in the model class
         , preds.check = map(.x=model, .f="fitted.values")
         , resi.check = map(.x=model, .f="residuals")
        )

perm.large.res <- area.model.all %>% unnest(data, preds,resi, preds.check, resi.check) 

# lets check the plots for Houston with the individual Houston model what we have above
perm.large.res %>% filter(grepl("Houston",area)) %>% 
  mutate(log.f1units = log(f1units)) %>% 
  select(yyyymm_G, log.f1units, preds) %>% 
  gather(key='type', value='value', -yyyymm_G) %>% 
  ggplot(aes(x=yyyymm_G, y=value, color=type)) +
  geom_line() +
  scale_color_manual(values = c("black", "red"))
```

![](BuildingPermits_EDA_files/figure-markdown_github/unnamed-chunk-16-1.png)

``` r
# This checks out to be the same as the above chart
```

Now lets plot the residuals for all the top 25% largest areas. By detrending the data, we can observe the long term trend for the top 25% largest areas for f1units.

``` r
perm.large.res %>% ggplot(aes(x=yyyymm_G, y=resi)) +
  geom_line(aes(group=area), alpha=1/10) +
  geom_smooth(se=F) +
  ggtitle("Detrended curve for top 25% of areas (f1units)") +
  ylab("Residuals") +theme_bw()
```

    ## `geom_smooth()` using method = 'gam' and formula 'y ~ s(x, bs = "cs")'

![](BuildingPermits_EDA_files/figure-markdown_github/unnamed-chunk-17-1.png)
