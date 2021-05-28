Kira\_Plastinina
================
Harrieth Rotich
5/27/2021

## Kira Plastinina Analysis

### Problem Definition

#### Questions

1.  Perform clustering stating insights drawn from your analysis and
    visualizations.
2.  Upon implementation, provide comparisons between the approaches
    learned this week i.e. K-Means clustering vs Hierarchical clustering
    highlighting the strengths and limitations of each approach in the
    context of your analysis.

#### Context

Kira Plastinina is a Russian brand that is sold through a defunct chain
of retail stores in Russia, Ukraine, Kazakhstan, Belarus, China,
Philippines, and Armenia. The brand’s Sales and Marketing team would
like to understand their customer’s behavior from data that they have
collected over the past year. More specifically, they would like to
learn the characteristics of customer groups.

#### Experimental Design Taken

1.  Problem Definition
2.  Data Sourcing
3.  Check the Data
4.  Perform Data Cleaning
5.  Perform Exploratory Data Analysis (Univariate, Bivariate &
    Multivariate)
6.  Implement the Solution
7.  Challenge the Solution
8.  Follow up Questions

### Data Sourcing

``` r
# loading necessary dependencies. 
library(data.table)
library(tidyverse)
```

    ## -- Attaching packages ---------------------------------------------------------- tidyverse 1.3.0 --

    ## v ggplot2 3.3.2     v purrr   0.3.4
    ## v tibble  3.0.2     v dplyr   1.0.0
    ## v tidyr   1.1.0     v stringr 1.4.0
    ## v readr   1.3.1     v forcats 0.5.0

    ## -- Conflicts ------------------------------------------------------------- tidyverse_conflicts() --
    ## x dplyr::between()   masks data.table::between()
    ## x dplyr::filter()    masks stats::filter()
    ## x dplyr::first()     masks data.table::first()
    ## x dplyr::lag()       masks stats::lag()
    ## x dplyr::last()      masks data.table::last()
    ## x purrr::transpose() masks data.table::transpose()

``` r
library(dplyr)
library(corrplot)
```

    ## corrplot 0.84 loaded

``` r
library(ggplot2)
library(dbscan)
library(factoextra)
```

    ## Welcome! Want to learn more? See two factoextra-related books at https://goo.gl/ve3WBa

``` r
library(purrr)
```

``` r
kira <- fread("http://bit.ly/EcommerceCustomersDataset")
```

### Checking the data

``` r
# Preview of the data
head(kira) 
```

    ##    Administrative Administrative_Duration Informational Informational_Duration
    ## 1:              0                       0             0                      0
    ## 2:              0                       0             0                      0
    ## 3:              0                      -1             0                     -1
    ## 4:              0                       0             0                      0
    ## 5:              0                       0             0                      0
    ## 6:              0                       0             0                      0
    ##    ProductRelated ProductRelated_Duration BounceRates ExitRates PageValues
    ## 1:              1                0.000000  0.20000000 0.2000000          0
    ## 2:              2               64.000000  0.00000000 0.1000000          0
    ## 3:              1               -1.000000  0.20000000 0.2000000          0
    ## 4:              2                2.666667  0.05000000 0.1400000          0
    ## 5:             10              627.500000  0.02000000 0.0500000          0
    ## 6:             19              154.216667  0.01578947 0.0245614          0
    ##    SpecialDay Month OperatingSystems Browser Region TrafficType
    ## 1:          0   Feb                1       1      1           1
    ## 2:          0   Feb                2       2      1           2
    ## 3:          0   Feb                4       1      9           3
    ## 4:          0   Feb                3       2      2           4
    ## 5:          0   Feb                3       3      1           4
    ## 6:          0   Feb                2       2      1           3
    ##          VisitorType Weekend Revenue
    ## 1: Returning_Visitor   FALSE   FALSE
    ## 2: Returning_Visitor   FALSE   FALSE
    ## 3: Returning_Visitor   FALSE   FALSE
    ## 4: Returning_Visitor   FALSE   FALSE
    ## 5: Returning_Visitor    TRUE   FALSE
    ## 6: Returning_Visitor   FALSE   FALSE

``` r
# Basic structure of the data
str(kira) 
```

    ## Classes 'data.table' and 'data.frame':   12330 obs. of  18 variables:
    ##  $ Administrative         : int  0 0 0 0 0 0 0 1 0 0 ...
    ##  $ Administrative_Duration: num  0 0 -1 0 0 0 -1 -1 0 0 ...
    ##  $ Informational          : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ Informational_Duration : num  0 0 -1 0 0 0 -1 -1 0 0 ...
    ##  $ ProductRelated         : int  1 2 1 2 10 19 1 1 2 3 ...
    ##  $ ProductRelated_Duration: num  0 64 -1 2.67 627.5 ...
    ##  $ BounceRates            : num  0.2 0 0.2 0.05 0.02 ...
    ##  $ ExitRates              : num  0.2 0.1 0.2 0.14 0.05 ...
    ##  $ PageValues             : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ SpecialDay             : num  0 0 0 0 0 0 0.4 0 0.8 0.4 ...
    ##  $ Month                  : chr  "Feb" "Feb" "Feb" "Feb" ...
    ##  $ OperatingSystems       : int  1 2 4 3 3 2 2 1 2 2 ...
    ##  $ Browser                : int  1 2 1 2 3 2 4 2 2 4 ...
    ##  $ Region                 : int  1 1 9 2 1 1 3 1 2 1 ...
    ##  $ TrafficType            : int  1 2 3 4 4 3 3 5 3 2 ...
    ##  $ VisitorType            : chr  "Returning_Visitor" "Returning_Visitor" "Returning_Visitor" "Returning_Visitor" ...
    ##  $ Weekend                : logi  FALSE FALSE FALSE FALSE TRUE FALSE ...
    ##  $ Revenue                : logi  FALSE FALSE FALSE FALSE FALSE FALSE ...
    ##  - attr(*, ".internal.selfref")=<externalptr>

### Data Cleaning

``` r
# Checking for duplicates in the data.
length(which(duplicated(kira)))
```

    ## [1] 119

``` r
# Removing duplicates 
kira <- kira[!duplicated(kira), ]
```

``` r
# Checking for null values.
length(which(is.na(kira)))
```

    ## [1] 96

``` r
# Null values per column

colSums(is.na(kira))
```

    ##          Administrative Administrative_Duration           Informational 
    ##                      12                      12                      12 
    ##  Informational_Duration          ProductRelated ProductRelated_Duration 
    ##                      12                      12                      12 
    ##             BounceRates               ExitRates              PageValues 
    ##                      12                      12                       0 
    ##              SpecialDay                   Month        OperatingSystems 
    ##                       0                       0                       0 
    ##                 Browser                  Region             TrafficType 
    ##                       0                       0                       0 
    ##             VisitorType                 Weekend                 Revenue 
    ##                       0                       0                       0

Missing values are the same number for 8 different columns, this could
mean that they might be the same rows. Hence the omission.

``` r
# Omitting null values
kira <- na.omit(kira)
```

``` r
# Outlier test
z <- kira[, c(1 ,2, 3, 4, 5, 6, 7, 8, 9, 10)]
boxplot(z, main="Outliers")
```

![](week_13_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

The columns with anything duration have outliers, the one having the
most being ‘ProductRelated\_Duration’

``` r
# Capping 
# ProductRelated_Duration column
x <- kira$ProductRelated_Duration
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] <- caps[1]
x[x > (qnt[2] + H)] <- caps[2] 


# Informational_Duration
w <- kira$Informational_Duration
qnt <- quantile(w, probs=c(.25, .75), na.rm = T)
caps <- quantile(w, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(w, na.rm = T)
w[w < (qnt[1] - H)] <- caps[1]
w[w > (qnt[2] + H)] <- caps[2]

#Administrative_Duration
v <- kira$Administrative_Duration
qnt <- quantile(v, probs=c(.25, .75), na.rm = T)
caps <- quantile(v, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(v, na.rm = T)
v[v < (qnt[1] - H)] <- caps[1]
v[v > (qnt[2] + H)] <- caps[2]
```

``` r
# Translating the capped values to the column
kira$ProductRelated_Duration <- x
boxplot(kira$ProductRelated_Duration, main="Capped Outliers")
```

![](week_13_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

``` r
# Translating the capped values to the column
kira$Informational_Duration <- w
boxplot(kira$Informational_Duration, main="Capped Outliers")
```

![](week_13_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

``` r
# Translating the capped values to the column 
kira$Administrative_Duration <- v
boxplot(kira$Administrative_Duration, main="Capped Outliers")
```

![](week_13_files/figure-gfm/unnamed-chunk-14-1.png)<!-- --> The
outliers have been appropriately dealt with.

### Exploratory Data Analysis

#### Univariate Analysis

``` r
# Dataset Information

summary(kira)
```

    ##  Administrative  Administrative_Duration Informational    
    ##  Min.   : 0.00   Min.   : -1.00          Min.   : 0.0000  
    ##  1st Qu.: 0.00   1st Qu.:  0.00          1st Qu.: 0.0000  
    ##  Median : 1.00   Median :  9.00          Median : 0.0000  
    ##  Mean   : 2.34   Mean   : 68.78          Mean   : 0.5088  
    ##  3rd Qu.: 4.00   3rd Qu.: 94.75          3rd Qu.: 0.0000  
    ##  Max.   :27.00   Max.   :352.23          Max.   :24.0000  
    ##  Informational_Duration ProductRelated   ProductRelated_Duration
    ##  Min.   :  0.00         Min.   :  0.00   Min.   :  -1.0         
    ##  1st Qu.:  0.00         1st Qu.:  8.00   1st Qu.: 193.6         
    ##  Median :  0.00         Median : 18.00   Median : 609.5         
    ##  Mean   : 39.22         Mean   : 32.06   Mean   :1072.7         
    ##  3rd Qu.:  0.00         3rd Qu.: 38.00   3rd Qu.:1477.6         
    ##  Max.   :199.00         Max.   :705.00   Max.   :4313.5         
    ##   BounceRates        ExitRates         PageValues        SpecialDay     
    ##  Min.   :0.00000   Min.   :0.00000   Min.   :  0.000   Min.   :0.00000  
    ##  1st Qu.:0.00000   1st Qu.:0.01422   1st Qu.:  0.000   1st Qu.:0.00000  
    ##  Median :0.00293   Median :0.02500   Median :  0.000   Median :0.00000  
    ##  Mean   :0.02045   Mean   :0.04150   Mean   :  5.952   Mean   :0.06197  
    ##  3rd Qu.:0.01667   3rd Qu.:0.04848   3rd Qu.:  0.000   3rd Qu.:0.00000  
    ##  Max.   :0.20000   Max.   :0.20000   Max.   :361.764   Max.   :1.00000  
    ##     Month           OperatingSystems    Browser           Region     
    ##  Length:12199       Min.   :1.000    Min.   : 1.000   Min.   :1.000  
    ##  Class :character   1st Qu.:2.000    1st Qu.: 2.000   1st Qu.:1.000  
    ##  Mode  :character   Median :2.000    Median : 2.000   Median :3.000  
    ##                     Mean   :2.124    Mean   : 2.358   Mean   :3.153  
    ##                     3rd Qu.:3.000    3rd Qu.: 2.000   3rd Qu.:4.000  
    ##                     Max.   :8.000    Max.   :13.000   Max.   :9.000  
    ##   TrafficType     VisitorType         Weekend         Revenue       
    ##  Min.   : 1.000   Length:12199       Mode :logical   Mode :logical  
    ##  1st Qu.: 2.000   Class :character   FALSE:9343      FALSE:10291    
    ##  Median : 2.000   Mode  :character   TRUE :2856      TRUE :1908     
    ##  Mean   : 4.075                                                     
    ##  3rd Qu.: 4.000                                                     
    ##  Max.   :20.000

``` r
# Unique values in column Visitor Type
unique(kira$VisitorType)
```

    ## [1] "Returning_Visitor" "New_Visitor"       "Other"

``` r
# Unique values in column OperatingSystems
unique(kira$OperatingSystems)
```

    ## [1] 1 2 4 3 7 6 8 5

``` r
# Unique values in column Browser
unique(kira$Browser)
```

    ##  [1]  1  2  3  4  5  6  7 10  8  9 12 13 11

``` r
# Unique values in column Traffic Type
unique(kira$TrafficType)
```

    ##  [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 18 19 16 17 20

``` r
# Unique values in column Special Day
unique(kira$SpecialDay)
```

    ## [1] 0.0 0.4 0.8 1.0 0.2 0.6

``` r
# Distribution of the visitors
visitor <- kira$VisitorType
vis <- table(visitor)
barplot(vis, main = "Visitor Type Distribution", 
        ylab = "Distribution")
```

![](week_13_files/figure-gfm/unnamed-chunk-21-1.png)<!-- -->

``` r
# Distribution of the visitors
month <- kira$Month
mon <- table(month)
barplot(mon, main = "Month Distribution", 
        ylab = "Distribution")
```

![](week_13_files/figure-gfm/unnamed-chunk-22-1.png)<!-- -->

``` r
# Distribution of the visitors
Region <- kira$Region
reg <- table(Region)
barplot(reg, main = "Regions Distribution", 
        ylab = "Distribution", xlab = "Regions")
```

![](week_13_files/figure-gfm/unnamed-chunk-23-1.png)<!-- -->

``` r
# Distribution of the visitors
revenue <- kira$Revenue
rev <- table(revenue)
barplot(rev, main = "Revenue Distribution", 
        ylab = "Distribution")
```

![](week_13_files/figure-gfm/unnamed-chunk-24-1.png)<!-- -->

``` r
count(kira, Revenue, sort = TRUE)
```

    ##    Revenue     n
    ## 1:   FALSE 10291
    ## 2:    TRUE  1908

#### Bivariate Analysis

``` r
# Getting correlation of the continuous variables
res <- cor(kira %>% select(1:10))
# Rounding off the correlations 
round(res, 2) 
```

    ##                         Administrative Administrative_Duration Informational
    ## Administrative                    1.00                    0.76          0.38
    ## Administrative_Duration           0.76                    1.00          0.32
    ## Informational                     0.38                    0.32          1.00
    ## Informational_Duration            0.37                    0.32          0.77
    ## ProductRelated                    0.43                    0.31          0.37
    ## ProductRelated_Duration           0.40                    0.34          0.35
    ## BounceRates                      -0.21                   -0.19         -0.11
    ## ExitRates                        -0.31                   -0.28         -0.16
    ## PageValues                        0.10                    0.09          0.05
    ## SpecialDay                       -0.10                   -0.10         -0.05
    ##                         Informational_Duration ProductRelated
    ## Administrative                            0.37           0.43
    ## Administrative_Duration                   0.32           0.31
    ## Informational                             0.77           0.37
    ## Informational_Duration                    1.00           0.34
    ## ProductRelated                            0.34           1.00
    ## ProductRelated_Duration                   0.37           0.76
    ## BounceRates                              -0.14          -0.19
    ## ExitRates                                -0.20          -0.29
    ## PageValues                                0.06           0.05
    ## SpecialDay                               -0.05          -0.03
    ##                         ProductRelated_Duration BounceRates ExitRates
    ## Administrative                             0.40       -0.21     -0.31
    ## Administrative_Duration                    0.34       -0.19     -0.28
    ## Informational                              0.35       -0.11     -0.16
    ## Informational_Duration                     0.37       -0.14     -0.20
    ## ProductRelated                             0.76       -0.19     -0.29
    ## ProductRelated_Duration                    1.00       -0.24     -0.34
    ## BounceRates                               -0.24        1.00      0.90
    ## ExitRates                                 -0.34        0.90      1.00
    ## PageValues                                 0.09       -0.12     -0.17
    ## SpecialDay                                -0.04        0.09      0.12
    ##                         PageValues SpecialDay
    ## Administrative                0.10      -0.10
    ## Administrative_Duration       0.09      -0.10
    ## Informational                 0.05      -0.05
    ## Informational_Duration        0.06      -0.05
    ## ProductRelated                0.05      -0.03
    ## ProductRelated_Duration       0.09      -0.04
    ## BounceRates                  -0.12       0.09
    ## ExitRates                    -0.17       0.12
    ## PageValues                    1.00      -0.06
    ## SpecialDay                   -0.06       1.00

``` r
#Plotting a correlation matrix plot
corrplot(res, type = "upper", order = "hclust",
         tl.col = "black", tl.srt = 45) 
```

![](week_13_files/figure-gfm/unnamed-chunk-27-1.png)<!-- -->

``` r
# Plotting pair plots
pairs(kira[,1:9], pch = 19, lower.panel = NULL, 
      main="Pair Plots showing the relationships between variables") 
```

![](week_13_files/figure-gfm/unnamed-chunk-28-1.png)<!-- -->

#### Multivariate Analysis

``` r
# Grouping according to Revenue

group <- NA
group[kira$Revenue == TRUE] <- 1
group[kira$Revenue == FALSE] <- 0
```

``` r
# Setting the x and y variables
x <- kira$BounceRates
y <- kira$ExitRates

# Scatter plot
plot(x, y, xlab = " A", ylab = "Time Spent on Site", col = c("red", "purple")[group],
     main="Scatter Plot showing age in relation to time spent on Site")
```

![](week_13_files/figure-gfm/unnamed-chunk-30-1.png)<!-- -->

### Implement the Solution

#### K-Means Clustering

``` r
# Transforming month into factors.
kira[["Month"]] = factor(kira[["Month"]])
kira[["VisitorType"]] = factor(kira[["VisitorType"]])
kira[["Weekend"]] = factor(kira[["Weekend"]])
```

``` r
# Removing the label
kira_p <- kira[, -18]
```

``` r
# Normalization
normalize <- function(x){
  return ((x-min(x)) / (max(x)-min(x)))
}

kira_p <- normalize(kira_p[, c(-11, -16, -17)])
```

``` r
# Testing the optimal number of clusters to be used.
# Determining Optimal clusters (k) Using Elbow method
fviz_nbclust(x = kira_p, FUNcluster= kmeans, method = 'wss' ) 
```

![](week_13_files/figure-gfm/unnamed-chunk-34-1.png)<!-- --> The optimal
number of clusters to be used are 3 but the labels are divided into two,
hence the decision to use 2 as k

``` r
# Fitting model with k as 2 
kms<- kmeans(kira_p, 2) 
```

``` r
# Number of records per cluster
kms$size 
```

    ## [1] 2220 9979

The k-means algorithm has performed pretty well. It has overestimated
the number of True Revenue and reduced the number of false’s in Revenue.
It has misclassified the values by 2%

``` r
# Cluster centre data point
kms$centers 
```

    ##   Administrative Administrative_Duration Informational Informational_Duration
    ## 1   0.0013005735              0.03004718  0.0005348670            0.020842168
    ## 2   0.0006570367              0.01308767  0.0003085202            0.006758201
    ##   ProductRelated ProductRelated_Duration  BounceRates    ExitRates  PageValues
    ## 1    0.021569349               0.7633625 0.0002334214 0.0002367711 0.002009494
    ## 2    0.004568389               0.1344026 0.0002372073 0.0002424264 0.001522892
    ##     SpecialDay OperatingSystems      Browser       Region TrafficType
    ## 1 0.0002433055     0.0007265546 0.0007665418 0.0009378704 0.001095522
    ## 2 0.0002467743     0.0007236276 0.0007809744 0.0009681583 0.001194131

#### Hierarchical Clustering

``` r
# Getting Euclidean distances between points
d <- dist(kira_p, method = "euclidean")
```

``` r
# Hierarchical Clustering
h.clust <- hclust(d, method = "ward.D2" )
```

``` r
# Plotting the obtained dendrograms
plot(h.clust, cex = 0.6, hang = -1) 
```

![](week_13_files/figure-gfm/unnamed-chunk-40-1.png)<!-- -->

``` r
# Cut tree into 2 groups
sub_grp <- cutree(h.clust, k = 2)

# Number of members in each cluster
table(sub_grp)
```

    ## sub_grp
    ##    1    2 
    ## 7745 4454

The Hclust Algorithm has performed very poorly. It has an error of 21%

### Conclusion

K- means clustering performs better than Hierarchical Clustering for
this particular problem.

Hierarchical clustering was easier to implement than K-means was. Number
of clusters did not need to be specified.

K-means was able to scale well with the data provided. From the
dendrogram, hierarchical clustering was done but due to the many
variables, the bottom was too tiny to be seen.
