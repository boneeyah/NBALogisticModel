``` r
library(tidyverse)
```

    ## -- Attaching packages --------------------------------------- tidyverse 1.3.1 --

    ## v ggplot2 3.3.5     v purrr   0.3.4
    ## v tibble  3.1.6     v dplyr   1.0.8
    ## v tidyr   1.2.0     v stringr 1.4.0
    ## v readr   2.1.2     v forcats 0.5.1

    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(GGally)
```

    ## Registered S3 method overwritten by 'GGally':
    ##   method from   
    ##   +.gg   ggplot2

``` r
library(MASS)
```

    ## 
    ## Attaching package: 'MASS'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     select

``` r
library(caret)
```

    ## Warning: package 'caret' was built under R version 4.1.3

    ## Loading required package: lattice

    ## 
    ## Attaching package: 'caret'

    ## The following object is masked from 'package:purrr':
    ## 
    ##     lift

``` r
library(glmnet)
```

    ## Warning: package 'glmnet' was built under R version 4.1.3

    ## Loading required package: Matrix

    ## 
    ## Attaching package: 'Matrix'

    ## The following objects are masked from 'package:tidyr':
    ## 
    ##     expand, pack, unpack

    ## Loaded glmnet 4.1-4

``` r
library(kableExtra)
```

    ## 
    ## Attaching package: 'kableExtra'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     group_rows

## load data

will clean and remove opposing team stats, since they’re included as the
primary on the mirror matchup (i.e. Atl-Tor, vs Tor-Atl) also removing
date, since Game is a better metric of when a game takes place in
relation to the point of the season

### Variable Descriptions

-   PTS - Team Points Scored
-   FG - Field Goals Made
-   FGA - Field Goals Attempted
-   FG% - Field Goal Percentage
-   3P - Three Points Made
-   3PA - Three Points Attempted
-   3P% - Three Point Percentage
-   FT - Free Throws Made
-   FTA - Free Throws Attempted
-   FT% - Free Throw Percentage
-   ORB - Offensive Rebounds
-   TRB - Total Rebounds
-   AST - Assists
-   STL - Steals
-   BLK - Blocks
-   TOV - Turnovers
-   PF - Fouls

``` r
rawdata <- read.csv("https://raw.githubusercontent.com/boneeyah/GroupProject2/main/DataFile/nba.games.stats.csv")
cleandata <- rawdata[,c(7,3,5,8,10:25)] #rearrange to leave W/L first

str(cleandata) #check variable type
```

    ## 'data.frame':    9840 obs. of  20 variables:
    ##  $ WINorLOSS            : chr  "L" "W" "L" "L" ...
    ##  $ Game                 : int  1 2 3 4 5 6 7 8 9 10 ...
    ##  $ Home                 : chr  "Away" "Home" "Away" "Away" ...
    ##  $ TeamPoints           : int  102 102 92 119 103 91 100 114 94 109 ...
    ##  $ FieldGoals           : int  40 35 38 43 33 27 39 42 40 41 ...
    ##  $ FieldGoalsAttempted  : int  80 69 92 93 81 71 76 75 90 85 ...
    ##  $ FieldGoals.          : num  0.5 0.507 0.413 0.462 0.407 0.38 0.513 0.56 0.444 0.482 ...
    ##  $ X3PointShots         : int  13 7 8 13 9 10 9 11 3 9 ...
    ##  $ X3PointShotsAttempted: int  22 20 25 33 22 27 20 28 22 27 ...
    ##  $ X3PointShots.        : num  0.591 0.35 0.32 0.394 0.409 0.37 0.45 0.393 0.136 0.333 ...
    ##  $ FreeThrows           : int  9 25 8 20 28 27 13 19 11 18 ...
    ##  $ FreeThrowsAttempted  : int  17 33 11 26 36 28 18 23 13 23 ...
    ##  $ FreeThrows.          : num  0.529 0.758 0.727 0.769 0.778 0.964 0.722 0.826 0.846 0.783 ...
    ##  $ OffRebounds          : int  10 3 10 7 12 9 13 3 11 13 ...
    ##  $ TotalRebounds        : int  42 37 37 38 41 38 46 36 37 38 ...
    ##  $ Assists              : int  26 26 26 28 18 20 23 33 26 22 ...
    ##  $ Steals               : int  6 10 14 8 10 7 8 10 6 7 ...
    ##  $ Blocks               : int  8 6 5 3 5 3 4 5 8 3 ...
    ##  $ Turnovers            : int  17 12 13 19 8 15 18 13 18 10 ...
    ##  $ TotalFouls           : int  24 20 25 33 17 16 12 20 12 17 ...

``` r
cleandata %>% count(WINorLOSS) %>% ggplot(aes(x=WINorLOSS, y= n, fill = WINorLOSS))+geom_bar(stat = "identity") 
```

![](NBAStats_files/figure-markdown_github/unnamed-chunk-2-1.png)

``` r
#data is perfectly balanced, becasue for each win on the data set there is an opposing team who lost and vice-versa

#set categorical variables to factors
cleandata$WINorLOSS <- as.factor(cleandata$WINorLOSS)
cleandata$Home <- as.factor(cleandata$Home)

##change variable names
cleandata <- rename(cleandata, c("PTS" = TeamPoints, "FG"=FieldGoals,"FGA"=FieldGoalsAttempted, "FG%"=FieldGoals., "3PA"=X3PointShotsAttempted, "3P" = X3PointShots, "3P%"=X3PointShots.,"FT"=FreeThrows, "FTA"=FreeThrowsAttempted, "FT%"=FreeThrows.,"ORB"=OffRebounds,"TRB"=TotalRebounds, "AST"=Assists, "STL"=Steals,"BLK"=Blocks,"TOV"=Turnovers, "PF"=TotalFouls))

sapply(cleandata, function(x) sum(is.na(x))) #no NAs present
```

    ## WINorLOSS      Game      Home       PTS        FG       FGA       FG%        3P 
    ##         0         0         0         0         0         0         0         0 
    ##       3PA       3P%        FT       FTA       FT%       ORB       TRB       AST 
    ##         0         0         0         0         0         0         0         0 
    ##       STL       BLK       TOV        PF 
    ##         0         0         0         0

``` r
ggpairs(cleandata, columns = 2:10, aes(color = WINorLOSS))
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](NBAStats_files/figure-markdown_github/unnamed-chunk-2-2.png)

``` r
ggpairs(cleandata, columns = 11:20, aes(color = WINorLOSS))
```

![](NBAStats_files/figure-markdown_github/unnamed-chunk-2-3.png) There
is a separation between home and away also for team points, field goals,
field goal %, 3 point shots and 3 point shot % to a lesser extent, total
rebounds, assists, and turnovers

``` r
plot(WINorLOSS~Home, col = c("#F8766D","#00bfc4"), data = cleandata)
```

![](NBAStats_files/figure-markdown_github/unnamed-chunk-3-1.png)

``` r
plot(PTS~WINorLOSS, col = c("#F8766D","#00bfc4"), data = cleandata)
```

![](NBAStats_files/figure-markdown_github/unnamed-chunk-3-2.png)

``` r
plot(`FG%`~WINorLOSS, col = c("#F8766D","#00bfc4"), data = cleandata)
```

![](NBAStats_files/figure-markdown_github/unnamed-chunk-3-3.png)

``` r
plot(FG~WINorLOSS, col = c("#F8766D","#00bfc4"), data = cleandata)
```

![](NBAStats_files/figure-markdown_github/unnamed-chunk-3-4.png)

``` r
plot(`3PA`~WINorLOSS, col = c("#F8766D","#00bfc4"), data = cleandata)
```

![](NBAStats_files/figure-markdown_github/unnamed-chunk-3-5.png)

``` r
plot(`3P%`~WINorLOSS, col = c("#F8766D","#00bfc4"), data = cleandata)
```

![](NBAStats_files/figure-markdown_github/unnamed-chunk-3-6.png)

``` r
plot(TRB~WINorLOSS, col = c("#F8766D","#00bfc4"), data = cleandata)
```

![](NBAStats_files/figure-markdown_github/unnamed-chunk-3-7.png)

``` r
plot(AST~WINorLOSS, col = c("#F8766D","#00bfc4"), data = cleandata)
```

![](NBAStats_files/figure-markdown_github/unnamed-chunk-3-8.png)
##correlation plot build a heatmap to check for correlation between
explanatory variables

``` r
library(lattice)
library(reshape2)
```

    ## 
    ## Attaching package: 'reshape2'

    ## The following object is masked from 'package:tidyr':
    ## 
    ##     smiths

``` r
library(ggthemes)
cleandata.corr <- round(cor(cleandata[,c(2,4:20)]),2)
cleandata.corr <- melt(cleandata.corr)

cleandata.corr %>% ggplot(aes(x=Var1, y=Var2, fill=value))+geom_tile()+scale_fill_viridis_c()+theme(axis.title.x = element_blank(),axis.title.y = element_blank())
```

![](NBAStats_files/figure-markdown_github/unnamed-chunk-4-1.png) Not
surprisingly, points and field goals, free throw attempts and free
throws seem to have a strong correlation

``` r
#set seed
set.seed(721)
##80-20 split of data
index <- sample(nrow(cleandata), round(.8*nrow(cleandata)))
train <- cleandata[index,]
test <- cleandata[-index,]


simple.mod <- glm(WINorLOSS~ Home + PTS + `FG%` + TRB, family = "binomial", data=train)
coef(simple.mod)
```

    ##  (Intercept)     HomeHome          PTS        `FG%`          TRB 
    ## -21.57151166   0.50304029   0.03353574  23.98643448   0.15890982

``` r
summary(simple.mod)
```

    ## 
    ## Call:
    ## glm(formula = WINorLOSS ~ Home + PTS + `FG%` + TRB, family = "binomial", 
    ##     data = train)
    ## 
    ## Deviance Residuals: 
    ##      Min        1Q    Median        3Q       Max  
    ## -3.06051  -0.76877  -0.07536   0.77005   2.75360  
    ## 
    ## Coefficients:
    ##               Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept) -21.571512   0.508665 -42.408   <2e-16 ***
    ## HomeHome      0.503040   0.056741   8.866   <2e-16 ***
    ## PTS           0.033536   0.003608   9.295   <2e-16 ***
    ## `FG%`        23.986434   0.910402  26.347   <2e-16 ***
    ## TRB           0.158910   0.005561  28.578   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 10912.9  on 7871  degrees of freedom
    ## Residual deviance:  7553.5  on 7867  degrees of freedom
    ## AIC: 7563.5
    ## 
    ## Number of Fisher Scoring iterations: 5

``` r
simple.pred <- predict(simple.mod, newdata = test, type = "response")
simple.pred <- ifelse(simple.pred > .5, "W", "L")
simple.pred <- factor(simple.pred, levels = c("W","L"))
test$WINorLOSS <- factor(test$WINorLOSS, levels = c("W", "L"))


cm.simple <- confusionMatrix(data =simple.pred, reference = test$WINorLOSS)
```

now adding feature selection to see if the model can improve with the
table in it’s present format

``` r
empty.mod <- glm(WINorLOSS~ 1,family = "binomial", data = cleandata) #as a starting point for forward and stepwise selection
full.mod <- glm(WINorLOSS~.,family = "binomial", data=cleandata) #starting point for backward selection
step.mod <- empty.mod %>% stepAIC(direction = "both", scope = list(lower = empty.mod, upper=~Game + Home + PTS + FG + FGA + `FG%` + `3P` + `3PA` + `3P%` + FT + FTA + `FT%` + ORB + TRB + AST + STL + BLK + TOV + PF),trace = FALSE)
summary(step.mod)
```

    ## 
    ## Call:
    ## glm(formula = WINorLOSS ~ PTS + TRB + FGA + TOV + STL + FTA + 
    ##     BLK + PF + Game + Home + `3PA` + AST + `3P%` + ORB, family = "binomial", 
    ##     data = cleandata)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -3.0445  -0.4796  -0.0023   0.4524   3.3740  
    ## 
    ## Coefficients:
    ##              Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept) -2.486978   0.538353  -4.620 3.84e-06 ***
    ## PTS          0.203043   0.005728  35.449  < 2e-16 ***
    ## TRB          0.371185   0.009284  39.983  < 2e-16 ***
    ## FGA         -0.346031   0.009552 -36.227  < 2e-16 ***
    ## TOV         -0.338836   0.011161 -30.358  < 2e-16 ***
    ## STL          0.399321   0.013330  29.957  < 2e-16 ***
    ## FTA         -0.126641   0.006295 -20.116  < 2e-16 ***
    ## BLK          0.121851   0.012705   9.591  < 2e-16 ***
    ## PF          -0.064956   0.007867  -8.256  < 2e-16 ***
    ## Game        -0.009265   0.001335  -6.940 3.93e-12 ***
    ## HomeHome     0.382336   0.062336   6.133 8.60e-10 ***
    ## `3PA`       -0.029297   0.004697  -6.237 4.46e-10 ***
    ## AST          0.025731   0.008230   3.127  0.00177 ** 
    ## `3P%`        1.067657   0.410689   2.600  0.00933 ** 
    ## ORB         -0.017356   0.011318  -1.533  0.12518    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 13641.1  on 9839  degrees of freedom
    ## Residual deviance:  6652.4  on 9825  degrees of freedom
    ## AIC: 6682.4
    ## 
    ## Number of Fisher Scoring iterations: 6

``` r
step.pred <- predict(step.mod, newdata = test, type = "response")
step.pred <- ifelse(step.pred>.5, "W", "L")
step.pred <- factor(step.pred, levels = c("W","L"))

cm.step <- confusionMatrix(data = step.pred, reference = test$WINorLOSS)

fwd.mod <- empty.mod %>% stepAIC(direction = "forward", scope = list(lower = empty.mod, upper=~Game + Home + PTS + FG + FGA + `FG%` + `3P` + `3PA` + `3P%` + FT + FTA + `FT%` + ORB + TRB + AST + STL + BLK + TOV + PF),trace = FALSE)
summary(fwd.mod)
```

    ## 
    ## Call:
    ## glm(formula = WINorLOSS ~ PTS + TRB + FGA + TOV + STL + FTA + 
    ##     BLK + PF + Game + Home + `3PA` + AST + `3P%` + ORB, family = "binomial", 
    ##     data = cleandata)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -3.0445  -0.4796  -0.0023   0.4524   3.3740  
    ## 
    ## Coefficients:
    ##              Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept) -2.486978   0.538353  -4.620 3.84e-06 ***
    ## PTS          0.203043   0.005728  35.449  < 2e-16 ***
    ## TRB          0.371185   0.009284  39.983  < 2e-16 ***
    ## FGA         -0.346031   0.009552 -36.227  < 2e-16 ***
    ## TOV         -0.338836   0.011161 -30.358  < 2e-16 ***
    ## STL          0.399321   0.013330  29.957  < 2e-16 ***
    ## FTA         -0.126641   0.006295 -20.116  < 2e-16 ***
    ## BLK          0.121851   0.012705   9.591  < 2e-16 ***
    ## PF          -0.064956   0.007867  -8.256  < 2e-16 ***
    ## Game        -0.009265   0.001335  -6.940 3.93e-12 ***
    ## HomeHome     0.382336   0.062336   6.133 8.60e-10 ***
    ## `3PA`       -0.029297   0.004697  -6.237 4.46e-10 ***
    ## AST          0.025731   0.008230   3.127  0.00177 ** 
    ## `3P%`        1.067657   0.410689   2.600  0.00933 ** 
    ## ORB         -0.017356   0.011318  -1.533  0.12518    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 13641.1  on 9839  degrees of freedom
    ## Residual deviance:  6652.4  on 9825  degrees of freedom
    ## AIC: 6682.4
    ## 
    ## Number of Fisher Scoring iterations: 6

``` r
fwd.pred <- predict(fwd.mod, newdata = test, type = "response")
fwd.pred <- ifelse(fwd.pred>.5, "W", "L")
fwd.pred <- factor(fwd.pred, levels = c("W","L"))

cm.fwd <- confusionMatrix(data = fwd.pred, reference = test$WINorLOSS)

bkw.mod <- full.mod %>% stepAIC(direction = "backward", trace = FALSE)
summary(bkw.mod)
```

    ## 
    ## Call:
    ## glm(formula = WINorLOSS ~ Game + Home + PTS + FGA + `FG%` + `3P%` + 
    ##     FTA + `FT%` + TRB + AST + STL + BLK + TOV + PF, family = "binomial", 
    ##     data = cleandata)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -3.0387  -0.4800  -0.0019   0.4511   3.3286  
    ## 
    ## Coefficients:
    ##               Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept) -10.136481   1.312057  -7.726 1.11e-14 ***
    ## Game         -0.009358   0.001336  -7.005 2.46e-12 ***
    ## HomeHome      0.376439   0.062333   6.039 1.55e-09 ***
    ## PTS           0.128024   0.011902  10.757  < 2e-16 ***
    ## FGA          -0.282979   0.013809 -20.493  < 2e-16 ***
    ## `FG%`        13.032984   2.012702   6.475 9.46e-11 ***
    ## `3P%`         2.735630   0.478129   5.722 1.06e-08 ***
    ## FTA          -0.070524   0.010077  -6.998 2.59e-12 ***
    ## `FT%`         1.951904   0.390061   5.004 5.61e-07 ***
    ## TRB           0.367789   0.008985  40.934  < 2e-16 ***
    ## AST           0.027277   0.008306   3.284  0.00102 ** 
    ## STL           0.398507   0.013329  29.898  < 2e-16 ***
    ## BLK           0.122687   0.012678   9.677  < 2e-16 ***
    ## TOV          -0.341337   0.011094 -30.767  < 2e-16 ***
    ## PF           -0.064846   0.007870  -8.240  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 13641.1  on 9839  degrees of freedom
    ## Residual deviance:  6647.9  on 9825  degrees of freedom
    ## AIC: 6677.9
    ## 
    ## Number of Fisher Scoring iterations: 6

``` r
bkw.pred <- predict(bkw.mod, newdata = test, type = "response")
bkw.pred <- ifelse(bkw.pred>.5, "W", "L")
bkw.pred <- factor(bkw.pred, levels = c("W","L"))

cm.bkw <- confusionMatrix(data = bkw.pred, reference = test$WINorLOSS)
```

The variable Game, probably does not make sense in the context of our
dataset without an interaction. Since for any given game there are
exactly the same number of wins and losses (each match has one winner
and one loser). We will therefore fit a new model without Game for our
no interaction model, keeping in mind that there might be potential
interactions between Game and other variables which might make for a
better more complex model in Objective 2

``` r
reduced.mod <- glm(WINorLOSS~ Home + PTS + FGA + `FG%` + `3P%` + FTA + `FT%` + TRB + AST + STL + BLK + TOV + PF, family = "binomial", data = train)
summary(reduced.mod)
```

    ## 
    ## Call:
    ## glm(formula = WINorLOSS ~ Home + PTS + FGA + `FG%` + `3P%` + 
    ##     FTA + `FT%` + TRB + AST + STL + BLK + TOV + PF, family = "binomial", 
    ##     data = train)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.9226  -0.4881  -0.0086   0.4574   3.4114  
    ## 
    ## Coefficients:
    ##               Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept) -11.007736   1.476679  -7.454 9.03e-14 ***
    ## HomeHome      0.372618   0.069560   5.357 8.47e-08 ***
    ## PTS           0.122723   0.013251   9.261  < 2e-16 ***
    ## FGA          -0.274253   0.015417 -17.789  < 2e-16 ***
    ## `FG%`        13.682133   2.258386   6.058 1.38e-09 ***
    ## `3P%`         2.738277   0.538583   5.084 3.69e-07 ***
    ## FTA          -0.062716   0.011220  -5.590 2.28e-08 ***
    ## `FT%`         2.209522   0.435828   5.070 3.98e-07 ***
    ## TRB           0.359609   0.009844  36.530  < 2e-16 ***
    ## AST           0.027514   0.009263   2.970  0.00298 ** 
    ## STL           0.389992   0.014735  26.467  < 2e-16 ***
    ## BLK           0.109141   0.013970   7.812 5.61e-15 ***
    ## TOV          -0.335648   0.012278 -27.338  < 2e-16 ***
    ## PF           -0.063843   0.008794  -7.260 3.87e-13 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 10912.9  on 7871  degrees of freedom
    ## Residual deviance:  5347.6  on 7858  degrees of freedom
    ## AIC: 5375.6
    ## 
    ## Number of Fisher Scoring iterations: 6

``` r
reduced.pred <- predict(reduced.mod, newdata = test, type = "response")
reduced.pred <- ifelse(reduced.pred>.5, "W", "L")
reduced.pred <- factor(reduced.pred, levels = c("W","L"))

cm.reduced <- confusionMatrix(reduced.pred, test$WINorLOSS)
cm.reduced
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction   W   L
    ##          W 825 149
    ##          L 162 832
    ##                                           
    ##                Accuracy : 0.842           
    ##                  95% CI : (0.8251, 0.8578)
    ##     No Information Rate : 0.5015          
    ##     P-Value [Acc > NIR] : <2e-16          
    ##                                           
    ##                   Kappa : 0.684           
    ##                                           
    ##  Mcnemar's Test P-Value : 0.4962          
    ##                                           
    ##             Sensitivity : 0.8359          
    ##             Specificity : 0.8481          
    ##          Pos Pred Value : 0.8470          
    ##          Neg Pred Value : 0.8370          
    ##              Prevalence : 0.5015          
    ##          Detection Rate : 0.4192          
    ##    Detection Prevalence : 0.4949          
    ##       Balanced Accuracy : 0.8420          
    ##                                           
    ##        'Positive' Class : W               
    ## 

Finally, we will now use LASSO for selection to compare to the simple

``` r
train.x <- model.matrix(WINorLOSS~.-1, data = train) #-1 removes intercept column
train.y <- train[,1]

cvfit <- cv.glmnet(train.x, train.y, family = "binomial", type.measure = "class")
plot(cvfit)
```

![](NBAStats_files/figure-markdown_github/unnamed-chunk-8-1.png)

``` r
coef(cvfit, s= "lambda.min")
```

    ## 21 x 1 sparse Matrix of class "dgCMatrix"
    ##                        s1
    ## (Intercept) -1.684891e+01
    ## Game        -7.047658e-03
    ## HomeAway    -3.252777e-01
    ## HomeHome     2.843299e-12
    ## PTS          4.390546e-02
    ## FG           .           
    ## FGA         -1.769670e-01
    ## `FG%`        2.460951e+01
    ## `3P`         6.026291e-02
    ## `3PA`        .           
    ## `3P%`        2.740908e+00
    ## FT           .           
    ## FTA          .           
    ## `FT%`        3.346091e+00
    ## ORB         -1.717622e-02
    ## TRB          3.292705e-01
    ## AST          2.457740e-02
    ## STL          3.502602e-01
    ## BLK          9.505957e-02
    ## TOV         -2.995976e-01
    ## PF          -6.342630e-02

``` r
cvfit$lambda.min # this is the optimal LASSO penalty value
```

    ## [1] 0.002433558

``` r
lasso.mod <- glmnet(train.x, train.y, family = "binomial", lambda = cvfit$lambda.min)

coef(lasso.mod)
```

    ## 21 x 1 sparse Matrix of class "dgCMatrix"
    ##                        s0
    ## (Intercept) -16.857936032
    ## Game         -0.007049395
    ## HomeAway     -0.321874276
    ## HomeHome      0.003452734
    ## PTS           0.043825272
    ## FG            .          
    ## FGA          -0.176964514
    ## `FG%`        24.628978321
    ## `3P`          0.060433705
    ## `3PA`         .          
    ## `3P%`         2.738715132
    ## FT            .          
    ## FTA           .          
    ## `FT%`         3.348166813
    ## ORB          -0.017124935
    ## TRB           0.329324040
    ## AST           0.024557572
    ## STL           0.350326887
    ## BLK           0.095066630
    ## TOV          -0.299666644
    ## PF           -0.063399775

``` r
test.x <- model.matrix(WINorLOSS~.-1, data = test)
lasso.pred <- predict(lasso.mod, newx = test.x, type = "response")
lasso.pred <- ifelse(lasso.pred>.5, "W", "L")
lasso.pred <- factor(lasso.pred, levels = c("W","L"))

cm.lasso <- confusionMatrix(data = lasso.pred, reference = test$WINorLOSS)
cm.lasso
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction   W   L
    ##          W 820 143
    ##          L 167 838
    ##                                           
    ##                Accuracy : 0.8425          
    ##                  95% CI : (0.8256, 0.8583)
    ##     No Information Rate : 0.5015          
    ##     P-Value [Acc > NIR] : <2e-16          
    ##                                           
    ##                   Kappa : 0.685           
    ##                                           
    ##  Mcnemar's Test P-Value : 0.1914          
    ##                                           
    ##             Sensitivity : 0.8308          
    ##             Specificity : 0.8542          
    ##          Pos Pred Value : 0.8515          
    ##          Neg Pred Value : 0.8338          
    ##              Prevalence : 0.5015          
    ##          Detection Rate : 0.4167          
    ##    Detection Prevalence : 0.4893          
    ##       Balanced Accuracy : 0.8425          
    ##                                           
    ##        'Positive' Class : W               
    ## 

Create a table with the results from all the confusion Matrix models

``` r
cm.df <- data.frame("Model" = c("Simple", "Forward", "Stepwise", "Backward", "Reduced", "LASSO"),
           "Accuracy"= c(cm.simple$overall[1],cm.fwd$overall[1], cm.step$overall[1],cm.bkw$overall[1], cm.reduced$overall[1],cm.lasso$overall[1]),
           "Sensitivity"=c(cm.simple$byClass[1],cm.fwd$byClass[1], cm.step$byClass[1], cm.bkw$byClass[1], cm.reduced$byClass[1], cm.lasso$byClass[1]),
           "Specificty" = c(cm.simple$byClass[2],cm.fwd$byClass[2], cm.step$byClass[2],cm.bkw$byClass[2], cm.reduced$byClass[2], cm.lasso$byClass[2]))
cm.df <- kable(cm.df, format = "html") %>% kable_styling(latex_options = c("striped", "scale_down"), full_width = FALSE) %>% 
  row_spec(row = 0, italic = T, background = "#21918c", color = "white") %>% 
  column_spec(1:2, width = "0.5in")
cm.df
```

<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;font-style: italic;color: white !important;background-color: #21918c !important;">
Model
</th>
<th style="text-align:right;font-style: italic;color: white !important;background-color: #21918c !important;">
Accuracy
</th>
<th style="text-align:right;font-style: italic;color: white !important;background-color: #21918c !important;">
Sensitivity
</th>
<th style="text-align:right;font-style: italic;color: white !important;background-color: #21918c !important;">
Specificty
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;width: 0.5in; ">
Simple
</td>
<td style="text-align:right;width: 0.5in; ">
0.7596545
</td>
<td style="text-align:right;">
0.7578521
</td>
<td style="text-align:right;">
0.7614679
</td>
</tr>
<tr>
<td style="text-align:left;width: 0.5in; ">
Forward
</td>
<td style="text-align:right;width: 0.5in; ">
0.8434959
</td>
<td style="text-align:right;">
0.8318136
</td>
<td style="text-align:right;">
0.8552497
</td>
</tr>
<tr>
<td style="text-align:left;width: 0.5in; ">
Stepwise
</td>
<td style="text-align:right;width: 0.5in; ">
0.8434959
</td>
<td style="text-align:right;">
0.8318136
</td>
<td style="text-align:right;">
0.8552497
</td>
</tr>
<tr>
<td style="text-align:left;width: 0.5in; ">
Backward
</td>
<td style="text-align:right;width: 0.5in; ">
0.8445122
</td>
<td style="text-align:right;">
0.8328267
</td>
<td style="text-align:right;">
0.8562691
</td>
</tr>
<tr>
<td style="text-align:left;width: 0.5in; ">
Reduced
</td>
<td style="text-align:right;width: 0.5in; ">
0.8419715
</td>
<td style="text-align:right;">
0.8358663
</td>
<td style="text-align:right;">
0.8481142
</td>
</tr>
<tr>
<td style="text-align:left;width: 0.5in; ">
LASSO
</td>
<td style="text-align:right;width: 0.5in; ">
0.8424797
</td>
<td style="text-align:right;">
0.8308004
</td>
<td style="text-align:right;">
0.8542304
</td>
</tr>
</tbody>
</table>

try a more complex model
