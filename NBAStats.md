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

``` r
library(DescTools)
```

    ## 
    ## Attaching package: 'DescTools'

    ## The following objects are masked from 'package:caret':
    ## 
    ##     MAE, RMSE

``` r
library(epitools)
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
library(cowplot)
```

    ## 
    ## Attaching package: 'cowplot'

    ## The following object is masked from 'package:ggthemes':
    ## 
    ##     theme_map

``` r
library(ROCR)
```

    ## Warning: package 'ROCR' was built under R version 4.1.3

``` r
library(blorr)
```

    ## Warning: package 'blorr' was built under R version 4.1.3

``` r
library(randomForest)
```

    ## Warning: package 'randomForest' was built under R version 4.1.3

    ## randomForest 4.7-1

    ## Type rfNews() to see new features/changes/bug fixes.

    ## 
    ## Attaching package: 'randomForest'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     combine

    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     margin

``` r
library(heplots)
```

    ## Warning: package 'heplots' was built under R version 4.1.3

    ## Loading required package: car

    ## Loading required package: carData

    ## 
    ## Attaching package: 'car'

    ## The following object is masked from 'package:DescTools':
    ## 
    ##     Recode

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     recode

    ## The following object is masked from 'package:purrr':
    ## 
    ##     some

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
##change variable names
rawdata <- rename(rawdata, c("PTS" = TeamPoints, "FG"=FieldGoals,"FGA"=FieldGoalsAttempted, "FG%"=FieldGoals., "3PA"=X3PointShotsAttempted, "3P" = X3PointShots, "3P%"=X3PointShots.,"FT"=FreeThrows, "FTA"=FreeThrowsAttempted, "FT%"=FreeThrows.,"ORB"=OffRebounds,"TRB"=TotalRebounds, "AST"=Assists, "STL"=Steals,"BLK"=Blocks,"TOV"=Turnovers, "PF"=TotalFouls, "Opp" = Opponent))
#set categorical variables to factors
rawdata$WINorLOSS <- as.factor(rawdata$WINorLOSS)
rawdata$Home <- as.factor(rawdata$Home)
rawdata$Team <- as.factor(rawdata$Team)
rawdata$Opp <- as.factor(rawdata$Opp)

cleandata <- rawdata[,c(7,3,5,8,10:25)] #rearrange to leave W/L first

str(cleandata) #check variable type
```

    ## 'data.frame':    9840 obs. of  20 variables:
    ##  $ WINorLOSS: Factor w/ 2 levels "L","W": 1 2 1 1 2 2 2 2 1 1 ...
    ##  $ Game     : int  1 2 3 4 5 6 7 8 9 10 ...
    ##  $ Home     : Factor w/ 2 levels "Away","Home": 1 2 1 1 2 1 2 2 1 2 ...
    ##  $ PTS      : int  102 102 92 119 103 91 100 114 94 109 ...
    ##  $ FG       : int  40 35 38 43 33 27 39 42 40 41 ...
    ##  $ FGA      : int  80 69 92 93 81 71 76 75 90 85 ...
    ##  $ FG%      : num  0.5 0.507 0.413 0.462 0.407 0.38 0.513 0.56 0.444 0.482 ...
    ##  $ 3P       : int  13 7 8 13 9 10 9 11 3 9 ...
    ##  $ 3PA      : int  22 20 25 33 22 27 20 28 22 27 ...
    ##  $ 3P%      : num  0.591 0.35 0.32 0.394 0.409 0.37 0.45 0.393 0.136 0.333 ...
    ##  $ FT       : int  9 25 8 20 28 27 13 19 11 18 ...
    ##  $ FTA      : int  17 33 11 26 36 28 18 23 13 23 ...
    ##  $ FT%      : num  0.529 0.758 0.727 0.769 0.778 0.964 0.722 0.826 0.846 0.783 ...
    ##  $ ORB      : int  10 3 10 7 12 9 13 3 11 13 ...
    ##  $ TRB      : int  42 37 37 38 41 38 46 36 37 38 ...
    ##  $ AST      : int  26 26 26 28 18 20 23 33 26 22 ...
    ##  $ STL      : int  6 10 14 8 10 7 8 10 6 7 ...
    ##  $ BLK      : int  8 6 5 3 5 3 4 5 8 3 ...
    ##  $ TOV      : int  17 12 13 19 8 15 18 13 18 10 ...
    ##  $ PF       : int  24 20 25 33 17 16 12 20 12 17 ...

``` r
cleandata %>% count(WINorLOSS) %>% ggplot(aes(x=WINorLOSS, y= n, fill = WINorLOSS))+geom_bar(stat = "identity") + scale_fill_viridis_d()+theme_cowplot()
```

![](NBAStats_files/figure-markdown_github/wrangling%20and%20corr%20mat-1.png)

``` r
#data is perfectly balanced, becasue for each win on the data set there is an opposing team who lost and vice-versa

sapply(cleandata, function(x) sum(is.na(x))) #no NAs present
```

    ## WINorLOSS      Game      Home       PTS        FG       FGA       FG%        3P 
    ##         0         0         0         0         0         0         0         0 
    ##       3PA       3P%        FT       FTA       FT%       ORB       TRB       AST 
    ##         0         0         0         0         0         0         0         0 
    ##       STL       BLK       TOV        PF 
    ##         0         0         0         0

``` r
ggpairs(cleandata, columns = 2:10, aes(color = WINorLOSS), upper = list(continuous = wrap("cor", size=3, color="black")))+theme_cowplot()+scale_fill_viridis_d()+scale_color_viridis_d()+theme(text = element_text(size = 10),axis.text =element_blank())
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](NBAStats_files/figure-markdown_github/wrangling%20and%20corr%20mat-2.png)

``` r
ggpairs(cleandata, columns = 11:20, aes(color = WINorLOSS), upper = list(continuous = wrap("cor", size=3, color = "black"))) + scale_color_viridis_d()+scale_fill_viridis_d()+theme_cowplot()+theme(text = element_text(size = 10),axis.text =element_blank())
```

![](NBAStats_files/figure-markdown_github/wrangling%20and%20corr%20mat-3.png)
There is a separation between home and away also for team points, field
goals, field goal %, 3 point shots and 3 point shot % to a lesser
extent, total rebounds, assists, and turnovers There is an outlier for
offensive rebounds and total rebounds. It comes from a game between
Milwaukee and Brooklyn which went to triple over time and had 81 total
rebounds for the Bucks, who lost the game. We validated the number of
rebounds, and since there is no error, we decided not to remove the
observation from our dataset

``` r
cleandata %>% ggplot(aes(x=Home, fill=WINorLOSS))+geom_bar(position = "fill")+scale_fill_viridis_d()+labs(y="WINorLOSS")+theme_cowplot()
```

![](NBAStats_files/figure-markdown_github/var%20plots-1.png)

``` r
cleandata %>% ggplot(aes(y=PTS, x=WINorLOSS, fill=WINorLOSS))+geom_boxplot()+scale_fill_viridis_d()+theme_cowplot()
```

![](NBAStats_files/figure-markdown_github/var%20plots-2.png)

``` r
cleandata %>% ggplot(aes(y=`FG%`,x=WINorLOSS, fill=WINorLOSS))+geom_boxplot()+scale_fill_viridis_d()+theme_cowplot()
```

![](NBAStats_files/figure-markdown_github/var%20plots-3.png)

``` r
cleandata %>% ggplot(aes(y=FG, x=WINorLOSS, fill=WINorLOSS))+geom_boxplot()+scale_fill_viridis_d()+theme_cowplot()
```

![](NBAStats_files/figure-markdown_github/var%20plots-4.png)

``` r
cleandata %>% ggplot(aes(y=`3PA`, x= WINorLOSS, fill=WINorLOSS))+geom_boxplot()+scale_fill_viridis_d()+theme_cowplot()
```

![](NBAStats_files/figure-markdown_github/var%20plots-5.png)

``` r
cleandata %>% ggplot(aes(y=`3P%`,x=WINorLOSS, fill=WINorLOSS))+geom_boxplot()+scale_fill_viridis_d()+theme_cowplot()
```

![](NBAStats_files/figure-markdown_github/var%20plots-6.png)

``` r
cleandata %>% ggplot(aes(y=TRB, x=WINorLOSS, fill=WINorLOSS))+geom_boxplot()+scale_fill_viridis_d()+theme_cowplot()
```

![](NBAStats_files/figure-markdown_github/var%20plots-7.png)

``` r
cleandata %>% ggplot(aes(y=AST, x=WINorLOSS, fill=WINorLOSS))+geom_boxplot()+scale_fill_viridis_d()+theme_cowplot()
```

![](NBAStats_files/figure-markdown_github/var%20plots-8.png)
##correlation plot build a heatmap to check for correlation between
explanatory variables

``` r
cleandata.corr <- round(cor(cleandata[,c(2,4:20)]),2)
cleandata.corr <- melt(cleandata.corr)

cleandata.corr %>% ggplot(aes(x=Var1, y=Var2, fill=value))+geom_tile()+scale_fill_viridis_c()+theme_cowplot()+theme(axis.title.x = element_blank(),axis.title.y = element_blank(), axis.text = element_text(size = 10))
```

![](NBAStats_files/figure-markdown_github/heatmap-1.png) Not
surprisingly, points and field goals, free throw attempts and free
throws seem to have a strong correlation

``` r
#set seed
set.seed(1776)
##80-20 split of data
index <- sample(nrow(cleandata), round(.8*nrow(cleandata)))
train <- cleandata[index,]
test <- cleandata[-index,]

#new set with only numerical variables
#removed colinear vars fg, 3p, ft
numdata <- rawdata[,c(7,3,8,11,12,14,15,17:25)]
train.num <- numdata[index,]
test.num <- numdata[-index,]

#run PCA
PCA <-  prcomp(train.num[,-1], scale. = TRUE)
PCA
```

    ## Standard deviations (1, .., p=15):
    ##  [1] 1.66933130 1.49371991 1.21702880 1.06601735 1.06241926 1.00882589
    ##  [7] 0.99279172 0.97962383 0.94705682 0.88963044 0.76980762 0.60327791
    ## [13] 0.60008146 0.51205055 0.07495786
    ## 
    ## Rotation (n x k) = (15 x 15):
    ##              PC1          PC2          PC3         PC4          PC5
    ## Game -0.05642841  0.064223814  0.252484331 -0.07701850  0.133429339
    ## PTS  -0.48811529  0.278879519 -0.176513046 -0.03966821  0.214085103
    ## FGA   0.04285344  0.565406734  0.215221508  0.15141348  0.097594436
    ## FG%  -0.52170561 -0.085198486 -0.063773795 -0.14886525 -0.068863339
    ## 3PA  -0.11885834  0.270483215  0.139759968  0.38113424 -0.039852954
    ## 3P%  -0.41732361 -0.032229467 -0.012016011 -0.14785264 -0.085672805
    ## FTA   0.01584157  0.004919354 -0.589859060 -0.13366285  0.423304035
    ## FT%  -0.09108730 -0.034136877 -0.003950874  0.01543189  0.256735077
    ## ORB   0.24407365  0.472129525 -0.122518467 -0.09819936 -0.004999932
    ## TRB   0.15291514  0.472858085 -0.152925036 -0.33499073 -0.173827085
    ## AST  -0.44580676  0.200318480  0.115337446  0.03285249 -0.181096101
    ## STL  -0.05730968  0.064960695 -0.120812543  0.63595889 -0.190724348
    ## BLK  -0.02447418  0.109259993 -0.087717385 -0.38184467 -0.410041593
    ## TOV   0.03292279 -0.080648088 -0.421569881  0.14221046 -0.606818215
    ## PF   -0.03234249  0.080487400 -0.485149582  0.26223273  0.179708572
    ##               PC6          PC7         PC8         PC9        PC10        PC11
    ## Game -0.460600683  0.473139530 -0.51258290  0.31156681  0.31790217  0.09084655
    ## PTS   0.001271391  0.063223592  0.03059282 -0.03779427 -0.09545737 -0.07558954
    ## FGA   0.044129771 -0.060039573  0.15800826 -0.09785335  0.20743207  0.01132910
    ## FG%  -0.117340159 -0.025186116  0.02358264 -0.15344926  0.09445835 -0.26123533
    ## 3PA   0.220728065 -0.133871581 -0.21569362  0.57412068 -0.47152198  0.01380061
    ## 3P%  -0.022165522 -0.192246097 -0.12519416 -0.12355555 -0.11414988  0.80866272
    ## FTA  -0.230236261  0.192731030  0.08416054  0.16628772 -0.36557174 -0.07005308
    ## FT%   0.730815465  0.501236431 -0.23178735 -0.23662921  0.05543544  0.02862068
    ## ORB  -0.112317111 -0.011811986 -0.09101874 -0.29947141 -0.01090606  0.15961756
    ## TRB   0.016873262  0.008102635 -0.18463235 -0.10723107 -0.11370963 -0.07670105
    ## AST  -0.038030923 -0.042180030  0.03197128 -0.03326981  0.08821239 -0.37861313
    ## STL  -0.248600754  0.459514088  0.31990185 -0.21984030 -0.05694223  0.18351247
    ## BLK   0.199993575  0.347198118  0.46827689  0.46637394  0.15054453  0.15129144
    ## TOV   0.046435304  0.045000091 -0.47053835 -0.07425080 -0.01142276 -0.12103712
    ## PF    0.132343803 -0.298211724 -0.03704006  0.25732109  0.64724794  0.10901296
    ##              PC12         PC13         PC14          PC15
    ## Game  0.005275483 -0.003728542  0.004662583  0.0003193939
    ## PTS   0.310255400 -0.049438489  0.170984115  0.6754847319
    ## FGA   0.129590961 -0.071825741  0.602791982 -0.3621413818
    ## FG%   0.463440423 -0.102986298 -0.286356996 -0.5162738407
    ## 3PA   0.127502588  0.035058901 -0.209572832 -0.1400574923
    ## 3P%  -0.187178785 -0.015753050  0.099499999 -0.1221406447
    ## FTA  -0.199067340  0.109548614  0.206860266 -0.3112562793
    ## FT%  -0.076149460  0.042271852 -0.057434831 -0.1206089484
    ## ORB   0.190169850  0.617008729 -0.373701151  0.0026612284
    ## TRB  -0.224041024 -0.643170355 -0.242003335  0.0047415947
    ## AST  -0.674808841  0.317751921 -0.033549540 -0.0028844794
    ## STL  -0.061374168 -0.191786660 -0.189681077  0.0033172289
    ## BLK   0.084691345  0.125330802  0.008021422  0.0002770348
    ## TOV   0.128485411  0.113414608  0.388550949 -0.0037698458
    ## PF   -0.079055184 -0.076320108 -0.197773632  0.0032573572

``` r
summary(PCA)
```

    ## Importance of components:
    ##                           PC1    PC2     PC3     PC4     PC5     PC6     PC7
    ## Standard deviation     1.6693 1.4937 1.21703 1.06602 1.06242 1.00883 0.99279
    ## Proportion of Variance 0.1858 0.1487 0.09874 0.07576 0.07525 0.06785 0.06571
    ## Cumulative Proportion  0.1858 0.3345 0.43327 0.50903 0.58428 0.65213 0.71783
    ##                            PC8     PC9    PC10    PC11    PC12    PC13    PC14
    ## Standard deviation     0.97962 0.94706 0.88963 0.76981 0.60328 0.60008 0.51205
    ## Proportion of Variance 0.06398 0.05979 0.05276 0.03951 0.02426 0.02401 0.01748
    ## Cumulative Proportion  0.78181 0.84161 0.89437 0.93388 0.95814 0.98215 0.99963
    ##                           PC15
    ## Standard deviation     0.07496
    ## Proportion of Variance 0.00037
    ## Cumulative Proportion  1.00000

``` r
simple.mod <- glm(WINorLOSS~1, family = "binomial", data=train)#intercept only simple model
coef(simple.mod)
```

    ## (Intercept) 
    ## -0.01626052

``` r
summary(simple.mod)
```

    ## 
    ## Call:
    ## glm(formula = WINorLOSS ~ 1, family = "binomial", data = train)
    ## 
    ## Deviance Residuals: 
    ##    Min      1Q  Median      3Q     Max  
    ## -1.171  -1.171  -1.171   1.184   1.184  
    ## 
    ## Coefficients:
    ##             Estimate Std. Error z value Pr(>|z|)
    ## (Intercept) -0.01626    0.02254  -0.721    0.471
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 10912  on 7871  degrees of freedom
    ## Residual deviance: 10912  on 7871  degrees of freedom
    ## AIC: 10914
    ## 
    ## Number of Fisher Scoring iterations: 3

``` r
simple.pred <- predict(simple.mod, newdata = test, type = "response")
simple.auc <- performance(prediction(simple.pred, test$WINorLOSS), "auc")
simple.auc <- simple.auc@y.values[[1]]
simple.pred <- ifelse(simple.pred > .5, "W", "L")
simple.pred <- factor(simple.pred, levels = c("W","L"))
test$WINorLOSS <- factor(test$WINorLOSS, levels = c("W", "L"))


cm.simple <- confusionMatrix(data =simple.pred, reference = test$WINorLOSS)
cm.simple
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction    W    L
    ##          W    0    0
    ##          L 1016  952
    ##                                           
    ##                Accuracy : 0.4837          
    ##                  95% CI : (0.4614, 0.5061)
    ##     No Information Rate : 0.5163          
    ##     P-Value [Acc > NIR] : 0.9982          
    ##                                           
    ##                   Kappa : 0               
    ##                                           
    ##  Mcnemar's Test P-Value : <2e-16          
    ##                                           
    ##             Sensitivity : 0.0000          
    ##             Specificity : 1.0000          
    ##          Pos Pred Value :    NaN          
    ##          Neg Pred Value : 0.4837          
    ##              Prevalence : 0.5163          
    ##          Detection Rate : 0.0000          
    ##    Detection Prevalence : 0.0000          
    ##       Balanced Accuracy : 0.5000          
    ##                                           
    ##        'Positive' Class : W               
    ## 

now adding feature selection to see if the model can improve with the
table in it’s present format

``` r
empty.mod <- glm(WINorLOSS~ 1,family = "binomial", data = train) #as a starting point for forward and stepwise selection
full.mod <- glm(WINorLOSS~.,family = "binomial", data=train) #starting point for backward selection
step.mod <- empty.mod %>% stepAIC(direction = "both", scope = list(lower = empty.mod, upper=~Game + Home + PTS + FG + FGA + `FG%` + `3P` + `3PA` + `3P%` + FT + FTA + `FT%` + ORB + TRB + AST + STL + BLK + TOV + PF),trace = FALSE)
summary(step.mod)
```

    ## 
    ## Call:
    ## glm(formula = WINorLOSS ~ PTS + TRB + FGA + TOV + STL + FTA + 
    ##     BLK + PF + Game + `3PA` + Home + AST + ORB + `3P%`, family = "binomial", 
    ##     data = train)
    ## 
    ## Deviance Residuals: 
    ##      Min        1Q    Median        3Q       Max  
    ## -3.07590  -0.48427  -0.02445   0.44542   3.06466  
    ## 
    ## Coefficients:
    ##              Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept) -2.661812   0.604763  -4.401 1.08e-05 ***
    ## PTS          0.203528   0.006414  31.730  < 2e-16 ***
    ## TRB          0.377689   0.010541  35.829  < 2e-16 ***
    ## FGA         -0.343628   0.010646 -32.277  < 2e-16 ***
    ## TOV         -0.339821   0.012482 -27.226  < 2e-16 ***
    ## STL          0.405092   0.015062  26.896  < 2e-16 ***
    ## FTA         -0.129470   0.007067 -18.320  < 2e-16 ***
    ## BLK          0.120771   0.014180   8.517  < 2e-16 ***
    ## PF          -0.066161   0.008873  -7.457 8.88e-14 ***
    ## Game        -0.010751   0.001507  -7.133 9.85e-13 ***
    ## `3PA`       -0.034333   0.005296  -6.483 8.98e-11 ***
    ## HomeHome     0.398512   0.069914   5.700 1.20e-08 ***
    ## AST          0.027332   0.009200   2.971  0.00297 ** 
    ## ORB         -0.033521   0.012633  -2.653  0.00797 ** 
    ## `3P%`        1.126795   0.458681   2.457  0.01403 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 10912.4  on 7871  degrees of freedom
    ## Residual deviance:  5300.5  on 7857  degrees of freedom
    ## AIC: 5330.5
    ## 
    ## Number of Fisher Scoring iterations: 6

``` r
step.pred <- predict(step.mod, newdata = test, type = "response")
step.auc <- performance(prediction(step.pred, test$WINorLOSS),"auc")
step.auc <- step.auc@y.values[[1]]
step.pred <- ifelse(step.pred>.5, "W", "L")
step.pred <- factor(step.pred, levels = c("W","L"))

cm.step <- confusionMatrix(data = step.pred, reference = test$WINorLOSS)

fwd.mod <- empty.mod %>% stepAIC(direction = "forward", scope = list(lower = empty.mod, upper=~Game + Home + PTS + FG + FGA + `FG%` + `3P` + `3PA` + `3P%` + FT + FTA + `FT%` + ORB + TRB + AST + STL + BLK + TOV + PF),trace = FALSE)
summary(fwd.mod)
```

    ## 
    ## Call:
    ## glm(formula = WINorLOSS ~ PTS + TRB + FGA + TOV + STL + FTA + 
    ##     BLK + PF + Game + `3PA` + Home + AST + ORB + `3P%`, family = "binomial", 
    ##     data = train)
    ## 
    ## Deviance Residuals: 
    ##      Min        1Q    Median        3Q       Max  
    ## -3.07590  -0.48427  -0.02445   0.44542   3.06466  
    ## 
    ## Coefficients:
    ##              Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept) -2.661812   0.604763  -4.401 1.08e-05 ***
    ## PTS          0.203528   0.006414  31.730  < 2e-16 ***
    ## TRB          0.377689   0.010541  35.829  < 2e-16 ***
    ## FGA         -0.343628   0.010646 -32.277  < 2e-16 ***
    ## TOV         -0.339821   0.012482 -27.226  < 2e-16 ***
    ## STL          0.405092   0.015062  26.896  < 2e-16 ***
    ## FTA         -0.129470   0.007067 -18.320  < 2e-16 ***
    ## BLK          0.120771   0.014180   8.517  < 2e-16 ***
    ## PF          -0.066161   0.008873  -7.457 8.88e-14 ***
    ## Game        -0.010751   0.001507  -7.133 9.85e-13 ***
    ## `3PA`       -0.034333   0.005296  -6.483 8.98e-11 ***
    ## HomeHome     0.398512   0.069914   5.700 1.20e-08 ***
    ## AST          0.027332   0.009200   2.971  0.00297 ** 
    ## ORB         -0.033521   0.012633  -2.653  0.00797 ** 
    ## `3P%`        1.126795   0.458681   2.457  0.01403 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 10912.4  on 7871  degrees of freedom
    ## Residual deviance:  5300.5  on 7857  degrees of freedom
    ## AIC: 5330.5
    ## 
    ## Number of Fisher Scoring iterations: 6

``` r
fwd.pred <- predict(fwd.mod, newdata = test, type = "response")
fwd.auc <- performance(prediction(fwd.pred, test$WINorLOSS), "auc")
fwd.auc <- fwd.auc@y.values[[1]]
fwd.pred <- ifelse(fwd.pred>.5, "W", "L")
fwd.pred <- factor(fwd.pred, levels = c("W","L"))

cm.fwd <- confusionMatrix(data = fwd.pred, reference = test$WINorLOSS)

bkw.mod <- full.mod %>% stepAIC(direction = "backward", trace = FALSE)
summary(bkw.mod)
```

    ## 
    ## Call:
    ## glm(formula = WINorLOSS ~ Game + Home + PTS + FGA + `FG%` + `3P%` + 
    ##     FTA + `FT%` + ORB + TRB + AST + STL + BLK + TOV + PF, family = "binomial", 
    ##     data = train)
    ## 
    ## Deviance Residuals: 
    ##      Min        1Q    Median        3Q       Max  
    ## -3.07344  -0.48175  -0.02353   0.44414   3.13000  
    ## 
    ## Coefficients:
    ##               Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept) -12.084100   1.520791  -7.946 1.93e-15 ***
    ## Game         -0.010790   0.001509  -7.149 8.72e-13 ***
    ## HomeHome      0.393652   0.069962   5.627 1.84e-08 ***
    ## PTS           0.113228   0.013452   8.417  < 2e-16 ***
    ## FGA          -0.263184   0.016059 -16.389  < 2e-16 ***
    ## `FG%`        15.444132   2.280349   6.773 1.26e-11 ***
    ## `3P%`         3.142938   0.538593   5.835 5.36e-09 ***
    ## FTA          -0.060799   0.011542  -5.267 1.38e-07 ***
    ## `FT%`         2.320437   0.441518   5.256 1.48e-07 ***
    ## ORB          -0.032245   0.012604  -2.558  0.01052 *  
    ## TRB           0.377284   0.010536  35.809  < 2e-16 ***
    ## AST           0.028076   0.009337   3.007  0.00264 ** 
    ## STL           0.404698   0.015073  26.849  < 2e-16 ***
    ## BLK           0.120655   0.014186   8.505  < 2e-16 ***
    ## TOV          -0.340376   0.012482 -27.270  < 2e-16 ***
    ## PF           -0.066584   0.008884  -7.495 6.62e-14 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 10912.4  on 7871  degrees of freedom
    ## Residual deviance:  5294.1  on 7856  degrees of freedom
    ## AIC: 5326.1
    ## 
    ## Number of Fisher Scoring iterations: 6

``` r
bkw.pred <- predict(bkw.mod, newdata = test, type = "response")
bkw.auc <- performance(prediction(bkw.pred, test$WINorLOSS), "auc")
bkw.auc <- bkw.auc@y.values[[1]]
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
better more complex model in Objective 2. From correlation scatterplot
and heatmap there is some evidence of potential colinearity between TRB
and ORB. Coupled with context knowledge that Offensive rebounds are one
of two values that go into total rebounds, we decided to fit the model
without ORB.

Fitting a custom model without ORB and without Game variable

``` r
custom.mod <- glm(WINorLOSS~ Home + PTS + FGA + `3PA` + `3P%` + FTA + TRB + AST + STL + BLK + TOV + PF, family = "binomial", data = train)
summary(custom.mod)
```

    ## 
    ## Call:
    ## glm(formula = WINorLOSS ~ Home + PTS + FGA + `3PA` + `3P%` + 
    ##     FTA + TRB + AST + STL + BLK + TOV + PF, family = "binomial", 
    ##     data = train)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -3.0146  -0.4959  -0.0271   0.4488   3.1857  
    ## 
    ## Coefficients:
    ##              Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept) -2.398014   0.551022  -4.352 1.35e-05 ***
    ## HomeHome     0.400765   0.069430   5.772 7.82e-09 ***
    ## PTS          0.202165   0.006301  32.084  < 2e-16 ***
    ## FGA         -0.349459   0.010001 -34.942  < 2e-16 ***
    ## `3PA`       -0.033857   0.005239  -6.462 1.03e-10 ***
    ## `3P%`        1.071836   0.455615   2.353  0.01865 *  
    ## FTA         -0.130910   0.006878 -19.033  < 2e-16 ***
    ## TRB          0.366223   0.010023  36.540  < 2e-16 ***
    ## AST          0.026746   0.009095   2.941  0.00327 ** 
    ## STL          0.399205   0.014907  26.779  < 2e-16 ***
    ## BLK          0.124461   0.014056   8.855  < 2e-16 ***
    ## TOV         -0.337469   0.012304 -27.427  < 2e-16 ***
    ## PF          -0.059664   0.008781  -6.795 1.09e-11 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 10912  on 7871  degrees of freedom
    ## Residual deviance:  5360  on 7859  degrees of freedom
    ## AIC: 5386
    ## 
    ## Number of Fisher Scoring iterations: 6

``` r
custom.pred <- predict(custom.mod, newdata = test, type = "response")
custom.auc <- performance(prediction(custom.pred, test$WINorLOSS), "auc")
custom.auc <- custom.auc@y.values[[1]]
custom.pred <- ifelse(custom.pred>.5, "W", "L")
custom.pred <- factor(custom.pred, levels = c("W","L"))

cm.custom <- confusionMatrix(custom.pred, test$WINorLOSS)
cm.custom
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction   W   L
    ##          W 873 156
    ##          L 143 796
    ##                                           
    ##                Accuracy : 0.8481          
    ##                  95% CI : (0.8314, 0.8637)
    ##     No Information Rate : 0.5163          
    ##     P-Value [Acc > NIR] : <2e-16          
    ##                                           
    ##                   Kappa : 0.6957          
    ##                                           
    ##  Mcnemar's Test P-Value : 0.4877          
    ##                                           
    ##             Sensitivity : 0.8593          
    ##             Specificity : 0.8361          
    ##          Pos Pred Value : 0.8484          
    ##          Neg Pred Value : 0.8477          
    ##              Prevalence : 0.5163          
    ##          Detection Rate : 0.4436          
    ##    Detection Prevalence : 0.5229          
    ##       Balanced Accuracy : 0.8477          
    ##                                           
    ##        'Positive' Class : W               
    ## 

Finally, we will now use LASSO for selection to compare to the simple

``` r
train.x <- model.matrix(WINorLOSS~.-1, data = train) #-1 removes intercept column
train.y <- train[,1]

cvfit <- cv.glmnet(train.x, train.y, family = "binomial", type.measure = "class")

coef(cvfit)
```

    ## 21 x 1 sparse Matrix of class "dgCMatrix"
    ##                        s1
    ## (Intercept) -1.472382e+01
    ## Game        -3.359168e-03
    ## HomeAway    -2.139539e-01
    ## HomeHome     2.240197e-14
    ## PTS          2.881811e-02
    ## FG           .           
    ## FGA         -1.112889e-01
    ## `FG%`        2.103736e+01
    ## `3P`         2.938312e-02
    ## `3PA`        .           
    ## `3P%`        2.604816e+00
    ## FT           .           
    ## FTA          .           
    ## `FT%`        2.104204e+00
    ## ORB         -2.153702e-03
    ## TRB          2.370588e-01
    ## AST          7.047881e-03
    ## STL          2.423032e-01
    ## BLK          7.665650e-02
    ## TOV         -2.013300e-01
    ## PF          -4.424970e-02

``` r
lasso.mod <- glm(WINorLOSS~ Game + Home + PTS + FGA + `FG%` + `3P` + `3P%` + `FT%` + ORB + TRB + AST + STL + BLK + TOV + PF, family = "binomial", data = train)

lasso.pred <- predict(lasso.mod, newdata = test, type = "response")
lasso.auc <- performance(prediction(lasso.pred, test$WINorLOSS), "auc")
lasso.auc <- lasso.auc@y.values[[1]]
lasso.pred <- ifelse(lasso.pred>.5, "W", "L")
lasso.pred <- factor(lasso.pred, levels = c("W","L"))

cm.lasso <- confusionMatrix(data = lasso.pred, reference = test$WINorLOSS)
cm.lasso
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction   W   L
    ##          W 866 162
    ##          L 150 790
    ##                                           
    ##                Accuracy : 0.8415          
    ##                  95% CI : (0.8246, 0.8573)
    ##     No Information Rate : 0.5163          
    ##     P-Value [Acc > NIR] : <2e-16          
    ##                                           
    ##                   Kappa : 0.6825          
    ##                                           
    ##  Mcnemar's Test P-Value : 0.5334          
    ##                                           
    ##             Sensitivity : 0.8524          
    ##             Specificity : 0.8298          
    ##          Pos Pred Value : 0.8424          
    ##          Neg Pred Value : 0.8404          
    ##              Prevalence : 0.5163          
    ##          Detection Rate : 0.4400          
    ##    Detection Prevalence : 0.5224          
    ##       Balanced Accuracy : 0.8411          
    ##                                           
    ##        'Positive' Class : W               
    ## 

Create a table with the results from all the confusion Matrix models

``` r
cm.df <- data.frame("Model" = c("Simple", "Forward", "Stepwise", "Backward", "Custom", "LASSO"),
           "Accuracy"= c(cm.simple$overall[1],cm.fwd$overall[1], cm.step$overall[1],cm.bkw$overall[1], cm.custom$overall[1],cm.lasso$overall[1]),
           "Sensitivity"=c(cm.simple$byClass[1],cm.fwd$byClass[1], cm.step$byClass[1], cm.bkw$byClass[1], cm.custom$byClass[1], cm.lasso$byClass[1]),
           "Specificty" = c(cm.simple$byClass[2],cm.fwd$byClass[2], cm.step$byClass[2],cm.bkw$byClass[2], cm.custom$byClass[2], cm.lasso$byClass[2]),
           "AUC" = c(simple.auc, fwd.auc, step.auc, bkw.auc, custom.auc, lasso.auc))
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
<th style="text-align:right;font-style: italic;color: white !important;background-color: #21918c !important;">
AUC
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;width: 0.5in; ">
Simple
</td>
<td style="text-align:right;width: 0.5in; ">
0.4837398
</td>
<td style="text-align:right;">
0.0000000
</td>
<td style="text-align:right;">
1.0000000
</td>
<td style="text-align:right;">
0.5000000
</td>
</tr>
<tr>
<td style="text-align:left;width: 0.5in; ">
Forward
</td>
<td style="text-align:right;width: 0.5in; ">
0.8414634
</td>
<td style="text-align:right;">
0.8543307
</td>
<td style="text-align:right;">
0.8277311
</td>
<td style="text-align:right;">
0.9260715
</td>
</tr>
<tr>
<td style="text-align:left;width: 0.5in; ">
Stepwise
</td>
<td style="text-align:right;width: 0.5in; ">
0.8414634
</td>
<td style="text-align:right;">
0.8543307
</td>
<td style="text-align:right;">
0.8277311
</td>
<td style="text-align:right;">
0.9260715
</td>
</tr>
<tr>
<td style="text-align:left;width: 0.5in; ">
Backward
</td>
<td style="text-align:right;width: 0.5in; ">
0.8404472
</td>
<td style="text-align:right;">
0.8533465
</td>
<td style="text-align:right;">
0.8266807
</td>
<td style="text-align:right;">
0.9260415
</td>
</tr>
<tr>
<td style="text-align:left;width: 0.5in; ">
Custom
</td>
<td style="text-align:right;width: 0.5in; ">
0.8480691
</td>
<td style="text-align:right;">
0.8592520
</td>
<td style="text-align:right;">
0.8361345
</td>
<td style="text-align:right;">
0.9273349
</td>
</tr>
<tr>
<td style="text-align:left;width: 0.5in; ">
LASSO
</td>
<td style="text-align:right;width: 0.5in; ">
0.8414634
</td>
<td style="text-align:right;">
0.8523622
</td>
<td style="text-align:right;">
0.8298319
</td>
<td style="text-align:right;">
0.9257624
</td>
</tr>
</tbody>
</table>

## will move this to Obj 2 section

## need to add new models to this curve

``` r
custom.pred.roc <- prediction(predict(custom.mod, newdata = test, type = "response"), test$WINorLOSS)
custom.roc <- performance(custom.pred.roc, "tpr", "fpr")

lasso.pred.roc <- prediction(predict(lasso.mod, newdata = test, type = "response"), test$WINorLOSS)
lasso.roc <- performance(lasso.pred.roc, "tpr", "fpr")
step.pred.roc <- prediction(predict(step.mod, newdata = test, type = "response"), test$WINorLOSS)
step.roc <- performance(step.pred.roc, "tpr", "fpr")
bkw.pred.roc <- prediction(predict(bkw.mod, newdata = test, type = "response"), test$WINorLOSS)
bkw.roc <- performance(bkw.pred.roc, "tpr", "fpr")
simple.pred.roc <- prediction(predict(simple.mod, newdata = test, type = "response"), test$WINorLOSS)
simple.roc <- performance(simple.pred.roc, "tpr", "fpr")

df.roc <- rbind(data.frame(x.vals=simple.roc@x.values[[1]], y.vals=simple.roc@y.values[[1]],model = "simple"),
                data.frame(x.vals=bkw.roc@x.values[[1]], y.vals=bkw.roc@y.values[[1]], model="bkw"),
                data.frame(x.vals=step.roc@x.values[[1]], y.vals=step.roc@y.values[[1]], model="step"),
                data.frame(x.vals=lasso.roc@x.values[[1]], y.vals=lasso.roc@y.values[[1]], model="lasso"),
                data.frame(x.vals=custom.roc@x.values[[1]], y.vals=custom.roc@y.values[[1]], model="custom"))
df.roc$model <- factor(df.roc$model, levels = c("simple", "bkw", "step", "lasso", "custom"))
df.roc %>% ggplot(aes(x=x.vals,y=y.vals, color = model))+geom_line(size=.55)+theme_cowplot()+labs(x="False positive rate", y="True positive rate")+scale_color_viridis_d(direction = -1)
```

![](NBAStats_files/figure-markdown_github/roc%20plot-1.png)

All the models, with the exception of the Simple model, perform
reasonably well and similar to each other. Forward and Stepwise have the
exact same performance, in this case they both selected the same model,
which is not uncommon given that Stepwise is a modified version of
forward selection (it means that the stepwise selection did not remove
any variables once they were added, so it behaved the same as forward
selection).

Our custom model performs marginally better, with 1% higher Specificity
(properly predicting true negatives), with slightly higher accuracy than
the rest of the models

``` r
#fit model with all data
custom.mod.full <- glm(WINorLOSS~ Home + PTS + FGA + `FG%` + `3PA` + `3P%` + FTA + `FT%` + TRB + AST + STL + BLK + TOV + PF, family = "binomial", data = cleandata)
summary(custom.mod.full)
```

    ## 
    ## Call:
    ## glm(formula = WINorLOSS ~ Home + PTS + FGA + `FG%` + `3PA` + 
    ##     `3P%` + FTA + `FT%` + TRB + AST + STL + BLK + TOV + PF, family = "binomial", 
    ##     data = cleandata)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -3.0134  -0.4928  -0.0016   0.4555   3.4227  
    ## 
    ## Coefficients:
    ##              Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept) -8.630924   2.551600  -3.383 0.000718 ***
    ## HomeHome     0.382181   0.062089   6.155 7.49e-10 ***
    ## PTS          0.143370   0.024703   5.804 6.48e-09 ***
    ## FGA         -0.295946   0.023478 -12.605  < 2e-16 ***
    ## `FG%`        9.939223   4.242419   2.343 0.019139 *  
    ## `3PA`       -0.008996   0.009857  -0.913 0.361442    
    ## `3P%`        2.391012   0.673687   3.549 0.000386 ***
    ## FTA         -0.082420   0.019312  -4.268 1.97e-05 ***
    ## `FT%`        1.561296   0.594186   2.628 0.008598 ** 
    ## TRB          0.364510   0.008908  40.919  < 2e-16 ***
    ## AST          0.025185   0.008272   3.045 0.002330 ** 
    ## STL          0.395266   0.013245  29.843  < 2e-16 ***
    ## BLK          0.123000   0.012625   9.743  < 2e-16 ***
    ## TOV         -0.336119   0.011014 -30.518  < 2e-16 ***
    ## PF          -0.060502   0.007817  -7.740 9.93e-15 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 13641.1  on 9839  degrees of freedom
    ## Residual deviance:  6696.8  on 9825  degrees of freedom
    ## AIC: 6726.8
    ## 
    ## Number of Fisher Scoring iterations: 6

``` r
custom.pred.full <- custom.mod.full$fitted.values
custom.pred.full2 <- ifelse(custom.pred.full>.5, "W", "L")
custom.pred.full2 <- factor(custom.pred.full2, levels = c("W","L"))

cm.custom.full <- confusionMatrix(custom.pred.full2, factor(cleandata$WINorLOSS, levels = c("W","L")))
cm.custom.full
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction    W    L
    ##          W 4149  754
    ##          L  771 4166
    ##                                           
    ##                Accuracy : 0.845           
    ##                  95% CI : (0.8377, 0.8521)
    ##     No Information Rate : 0.5             
    ##     P-Value [Acc > NIR] : <2e-16          
    ##                                           
    ##                   Kappa : 0.69            
    ##                                           
    ##  Mcnemar's Test P-Value : 0.682           
    ##                                           
    ##             Sensitivity : 0.8433          
    ##             Specificity : 0.8467          
    ##          Pos Pred Value : 0.8462          
    ##          Neg Pred Value : 0.8438          
    ##              Prevalence : 0.5000          
    ##          Detection Rate : 0.4216          
    ##    Detection Prevalence : 0.4983          
    ##       Balanced Accuracy : 0.8450          
    ##                                           
    ##        'Positive' Class : W               
    ## 

Calculate leverage and influence plots to check assumptions are met

``` r
blr_plot_diag_fit(custom.mod.full)
```

![](NBAStats_files/figure-markdown_github/assumptions-1.png)

``` r
blr_plot_diag_leverage(custom.mod.full)
```

![](NBAStats_files/figure-markdown_github/assumptions-2.png)

``` r
blr_plot_diag_influence(custom.mod.full)
```

![](NBAStats_files/figure-markdown_github/assumptions-3.png)

create table with interpretable values on odds ratio scale

``` r
## p hat probably doesn't make sense in this context, will remove
data.frame("coefficient" = coef(custom.mod.full),
           "Odds Ratio" = exp(coef(custom.mod.full)),
           exp(confint.default(custom.mod.full, level = .95))) %>% 
  kable(format = "html", col.names = c("Coefficient", "Odds Ratio", "2.5%", "97.5%")) %>% 
  kable_styling(latex_options = c("striped", "scale_down"), full_width = FALSE) %>% 
  row_spec(row = 0, italic = T, background = "#21918c", color = "white") %>% 
  column_spec(1:2, width = "0.5in")
```

<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;font-style: italic;color: white !important;background-color: #21918c !important;">
</th>
<th style="text-align:right;font-style: italic;color: white !important;background-color: #21918c !important;">
Coefficient
</th>
<th style="text-align:right;font-style: italic;color: white !important;background-color: #21918c !important;">
Odds Ratio
</th>
<th style="text-align:right;font-style: italic;color: white !important;background-color: #21918c !important;">
2.5%
</th>
<th style="text-align:right;font-style: italic;color: white !important;background-color: #21918c !important;">
97.5%
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;width: 0.5in; ">
(Intercept)
</td>
<td style="text-align:right;width: 0.5in; ">
-8.6309239
</td>
<td style="text-align:right;">
1.785000e-04
</td>
<td style="text-align:right;">
0.0000012
</td>
<td style="text-align:right;">
2.651940e-02
</td>
</tr>
<tr>
<td style="text-align:left;width: 0.5in; ">
HomeHome
</td>
<td style="text-align:right;width: 0.5in; ">
0.3821813
</td>
<td style="text-align:right;">
1.465478e+00
</td>
<td style="text-align:right;">
1.2975637
</td>
<td style="text-align:right;">
1.655121e+00
</td>
</tr>
<tr>
<td style="text-align:left;width: 0.5in; ">
PTS
</td>
<td style="text-align:right;width: 0.5in; ">
0.1433704
</td>
<td style="text-align:right;">
1.154157e+00
</td>
<td style="text-align:right;">
1.0996082
</td>
<td style="text-align:right;">
1.211412e+00
</td>
</tr>
<tr>
<td style="text-align:left;width: 0.5in; ">
FGA
</td>
<td style="text-align:right;width: 0.5in; ">
-0.2959456
</td>
<td style="text-align:right;">
7.438279e-01
</td>
<td style="text-align:right;">
0.7103747
</td>
<td style="text-align:right;">
7.788564e-01
</td>
</tr>
<tr>
<td style="text-align:left;width: 0.5in; ">
`FG%`
</td>
<td style="text-align:right;width: 0.5in; ">
9.9392235
</td>
<td style="text-align:right;">
2.072764e+04
</td>
<td style="text-align:right;">
5.0745365
</td>
<td style="text-align:right;">
8.466491e+07
</td>
</tr>
<tr>
<td style="text-align:left;width: 0.5in; ">
`3PA`
</td>
<td style="text-align:right;width: 0.5in; ">
-0.0089960
</td>
<td style="text-align:right;">
9.910443e-01
</td>
<td style="text-align:right;">
0.9720811
</td>
<td style="text-align:right;">
1.010377e+00
</td>
</tr>
<tr>
<td style="text-align:left;width: 0.5in; ">
`3P%`
</td>
<td style="text-align:right;width: 0.5in; ">
2.3910118
</td>
<td style="text-align:right;">
1.092454e+01
</td>
<td style="text-align:right;">
2.9171571
</td>
<td style="text-align:right;">
4.091162e+01
</td>
</tr>
<tr>
<td style="text-align:left;width: 0.5in; ">
FTA
</td>
<td style="text-align:right;width: 0.5in; ">
-0.0824201
</td>
<td style="text-align:right;">
9.208850e-01
</td>
<td style="text-align:right;">
0.8866797
</td>
<td style="text-align:right;">
9.564098e-01
</td>
</tr>
<tr>
<td style="text-align:left;width: 0.5in; ">
`FT%`
</td>
<td style="text-align:right;width: 0.5in; ">
1.5612959
</td>
<td style="text-align:right;">
4.764992e+00
</td>
<td style="text-align:right;">
1.4869295
</td>
<td style="text-align:right;">
1.526982e+01
</td>
</tr>
<tr>
<td style="text-align:left;width: 0.5in; ">
TRB
</td>
<td style="text-align:right;width: 0.5in; ">
0.3645101
</td>
<td style="text-align:right;">
1.439808e+00
</td>
<td style="text-align:right;">
1.4148881
</td>
<td style="text-align:right;">
1.465168e+00
</td>
</tr>
<tr>
<td style="text-align:left;width: 0.5in; ">
AST
</td>
<td style="text-align:right;width: 0.5in; ">
0.0251854
</td>
<td style="text-align:right;">
1.025505e+00
</td>
<td style="text-align:right;">
1.0090126
</td>
<td style="text-align:right;">
1.042267e+00
</td>
</tr>
<tr>
<td style="text-align:left;width: 0.5in; ">
STL
</td>
<td style="text-align:right;width: 0.5in; ">
0.3952663
</td>
<td style="text-align:right;">
1.484779e+00
</td>
<td style="text-align:right;">
1.4467318
</td>
<td style="text-align:right;">
1.523828e+00
</td>
</tr>
<tr>
<td style="text-align:left;width: 0.5in; ">
BLK
</td>
<td style="text-align:right;width: 0.5in; ">
0.1230003
</td>
<td style="text-align:right;">
1.130885e+00
</td>
<td style="text-align:right;">
1.1032460
</td>
<td style="text-align:right;">
1.159216e+00
</td>
</tr>
<tr>
<td style="text-align:left;width: 0.5in; ">
TOV
</td>
<td style="text-align:right;width: 0.5in; ">
-0.3361189
</td>
<td style="text-align:right;">
7.145382e-01
</td>
<td style="text-align:right;">
0.6992791
</td>
<td style="text-align:right;">
7.301301e-01
</td>
</tr>
<tr>
<td style="text-align:left;width: 0.5in; ">
PF
</td>
<td style="text-align:right;width: 0.5in; ">
-0.0605019
</td>
<td style="text-align:right;">
9.412920e-01
</td>
<td style="text-align:right;">
0.9269811
</td>
<td style="text-align:right;">
9.558238e-01
</td>
</tr>
</tbody>
</table>

## Objective 2

Creating a complex model introducing interactions Added team and Opp
Categorical variables, largely increases complexity since they’re
categorical variables with 30 levels

``` r
complexdata <- rawdata[,c(7,2,3,5,6,8,10:25)]
complexdata <- complexdata %>%  mutate(PTSsq = PTS^2, DIV = 2*((TRB + AST + 4*STL + 2*BLK)-(4*TOV + 2*PF)))
train.complex <- complexdata[index,]
test.complex <- complexdata[-index,]

complex.mod <- glm(formula = WINorLOSS ~ Team + Home + Opp + PTS + FGA + 
                     `FG%` + `3P%` + `3PA` + FTA + `FT%` + TRB + AST + 
                     STL + BLK + TOV + PF, family = "binomial", data = train.complex)
complex.pred <- predict(complex.mod, test.complex, type = "response")
complex.auc <- performance(prediction(complex.pred, test.complex$WINorLOSS),"auc")
complex.auc <- complex.auc@y.values[[1]]
complex.pred <- ifelse(complex.pred>.5, "W", "L")
complex.pred <- factor(complex.pred, levels = c("W", "L"))
test.complex$WINorLOSS <- factor(test.complex$WINorLOSS, levels = c("W", "L"))

cm.complex <- confusionMatrix(complex.pred, test.complex$WINorLOSS)
cm.complex
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction   W   L
    ##          W 875 142
    ##          L 141 810
    ##                                           
    ##                Accuracy : 0.8562          
    ##                  95% CI : (0.8399, 0.8714)
    ##     No Information Rate : 0.5163          
    ##     P-Value [Acc > NIR] : <2e-16          
    ##                                           
    ##                   Kappa : 0.7121          
    ##                                           
    ##  Mcnemar's Test P-Value : 1               
    ##                                           
    ##             Sensitivity : 0.8612          
    ##             Specificity : 0.8508          
    ##          Pos Pred Value : 0.8604          
    ##          Neg Pred Value : 0.8517          
    ##              Prevalence : 0.5163          
    ##          Detection Rate : 0.4446          
    ##    Detection Prevalence : 0.5168          
    ##       Balanced Accuracy : 0.8560          
    ##                                           
    ##        'Positive' Class : W               
    ## 

LASSO fit

``` r
train.complex.x <- model.matrix(WINorLOSS~.-1, data = train.complex) #-1 removes intercept column
train.complex.y <- train.complex[,1]

cvfit2 <- cv.glmnet(train.complex.x, train.complex.y, family = "binomial", type.measure = "class")
plot(cvfit2)
```

![](NBAStats_files/figure-markdown_github/lasso%202-1.png)

``` r
coef(cvfit2, s= "lambda.min")
```

    ## 81 x 1 sparse Matrix of class "dgCMatrix"
    ##                        s1
    ## (Intercept) -19.792925220
    ## TeamATL       0.137218940
    ## TeamBOS       0.673583793
    ## TeamBRK      -0.724244539
    ## TeamCHI      -0.287562890
    ## TeamCHO      -0.669744709
    ## TeamCLE       0.663828737
    ## TeamDAL       0.202503300
    ## TeamDEN      -0.586904271
    ## TeamDET       .          
    ## TeamGSW       0.690145465
    ## TeamHOU       1.107667936
    ## TeamIND       .          
    ## TeamLAC       .          
    ## TeamLAL      -0.842160750
    ## TeamMEM       0.294321646
    ## TeamMIA       .          
    ## TeamMIL      -0.117555930
    ## TeamMIN      -0.891156659
    ## TeamNOP      -0.587856466
    ## TeamNYK      -0.772057910
    ## TeamOKC       0.110603422
    ## TeamORL      -0.568474844
    ## TeamPHI      -0.402024397
    ## TeamPHO      -0.258587748
    ## TeamPOR       0.104301492
    ## TeamSAC      -0.530487855
    ## TeamSAS       0.045923708
    ## TeamTOR       0.355025238
    ## TeamUTA      -0.041840013
    ## TeamWAS       .          
    ## Game         -0.009432524
    ## HomeHome      0.431091125
    ## OppBOS       -0.289040090
    ## OppBRK        0.165330897
    ## OppCHI       -0.081201686
    ## OppCHO        0.180770315
    ## OppCLE       -0.926387743
    ## OppDAL       -0.130417338
    ## OppDEN        .          
    ## OppDET        .          
    ## OppGSW       -1.584942029
    ## OppHOU       -1.092676804
    ## OppIND        0.222649895
    ## OppLAC       -0.533994196
    ## OppLAL        0.689101393
    ## OppMEM        0.370721802
    ## OppMIA        0.348699332
    ## OppMIL        0.452951321
    ## OppMIN        1.187168786
    ## OppNOP       -0.010749444
    ## OppNYK        1.118735179
    ## OppOKC        .          
    ## OppORL        0.441973834
    ## OppPHI        1.122914248
    ## OppPHO        0.777969924
    ## OppPOR       -0.413711702
    ## OppSAC        0.593960134
    ## OppSAS       -0.454511698
    ## OppTOR       -0.211553598
    ## OppUTA        0.471679352
    ## OppWAS        .          
    ## PTS           0.036248833
    ## FG            .          
    ## FGA          -0.173234353
    ## `FG%`        27.006435922
    ## `3P`          0.057245636
    ## `3PA`        -0.022461123
    ## `3P%`         3.588971856
    ## FT            .          
    ## FTA           .          
    ## `FT%`         4.036031880
    ## ORB          -0.044359946
    ## TRB           0.324475716
    ## AST           .          
    ## STL           0.188417171
    ## BLK           .          
    ## TOV          -0.128543293
    ## PF            0.022316027
    ## PTSsq         .          
    ## DIV           0.024089321

``` r
paste("optimal LASSO penalty value",sprintf("%.10f",cvfit2$lambda.min)) # this is the optimal LASSO penalty value
```

    ## [1] "optimal LASSO penalty value 0.0012378394"

``` r
lasso.mod2 <- glmnet(train.complex.x, train.complex.y, family = "binomial", lambda = cvfit2$lambda.min)

coef(lasso.mod2)
```

    ## 81 x 1 sparse Matrix of class "dgCMatrix"
    ##                        s0
    ## (Intercept) -19.765503398
    ## TeamATL       0.138843618
    ## TeamBOS       0.674114283
    ## TeamBRK      -0.723874762
    ## TeamCHI      -0.287170475
    ## TeamCHO      -0.669902144
    ## TeamCLE       0.663184779
    ## TeamDAL       0.202027918
    ## TeamDEN      -0.585958275
    ## TeamDET       .          
    ## TeamGSW       0.694611067
    ## TeamHOU       1.109554266
    ## TeamIND       .          
    ## TeamLAC       .          
    ## TeamLAL      -0.842649690
    ## TeamMEM       0.294494008
    ## TeamMIA       .          
    ## TeamMIL      -0.115645937
    ## TeamMIN      -0.890510829
    ## TeamNOP      -0.585978604
    ## TeamNYK      -0.770690847
    ## TeamOKC       0.110779309
    ## TeamORL      -0.568080055
    ## TeamPHI      -0.399316909
    ## TeamPHO      -0.258959999
    ## TeamPOR       0.103958420
    ## TeamSAC      -0.530637366
    ## TeamSAS       0.047464743
    ## TeamTOR       0.355009552
    ## TeamUTA      -0.040874004
    ## TeamWAS       .          
    ## Game         -0.009436784
    ## HomeHome      0.431831662
    ## OppBOS       -0.289271480
    ## OppBRK        0.163781705
    ## OppCHI       -0.082051314
    ## OppCHO        0.180141581
    ## OppCLE       -0.927709729
    ## OppDAL       -0.132464006
    ## OppDEN        .          
    ## OppDET        .          
    ## OppGSW       -1.587186729
    ## OppHOU       -1.092770028
    ## OppIND        0.222193291
    ## OppLAC       -0.534882451
    ## OppLAL        0.688998181
    ## OppMEM        0.370973058
    ## OppMIA        0.346946902
    ## OppMIL        0.453495617
    ## OppMIN        1.187320707
    ## OppNOP       -0.012046790
    ## OppNYK        1.117309363
    ## OppOKC        .          
    ## OppORL        0.441923962
    ## OppPHI        1.122521952
    ## OppPHO        0.776905138
    ## OppPOR       -0.416111931
    ## OppSAC        0.594210580
    ## OppSAS       -0.455888914
    ## OppTOR       -0.211454982
    ## OppUTA        0.470459566
    ## OppWAS        .          
    ## PTS           0.036204065
    ## FG            .          
    ## FGA          -0.173137492
    ## `FG%`        27.034563285
    ## `3P`          0.062202287
    ## `3PA`        -0.024155932
    ## `3P%`         3.479983469
    ## FT            .          
    ## FTA           .          
    ## `FT%`         4.037062941
    ## ORB          -0.044493025
    ## TRB           0.325188441
    ## AST           .          
    ## STL           0.190622203
    ## BLK           .          
    ## TOV          -0.130633763
    ## PF            0.021295021
    ## PTSsq         .          
    ## DIV           0.023854409

``` r
test.complex.x <- model.matrix(WINorLOSS~.-1, data = test.complex)
lasso.pred2 <- predict(lasso.mod2, newx = test.complex.x, type = "response")
lasso.auc2 <- performance(prediction(lasso.pred2,test.complex$WINorLOSS),"auc")
lasso.auc2 <- lasso.auc2@y.values[[1]]
lasso.pred2 <- ifelse(lasso.pred2>.5, "W", "L")
lasso.pred2 <- factor(lasso.pred2, levels = c("W","L"))

cm.lasso2 <- confusionMatrix(data = lasso.pred2, reference = test.complex$WINorLOSS)
cm.lasso2
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction   W   L
    ##          W 870 143
    ##          L 146 809
    ##                                           
    ##                Accuracy : 0.8532          
    ##                  95% CI : (0.8367, 0.8685)
    ##     No Information Rate : 0.5163          
    ##     P-Value [Acc > NIR] : <2e-16          
    ##                                           
    ##                   Kappa : 0.706           
    ##                                           
    ##  Mcnemar's Test P-Value : 0.9063          
    ##                                           
    ##             Sensitivity : 0.8563          
    ##             Specificity : 0.8498          
    ##          Pos Pred Value : 0.8588          
    ##          Neg Pred Value : 0.8471          
    ##              Prevalence : 0.5163          
    ##          Detection Rate : 0.4421          
    ##    Detection Prevalence : 0.5147          
    ##       Balanced Accuracy : 0.8530          
    ##                                           
    ##        'Positive' Class : W               
    ## 

Create QDA LDA models

``` r
#quick assumption check of equal covariances by using ellispes plot
covEllipses(train.num[,2:16], train.num$WINorLOSS, fill = TRUE, pooled = FALSE, variables = 1:15, col = c("#277f8e","#440154"), fill.alpha = .05)
```

![](NBAStats_files/figure-markdown_github/LDA%20QDA-1.png)

``` r
lda.mod <- lda(WINorLOSS~., data=train.num, prior = c(.5,.5))
lda.pred <-  predict(lda.mod, test.num)
lda.auc <- performance(prediction(lda.pred$posterior[,2],test.num$WINorLOSS),"auc")
lda.auc <- lda.auc@y.values[[1]]

cm.lda <- confusionMatrix(lda.pred$class, test.num$WINorLOSS)
cm.lda
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction   L   W
    ##          L 792 163
    ##          W 160 853
    ##                                          
    ##                Accuracy : 0.8359         
    ##                  95% CI : (0.8188, 0.852)
    ##     No Information Rate : 0.5163         
    ##     P-Value [Acc > NIR] : <2e-16         
    ##                                          
    ##                   Kappa : 0.6714         
    ##                                          
    ##  Mcnemar's Test P-Value : 0.9114         
    ##                                          
    ##             Sensitivity : 0.8319         
    ##             Specificity : 0.8396         
    ##          Pos Pred Value : 0.8293         
    ##          Neg Pred Value : 0.8421         
    ##              Prevalence : 0.4837         
    ##          Detection Rate : 0.4024         
    ##    Detection Prevalence : 0.4853         
    ##       Balanced Accuracy : 0.8357         
    ##                                          
    ##        'Positive' Class : L              
    ## 

``` r
qda.mod <- qda(WINorLOSS~., data=train.num, prior = c(.5,.5))
qda.pred <- predict(qda.mod, test.num)
qda.auc <- performance(prediction(qda.pred$posterior[,2],test.num$WINorLOSS),"auc")
qda.auc <- qda.auc@y.values[[1]]

cm.qda <- confusionMatrix(qda.pred$class, test.num$WINorLOSS)
cm.qda
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction   L   W
    ##          L 783 172
    ##          W 169 844
    ##                                           
    ##                Accuracy : 0.8267          
    ##                  95% CI : (0.8093, 0.8432)
    ##     No Information Rate : 0.5163          
    ##     P-Value [Acc > NIR] : <2e-16          
    ##                                           
    ##                   Kappa : 0.6531          
    ##                                           
    ##  Mcnemar's Test P-Value : 0.9138          
    ##                                           
    ##             Sensitivity : 0.8225          
    ##             Specificity : 0.8307          
    ##          Pos Pred Value : 0.8199          
    ##          Neg Pred Value : 0.8332          
    ##              Prevalence : 0.4837          
    ##          Detection Rate : 0.3979          
    ##    Detection Prevalence : 0.4853          
    ##       Balanced Accuracy : 0.8266          
    ##                                           
    ##        'Positive' Class : L               
    ## 

## random forests classification

finding the tuning parameter mtry takes the longest from running this
code it can be skipped to run this much faster

``` r
train.expl <- train.complex[,2:24]
train.resp <- train.complex[,1]

train(train.expl, train.resp, method = "rf") # this takes 10+ minutes to run, # returns ideal mtry=12
```

    ## Random Forest 
    ## 
    ## 7872 samples
    ##   23 predictor
    ##    2 classes: 'L', 'W' 
    ## 
    ## No pre-processing
    ## Resampling: Bootstrapped (25 reps) 
    ## Summary of sample sizes: 7872, 7872, 7872, 7872, 7872, 7872, ... 
    ## Resampling results across tuning parameters:
    ## 
    ##   mtry  Accuracy   Kappa    
    ##    2    0.7986088  0.5978322
    ##   12    0.8134209  0.6271286
    ##   23    0.8099719  0.6201235
    ## 
    ## Accuracy was used to select the optimal model using the largest value.
    ## The final value used for the model was mtry = 12.

``` r
rfdata <- complexdata %>% rename("FGp" = "FG%", "3P","ThP" = "3P", "ThPA" = "3PA", "ThPp" = "3P%", "FTp" = "FT%")
train.rf <- rfdata[index,]
test.rf <- rfdata[-index,]
rf.mod <- randomForest(WINorLOSS~., data = train.rf, mtry = 12, ntree =1000, cutoff = c(.45,.55))
rf.pred <- predict(rf.mod, test.rf)
rf.auc <- performance(prediction(predict(rf.mod, test.rf, type = "prob")[,2],test.rf$WINorLOSS),"auc", x.measure = "cutoff")
rf.auc <- rf.auc@y.values[[1]]
rf.pred <- factor(rf.pred, levels = c("W", "L"))
test.rf$WINorLOSS <- factor(test.rf$WINorLOSS, levels = c("W","L"))

cm.rf <- confusionMatrix(rf.pred, test.rf$WINorLOSS)
cm.rf
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction   W   L
    ##          W 847 162
    ##          L 169 790
    ##                                           
    ##                Accuracy : 0.8318          
    ##                  95% CI : (0.8145, 0.8481)
    ##     No Information Rate : 0.5163          
    ##     P-Value [Acc > NIR] : <2e-16          
    ##                                           
    ##                   Kappa : 0.6633          
    ##                                           
    ##  Mcnemar's Test P-Value : 0.7416          
    ##                                           
    ##             Sensitivity : 0.8337          
    ##             Specificity : 0.8298          
    ##          Pos Pred Value : 0.8394          
    ##          Neg Pred Value : 0.8238          
    ##              Prevalence : 0.5163          
    ##          Detection Rate : 0.4304          
    ##    Detection Prevalence : 0.5127          
    ##       Balanced Accuracy : 0.8317          
    ##                                           
    ##        'Positive' Class : W               
    ## 

Update table with Obj 2 models

``` r
cm.df <- data.frame("Model" = c("Simple", "Forward", "Stepwise", "Backward", "Custom", "LASSO", "Complex", "LASSO 2", "LDA", "QDA", "RandomForests"),
           "Accuracy"= c(cm.simple$overall[1],cm.fwd$overall[1], cm.step$overall[1],cm.bkw$overall[1], cm.custom$overall[1],cm.lasso$overall[1],cm.complex$overall[1], cm.lasso2$overall[1], cm.lda$overall[1], cm.qda$overall[1], cm.rf$overall[1]),
           "Sensitivity"=c(cm.simple$byClass[1],cm.fwd$byClass[1], cm.step$byClass[1], cm.bkw$byClass[1], cm.custom$byClass[1], cm.lasso$byClass[1], cm.complex$byClass[1], cm.lasso2$byClass[1], cm.lda$byClass[1], cm.qda$byClass[1], cm.rf$byClass[1]),
           "Specificty" = c(cm.simple$byClass[2],cm.fwd$byClass[2], cm.step$byClass[2],cm.bkw$byClass[2], cm.custom$byClass[2], cm.lasso$byClass[2], cm.complex$byClass[2], cm.lasso2$byClass[2], cm.lda$byClass[2], cm.qda$byClass[2], cm.rf$byClass[2]),
           "AUC" = c(simple.auc, fwd.auc, step.auc, bkw.auc, custom.auc, lasso.auc, complex.auc, lasso.auc2, lda.auc, qda.auc, rf.auc))
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
<th style="text-align:right;font-style: italic;color: white !important;background-color: #21918c !important;">
AUC
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;width: 0.5in; ">
Simple
</td>
<td style="text-align:right;width: 0.5in; ">
0.4837398
</td>
<td style="text-align:right;">
0.0000000
</td>
<td style="text-align:right;">
1.0000000
</td>
<td style="text-align:right;">
0.5000000
</td>
</tr>
<tr>
<td style="text-align:left;width: 0.5in; ">
Forward
</td>
<td style="text-align:right;width: 0.5in; ">
0.8414634
</td>
<td style="text-align:right;">
0.8543307
</td>
<td style="text-align:right;">
0.8277311
</td>
<td style="text-align:right;">
0.9260715
</td>
</tr>
<tr>
<td style="text-align:left;width: 0.5in; ">
Stepwise
</td>
<td style="text-align:right;width: 0.5in; ">
0.8414634
</td>
<td style="text-align:right;">
0.8543307
</td>
<td style="text-align:right;">
0.8277311
</td>
<td style="text-align:right;">
0.9260715
</td>
</tr>
<tr>
<td style="text-align:left;width: 0.5in; ">
Backward
</td>
<td style="text-align:right;width: 0.5in; ">
0.8404472
</td>
<td style="text-align:right;">
0.8533465
</td>
<td style="text-align:right;">
0.8266807
</td>
<td style="text-align:right;">
0.9260415
</td>
</tr>
<tr>
<td style="text-align:left;width: 0.5in; ">
Custom
</td>
<td style="text-align:right;width: 0.5in; ">
0.8480691
</td>
<td style="text-align:right;">
0.8592520
</td>
<td style="text-align:right;">
0.8361345
</td>
<td style="text-align:right;">
0.9273349
</td>
</tr>
<tr>
<td style="text-align:left;width: 0.5in; ">
LASSO
</td>
<td style="text-align:right;width: 0.5in; ">
0.8414634
</td>
<td style="text-align:right;">
0.8523622
</td>
<td style="text-align:right;">
0.8298319
</td>
<td style="text-align:right;">
0.9257624
</td>
</tr>
<tr>
<td style="text-align:left;width: 0.5in; ">
Complex
</td>
<td style="text-align:right;width: 0.5in; ">
0.8561992
</td>
<td style="text-align:right;">
0.8612205
</td>
<td style="text-align:right;">
0.8508403
</td>
<td style="text-align:right;">
0.9324940
</td>
</tr>
<tr>
<td style="text-align:left;width: 0.5in; ">
LASSO 2
</td>
<td style="text-align:right;width: 0.5in; ">
0.8531504
</td>
<td style="text-align:right;">
0.8562992
</td>
<td style="text-align:right;">
0.8497899
</td>
<td style="text-align:right;">
0.9317423
</td>
</tr>
<tr>
<td style="text-align:left;width: 0.5in; ">
LDA
</td>
<td style="text-align:right;width: 0.5in; ">
0.8358740
</td>
<td style="text-align:right;">
0.8319328
</td>
<td style="text-align:right;">
0.8395669
</td>
<td style="text-align:right;">
0.9257355
</td>
</tr>
<tr>
<td style="text-align:left;width: 0.5in; ">
QDA
</td>
<td style="text-align:right;width: 0.5in; ">
0.8267276
</td>
<td style="text-align:right;">
0.8224790
</td>
<td style="text-align:right;">
0.8307087
</td>
<td style="text-align:right;">
0.9100361
</td>
</tr>
<tr>
<td style="text-align:left;width: 0.5in; ">
RandomForests
</td>
<td style="text-align:right;width: 0.5in; ">
0.8318089
</td>
<td style="text-align:right;">
0.8336614
</td>
<td style="text-align:right;">
0.8298319
</td>
<td style="text-align:right;">
0.9071670
</td>
</tr>
</tbody>
</table>

New ROC with Obj 2 models Might Add LDA and QDA to this Need to add
Legend

``` r
complex.pred.roc <- prediction(predict(complex.mod, newdata = test.complex, type = "response"), test.complex$WINorLOSS)
complex.roc <- performance(complex.pred.roc, "tpr", "fpr")
lasso2.pred.roc <- prediction(predict(lasso.mod2, newx = test.complex.x, type = "response"), test.complex$WINorLOSS)
lasso2.roc <- performance(lasso2.pred.roc, "tpr", "fpr")
lda.pred.roc <- prediction(lda.pred$posterior[,2],test.num$WINorLOSS)
lda.roc <- performance(lda.pred.roc, "tpr", "fpr")
qda.pred.roc <- prediction(qda.pred$posterior[,2], test.num$WINorLOSS)
qda.roc <- performance(qda.pred.roc, "tpr", "fpr")
rf.pred.roc <- prediction(predict(rf.mod, test.rf, type = "prob")[,2],test.rf$WINorLOSS)
rf.roc <- performance(rf.pred.roc, "tpr", "fpr")

df.roc <- rbind(data.frame(x.vals=lasso2.roc@x.values[[1]], y.vals=lasso2.roc@y.values[[1]],model="lasso2"),
                data.frame(x.vals=lda.roc@x.values[[1]], y.vals=lda.roc@y.values[[1]], model="lda"),
                data.frame(x.vals=qda.roc@x.values[[1]], y.vals=qda.roc@y.values[[1]], model="qda"),
                data.frame(x.vals=rf.roc@x.values[[1]], y.vals=rf.roc@y.values[[1]], model = "rf"),
                data.frame(x.vals=complex.roc@x.values[[1]], y.vals=complex.roc@y.values[[1]], model="complex"),
                df.roc)
df.roc$model <- factor(df.roc$model, levels = c("simple", "bkw", "step", "lasso", "custom", "rf", "lda", "qda", "lasso2", "complex"))

df.roc %>% filter(model %in% c("custom", "rf", "lda", "qda", "lasso2", "complex")) %>% 
  ggplot(aes(x=x.vals, y=y.vals, color = model))+geom_line()+theme_cowplot()+scale_color_viridis_d(direction = -1)+labs(x="False positive rate", y="True positive rate")
```

![](NBAStats_files/figure-markdown_github/new%20roc%20plot-1.png)

``` r
plot(custom.roc, col="#fde725", lwd=1)
plot(lasso2.roc, lwd = 1, add = TRUE, col = "#3b528b")
plot(lda.roc, lwd = 1, add = TRUE, col = "#21918c")
plot(qda.roc, lwd = 1, add = TRUE, col = "#7ad151")
plot(rf.roc, lwd = 1, add = TRUE, col = "#414487")
plot(complex.roc, col = "#440154", lwd = 1, add = TRUE)
legend(x = .75, y = .50, legend = c("simple", "backwards", "stepwise", "lasso", "custom","complex"), col = c("#fde725", "#3b528b", "#21918c", "#7ad151", "#414487", "440154"), lty =1)
```

![](NBAStats_files/figure-markdown_github/new%20roc%20plot-2.png)
