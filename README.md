---
title: "rhodyRstats: Basic Statistics with R"
---

# Introduction
This notebook will cover calculating basic statistics with R, conducting statistical tests, and building simple linear models.  We will use the 2007 NLA data for the examples and show steps from getting data, to cleaning data, to analysis and statistics.

## Get Data
First step in any project will be getting the data read into R.  For this lesson we are using the 2007 National Lakes Assessment data, which ,luckily, can be accessed directly from a URL.  


```r
# URL for 2007 NLA water quality data
nla_wq_url <- "https://www.epa.gov/sites/production/files/2014-10/nla2007_chemical_conditionestimates_20091123.csv"

nla_secchi_url <- "https://www.epa.gov/sites/production/files/2014-10/nla2007_secchi_20091008.csv"

# Read into an R data.frame with read.csv
nla_wq <- read.csv(nla_wq_url, stringsAsFactors = FALSE)
nla_secchi <- read.csv(nla_secchi_url, stringsAsFactor = FALSE)
```


## Clean Data
So this dataset is a bit bigger than we probably want, let's do some clean up using `dplyr`.  We want to select out a few columns, filter out the data that we want and get our data.frame ready for futher analysis.


```r
#Load dplyr into current session
library(dplyr)

#Clean up NLA Water quality
nla_wq_cln <- nla_wq %>%
  filter(VISIT_NO == 1,
         SITE_TYPE == "PROB_Lake") %>%
  select(SITE_ID,ST,EPA_REG,RT_NLA,LAKE_ORIGIN,PTL,NTL,TURB,CHLA)

#Clean up NLA Secchi
nla_secchi_cln <- nla_secchi %>%
  filter(VISIT_NO == 1) %>%
  select(SITE_ID, SECMEAN)

#Join the two together based on SITE_ID and the finally filter out NA's
nla <- left_join(x = nla_wq_cln, y = nla_secchi_cln, by = "SITE_ID") %>%
  filter(complete.cases(NTL,PTL,TURB,CHLA,SECMEAN))
tbl_df(nla)
```

```
## Source: local data frame [974 x 10]
## 
##          SITE_ID    ST  EPA_REG RT_NLA LAKE_ORIGIN   PTL   NTL   TURB
##            <chr> <chr>    <chr>  <chr>       <chr> <int> <int>  <dbl>
## 1  NLA06608-0001    MT Region_8    REF     NATURAL     6   151  0.474
## 2  NLA06608-0002    SC Region_4  SO-SO    MAN-MADE    36   695  3.550
## 3  NLA06608-0003    TX Region_6  TRASH     NATURAL    43   738  7.670
## 4  NLA06608-0004    CO Region_8  SO-SO    MAN-MADE    18   344  3.810
## 5  NLA06608-0006    CT Region_1    REF    MAN-MADE     7   184  0.901
## 6  NLA06608-0007    WI Region_5    REF     NATURAL     8   493  1.050
## 7  NLA06608-0008    IA Region_7  SO-SO    MAN-MADE    66   801  8.620
## 8  NLA06608-0010    MI Region_5  SO-SO     NATURAL    10   473  3.050
## 9  NLA06608-0012    OK Region_6  TRASH    MAN-MADE   159  1026 50.300
## 10 NLA06608-0013    NJ Region_2  SO-SO    MAN-MADE    28   384  4.210
## ..           ...   ...      ...    ...         ...   ...   ...    ...
## Variables not shown: CHLA <dbl>, SECMEAN <dbl>.
```

So now we have a dataset ready for analysis.

## Analyze Data

### Basic Stats
First step in analyzing a dataset like this is going to be to dig through some basic statistics as well as some basic plots.  

We can get a summary of the full data frame:


```r
#Get a summary of the data frame
summary(nla)
```

```
##    SITE_ID               ST              EPA_REG         
##  Length:974         Length:974         Length:974        
##  Class :character   Class :character   Class :character  
##  Mode  :character   Mode  :character   Mode  :character  
##                                                          
##                                                          
##                                                          
##     RT_NLA          LAKE_ORIGIN             PTL              NTL         
##  Length:974         Length:974         Min.   :   1.0   Min.   :    5.0  
##  Class :character   Class :character   1st Qu.:  11.0   1st Qu.:  329.5  
##  Mode  :character   Mode  :character   Median :  30.0   Median :  603.5  
##                                        Mean   : 114.1   Mean   : 1190.5  
##                                        3rd Qu.: 100.0   3rd Qu.: 1214.2  
##                                        Max.   :4679.0   Max.   :26100.0  
##       TURB              CHLA            SECMEAN       
##  Min.   :  0.237   Min.   :  0.070   Min.   : 0.0400  
##  1st Qu.:  1.643   1st Qu.:  3.163   1st Qu.: 0.6125  
##  Median :  4.145   Median :  8.670   Median : 1.3000  
##  Mean   : 14.133   Mean   : 30.884   Mean   : 2.0759  
##  3rd Qu.: 11.675   3rd Qu.: 27.492   3rd Qu.: 2.7475  
##  Max.   :574.000   Max.   :936.000   Max.   :36.7100
```

Or, we can pick and choose what stats we want.  For instance:


```r
#Stats for Total Nitrogen
mean(nla$NTL)
```

```
## [1] 1190.468
```

```r
median(nla$NTL)
```

```
## [1] 603.5
```

```r
min(nla$NTL)
```

```
## [1] 5
```

```r
max(nla$NTL)
```

```
## [1] 26100
```

```r
sd(nla$NTL)
```

```
## [1] 2122.182
```

```r
IQR(nla$NTL)
```

```
## [1] 884.75
```

```r
range(nla$NTL)
```

```
## [1]     5 26100
```

In these cases we took care of our NA values during our data clean up, but there may be reasons you would not want to do that.  If you retained NA values, you would need to think about how to handle those.  One way is to remove it from the calculation of the statistics using the `na.rm = TRUE` argument.  For instance:


```r
#An example with NA's
x <- c(37,22,NA,41,19)
mean(x) #Returns NA
```

```
## [1] NA
```

```r
mean(x, na.rm = TRUE) #Returns mean of 37, 22, 41, and 19
```

```
## [1] 29.75
```

### Some quick useful viz

While visualization isn't the point of this lesson, some things are useful to do at this stage of analysis.  In particular is looking at distributions and some basic scatterplots.

We can look at histograms and density:


```r
#A single histogram using base
hist(nla$NTL)
```

![plot of chunk histogram_density](figure/histogram_density-1.png)

```r
#Log transform it
hist(log1p(nla$NTL)) #log1p adds one to deal with zeros
```

![plot of chunk histogram_density](figure/histogram_density-2.png)

```r
#Density plot
plot(density(log1p(nla$NTL)))
```

![plot of chunk histogram_density](figure/histogram_density-3.png)


And boxplots:


```r
#Simple boxplots
boxplot(nla$CHLA)
```

![plot of chunk boxplots](figure/boxplots-1.png)

```r
boxplot(log1p(nla$CHLA))
```

![plot of chunk boxplots](figure/boxplots-2.png)

```r
#Boxplots per group
boxplot(log1p(nla$CHLA)~nla$EPA_REG)
```

![plot of chunk boxplots](figure/boxplots-3.png)

And scatterplots:


```r
#A single scatterplot
plot(log1p(nla$PTL),log1p(nla$CHLA))
```

![plot of chunk scatterplots](figure/scatterplots-1.png)

```r
#A matrix of scatterplot
plot(log1p(nla[,6:10]))
```

![plot of chunk scatterplots](figure/scatterplots-2.png)


Lastly, it might be nice to look at these on a per variable basis or on some grouping variable. First we could look at the density of each measured variable. This requires some manipulation of the data which will allow us to use facets in ggplot to create a density distribution for each of the variables.


```r
#Getting super fancy with tidyr, plotly, and ggplot2 to visualize all variables
library(tidyr)
library(ggplot2)
library(plotly)
nla_gather <- gather(nla,parameter,value,6:10)
dens_gg <-ggplot(nla_gather,aes(x=log1p(value))) +
  geom_density() +
  facet_wrap("parameter") +
  labs(x="log1p of measured value")
ggplotly(dens_gg)
```

```
## Error in file(con, "rb"): cannot open the connection
```

Next we could look at a scatterplot matrix of the relationship between phosphorus and chlorophyl by each EPA Region.  No need to re-do the shape of the data frame for this one.


```r
ggplot(nla, aes(x=log1p(PTL),y=log1p(NTL))) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap("EPA_REG")
```

![plot of chunk fancy_matrix](figure/fancy_matrix-1.png)

## Some tests: t-test and ANOVA
There are way more tests than we can show examples for.  For today we will show two very common and straightforward tests.  The t-test and an ANOVA.

### t-test
First we will look at the t-test to test and see if `LAKE_ORIGIN` shows a difference in `SECMEAN`.  In other words can we expect a difference in clarity due to whether a lake is man-made or natural.  This is a two-tailed test. There are two approaches for this 1) using the formula notation if your dataset is in a "long" format or 2) using two separate vectors if your dataset is in a "wide" format.


```r
#Long Format - original format for LAKE_ORIGIN and SECMEAN
t.test(nla$SECMEAN ~ nla$LAKE_ORIGIN)
```

```
## 
## 	Welch Two Sample t-test
## 
## data:  nla$SECMEAN by nla$LAKE_ORIGIN
## t = -4.7252, df = 611.31, p-value = 2.854e-06
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -1.0972582 -0.4529701
## sample estimates:
## mean in group MAN-MADE  mean in group NATURAL 
##               1.752817               2.527931
```

```r
#Wide Format - need to do some work to get there - tidyr is handy!
wide_nla <- spread(nla,LAKE_ORIGIN,SECMEAN)
names(wide_nla)[9:10]<-c("man_made", "natural")
t.test(wide_nla$man_made, wide_nla$natural)
```

```
## 
## 	Welch Two Sample t-test
## 
## data:  wide_nla$man_made and wide_nla$natural
## t = -4.7252, df = 611.31, p-value = 2.854e-06
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -1.0972582 -0.4529701
## sample estimates:
## mean of x mean of y 
##  1.752817  2.527931
```

Same results, two different ways to approach.  Take a look at the help (e.g. `?t.test`) for more details on other types of t-tests (e.g. paired, one-tailed, etc.)

### ANOVA
ANOVA can get involved quickly and I haven't done them since my last stats class, so I'm not the best to talk about these, but the very basics require fitting a model and wrapping that ins `aov` function.  In the [Getting More Help section](#getting-more-help) I provide a link that would be a good first start for you ANOVA junkies.  For todays lesson though, lets look at the simple case of a one-vay analysis of variance and check if reference class results in differences in our chlorophyll


```r
# A quick visual of this:
boxplot(log1p(nla$CHLA)~nla$RT_NLA)
```

![plot of chunk simple_anova](figure/simple_anova-1.png)

```r
# One way analysis of variance
nla_anova <- aov(log1p(CHLA)~RT_NLA, data=nla)
nla_anova #Terms
```

```
## Call:
##    aov(formula = log1p(CHLA) ~ RT_NLA, data = nla)
## 
## Terms:
##                    RT_NLA Residuals
## Sum of Squares   151.9282 1508.3926
## Deg. of Freedom         2       971
## 
## Residual standard error: 1.246372
## Estimated effects may be unbalanced
```

```r
summary(nla_anova) #The table
```

```
##              Df Sum Sq Mean Sq F value Pr(>F)    
## RT_NLA        2  151.9   75.96    48.9 <2e-16 ***
## Residuals   971 1508.4    1.55                   
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
anova(nla_anova) #The table with a bit more
```

```
## Analysis of Variance Table
## 
## Response: log1p(CHLA)
##            Df  Sum Sq Mean Sq F value    Pr(>F)    
## RT_NLA      2  151.93  75.964  48.901 < 2.2e-16 ***
## Residuals 971 1508.39   1.553                      
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```


## Correlations and Linear modeling
The last bit of basic stats we will cover is going to be linear relationships.

### Correlations
Let's first take a look at correlations.  These can be done with `cor()`.


```r
#For a pair
cor(log1p(nla$PTL),log1p(nla$NTL))
```

```
## [1] 0.8065128
```

```r
#For a correlation matrix
cor(log1p(nla[,6:10]))
```

```
##                PTL        NTL       TURB       CHLA    SECMEAN
## PTL      1.0000000  0.8065128  0.8019849  0.7204703 -0.7548438
## NTL      0.8065128  1.0000000  0.6995560  0.7342557 -0.6992012
## TURB     0.8019849  0.6995560  1.0000000  0.7225992 -0.8435743
## CHLA     0.7204703  0.7342557  0.7225992  1.0000000 -0.7823140
## SECMEAN -0.7548438 -0.6992012 -0.8435743 -0.7823140  1.0000000
```

```r
#Spearman Rank Correlations
cor(log1p(nla[,6:10]),method = "spearman")
```

```
##                PTL        NTL       TURB       CHLA    SECMEAN
## PTL      1.0000000  0.8185463  0.8367840  0.7564151 -0.8199255
## NTL      0.8185463  1.0000000  0.7218904  0.7208925 -0.7176582
## TURB     0.8367840  0.7218904  1.0000000  0.7852845 -0.9305093
## CHLA     0.7564151  0.7208925  0.7852845  1.0000000 -0.8151644
## SECMEAN -0.8199255 -0.7176582 -0.9305093 -0.8151644  1.0000000
```

You can also test for differences using:


```r
cor.test(log1p(nla$PTL),log1p(nla$NTL))
```

```
## 
## 	Pearson's product-moment correlation
## 
## data:  log1p(nla$PTL) and log1p(nla$NTL)
## t = 42.53, df = 972, p-value < 2.2e-16
## alternative hypothesis: true correlation is not equal to 0
## 95 percent confidence interval:
##  0.7833848 0.8274104
## sample estimates:
##       cor 
## 0.8065128
```

### Linear models
Basic linear models in R can be built with the `lm()` function.  If you aren't buiding stadard least squares regressin models, (e.g. logistic) or aren't doing linear models then you will need to look elsewhere (e.g `glm()`, or `nls()`).  For today our focus is going to be on simple linear models.  Let's look at our ability to model chlorophyll, given the other variables we have.


```r
# The simplest case
chla_tp <- lm(log1p(CHLA) ~ log1p(PTL), data=nla) #Creates the model
summary(chla_tp) #Basic Summary
```

```
## 
## Call:
## lm(formula = log1p(CHLA) ~ log1p(PTL), data = nla)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -5.1824 -0.4899 -0.0176  0.5734  2.7511 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  0.20573    0.07589   2.711  0.00683 ** 
## log1p(PTL)   0.63607    0.01964  32.390  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.9064 on 972 degrees of freedom
## Multiple R-squared:  0.5191,	Adjusted R-squared:  0.5186 
## F-statistic:  1049 on 1 and 972 DF,  p-value: < 2.2e-16
```

```r
names(chla_tp) #The bits
```

```
##  [1] "coefficients"  "residuals"     "effects"       "rank"         
##  [5] "fitted.values" "assign"        "qr"            "df.residual"  
##  [9] "xlevels"       "call"          "terms"         "model"
```

```r
chla_tp$coefficients #My preference
```

```
## (Intercept)  log1p(PTL) 
##   0.2057317   0.6360718
```

```r
coef(chla_tp) #Same thing, but from a function
```

```
## (Intercept)  log1p(PTL) 
##   0.2057317   0.6360718
```

```r
resid(chla_tp) # The resdiuals
```

```
##            1            2            3            4            5 
## -1.228358845 -0.925619933  0.275399159 -0.355839624  0.096905488 
##            6            7            8            9           10 
## -0.370763976  0.561481345 -0.048276826 -1.658954141  0.487402969 
##           11           12           13           14           15 
## -1.217263322 -0.151052130  1.195587105 -2.843792719  0.665892171 
##           16           17           18           19           20 
## -0.337518208 -0.507499578  1.324883340 -0.898056078  1.108580759 
##           21           22           23           24           25 
## -0.595581274 -0.595088517  0.200034368  0.271198340  0.443901504 
##           26           27           28           29           30 
## -0.015351293 -0.803026188  0.187518185  0.640375374  0.297429391 
##           31           32           33           34           35 
##  0.800297555 -1.208491296 -0.408471158 -0.386757516  1.205883389 
##           36           37           38           39           40 
## -0.607155041 -2.248570056 -2.057806309 -0.195547451 -0.516804862 
##           41           42           43           44           45 
##  0.021206302 -0.422702315  1.255236041 -1.081262091 -5.182432056 
##           46           47           48           49           50 
##  0.221688563 -0.935480873 -0.020489389  0.944024240 -0.164119079 
##           51           52           53           54           55 
## -2.427324972  0.814724962  0.029358512 -0.107346706  1.309624946 
##           56           57           58           59           60 
##  0.279296373 -0.115994819  0.141834290  0.514924774 -0.697973498 
##           61           62           63           64           65 
## -0.818376267  0.005798936  1.335976486  0.068369162 -0.491004001 
##           66           67           68           69           70 
## -0.961870073 -2.602670108 -0.134872068  1.272295993 -0.176816668 
##           71           72           73           74           75 
## -2.590267271 -0.987744885  0.130073352 -0.568029769 -0.281660336 
##           76           77           78           79           80 
## -0.241646154  0.175918506  0.493541385  1.767544600 -0.669744154 
##           81           82           83           84           85 
##  0.238225540 -0.953546705 -0.192418531  0.213516302 -0.347307260 
##           86           87           88           89           90 
## -0.033574885  1.168127886 -0.587162947  1.298885428 -0.658485449 
##           91           92           93           94           95 
##  2.135154875  0.294119232 -1.170597884  1.479832993 -0.063539594 
##           96           97           98           99          100 
##  0.263022771 -0.313880758  0.078228774 -0.641602785  0.622904455 
##          101          102          103          104          105 
## -0.576369737  0.538280497  0.590419873  0.527444276 -0.188159897 
##          106          107          108          109          110 
##  0.109498909 -1.282432232  0.375834186  0.178906508 -0.499727757 
##          111          112          113          114          115 
## -0.075643524  0.964395992  0.089320144  0.752982171 -0.002737443 
##          116          117          118          119          120 
##  0.075639738  1.061254631 -1.289812140  1.479691356  1.533120912 
##          121          122          123          124          125 
##  2.257850026  0.950743424  0.458509158  0.490987477 -0.870455699 
##          126          127          128          129          130 
## -0.598177958  0.326746848 -1.390223681  0.296606569 -0.173842311 
##          131          132          133          134          135 
##  0.571427485 -0.147123954 -1.205071506 -0.796843882 -0.626941666 
##          136          137          138          139          140 
## -0.789011976 -0.491668888 -0.023386976 -0.488847533  0.454224295 
##          141          142          143          144          145 
##  0.220767577  0.429420970  0.006021737  0.118844772 -0.392602932 
##          146          147          148          149          150 
## -0.783544405  0.021056542  0.300326242 -1.854399809 -0.436482473 
##          151          152          153          154          155 
## -0.606184727  1.154499414  0.080045725  0.634893062 -0.063663772 
##          156          157          158          159          160 
##  0.088894829  0.246855356 -0.176037970 -1.837574551 -0.353966871 
##          161          162          163          164          165 
##  0.740706108  0.559347737 -1.793137870  0.306491052 -0.514165675 
##          166          167          168          169          170 
##  0.323457050  1.452455667 -0.089317571  0.520575665  0.524408513 
##          171          172          173          174          175 
##  0.087941943 -0.306753634 -0.410556290  1.143492608 -0.530054516 
##          176          177          178          179          180 
##  2.171588598  1.758481491 -1.142424231  0.220560168 -0.170893113 
##          181          182          183          184          185 
## -1.433838417 -0.047915156 -1.846083085  0.161560757 -0.389832052 
##          186          187          188          189          190 
##  0.596207504  0.507362645 -0.247103446  0.835915997  1.250876168 
##          191          192          193          194          195 
## -0.038150624  0.017026809  0.226556264  0.442889281  0.461419728 
##          196          197          198          199          200 
##  0.220021739  1.346305889  0.361334850 -0.582343661 -0.127829277 
##          201          202          203          204          205 
##  0.214757852  1.172416594 -1.232086155 -1.066025119 -0.802335480 
##          206          207          208          209          210 
## -0.796848371  1.562446317 -0.414185731  1.111615894  0.815747196 
##          211          212          213          214          215 
## -0.292238106  0.249728992 -3.394319675 -0.071677265  1.518668667 
##          216          217          218          219          220 
## -0.527909279 -0.513459889 -0.835838826  0.070433834 -0.383193601 
##          221          222          223          224          225 
## -0.542588979  0.201338501  0.586814058  1.300814389 -1.653709460 
##          226          227          228          229          230 
## -2.289323622  0.091216407 -0.294521906 -0.323313350 -0.409284372 
##          231          232          233          234          235 
## -1.011616895  0.892797969  0.224207919  0.002105654  0.440515927 
##          236          237          238          239          240 
##  0.649557498 -1.108627995 -0.316134836 -0.683041484 -0.378293375 
##          241          242          243          244          245 
##  0.020668472  0.866693784 -0.087696360 -0.163255520  0.347464986 
##          246          247          248          249          250 
##  1.300777187  0.479808949  0.047691398  0.379216900  2.014703897 
##          251          252          253          254          255 
## -0.767463361 -1.423639101  0.684230421  0.475735680 -0.954890210 
##          256          257          258          259          260 
## -0.157575907  1.440306390 -0.431588911 -0.427448149  0.822117225 
##          261          262          263          264          265 
## -0.675401230 -0.278320079  0.052632665 -1.971416120  0.674634605 
##          266          267          268          269          270 
##  0.441524048 -0.321989195  0.700670528 -0.308502571 -1.217767793 
##          271          272          273          274          275 
## -0.013158978 -0.580339874  0.883643905  0.020517224 -0.747021771 
##          276          277          278          279          280 
## -0.596342474  0.190624454  0.124515241  0.291292618  0.062354621 
##          281          282          283          284          285 
##  0.310943170 -1.214391067 -0.545411489  0.106459654  0.726564413 
##          286          287          288          289          290 
## -0.175058080 -0.335370886 -1.567950251 -0.694228316 -0.528321814 
##          291          292          293          294          295 
## -1.051388116  1.527622982 -1.910715790  0.420800015 -2.127417022 
##          296          297          298          299          300 
##  0.572972791  1.229284041  0.491385619  0.937775672  0.041511568 
##          301          302          303          304          305 
## -0.025379342  0.861948628 -0.049437653  0.750033212  1.549512245 
##          306          307          308          309          310 
##  0.132081439  0.331531607 -0.805817041 -1.332358591  0.186029581 
##          311          312          313          314          315 
## -0.154690194 -0.321459782  0.573604730  0.895000017 -0.164196921 
##          316          317          318          319          320 
## -0.353609947 -1.373907594 -0.316850211  0.928305809  1.136502056 
##          321          322          323          324          325 
## -0.262154139 -0.042435078  0.580818913  0.441314410 -0.135626720 
##          326          327          328          329          330 
## -0.160339273  0.798972510  0.117342642  0.458940622 -0.594003241 
##          331          332          333          334          335 
## -0.842696386 -0.242347684  1.092738442 -0.458457493  1.320847499 
##          336          337          338          339          340 
##  0.606139898  0.545614761 -0.111669418  0.689013722 -0.179686710 
##          341          342          343          344          345 
## -0.696518154 -0.244746405  0.464718787 -0.210243417 -1.404901530 
##          346          347          348          349          350 
##  1.014443522 -0.038874243 -0.100102106 -0.527063020 -0.553846133 
##          351          352          353          354          355 
##  0.285677498  1.726316363 -3.000425826 -0.557308829 -0.079992490 
##          356          357          358          359          360 
## -0.358550896 -0.291574991 -0.527179493  1.328370019 -0.688380435 
##          361          362          363          364          365 
## -0.999500377 -0.584750401 -0.094262649  0.604767377  1.448675442 
##          366          367          368          369          370 
## -0.016090107  0.883771635  0.313085261 -0.322046580  0.087067337 
##          371          372          373          374          375 
##  1.394053327  1.276414178  2.170109598  1.144490899  0.429883954 
##          376          377          378          379          380 
##  0.147752350  0.985802242  0.786980596 -0.290888971 -0.140786483 
##          381          382          383          384          385 
## -1.041869336 -0.275696774  0.473031323 -0.074452764 -0.523434124 
##          386          387          388          389          390 
##  0.054895943  0.773605941 -0.707589438 -0.364808439 -0.596846252 
##          391          392          393          394          395 
## -0.249229888 -0.090936827 -1.534216305  0.089896145 -0.823849795 
##          396          397          398          399          400 
##  1.067159076 -0.495570335  1.213603349  0.221903352  0.787198189 
##          401          402          403          404          405 
##  0.919680897 -0.403476176 -0.221892027  0.645401509  0.306494782 
##          406          407          408          409          410 
##  1.016037788  0.703840080 -0.396540612 -0.024437275 -1.410673471 
##          411          412          413          414          415 
##  0.840080998 -0.944354318 -0.330758641  0.939523495  1.238576595 
##          416          417          418          419          420 
##  0.395046846 -0.917309969 -0.452290531 -0.061674451  0.208666146 
##          421          422          423          424          425 
##  0.074575673 -1.146107112 -0.266797476  0.230533310  0.538035904 
##          426          427          428          429          430 
##  1.295708615  0.586185354 -0.088594836 -2.213971784  1.520991398 
##          431          432          433          434          435 
## -1.053851304 -0.961917953  0.349523035 -0.042145959  0.621199177 
##          436          437          438          439          440 
## -0.117103977  0.426015742  0.228596811 -1.627927227 -1.031997886 
##          441          442          443          444          445 
## -0.126275512  0.134576235  0.146183006 -0.987815553  0.097646419 
##          446          447          448          449          450 
##  0.131201486  1.028736858 -1.008671770 -0.042307103 -0.240310824 
##          451          452          453          454          455 
## -0.902413383  0.506470772 -1.003557126 -0.011574025  1.492019113 
##          456          457          458          459          460 
## -1.030747721  0.506301517  0.056474441 -0.325231584 -0.764882553 
##          461          462          463          464          465 
## -0.988238735 -0.396540612 -1.120346498 -0.578964422  0.479098695 
##          466          467          468          469          470 
##  0.089263577  0.640052786 -0.225594515 -0.154531765 -0.921586001 
##          471          472          473          474          475 
##  0.380091566 -1.141634603 -0.401241191 -0.452752209  0.178981506 
##          476          477          478          479          480 
## -0.251596315 -0.568001487 -1.806921776  0.507960345 -0.466937934 
##          481          482          483          484          485 
##  0.632415241 -1.631048181 -0.243632940 -1.340782704  0.944516834 
##          486          487          488          489          490 
## -0.318540628  0.286359607  0.299364133  1.103394456  1.415277983 
##          491          492          493          494          495 
##  0.491815536 -0.221355335  0.210617806 -0.313159003 -0.415797770 
##          496          497          498          499          500 
## -0.264449659  0.605211226 -0.226540461 -0.061741186 -0.810739400 
##          501          502          503          504          505 
##  0.630298728 -0.480243937  0.411174507  1.231357111  0.202765489 
##          506          507          508          509          510 
## -0.355434464  0.712297243 -0.075220454 -0.253496029  2.109648883 
##          511          512          513          514          515 
##  0.608561297  0.169435924 -0.532828571  1.829780249 -2.085616652 
##          516          517          518          519          520 
##  0.210589850 -0.628105411  1.273339579 -0.435409372  1.446841021 
##          521          522          523          524          525 
##  0.077136222 -0.506861128 -0.058836405  0.266873975  1.350858575 
##          526          527          528          529          530 
## -1.254803365 -0.794844808  1.569012490 -0.390117245 -0.406546996 
##          531          532          533          534          535 
## -0.890374429 -0.459758442  0.135594361 -0.242347684 -0.098047623 
##          536          537          538          539          540 
## -1.167521676 -0.490234187  0.713783860  0.298349969  0.477926646 
##          541          542          543          544          545 
## -0.248390142 -0.421136622 -0.841587798 -1.171847207  0.264074684 
##          546          547          548          549          550 
## -0.399410532 -0.165750284  0.335340388  1.326068101  0.091518519 
##          551          552          553          554          555 
## -0.228686458  0.114908824 -0.671484273  0.192107284 -0.219780269 
##          556          557          558          559          560 
## -0.612625841 -0.864370870  0.123847096 -0.197227248 -0.174874766 
##          561          562          563          564          565 
## -0.021127840  0.118844772 -0.058945303 -0.645963029  0.156844627 
##          566          567          568          569          570 
##  0.094969223  0.591755760  0.617001300  2.537395088  2.751146617 
##          571          572          573          574          575 
## -0.054329938  1.817932530 -0.616029056  0.883784961  0.505771432 
##          576          577          578          579          580 
##  0.564072406 -0.378357404  1.296513989  0.067438811 -0.662933769 
##          581          582          583          584          585 
## -0.533547712  1.421017103 -0.178583500  0.257008303  0.550673708 
##          586          587          588          589          590 
##  0.742547546  0.596389768  0.361715258  0.201680459  0.057393267 
##          591          592          593          594          595 
## -0.981922454  0.393653641  1.195100892  0.038861243  0.246181122 
##          596          597          598          599          600 
## -0.425334128  0.993141705  0.925133808  0.636407279  1.468525149 
##          601          602          603          604          605 
## -0.866903683 -0.330762174 -0.281549845  0.633159883  0.259300115 
##          606          607          608          609          610 
## -3.898609575  0.751371052  0.414817306 -1.013785680  0.324052767 
##          611          612          613          614          615 
##  1.712942146 -0.164738998 -0.437978737 -0.110808617 -0.445548191 
##          616          617          618          619          620 
##  0.959061795 -0.561517785  0.787042821  0.045176284 -0.045615822 
##          621          622          623          624          625 
## -0.089770537 -0.393761454 -2.314218171 -0.389907884  0.382733998 
##          626          627          628          629          630 
##  1.442407436 -0.787059302  1.125719992 -0.361369744 -0.237363492 
##          631          632          633          634          635 
## -0.142111412  0.826752523  1.475005463 -0.272783041 -0.337378579 
##          636          637          638          639          640 
##  0.063057606 -0.033864982 -0.508171805  0.949327952  0.089086644 
##          641          642          643          644          645 
##  0.231495391  0.370091902  0.389577914  1.109713255  1.027434568 
##          646          647          648          649          650 
## -0.034333199  1.520642811  0.301218963  0.729909531  0.290457305 
##          651          652          653          654          655 
##  0.934948372  1.180448626  0.235907443 -0.977385806 -0.844781551 
##          656          657          658          659          660 
##  0.618582475  0.117049336  0.939832619 -0.083743776  0.750076621 
##          661          662          663          664          665 
## -0.299670341  1.391001231 -0.070492954  1.276312551 -0.453403350 
##          666          667          668          669          670 
##  1.637900088  0.396390803 -0.433132386 -0.546851287  1.755439242 
##          671          672          673          674          675 
##  0.171046821  0.107906451 -1.263836989 -0.148471140 -0.364849188 
##          676          677          678          679          680 
## -0.398297678 -0.371656045 -2.352602108 -0.077713494 -0.359784477 
##          681          682          683          684          685 
## -1.941703671 -0.237115187 -0.502682273  0.287858020 -0.768027524 
##          686          687          688          689          690 
##  0.596195895  0.984813015 -0.501384158  0.620329609 -0.688738302 
##          691          692          693          694          695 
##  0.198572356 -0.124781005 -0.217694763 -0.084255694  1.312439383 
##          696          697          698          699          700 
##  0.862669866  1.145182688  0.629769002 -0.680678214  2.151441374 
##          701          702          703          704          705 
## -1.318011621 -0.945741196 -2.143003386  0.084946727  0.070527645 
##          706          707          708          709          710 
##  1.206672553 -1.260286246  1.685202365 -0.814536109  0.966403654 
##          711          712          713          714          715 
## -0.446443040  0.639850956  0.411184407 -0.256498169  0.750725981 
##          716          717          718          719          720 
## -0.286401931 -0.636678012 -0.817139514 -0.704427904  0.818829336 
##          721          722          723          724          725 
## -0.606504887  0.866086489  0.507024337 -0.336385432 -0.008388470 
##          726          727          728          729          730 
## -0.346678578 -1.293643513 -0.286846752  1.675355571 -0.211308467 
##          731          732          733          734          735 
## -0.420378145  0.620913682 -0.530809214 -0.100583080  0.368749290 
##          736          737          738          739          740 
##  0.322380204  0.046000081 -1.186973536  0.608551262 -1.236456055 
##          741          742          743          744          745 
##  0.111390540 -1.633562152  0.082496707  0.928934901  0.257634876 
##          746          747          748          749          750 
##  1.767426506  0.903027128  2.650860191 -1.285142157 -0.619067494 
##          751          752          753          754          755 
## -0.187471073  1.118301503 -1.937825614  0.199703669 -0.920344873 
##          756          757          758          759          760 
## -0.346541586  0.914613836 -0.147037504 -0.165750284 -0.042870542 
##          761          762          763          764          765 
## -0.456242645  1.674265319 -3.533127398  1.264194953  2.483179632 
##          766          767          768          769          770 
##  1.322592207  0.925664899 -0.355923636  0.447195184  1.488701141 
##          771          772          773          774          775 
## -0.053642764 -0.516499927  0.220075767 -0.115545216  0.226089821 
##          776          777          778          779          780 
## -0.054634644  0.484760759  0.207792258 -0.378435483  0.193739045 
##          781          782          783          784          785 
##  1.091751285 -0.238056588  0.019813334  0.569165762 -0.148069652 
##          786          787          788          789          790 
## -0.176985195 -0.144399354 -1.009611980  1.555404500  0.486064228 
##          791          792          793          794          795 
##  0.203886969 -0.902601904  1.048308072  0.462642959  0.662726910 
##          796          797          798          799          800 
## -0.274174415 -2.855980735  0.164307146  1.070660788 -2.033075495 
##          801          802          803          804          805 
##  1.704444636  1.031448992  0.130728541  0.065567644 -0.268186635 
##          806          807          808          809          810 
## -0.784797872  0.814022027  0.722754226 -1.768162562 -0.796843882 
##          811          812          813          814          815 
## -1.068277227  0.626377359 -1.501929916  0.410224819 -1.056338833 
##          816          817          818          819          820 
##  0.350145085 -1.218141574 -1.233759396 -0.426596153  0.830756228 
##          821          822          823          824          825 
## -0.290185653 -0.580696819 -0.028981991  1.467996983 -0.421106174 
##          826          827          828          829          830 
##  1.492038271 -0.766214333  0.474685078 -1.094671525 -0.098061588 
##          831          832          833          834          835 
## -0.488848002  2.041876732 -0.461651789  0.288967630  1.686318256 
##          836          837          838          839          840 
##  1.193813111 -0.262430657  0.737515335 -0.524702383  0.612753733 
##          841          842          843          844          845 
##  0.321779962  1.123349282  0.589436514 -1.322799573 -1.264633045 
##          846          847          848          849          850 
##  0.837251619  1.542159213  0.324504015  0.832449937  0.741756140 
##          851          852          853          854          855 
## -0.204744195  0.533077298  0.842915254  0.645932590  0.253538280 
##          856          857          858          859          860 
## -0.615819416 -0.216221056 -0.320638499  0.627819108 -0.492248400 
##          861          862          863          864          865 
## -0.337975067  0.620232649 -0.914219106 -1.255682430  0.390194741 
##          866          867          868          869          870 
## -0.375317143 -1.208682234 -0.026823140 -0.040006189  0.349881096 
##          871          872          873          874          875 
## -0.890675271 -1.145607987  1.286390561 -0.573052433  0.335252820 
##          876          877          878          879          880 
## -0.989952420 -0.953456047  0.746487657 -0.679298565 -0.431966855 
##          881          882          883          884          885 
## -2.582816533 -0.445660536 -2.077905454 -0.462426153 -0.495680826 
##          886          887          888          889          890 
## -1.389484748 -1.834705281  0.564822105  0.226001550  1.196886095 
##          891          892          893          894          895 
##  0.167507683 -0.103253831  1.310121739 -0.019368376  1.085518912 
##          896          897          898          899          900 
##  0.457684551  0.581492574 -1.569494737  0.262662374 -2.088903567 
##          901          902          903          904          905 
## -0.295845512  0.795214162  0.119368855 -0.019146810  0.706236470 
##          906          907          908          909          910 
## -0.753334799 -0.566094452 -0.746159684 -0.528529508  0.196138386 
##          911          912          913          914          915 
## -0.225946403 -1.147910919 -0.559598390 -0.445850099  0.821406121 
##          916          917          918          919          920 
## -0.335031509  0.469349078 -0.606675761 -1.371338375 -0.010046241 
##          921          922          923          924          925 
## -3.232029783  0.500809917 -0.877373824  0.728645076  0.408710386 
##          926          927          928          929          930 
##  0.726433953  1.055454134  0.414638566 -0.436538041  0.724794261 
##          931          932          933          934          935 
## -0.081486790 -0.041032758  0.341791113  0.095129830  1.000103569 
##          936          937          938          939          940 
##  0.269667662 -0.437053300  1.920929814  0.843107303 -0.679838124 
##          941          942          943          944          945 
## -0.509558453  0.586396660 -0.416392465 -0.061600420 -0.183402692 
##          946          947          948          949          950 
##  0.625048362  0.924402626  0.436326317 -0.979755931  0.206344441 
##          951          952          953          954          955 
##  1.939444701 -2.948372631  0.277509063 -0.445660536  0.680451931 
##          956          957          958          959          960 
##  0.792441499 -0.754314732 -0.253072861  1.388033920 -2.248954966 
##          961          962          963          964          965 
##  0.085977406 -0.333563692  0.036592384  1.025026236 -0.382198491 
##          966          967          968          969          970 
##  0.541165695 -0.346962966  0.585419719  0.686047544  1.272990561 
##          971          972          973          974 
## -0.152378294  1.295305567 -0.526836087  0.409790648
```

We can also do multiple linear regression.


```r
chla_tp_tn_turb <- lm(log1p(CHLA) ~ log1p(PTL) + log1p(NTL) + log1p(TURB), data = nla)
summary(chla_tp_tn_turb)
```

```
## 
## Call:
## lm(formula = log1p(CHLA) ~ log1p(PTL) + log1p(NTL) + log1p(TURB), 
##     data = nla)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -4.5990 -0.4362  0.0293  0.5239  2.2750 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept) -1.77639    0.19696  -9.019  < 2e-16 ***
## log1p(PTL)   0.11454    0.03535   3.240  0.00123 ** 
## log1p(NTL)   0.47798    0.04149  11.522  < 2e-16 ***
## log1p(TURB)  0.40360    0.03833  10.529  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.7973 on 970 degrees of freedom
## Multiple R-squared:  0.6287,	Adjusted R-squared:  0.6275 
## F-statistic: 547.4 on 3 and 970 DF,  p-value: < 2.2e-16
```

There's a lot more we can do with linear models including dummy variables (character or factors will work), interactions, etc.  That's a bit more than we want to get into.  Again the link below is a good place to start for more info.

## Getting More Help
One nice site that covers basic stats in R is [Quick R: Basic Statistics](http://www.statmethods.net/stats/index.html).  There are others, but that is a good first stop.
