Project \#2
================

# Homophobia in Germany

Made by Liskonozhenko, Poroshina, Scherbakova, Vasyukova BSC-193C

## Data and RQ

For this project, we use the data from 9th round of ESS in Germany.

**Research question**: what social factors influence homophobia in
Germany?

## Literature review

In our second group project, we would like to explore the factors that
can influence whether a person is homophobic or not. There are several
studies that explore various factors that can predict homophobia. For
example, in the study by Basow & Johnson (2000) were examined predictors
of homophobia among female college students. The authors investigated
how self-esteem, self-discrepancy, gender-attribute importance, gender
role attitudes, authoritarian attitudes, and extent of contact with LGBT
persons affect homophobia. The study was conducted among students of a
private college of liberal arts; there were 71 respondents in total.
Students completed several questionnaires related to the topic of
predictors that were investigated in the study. The researchers then
performed a correlational analysis. The results of the analysis showed
that the highest correlations with homophobia were in right-wing
authoritarian views, belief in the egalitarianism of gender roles, the
extent of contact with LGBT persons, and the importance of feminine
attributes to participant’s femininity. However, the only significant
predictor was adherence to right-wing authoritarian attitudes.

Another study (Nagoshi, 2008) is also dedicated to finding the
predictors of homophobia and transphobia among college students (157 men
and 153 women). This time both male and female students were asked to
complete the questionnaire about their attitude towards LGBT community
and trans people. Students also completed questionnaires which then
helped to assess their political preferences, religion, propensity to
aggression. The correlation analysis revealed that there is a positive
relation between homophobia and radical right authorism, sexism and
religious fundamentalism.

There is also a relationship between LGBT acceptance and education
levels. An article by Ohlander, Batalova & Treas (2005) found that
people with at least a high school education and higher were more likely
to be tolerant of LGBT people. Whereas people with no high school
education were more likely to show strong intolerance towards the LGBT
community. The study was conducted on the basis of data from the annual
GSS survey (1988-1994), and the sample consisted of 2733 observations.

Another article by Ying, Xie, Minggang, Peng (2017) examines the effects
of religion an exposure to internet information on attitudes towards
homosexuality in China. The author used data from the Chinese General
Social Surveys 2013. The sample included 380 observations from the
CGSS2013. The survey checked the attitude towards LGBT through the
question “What do you generally think about same-sex sexual behaviors?”.
Responses to the question were coded on a 1-5 scale where 1-always wrong
and 5-always right. The internet usage was checked by the question
whether the respondent uses the internet as the main source of
information. Block about religion contained questions about confession
the respondent belonged to. The analysis revealed that people who use
internet as main sourse oh information are less likely to give answer
“always wrong” to the question about normality of homosexual
relationships compared to respondents who prefer other sourses of
information. Also, only Muslim grout showed significant effect on
attitude towards homosexuality. Muslim people are more likely to give
answer “always wrong” to the question about normality of homosexual
relationships.

So, after reviewing several studies of various factors influencing
people’s attitudes towards LGBT rights, we would like to find out if
there is any correlation between these factors and attitudes towards
LGBT rights in Germany. We investigate whether gender, education,
religiosity, and political preferences affect people’s attitudes towards
LGBT rights.

**Loading packages and data**

``` r
library(base)
library(car)
library(foreign)
library(dplyr)
library(ggplot2)
library(MASS)
library(stargazer)
```

``` r
data = read.spss("./data/ESS9e03_1.sav",
                    to.data.frame=TRUE)
var.labels <- attr(data, "variable.labels")
dd <- data.frame(names(var.labels), var.labels)

ess1 <- filter(data, cntry == "Germany")
```

## Used variables

Dependent variable: freehms -\> freegays (Gays and lesbians free to live
life as they wish)

Independent variables: lrscale -\> left_right (left or right political
views), rlgdgr -\> religiosity (How religious are you)

Control variables: gndr -\> gender (Gender), eisced -\> education
(Highest level of education), age (Age)

## Data manipulation

To predict the homophobia level with logit model, we need to make it
binary. So, we excluded the unsure level, omitting only 140
observations. Agree are 0, disagree are 1 in the new encoding, so that 1
is for homophobic attitudes, and 0 for non-homophobic.

``` r
essger <- filter(ess1, freehms != "Neither agree nor disagree")

essger$freegays <- ifelse(essger$freehms == "Agree strongly" | essger$freehms == "Agree", 0, 1)
```

``` r
ess <- dplyr :: select(essger, freegays, gndr, lrscale, rlgdgr, eisced, agea)
```

There are too many levels in existing education variable. We recoded
them to get only 3 levels - primary, secondary and higher education (+
other and no education, which were not present in Germany).

``` r
levels(ess$eisced)
```

    ## [1] "Not possible to harmonise into ES-ISCED"            
    ## [2] "ES-ISCED I , less than lower secondary"             
    ## [3] "ES-ISCED II, lower secondary"                       
    ## [4] "ES-ISCED IIIb, lower tier upper secondary"          
    ## [5] "ES-ISCED IIIa, upper tier upper secondary"          
    ## [6] "ES-ISCED IV, advanced vocational, sub-degree"       
    ## [7] "ES-ISCED V1, lower tertiary education, BA level"    
    ## [8] "ES-ISCED V2, higher tertiary education, >= MA level"
    ## [9] "Other"

``` r
ess$edu1[ess$eisced == "ES-ISCED I , less than lower secondary" | ess$eisced == "ES-ISCED II, lower secondary"] <- "primary"

ess$edu1[ess$eisced == "ES-ISCED IIIb, lower tier upper secondary" | ess$eisced == "ES-ISCED IIIa, upper tier upper secondary" | ess$eisced == "ES-ISCED IV, advanced vocational, sub-degree"] <- "secondary"

ess$edu1[ess$eisced == "ES-ISCED V1, lower tertiary education, BA level" | ess$eisced == "ES-ISCED V2, higher tertiary education, >= MA level"] <- "higher"

ess$edu1[ess$eisced == "Other"] <- "other"

ess$edu1[ess$eisced == "Not possible to harmonise into ES-ISCED"] <- "no"
```

Education also has to be factor variable. And, it was releveled to have
primary education as a reference in the model.

``` r
ess$edu2 <- as.factor(ess$edu1)
levels(ess$edu2)
```

    ## [1] "higher"    "primary"   "secondary"

``` r
ess$edu <- relevel(ess$edu2, ref = "primary")
levels(ess$edu)
```

    ## [1] "primary"   "higher"    "secondary"

``` r
ess$age <- as.numeric(as.character(ess$agea))
```

Age variable was made into numeric one.

``` r
ess <- rename(ess, religiosity = rlgdgr)
ess <- rename(ess, left_right = lrscale)
ess <- rename(ess, gender = gndr)
ess <- rename(ess, education = edu)
```

The religiosity, political views, gender and education variables were
renamed to the more readable names.

## Descriptive statistics

### Gender

``` r
ggplot(data = ess, aes(x = gender)) +
  geom_bar(stat = "count")
```

![](data-project-2_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

The proportions of males and females in the sample are approximately the
same, but there are slightly more males.

### Age

``` r
hist(ess$age)
```

![](data-project-2_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

``` r
shapiro.test(ess$age)
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  ess$age
    ## W = 0.97201, p-value < 2.2e-16

As we can see from histogram and the test, the data is not distributed
normally for age variable. There are more older people than younger.

### Religion

``` r
ggplot(data = ess, aes(x = religiosity)) +
  geom_bar(stat = "count")
```

![](data-project-2_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

The religious variable is also not distributed normally. There is a huge
share of people who are not religious at all, smaller share is for
middle and the smallest share is for very religious people.

### Education

``` r
ggplot(data = ess, aes(x = education)) +
  geom_bar(stat = "count")
```

![](data-project-2_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

The biggest proportion of people has secondary education, much less -
higher and primary.

### Left-right scale

``` r
ggplot(data = ess, aes(x = left_right)) +
  geom_bar(stat = "count")
```

![](data-project-2_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

Most people have moderate political views. We can see that the
distribution is slightly more left-skewed, more people tend to have left
views.

### Dependent variable - should gays be free

``` r
ggplot(data = ess, aes(x = freegays)) +
  geom_histogram(binwidth = 1)
```

![](data-project-2_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

``` r
shapiro.test(ess$freegays)
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  ess$freegays
    ## W = 0.25428, p-value < 2.2e-16

Distribution is not normal for homophobia. Vast majority of people think
that gays and lesbians can live a normal life, and few are of opposite,
homophobic, views.

## Relationships between dependent and independent variables

### Gender and homophobia

``` r
leveneTest(ess$freegays ~ ess$gender)
```

    ## Levene's Test for Homogeneity of Variance (center = median)
    ##         Df F value  Pr(>F)  
    ## group    1  3.2944 0.06965 .
    ##       2193                  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
oneway.test(ess$freegays ~ ess$gender, var.equal = T) 
```

    ## 
    ##  One-way analysis of means
    ## 
    ## data:  ess$freegays and ess$gender
    ## F = 3.2944, num df = 1, denom df = 2193, p-value = 0.06965

As ANOVA test shows, the relationship is not very significant for
gender, but we will use gender as our control variable in the model
still, and will check if regression method will show significant
results.

### Age and homophobia

``` r
t.test(ess$age, ess$freegays)
```

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  ess$age and ess$freegays
    ## t = 122.15, df = 2192.7, p-value < 2.2e-16
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  48.38615 49.96508
    ## sample estimates:
    ##   mean of x   mean of y 
    ## 49.23666211  0.06104784

T-test results say that relationship is significant, according to
p-value.

### Religion and homophobia

``` r
par(mfrow = c(1, 1), mar = c(12, 4, 4, 1))
boxplot(ess$freegays ~ ess$religiosity, las = 2)
```

![](data-project-2_files/figure-gfm/unnamed-chunk-20-1.png)<!-- -->

``` r
t.test(ess$freegays, as.numeric(ess$religiosity))
```

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  ess$freegays and as.numeric(ess$religiosity)
    ## t = -77.087, df = 2217, p-value < 2.2e-16
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -5.159135 -4.903160
    ## sample estimates:
    ##  mean of x  mean of y 
    ## 0.06104784 5.09219534

Judging by boxplot and test, the most religious people tend to be more
negative about gay people living as they wish. The relationship is
significant.

### Education and homophobia

``` r
leveneTest(ess$freegays ~ ess$education)
```

    ## Levene's Test for Homogeneity of Variance (center = median)
    ##         Df F value    Pr(>F)    
    ## group    2  7.0691 0.0008706 ***
    ##       2186                      
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
oneway.test(ess$freegays ~ ess$education, var.equal = T) 
```

    ## 
    ##  One-way analysis of means
    ## 
    ## data:  ess$freegays and ess$education
    ## F = 7.0691, num df = 2, denom df = 2186, p-value = 0.0008706

This relationship also seem significant. The level of education may
predict the level of homophobia and we should use it in the model.

### Left-right scale and homophobia

``` r
leveneTest(ess$freegays ~ ess$left_right)
```

    ## Levene's Test for Homogeneity of Variance (center = median)
    ##         Df F value   Pr(>F)    
    ## group   10  6.4989 5.95e-10 ***
    ##       2106                     
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
oneway.test(ess$freegays ~ ess$left_right, var.equal = T) 
```

    ## 
    ##  One-way analysis of means
    ## 
    ## data:  ess$freegays and ess$left_right
    ## F = 6.4989, num df = 10, denom df = 2106, p-value = 5.95e-10

The position on left - right scale is also significant in relationship
with the dependent variable. It should also be included in our model.

## Model

``` r
essfin <- na.omit(ess)
model <- glm(freegays ~  gender + age + religiosity + education + left_right, data = essfin, family = binomial(link = "logit"))

stargazer(model, title="Logit Regression Results",
align=TRUE, dep.var.labels=c("Homophobic attitudes"),
covariate.labels=c("Gender","Age", "Religiosity lvl 1", "Religiosity lvl 2", "Religiosity lvl 3", "Religiosity lvl 4", "Religiosity lvl 5", "Religiosity lvl 6", "Religiosity lvl 7", "Religiosity lvl 8", "Religiosity lvl 9", "Religiosity - Very religious", "Education - Higher","Education - Secondary", "Left-right scale lvl 1", "Left-right scale lvl 2", "Left-right scale lvl 3", "Left-right scale lvl 4", "Left-right scale lvl 5", "Left-right scale lvl 6", "Left-right scale lvl 7", "Left-right scale lvl 8", "Left-right scale lvl 9", "Left-right scale - Right"), omit.stat=c("ser","f"), no.space=TRUE, type = 'html')
```

<table style="text-align:center">
<caption>
<strong>Logit Regression Results</strong>
</caption>
<tr>
<td colspan="2" style="border-bottom: 1px solid black">
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
<em>Dependent variable:</em>
</td>
</tr>
<tr>
<td>
</td>
<td colspan="1" style="border-bottom: 1px solid black">
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
Homophobic attitudes
</td>
</tr>
<tr>
<td colspan="2" style="border-bottom: 1px solid black">
</td>
</tr>
<tr>
<td style="text-align:left">
Gender
</td>
<td>
-0.667<sup>\*\*\*</sup>
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
(0.209)
</td>
</tr>
<tr>
<td style="text-align:left">
Age
</td>
<td>
0.017<sup>\*\*\*</sup>
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
(0.005)
</td>
</tr>
<tr>
<td style="text-align:left">
Religiosity lvl 1
</td>
<td>
0.831<sup>\*</sup>
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
(0.492)
</td>
</tr>
<tr>
<td style="text-align:left">
Religiosity lvl 2
</td>
<td>
0.353
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
(0.510)
</td>
</tr>
<tr>
<td style="text-align:left">
Religiosity lvl 3
</td>
<td>
0.876<sup>\*\*</sup>
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
(0.428)
</td>
</tr>
<tr>
<td style="text-align:left">
Religiosity lvl 4
</td>
<td>
0.900<sup>\*</sup>
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
(0.471)
</td>
</tr>
<tr>
<td style="text-align:left">
Religiosity lvl 5
</td>
<td>
0.627
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
(0.385)
</td>
</tr>
<tr>
<td style="text-align:left">
Religiosity lvl 6
</td>
<td>
0.610
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
(0.440)
</td>
</tr>
<tr>
<td style="text-align:left">
Religiosity lvl 7
</td>
<td>
0.161
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
(0.465)
</td>
</tr>
<tr>
<td style="text-align:left">
Religiosity lvl 8
</td>
<td>
0.860<sup>\*\*</sup>
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
(0.418)
</td>
</tr>
<tr>
<td style="text-align:left">
Religiosity lvl 9
</td>
<td>
1.977<sup>\*\*\*</sup>
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
(0.479)
</td>
</tr>
<tr>
<td style="text-align:left">
Religiosity - Very religious
</td>
<td>
2.261<sup>\*\*\*</sup>
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
(0.420)
</td>
</tr>
<tr>
<td style="text-align:left">
Education - Higher
</td>
<td>
-1.085<sup>\*\*\*</sup>
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
(0.314)
</td>
</tr>
<tr>
<td style="text-align:left">
Education - Secondary
</td>
<td>
-1.153<sup>\*\*\*</sup>
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
(0.271)
</td>
</tr>
<tr>
<td style="text-align:left">
Left-right scale lvl 1
</td>
<td>
-0.644
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
(0.872)
</td>
</tr>
<tr>
<td style="text-align:left">
Left-right scale lvl 2
</td>
<td>
-0.802
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
(0.664)
</td>
</tr>
<tr>
<td style="text-align:left">
Left-right scale lvl 3
</td>
<td>
-0.459
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
(0.549)
</td>
</tr>
<tr>
<td style="text-align:left">
Left-right scale lvl 4
</td>
<td>
-0.151
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
(0.545)
</td>
</tr>
<tr>
<td style="text-align:left">
Left-right scale lvl 5
</td>
<td>
-0.115
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
(0.496)
</td>
</tr>
<tr>
<td style="text-align:left">
Left-right scale lvl 6
</td>
<td>
-1.079
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
(0.663)
</td>
</tr>
<tr>
<td style="text-align:left">
Left-right scale lvl 7
</td>
<td>
0.245
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
(0.575)
</td>
</tr>
<tr>
<td style="text-align:left">
Left-right scale lvl 8
</td>
<td>
0.113
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
(0.646)
</td>
</tr>
<tr>
<td style="text-align:left">
Left-right scale lvl 9
</td>
<td>
-0.121
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
(0.947)
</td>
</tr>
<tr>
<td style="text-align:left">
Left-right scale - Right
</td>
<td>
1.907<sup>\*\*\*</sup>
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
(0.671)
</td>
</tr>
<tr>
<td style="text-align:left">
Constant
</td>
<td>
-2.993<sup>\*\*\*</sup>
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
(0.599)
</td>
</tr>
<tr>
<td colspan="2" style="border-bottom: 1px solid black">
</td>
</tr>
<tr>
<td style="text-align:left">
Observations
</td>
<td>
2,109
</td>
</tr>
<tr>
<td style="text-align:left">
Log Likelihood
</td>
<td>
-408.812
</td>
</tr>
<tr>
<td style="text-align:left">
Akaike Inf. Crit.
</td>
<td>
867.624
</td>
</tr>
<tr>
<td colspan="2" style="border-bottom: 1px solid black">
</td>
</tr>
<tr>
<td style="text-align:left">
<em>Note:</em>
</td>
<td style="text-align:right">
<sup>*</sup>p\<0.1; <sup>**</sup>p\<0.05; <sup>***</sup>p\<0.01
</td>
</tr>
</table>

**Gender**: the log of odds ratio of being homophobic is smaller by 0.67
for females compared to males; women are less likely to be homophobic.

**Age**: the log of odds ratio of being homophobic increases by 0.017
with every year of age; older people are more likely to be more
homophobic.

**Religiosity**: the log of odds ratio is bigger by 0.87 for slightly
religious people (with level 3), by 0.85 for religious people (with
level 8), and by 1.97 and 2.26 for very religious people (level 9 and
Very religious correspondingly) compared to not at all religious ones;
very religious people are more likely to be homophobic.

**Education**: the log of odds ratio is smaller by 1.08 for people with
the higher education, and by 1.15 for people with secondary education,
compared to people with primary education; more educated people are less
likely to be homophobic.

**Left-right political views**: the log of odds ratio is bigger by 1.9
for people with exactly right political views compared to ones with
strictly left views; people with right-wing political views are more
likely to be homophobic.

### Odds ratio

``` r
library(sjPlot)
tab_model(model)
```

<table style="border-collapse:collapse; border:none;">
<tr>
<th style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm;  text-align:left; ">
 
</th>
<th colspan="3" style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm; ">
freegays
</th>
</tr>
<tr>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  text-align:left; ">
Predictors
</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">
Odds Ratios
</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">
CI
</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">
p
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
(Intercept)
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.05
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.01 – 0.15
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
<strong>\<0.001</strong>
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
gender \[Female\]
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.51
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.34 – 0.77
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
<strong>0.001</strong>
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
age
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
1.02
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
1.01 – 1.03
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
<strong>0.001</strong>
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
religiosity \[1\]
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
2.30
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.83 – 5.89
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.091
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
religiosity \[2\]
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
1.42
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.49 – 3.73
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.488
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
religiosity \[3\]
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
2.40
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
1.02 – 5.57
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
<strong>0.041</strong>
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
religiosity \[4\]
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
2.46
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.94 – 6.09
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.056
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
religiosity \[5\]
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
1.87
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.88 – 4.06
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.104
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
religiosity \[6\]
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
1.84
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.76 – 4.34
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.166
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
religiosity \[7\]
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
1.17
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.45 – 2.88
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.729
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
religiosity \[8\]
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
2.36
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
1.03 – 5.40
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
<strong>0.040</strong>
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
religiosity \[9\]
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
7.22
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
2.75 – 18.36
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
<strong>\<0.001</strong>
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
religiosity \[Very<br>religious\]
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
9.60
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
4.23 – 22.21
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
<strong>\<0.001</strong>
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
education \[higher\]
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.34
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.18 – 0.63
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
<strong>0.001</strong>
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
education \[secondary\]
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.32
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.19 – 0.54
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
<strong>\<0.001</strong>
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
left right \[1\]
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.53
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.07 – 2.62
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.461
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
left right \[2\]
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.45
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.12 – 1.70
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.227
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
left right \[3\]
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.63
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.23 – 2.04
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.403
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
left right \[4\]
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.86
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.31 – 2.77
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.782
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
left right \[5\]
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.89
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.37 – 2.67
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.816
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
left right \[6\]
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.34
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.09 – 1.29
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.104
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
left right \[7\]
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
1.28
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.43 – 4.30
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.671
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
left right \[8\]
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
1.12
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.31 – 4.17
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.861
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
left right \[9\]
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.89
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.11 – 5.19
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.899
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
left right \[Right\]
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
6.73
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
1.85 – 26.68
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
<strong>0.004</strong>
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm; border-top:1px solid;">
Observations
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left; border-top:1px solid;" colspan="3">
2109
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm;">
R<sup>2</sup> Tjur
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="3">
0.076
</td>
</tr>
</table>

**Gender**: the odds of being homophobic decrease by 49% for females
compared to males.

**Age**: the odds of being homophobic increase by 2% with every year of
age.

**Religiosity**: the odds of being homophobic increase by 140% for
slightly religious people (with level 3), increase by 136% for religious
people (with level 8), and increase by 622% and 860% for very religious
people (level 9 and Very religious correspondingly) compared to not at
all religious ones.

**Education**: the odds of being homophobic decrease by 66% for people
with the higher education, and by 68% for people with secondary
education, compared to people with primary education.

**Left-right political views**: the odds of being homophobic increase by
573% for people with exactly right political views compared to ones with
strictly left views.

``` r
library(margins)
margins <- data.frame(summary(margins(model)))
stargazer(margins, title="Margins of the model",
align=TRUE, summary = F, dep.var.labels=c("Homophobic attitudes"), no.space=TRUE, type = 'html')
```

<table style="text-align:center">
<caption>
<strong>Margins of the model</strong>
</caption>
<tr>
<td colspan="8" style="border-bottom: 1px solid black">
</td>
</tr>
<tr>
<td>
</td>
<td>
factor
</td>
<td>
AME
</td>
<td>
SE
</td>
<td>
z
</td>
<td>
p
</td>
<td>
lower
</td>
<td>
upper
</td>
</tr>
<tr>
<td colspan="8" style="border-bottom: 1px solid black">
</td>
</tr>
<tr>
<td>
1
</td>
<td>
age
</td>
<td>
0.001
</td>
<td>
0.0003
</td>
<td>
3.176
</td>
<td>
0.001
</td>
<td>
0.0003
</td>
<td>
0.001
</td>
</tr>
<tr>
<td>
2
</td>
<td>
educationhigher
</td>
<td>
-0.077
</td>
<td>
0.025
</td>
<td>
-3.012
</td>
<td>
0.003
</td>
<td>
-0.127
</td>
<td>
-0.027
</td>
</tr>
<tr>
<td>
3
</td>
<td>
educationsecondary
</td>
<td>
-0.080
</td>
<td>
0.024
</td>
<td>
-3.295
</td>
<td>
0.001
</td>
<td>
-0.127
</td>
<td>
-0.032
</td>
</tr>
<tr>
<td>
4
</td>
<td>
genderFemale
</td>
<td>
-0.033
</td>
<td>
0.010
</td>
<td>
-3.228
</td>
<td>
0.001
</td>
<td>
-0.052
</td>
<td>
-0.013
</td>
</tr>
<tr>
<td>
5
</td>
<td>
left_right1
</td>
<td>
-0.030
</td>
<td>
0.038
</td>
<td>
-0.786
</td>
<td>
0.432
</td>
<td>
-0.104
</td>
<td>
0.044
</td>
</tr>
<tr>
<td>
6
</td>
<td>
left_right2
</td>
<td>
-0.035
</td>
<td>
0.031
</td>
<td>
-1.113
</td>
<td>
0.266
</td>
<td>
-0.096
</td>
<td>
0.027
</td>
</tr>
<tr>
<td>
7
</td>
<td>
left_right3
</td>
<td>
-0.023
</td>
<td>
0.030
</td>
<td>
-0.753
</td>
<td>
0.451
</td>
<td>
-0.082
</td>
<td>
0.036
</td>
</tr>
<tr>
<td>
8
</td>
<td>
left_right4
</td>
<td>
-0.008
</td>
<td>
0.031
</td>
<td>
-0.268
</td>
<td>
0.789
</td>
<td>
-0.070
</td>
<td>
0.053
</td>
</tr>
<tr>
<td>
9
</td>
<td>
left_right5
</td>
<td>
-0.007
</td>
<td>
0.029
</td>
<td>
-0.224
</td>
<td>
0.823
</td>
<td>
-0.064
</td>
<td>
0.051
</td>
</tr>
<tr>
<td>
10
</td>
<td>
left_right6
</td>
<td>
-0.042
</td>
<td>
0.030
</td>
<td>
-1.407
</td>
<td>
0.159
</td>
<td>
-0.101
</td>
<td>
0.017
</td>
</tr>
<tr>
<td>
11
</td>
<td>
left_right7
</td>
<td>
0.016
</td>
<td>
0.037
</td>
<td>
0.436
</td>
<td>
0.663
</td>
<td>
-0.056
</td>
<td>
0.088
</td>
</tr>
<tr>
<td>
12
</td>
<td>
left_right8
</td>
<td>
0.007
</td>
<td>
0.040
</td>
<td>
0.175
</td>
<td>
0.861
</td>
<td>
-0.071
</td>
<td>
0.085
</td>
</tr>
<tr>
<td>
13
</td>
<td>
left_right9
</td>
<td>
-0.007
</td>
<td>
0.052
</td>
<td>
-0.130
</td>
<td>
0.896
</td>
<td>
-0.109
</td>
<td>
0.096
</td>
</tr>
<tr>
<td>
14
</td>
<td>
left_rightRight
</td>
<td>
0.226
</td>
<td>
0.094
</td>
<td>
2.407
</td>
<td>
0.016
</td>
<td>
0.042
</td>
<td>
0.409
</td>
</tr>
<tr>
<td>
15
</td>
<td>
religiosity1
</td>
<td>
0.034
</td>
<td>
0.024
</td>
<td>
1.412
</td>
<td>
0.158
</td>
<td>
-0.013
</td>
<td>
0.080
</td>
</tr>
<tr>
<td>
16
</td>
<td>
religiosity2
</td>
<td>
0.011
</td>
<td>
0.018
</td>
<td>
0.646
</td>
<td>
0.518
</td>
<td>
-0.023
</td>
<td>
0.046
</td>
</tr>
<tr>
<td>
17
</td>
<td>
religiosity3
</td>
<td>
0.036
</td>
<td>
0.020
</td>
<td>
1.803
</td>
<td>
0.071
</td>
<td>
-0.003
</td>
<td>
0.076
</td>
</tr>
<tr>
<td>
18
</td>
<td>
religiosity4
</td>
<td>
0.038
</td>
<td>
0.023
</td>
<td>
1.605
</td>
<td>
0.108
</td>
<td>
-0.008
</td>
<td>
0.084
</td>
</tr>
<tr>
<td>
19
</td>
<td>
religiosity5
</td>
<td>
0.023
</td>
<td>
0.015
</td>
<td>
1.587
</td>
<td>
0.113
</td>
<td>
-0.005
</td>
<td>
0.052
</td>
</tr>
<tr>
<td>
20
</td>
<td>
religiosity6
</td>
<td>
0.022
</td>
<td>
0.017
</td>
<td>
1.275
</td>
<td>
0.202
</td>
<td>
-0.012
</td>
<td>
0.057
</td>
</tr>
<tr>
<td>
21
</td>
<td>
religiosity7
</td>
<td>
0.005
</td>
<td>
0.014
</td>
<td>
0.339
</td>
<td>
0.734
</td>
<td>
-0.023
</td>
<td>
0.032
</td>
</tr>
<tr>
<td>
22
</td>
<td>
religiosity8
</td>
<td>
0.035
</td>
<td>
0.019
</td>
<td>
1.840
</td>
<td>
0.066
</td>
<td>
-0.002
</td>
<td>
0.073
</td>
</tr>
<tr>
<td>
23
</td>
<td>
religiosity9
</td>
<td>
0.136
</td>
<td>
0.049
</td>
<td>
2.767
</td>
<td>
0.006
</td>
<td>
0.040
</td>
<td>
0.232
</td>
</tr>
<tr>
<td>
24
</td>
<td>
religiosityVery religious
</td>
<td>
0.176
</td>
<td>
0.047
</td>
<td>
3.768
</td>
<td>
0.0002
</td>
<td>
0.084
</td>
<td>
0.267
</td>
</tr>
<tr>
<td colspan="8" style="border-bottom: 1px solid black">
</td>
</tr>
</table>

**Gender**: on average the probability of being homophobic is greater by
0.03 for females.

**Age**: on average the probability of being homophobic increases by
0.0008 with every year of age.

**Religiosity**: on average the probability of being homophobic is
greater by 0.036 for slightly religious people (with level 3), by 0.035
for religious people (with level 8), and by 0.136 and 0.176 for very
religious people (level 9 and Very religious correspondingly) compared
to not at all religious ones.

**Education**: on average the probability of being homophobic is smaller
by 0.077 for people with the higher education, and by 0.8 for people
with secondary education, compared to people with primary education.

**Left-right political views**: on average the probability of being
homophobic is bigger by 0.23 for people with exactly right political
views compared to ones with strictly left views.

### Model fit

``` r
library(pscl)
hitmiss(model)
```

    ## Classification Threshold = 0.5 
    ##         y=0 y=1
    ## yhat=0 1982 118
    ## yhat=1    6   3
    ## Percent Correctly Predicted = 94.12%
    ## Percent Correctly Predicted = 99.7%, for y = 0
    ## Percent Correctly Predicted = 2.479%  for y = 1
    ## Null Model Correctly Predicts 94.26%

    ## [1] 94.120436 99.698189  2.479339

The model correctly predicts 94% of the data. However, non-homophobic
people are correctly predicted in 99.7% of cases, while homophobes are
correctly predicted in only 2.48%, which means that out model fails to
predict homophobic people. It is probably because there are very few
homophobic people in the sample compared to non-homophobic.

``` r
library(DescTools)
PseudoR2(model)
```

    ##  McFadden 
    ## 0.1176088

Our model explains about 11% of the data, which is not a perfect fit.

## Conclusion

Our model is not perfect, but it showed significant results. Consistent
with the literature, the model showed that people that are most
religious, people with right-wing political views, and people with
education lower than secondary are more likely to be homophobic.

## References

1.  Basow, S.A., Johnson, K. Predictors of Homophobia in Female College
    Students. Sex Roles 42, 391–404 (2000).
    <https://doi.org/10.1023/A:1007098221316>

2.  Nagoshi J. L. et al. Gender differences in correlates of homophobia
    and transphobia //Sex roles. – 2008. – Т. 59. – №. 7. – С. 521-531.

3.  Julianne Ohlander; Jeanne Batalova; Judith Treas (2005). Explaining
    educational influences on attitudes toward homosexual relations. ,
    34(4), 781–799. <doi:10.1016/j.ssresearch.2004.12.004>

4.  Ying, Xie; Minggang, Peng (2017). Attitudes towards Homosexuality in
    China: Exploring the Effects of Religion, Modernizing Factors, and
    Traditional Culture. Journal of Homosexuality.
    <doi:10.1080/00918369.2017.1386025>
