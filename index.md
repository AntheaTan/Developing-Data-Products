---
title       : Introductory Exploratory Analysis for mtcars
subtitle    : A Developing Data Products Project
author      : Anthea Tan
job         : Apr 2015
framework   : io2012        # {io2012, html5slides, shower, dzslides, ...}
highlighter : highlight.js  # {highlight.js, prettify, highlight}
hitheme     : tomorrow      # 
widgets     : []            # {mathjax, quiz, bootstrap}
mode        : selfcontained # {standalone, draft}
---

## Objective

The objective of this presentation is to showcase the use of Shiny on the day-to-day analysis of data. Shiny allows the ease of data analysis using R capabilities for users who are not data scientists.

This is a simple yet powerful tool to present data and predict possible outcomes, in a web-based interactive environment.

The dataset used would be mtcars that was used during the course Regression Models.

The Shiny app developed was to show how simple it is to do introductory exploratory analysis, coupled with boxplot, and allow users to study the relationship of different variables on mpg.

--- .class #id

## Introduction to mtcars dataset

#### Description

The data was extracted from the 1974 Motor Trend US magazine, and comprises fuel consumption and 10 aspects of automobile design and performance for 32 automobiles (1973-74 models).

#### Format

A data frame with 32 observations on 11 variables.


```r
head(mtcars,5)
```

```
##                    mpg cyl disp  hp drat    wt  qsec vs am gear carb
## Mazda RX4         21.0   6  160 110 3.90 2.620 16.46  0  1    4    4
## Mazda RX4 Wag     21.0   6  160 110 3.90 2.875 17.02  0  1    4    4
## Datsun 710        22.8   4  108  93 3.85 2.320 18.61  1  1    4    1
## Hornet 4 Drive    21.4   6  258 110 3.08 3.215 19.44  1  0    3    1
## Hornet Sportabout 18.7   8  360 175 3.15 3.440 17.02  0  0    3    2
```

--- .class #id

## The Shiny App

- The Shiny app allows users to select the variables to be compared and provides a basic introductory exploratory analysis for mtCars which includes a box plot, GG plot and pair plots tabs for users to see the data behaviour.

<iframe src = 'https://antheatan.shinyapps.io/Developing-Data-Products/' height='500px'></iframe>

--- .class #id

## Links

Github repository

https://github.com/AntheaTan/Developing-Data-Products

Shiny App Link

https://antheatan.shinyapps.io/Developing-Data-Products/

Presentation link

http://antheatan.github.io/Developing-Data-Products
