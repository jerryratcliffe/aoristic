
<!-- README.md is generated from README.Rmd. Please edit that file -->

# aoristic

<!-- badges: start -->

[![Travis build
status](https://travis-ci.com/jerryratcliffe/aoristic.svg?branch=master)](https://travis-ci.com/jerryratcliffe/aoristic)
<!-- badges: end -->

The goal of aoristic is to make some sense of temporally vague data. It
can sometimes be difficult to ascertain when some events (such as
property crime) occur because the victim is not present when the crime
occurs. As a result, police databases often record a ‘start’ or ‘from’
date and time, and an ‘end’ or ‘to’ date and time. The ‘start’ datetime
usually references when the victim last saw their stolen property, and
the ‘to’ datetime records when they first discovered their property
missing. The period between the ‘start’ datetime and ‘end’ datetime is
referred to as the event’s ‘time span’.

The time span between these date/times can be minutes, hours, or
sometimes days: hence the term ‘Aoristic’, a word meaning “denoting
simple occurrence of an action without reference to its completeness,
duration, or repetition”. It has its origins in the Greek word
*aoristos* which means undefined. For events with a location describes
with either a latitude/longitude, or X,Y coordinate pair, and a start
and end date/time, this package generates an aoristic data.frame with
aoristic weighted probability values for each hour of the week, for each
row. Various descriptive outputs are available.

## What’s new?

### Version 1.0.0

A lot\! Version 0.6 was originally released on CRAN in 2015 by
Dr. George Kikuchi then of Fresno State University and now at the
Philadelphia Police Department. Given his extensive responsibilities he
has been unable to maintain and update the program since the initial
release. With his permission, the package has been taken over in 2020
and updated by Dr. Jerry Ratcliffe of Temple University.

Much of the original functionality has been deprecated and replaced by
this updated package. In particular, version 1.0.0 onwards dispenses
with rounding to the nearest hour for time spans, and uses a
minute-by-minute method. In earlier versions, as was common in aoristic
approaches until recently, time spans were rounded up and down. So an
event that happened between 10.55am and 11.55am would have an aoristic
weight of 0.5 assigned to each hour, 1000-1059 and 1100-1159. This is
despite the majority of the event occurring in the 11am hour. That
rounding is removed in v1.0.0 and aoristic weightings are assigned by
the minute.

The kml mapping function from v0.6 is replaced here with a simpler plot
function that maps the individual points for an user-selected hour. See
?aoristic.map

There is a new graph function that plots the overall aoristic
distribution for an entire week, as well as each individual day of the
week. see ?aoristic.graph

See the example below for a workflow example.

## Installation

You can install the released version of aoristic from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("aoristic")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("jerryratcliffe/aoristic")
```

## Example

The package has some limited error checking; however, the main challenge
that users will face is getting the data into the correct datetime
format. Most of the heavy lifting is done by the aoristic.df() function.
The user passes the name of a data frame and four parameters
representing columns that contain

  - **Xcoord** a vector of the event X coordinate or latitude (passed
    through for user)

  - **Ycoord** a vector of the event Y coordinate or longitude (passed
    through for user)

  - **DateTimeFrom** a vector for the ‘From’ datetime (POSIXct date-time
    object)

  - **DateTimeTo** a vector for the ‘To’ datetime (POSIXct date-time
    object)

The package ‘lubridate’ is recommended as a way to more easily get the
date time data into the correct format.

This is a basic example which shows you how to solve a common problem:

``` r
library(aoristic)
## basic example code
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub\!
