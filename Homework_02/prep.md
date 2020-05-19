---
title: "Preparation for Homework 02"
author: "Renato Albolea"
date: "4/28/2020"
output: 
  html_document: 
    keep_md: yes
    toc: yes
    toc_float: true
    toc_depth: 4
    code_download: yes

---


```r
knitr::opts_chunk$set(echo = TRUE)
# Clear environment of variables and functions
rm(list = ls(all = TRUE)) 

# Clear environmet of packages
if(is.null(sessionInfo()$otherPkgs) == FALSE)lapply(paste("package:", names(sessionInfo()$otherPkgs), sep=""), 
                                                    detach, character.only = TRUE, unload = TRUE)
# Load lpSolve package to demonstrate simple LP
library(lpSolveAPI)
`%>%` <- magrittr::`%>%`
library(knitr)
library(kableExtra)
```


```r
# Course: 5260 Math models for decision making
# Title: Preparation for Homework #2 
# Date: April 28th, 2020
# Author: Renato Albolea
```




# Problem 9.1-2  The Childfair Company

#### a. Formulate a linear programming model algebraically  

  
####   b. Set up and solve in R.  




```r
# Set up the cost minimization
trans_9_2 <- make.lp(0, 12)

# Build objective function and constraints
obj_fn <- c(100+800*0.5, 100+1300*0.5, 100+400*0.5, 100+700*0.5, 100+1100*0.5, 100+1400*0.5, 100+600*0.5, 100+1000*0.5, 100+600*0.5, 100+1200*0.5, 100+800*0.5, 100+900*0.5)
set.objfn(trans_9_2, obj_fn)

# Distribution Constraint
add.constraint(trans_9_2, c(rep(c(1, 0, 0, 0),3)), "=", 10)
add.constraint(trans_9_2, c(rep(c(0, 1, 0, 0),3)), "=", 10)
add.constraint(trans_9_2, c(rep(c(0, 0, 1, 0),3)), "=", 10)
add.constraint(trans_9_2, c(rep(c(0, 0, 0, 1),3)), "=", 10)

# Plant Constraint
add.constraint(trans_9_2, c(rep(1, 4), rep(0, 8)), "<=", 12)
add.constraint(trans_9_2, c(rep(0, 4),rep(1, 4), rep(0, 4)), "<=", 17)
add.constraint(trans_9_2, c(rep(0, 8),rep(1, 4)), "<=", 11)


# Solve the model, if this return 0 an optimal solution is found
solve(trans_9_2)
```

```
## [1] 0
```

```r
sensitivity_table(trans_9_2)
```

<table class="table table-striped table-bordered" style="margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:left;"> R1 </th>
   <th style="text-align:left;"> R2 </th>
   <th style="text-align:left;"> R3 </th>
   <th style="text-align:left;"> R4 </th>
   <th style="text-align:left;"> R5 </th>
   <th style="text-align:left;"> R6 </th>
   <th style="text-align:left;"> R7 </th>
   <th style="text-align:left;"> C1 </th>
   <th style="text-align:left;"> C2 </th>
   <th style="text-align:left;"> C3 </th>
   <th style="text-align:left;"> C4 </th>
   <th style="text-align:left;"> C5 </th>
   <th style="text-align:left;"> C6 </th>
   <th style="text-align:left;"> C7 </th>
   <th style="text-align:left;"> C8 </th>
   <th style="text-align:left;"> C9 </th>
   <th style="text-align:left;"> C10 </th>
   <th style="text-align:left;"> C11 </th>
   <th style="text-align:left;"> C12 </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> solution </td>
   <td style="text-align:left;"> 10 </td>
   <td style="text-align:left;"> 10 </td>
   <td style="text-align:left;"> 10 </td>
   <td style="text-align:left;"> 10 </td>
   <td style="text-align:left;"> 12 </td>
   <td style="text-align:left;"> 17 </td>
   <td style="text-align:left;"> 11 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> 10 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 9 </td>
   <td style="text-align:left;"> 8 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 10 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> duals/coef </td>
   <td style="text-align:left;"> 500 </td>
   <td style="text-align:left;"> 800 </td>
   <td style="text-align:left;"> 400 </td>
   <td style="text-align:left;"> 550 </td>
   <td style="text-align:left;"> -100 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> -100 </td>
   <td style="text-align:left;"> 500 </td>
   <td style="text-align:left;"> 750 </td>
   <td style="text-align:left;"> 300 </td>
   <td style="text-align:left;"> 450 </td>
   <td style="text-align:left;"> 650 </td>
   <td style="text-align:left;"> 800 </td>
   <td style="text-align:left;"> 400 </td>
   <td style="text-align:left;"> 600 </td>
   <td style="text-align:left;"> 400 </td>
   <td style="text-align:left;"> 700 </td>
   <td style="text-align:left;"> 500 </td>
   <td style="text-align:left;"> 550 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Sens From </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> 12 </td>
   <td style="text-align:left;"> -inf </td>
   <td style="text-align:left;"> 11 </td>
   <td style="text-align:left;"> 400 </td>
   <td style="text-align:left;"> 700 </td>
   <td style="text-align:left;"> 250 </td>
   <td style="text-align:left;"> -inf </td>
   <td style="text-align:left;"> 500 </td>
   <td style="text-align:left;"> 700 </td>
   <td style="text-align:left;"> 350 </td>
   <td style="text-align:left;"> 550 </td>
   <td style="text-align:left;"> -inf </td>
   <td style="text-align:left;"> 600 </td>
   <td style="text-align:left;"> 300 </td>
   <td style="text-align:left;"> 450 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Sens Till </td>
   <td style="text-align:left;"> 10 </td>
   <td style="text-align:left;"> 10 </td>
   <td style="text-align:left;"> 10 </td>
   <td style="text-align:left;"> 10 </td>
   <td style="text-align:left;"> 20 </td>
   <td style="text-align:left;"> inf </td>
   <td style="text-align:left;"> 20 </td>
   <td style="text-align:left;"> inf </td>
   <td style="text-align:left;"> inf </td>
   <td style="text-align:left;"> 350 </td>
   <td style="text-align:left;"> 500 </td>
   <td style="text-align:left;"> inf </td>
   <td style="text-align:left;"> 850 </td>
   <td style="text-align:left;"> 450 </td>
   <td style="text-align:left;"> inf </td>
   <td style="text-align:left;"> 500 </td>
   <td style="text-align:left;"> 800 </td>
   <td style="text-align:left;"> inf </td>
   <td style="text-align:left;"> inf </td>
  </tr>
</tbody>
<tfoot>
<tr>
<td style = 'padding: 0; border:0;' colspan='100%'><sup></sup> Objective Value = 20200</td>
</tr>
</tfoot>
</table>


# Problem 9.1-4  The Versatech Corporation

Plant      | Prod 1  |  Prod 2  | Prod 3  | Adjst Demand  | Capacity 
---------- | ------  |  ------- | ------- | -------       | -------     
1          | 31      | 45       | 38      | 0             | 400       
2          | 29      | 41       | 35      | 0             | 600       
3          | 32      | 46       | 40      | 0             | 400
4          | 28      | 42       | 1000    | 0             | 600
5          | 29      | 43       | 1000    | 0             | 1,000
Forecast   | 600     | 1,000    | 800     | 600           |
      
      
#### b. Set up and solve in R.  




```r
# Set up the cost minimization
trans_9_4 <- make.lp(0, 20)

# Build objective function and constraints
obj_fn <- c(31, 45, 38, 0, 
            29, 41, 35, 0, 
            32, 46, 40, 0,
            28, 42, 1000, 0,
            29, 43, 1000, 0)

matrix(obj_fn, nrow = 5, byrow=TRUE)
```

```
##      [,1] [,2] [,3] [,4]
## [1,]   31   45   38    0
## [2,]   29   41   35    0
## [3,]   32   46   40    0
## [4,]   28   42 1000    0
## [5,]   29   43 1000    0
```

```r
set.objfn(trans_9_4, obj_fn)


# Distribution Constraint
add.constraint(trans_9_4, c(rep(c(1, 0, 0, 0),5)), "=", 600)
add.constraint(trans_9_4, c(rep(c(0, 1, 0, 0),5)), "=", 1000)
add.constraint(trans_9_4, c(rep(c(0, 0, 1, 0),5)), "=", 800)
add.constraint(trans_9_4, c(rep(c(0, 0, 0, 1),5)), "=", 600)

# Plant Constraint
add.constraint(trans_9_4, c(rep(1, 4), rep(0, 16)), "<=", 400)
add.constraint(trans_9_4, c(rep(0, 4),rep(1, 4), rep(0, 12)), "<=", 600)
add.constraint(trans_9_4, c(rep(0, 8),rep(1, 4), rep(0, 8)), "<=", 400)
add.constraint(trans_9_4, c(rep(0, 12),rep(1, 4), rep(0, 4)), "<=", 600)
add.constraint(trans_9_4, c(rep(0, 16),rep(1, 4)), "<=", 1000)

# # Add names
# dimnames(trans_9_4) <- list(c("O", "A","B", "C", "D", "E", "T"), 
#                       c("OA", "OB", "OC", "AB", "AD", "BC", "BD", "BE", "CE", "DE", "DT", "ET"))

# Solve the model, if this return 0 an optimal solution is found
solve(trans_9_4)
```

```
## [1] 0
```

```r
sensitivity_table(trans_9_4)
```

<table class="table table-striped table-bordered" style="margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:left;"> R1 </th>
   <th style="text-align:left;"> R2 </th>
   <th style="text-align:left;"> R3 </th>
   <th style="text-align:left;"> R4 </th>
   <th style="text-align:left;"> R5 </th>
   <th style="text-align:left;"> R6 </th>
   <th style="text-align:left;"> R7 </th>
   <th style="text-align:left;"> R8 </th>
   <th style="text-align:left;"> R9 </th>
   <th style="text-align:left;"> C1 </th>
   <th style="text-align:left;"> C2 </th>
   <th style="text-align:left;"> C3 </th>
   <th style="text-align:left;"> C4 </th>
   <th style="text-align:left;"> C5 </th>
   <th style="text-align:left;"> C6 </th>
   <th style="text-align:left;"> C7 </th>
   <th style="text-align:left;"> C8 </th>
   <th style="text-align:left;"> C9 </th>
   <th style="text-align:left;"> C10 </th>
   <th style="text-align:left;"> C11 </th>
   <th style="text-align:left;"> C12 </th>
   <th style="text-align:left;"> C13 </th>
   <th style="text-align:left;"> C14 </th>
   <th style="text-align:left;"> C15 </th>
   <th style="text-align:left;"> C16 </th>
   <th style="text-align:left;"> C17 </th>
   <th style="text-align:left;"> C18 </th>
   <th style="text-align:left;"> C19 </th>
   <th style="text-align:left;"> C20 </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> solution </td>
   <td style="text-align:left;"> 600 </td>
   <td style="text-align:left;"> 1000 </td>
   <td style="text-align:left;"> 800 </td>
   <td style="text-align:left;"> 600 </td>
   <td style="text-align:left;"> 400 </td>
   <td style="text-align:left;"> 600 </td>
   <td style="text-align:left;"> 400 </td>
   <td style="text-align:left;"> 600 </td>
   <td style="text-align:left;"> 1000 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 200 </td>
   <td style="text-align:left;"> 200 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 600 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 400 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 600 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 600 </td>
   <td style="text-align:left;"> 400 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> duals/coef </td>
   <td style="text-align:left;"> 29 </td>
   <td style="text-align:left;"> 43 </td>
   <td style="text-align:left;"> 38 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> -3 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> -1 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 31 </td>
   <td style="text-align:left;"> 45 </td>
   <td style="text-align:left;"> 38 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 29 </td>
   <td style="text-align:left;"> 41 </td>
   <td style="text-align:left;"> 35 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 32 </td>
   <td style="text-align:left;"> 46 </td>
   <td style="text-align:left;"> 40 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 28 </td>
   <td style="text-align:left;"> 42 </td>
   <td style="text-align:left;"> 1000 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 29 </td>
   <td style="text-align:left;"> 43 </td>
   <td style="text-align:left;"> 1000 </td>
   <td style="text-align:left;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Sens From </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 600 </td>
   <td style="text-align:left;"> 600 </td>
   <td style="text-align:left;"> 200 </td>
   <td style="text-align:left;"> 400 </td>
   <td style="text-align:left;"> 600 </td>
   <td style="text-align:left;"> -inf </td>
   <td style="text-align:left;"> 600 </td>
   <td style="text-align:left;"> -inf </td>
   <td style="text-align:left;"> 29 </td>
   <td style="text-align:left;"> 43 </td>
   <td style="text-align:left;"> 37 </td>
   <td style="text-align:left;"> -2 </td>
   <td style="text-align:left;"> 26 </td>
   <td style="text-align:left;"> 40 </td>
   <td style="text-align:left;"> -inf </td>
   <td style="text-align:left;"> -3 </td>
   <td style="text-align:left;"> 29 </td>
   <td style="text-align:left;"> 43 </td>
   <td style="text-align:left;"> 38 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 28 </td>
   <td style="text-align:left;"> -inf </td>
   <td style="text-align:left;"> 37 </td>
   <td style="text-align:left;"> -1 </td>
   <td style="text-align:left;"> -inf </td>
   <td style="text-align:left;"> 43 </td>
   <td style="text-align:left;"> 38 </td>
   <td style="text-align:left;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Sens Till </td>
   <td style="text-align:left;"> 600 </td>
   <td style="text-align:left;"> 1000 </td>
   <td style="text-align:left;"> 800 </td>
   <td style="text-align:left;"> 600 </td>
   <td style="text-align:left;"> 800 </td>
   <td style="text-align:left;"> 800 </td>
   <td style="text-align:left;"> inf </td>
   <td style="text-align:left;"> 1000 </td>
   <td style="text-align:left;"> inf </td>
   <td style="text-align:left;"> inf </td>
   <td style="text-align:left;"> inf </td>
   <td style="text-align:left;"> 40 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> inf </td>
   <td style="text-align:left;"> inf </td>
   <td style="text-align:left;"> 36 </td>
   <td style="text-align:left;"> inf </td>
   <td style="text-align:left;"> inf </td>
   <td style="text-align:left;"> inf </td>
   <td style="text-align:left;"> inf </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> inf </td>
   <td style="text-align:left;"> 42 </td>
   <td style="text-align:left;"> inf </td>
   <td style="text-align:left;"> inf </td>
   <td style="text-align:left;"> 29 </td>
   <td style="text-align:left;"> 44 </td>
   <td style="text-align:left;"> inf </td>
   <td style="text-align:left;"> inf </td>
  </tr>
</tbody>
<tfoot>
<tr>
<td style = 'padding: 0; border:0;' colspan='100%'><sup></sup> Objective Value = 88400</td>
</tr>
</tfoot>
</table>


# Problem 9.3-1  Assignee x Task

Assignee   | Task 1  |  Task 2  | Task 3  | Task 4 
---------- | ------  |  ------- | ------- | -------    
A          | 8       | 6        | 5       | 7             
B          | 6       | 5        | 3       | 4             
C          | 7       | 8        | 4       | 6       
D          | 6       | 7        | 5       | 6       
      
      
#### b. Set up and solve in R.  




```r
# Set up the cost minimization
trans_9_3_1 <- make.lp(0, 16)

# Build objective function and constraints
obj_fn <- c(8, 6, 5, 7,
            6, 5, 3, 4,
            7, 8, 4, 6, 
            6, 7, 5, 6)

matrix(obj_fn, nrow = 5, byrow=TRUE)
```

```
## Warning in matrix(obj_fn, nrow = 5, byrow = TRUE): data length [16] is not a
## sub-multiple or multiple of the number of rows [5]
```

```
##      [,1] [,2] [,3] [,4]
## [1,]    8    6    5    7
## [2,]    6    5    3    4
## [3,]    7    8    4    6
## [4,]    6    7    5    6
## [5,]    8    6    5    7
```

```r
set.objfn(trans_9_3_1, obj_fn)

# Task Constraint
add.constraint(trans_9_3_1, c(rep(c(1, 0, 0, 0),4)), "=", 1)
add.constraint(trans_9_3_1, c(rep(c(0, 1, 0, 0),4)), "=", 1)
add.constraint(trans_9_3_1, c(rep(c(0, 0, 1, 0),4)), "=", 1)
add.constraint(trans_9_3_1, c(rep(c(0, 0, 0, 1),4)), "=", 1)

# Assignee Constraint
add.constraint(trans_9_3_1, c(rep(1, 4), rep(0, 12)), "=", 1)
add.constraint(trans_9_3_1, c(rep(0, 4),rep(1, 4), rep(0, 8)), "=", 1)
add.constraint(trans_9_3_1, c(rep(0, 8),rep(1, 4), rep(0, 4)), "=", 1)
add.constraint(trans_9_3_1, c(rep(0, 12),rep(1, 4)), "=", 1)


# Solve the model, if this return 0 an optimal solution is found
solve(trans_9_3_1)
```

```
## [1] 0
```

```r
sensitivity_table(trans_9_3_1)
```

<table class="table table-striped table-bordered" style="margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:left;"> R1 </th>
   <th style="text-align:left;"> R2 </th>
   <th style="text-align:left;"> R3 </th>
   <th style="text-align:left;"> R4 </th>
   <th style="text-align:left;"> R5 </th>
   <th style="text-align:left;"> R6 </th>
   <th style="text-align:left;"> R7 </th>
   <th style="text-align:left;"> R8 </th>
   <th style="text-align:left;"> C1 </th>
   <th style="text-align:left;"> C2 </th>
   <th style="text-align:left;"> C3 </th>
   <th style="text-align:left;"> C4 </th>
   <th style="text-align:left;"> C5 </th>
   <th style="text-align:left;"> C6 </th>
   <th style="text-align:left;"> C7 </th>
   <th style="text-align:left;"> C8 </th>
   <th style="text-align:left;"> C9 </th>
   <th style="text-align:left;"> C10 </th>
   <th style="text-align:left;"> C11 </th>
   <th style="text-align:left;"> C12 </th>
   <th style="text-align:left;"> C13 </th>
   <th style="text-align:left;"> C14 </th>
   <th style="text-align:left;"> C15 </th>
   <th style="text-align:left;"> C16 </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> solution </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> duals/coef </td>
   <td style="text-align:left;"> 6 </td>
   <td style="text-align:left;"> 5 </td>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 8 </td>
   <td style="text-align:left;"> 6 </td>
   <td style="text-align:left;"> 5 </td>
   <td style="text-align:left;"> 7 </td>
   <td style="text-align:left;"> 6 </td>
   <td style="text-align:left;"> 5 </td>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:left;"> 7 </td>
   <td style="text-align:left;"> 8 </td>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:left;"> 6 </td>
   <td style="text-align:left;"> 6 </td>
   <td style="text-align:left;"> 7 </td>
   <td style="text-align:left;"> 5 </td>
   <td style="text-align:left;"> 6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Sens From </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> -inf </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 7 </td>
   <td style="text-align:left;"> -inf </td>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:left;"> 5 </td>
   <td style="text-align:left;"> -inf </td>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> -inf </td>
   <td style="text-align:left;"> 5 </td>
   <td style="text-align:left;"> 6 </td>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:left;"> 5 </td>
   <td style="text-align:left;"> -inf </td>
   <td style="text-align:left;"> 5 </td>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> 4 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Sens Till </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> inf </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> inf </td>
   <td style="text-align:left;"> 7 </td>
   <td style="text-align:left;"> inf </td>
   <td style="text-align:left;"> inf </td>
   <td style="text-align:left;"> 6 </td>
   <td style="text-align:left;"> inf </td>
   <td style="text-align:left;"> inf </td>
   <td style="text-align:left;"> 5 </td>
   <td style="text-align:left;"> inf </td>
   <td style="text-align:left;"> inf </td>
   <td style="text-align:left;"> 5 </td>
   <td style="text-align:left;"> inf </td>
   <td style="text-align:left;"> 8 </td>
   <td style="text-align:left;"> inf </td>
   <td style="text-align:left;"> inf </td>
   <td style="text-align:left;"> inf </td>
  </tr>
</tbody>
<tfoot>
<tr>
<td style = 'padding: 0; border:0;' colspan='100%'><sup></sup> Objective Value = 20</td>
</tr>
</tfoot>
</table>



# Problem 10.3-2 Trip to another town


Town   | A       |  B       | C       | D       |E       |Destination
-----  | ------  |  ------- | ------- | ------- |------- |-------     
Origin | 40      | 60       | 50      | ----    | ----   | ----            
A      |         | 10       | ----    | 70      | ----   | ----            
B      |         |          | 20      | 55      | 40     | ---- 
C      |         |          |         |         | 50     | ----
D      |         |          |         |         | 10     | 60
E      |         |          |         |         |        | 80
      
      
#### b. Set up and solve in R.  




```r
# Set up the cost minimization
shot_path_10_3_2 <- make.lp(0, 19)

# Build objective function and constraints
obj_fn <- c(40, 60, 50, # Origin
            10, 10, 70, 70,     # A
            20, 20, 55, 55, 40, 40, # B
            50, 50,         # C
            10, 10, 60,     # D
            80)         # E
            

set.objfn(shot_path_10_3_2, obj_fn)

# Edges Constraint                 OA,OB,OC,AB,BA,AD,DA,BC,CB,BD,DB,BE,EB,CE,EC,ED,DE,DT,ET
add.constraint(shot_path_10_3_2, c( 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), "=", 1) # Origin
add.constraint(shot_path_10_3_2, c(-1, 0, 0, 1,-1, 1,-1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), "=", 0) # A
add.constraint(shot_path_10_3_2, c( 0,-1, 0,-1, 1, 0, 0, 1,-1, 1,-1, 1,-1, 0, 0, 0, 0, 0, 0), "=", 0) # B
add.constraint(shot_path_10_3_2, c( 0, 0,-1, 0, 0, 0, 0,-1, 1, 0, 0, 0, 0, 1,-1, 0, 0, 0, 0), "=", 0) # C
add.constraint(shot_path_10_3_2, c( 0, 0, 0, 0, 0,-1, 1, 0, 0,-1, 1, 0, 0, 0, 0,-1, 1, 1, 0), "=", 0) # D
add.constraint(shot_path_10_3_2, c( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,-1, 1,-1, 1, 1,-1, 0, 1), "=", 0) # E
add.constraint(shot_path_10_3_2, c( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,-1,-1), "=", -1) # T

# Add names
dimnames(shot_path_10_3_2) <- list(c("O", "A","B", "C", "D", "E", "T"), 
                      c("OA", "OB", "OC", "AB", "BA", "AD", "DA", "BC", "CB", "BD", "DB", "BE", "EB", "CE", "EC", 
                        "DE", "ED", "DT", "ET"))


# Write to view the algebraic formulation
write.lp(shot_path_10_3_2, "shot_path_10_3_2.lp",type = 'lp')

# Solve the model
solve(shot_path_10_3_2)
```

```
## [1] 0
```

```r
sensitivity_table(shot_path_10_3_2)
```

<table class="table table-striped table-bordered" style="margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:left;"> O </th>
   <th style="text-align:left;"> A </th>
   <th style="text-align:left;"> B </th>
   <th style="text-align:left;"> C </th>
   <th style="text-align:left;"> D </th>
   <th style="text-align:left;"> E </th>
   <th style="text-align:left;"> T </th>
   <th style="text-align:left;"> OA </th>
   <th style="text-align:left;"> OB </th>
   <th style="text-align:left;"> OC </th>
   <th style="text-align:left;"> AB </th>
   <th style="text-align:left;"> BA </th>
   <th style="text-align:left;"> AD </th>
   <th style="text-align:left;"> DA </th>
   <th style="text-align:left;"> BC </th>
   <th style="text-align:left;"> CB </th>
   <th style="text-align:left;"> BD </th>
   <th style="text-align:left;"> DB </th>
   <th style="text-align:left;"> BE </th>
   <th style="text-align:left;"> EB </th>
   <th style="text-align:left;"> CE </th>
   <th style="text-align:left;"> EC </th>
   <th style="text-align:left;"> DE </th>
   <th style="text-align:left;"> ED </th>
   <th style="text-align:left;"> DT </th>
   <th style="text-align:left;"> ET </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> solution </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> -1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> duals/coef </td>
   <td style="text-align:left;"> 50 </td>
   <td style="text-align:left;"> 10 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> -50 </td>
   <td style="text-align:left;"> -40 </td>
   <td style="text-align:left;"> -110 </td>
   <td style="text-align:left;"> 40 </td>
   <td style="text-align:left;"> 60 </td>
   <td style="text-align:left;"> 50 </td>
   <td style="text-align:left;"> 10 </td>
   <td style="text-align:left;"> 10 </td>
   <td style="text-align:left;"> 70 </td>
   <td style="text-align:left;"> 70 </td>
   <td style="text-align:left;"> 20 </td>
   <td style="text-align:left;"> 20 </td>
   <td style="text-align:left;"> 55 </td>
   <td style="text-align:left;"> 55 </td>
   <td style="text-align:left;"> 40 </td>
   <td style="text-align:left;"> 40 </td>
   <td style="text-align:left;"> 50 </td>
   <td style="text-align:left;"> 50 </td>
   <td style="text-align:left;"> 10 </td>
   <td style="text-align:left;"> 10 </td>
   <td style="text-align:left;"> 60 </td>
   <td style="text-align:left;"> 80 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Sens From </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> -inf </td>
   <td style="text-align:left;"> -inf </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> -1 </td>
   <td style="text-align:left;"> -inf </td>
   <td style="text-align:left;"> 50 </td>
   <td style="text-align:left;"> -inf </td>
   <td style="text-align:left;"> -10 </td>
   <td style="text-align:left;"> -10 </td>
   <td style="text-align:left;"> 60 </td>
   <td style="text-align:left;"> -60 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 50 </td>
   <td style="text-align:left;"> -50 </td>
   <td style="text-align:left;"> -40 </td>
   <td style="text-align:left;"> -40 </td>
   <td style="text-align:left;"> 40 </td>
   <td style="text-align:left;"> -40 </td>
   <td style="text-align:left;"> -10 </td>
   <td style="text-align:left;"> -10 </td>
   <td style="text-align:left;"> -inf </td>
   <td style="text-align:left;"> 70 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Sens Till </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> inf </td>
   <td style="text-align:left;"> inf </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> -1 </td>
   <td style="text-align:left;"> 40 </td>
   <td style="text-align:left;"> inf </td>
   <td style="text-align:left;"> 50 </td>
   <td style="text-align:left;"> 10 </td>
   <td style="text-align:left;"> inf </td>
   <td style="text-align:left;"> inf </td>
   <td style="text-align:left;"> inf </td>
   <td style="text-align:left;"> inf </td>
   <td style="text-align:left;"> inf </td>
   <td style="text-align:left;"> inf </td>
   <td style="text-align:left;"> inf </td>
   <td style="text-align:left;"> 45 </td>
   <td style="text-align:left;"> inf </td>
   <td style="text-align:left;"> inf </td>
   <td style="text-align:left;"> inf </td>
   <td style="text-align:left;"> 15 </td>
   <td style="text-align:left;"> inf </td>
   <td style="text-align:left;"> 70 </td>
   <td style="text-align:left;"> inf </td>
  </tr>
</tbody>
<tfoot>
<tr>
<td style = 'padding: 0; border:0;' colspan='100%'><sup></sup> Objective Value = 160</td>
</tr>
</tfoot>
</table>


# Problem 10.6-2 Flow from A to F


Town   | B       |  C       | D       | E       | F      
-----  | ------  |  ------- | ------- | ------- |-------      
A      | 9       | 7        | ----    | ----    | 20               
B      |         | ----     | 7       | 2       | ----               
C      |         |          | 4       | 6       | ----      
D      |         |          |         | 3       | 6     
E      |         |          |         |         | 9     
      
      
#### b. Set up and solve in R.  




```r
# Set up the cost minimization
shot_path_10_6_2 <- make.lp(0, 10)

# Build objective function and constraints
obj_fn <- c(9, 7, 20, # A
            7, 2,     # B
            4, 6,     # C
            3, 6,     # D
            9)        # E
            

set.objfn(shot_path_10_6_2, obj_fn)

# Edges Constraint
add.constraint(shot_path_10_6_2, c( 1, 1, 1, 0, 0, 0, 0, 0, 0, 0), "=", 1) # A
add.constraint(shot_path_10_6_2, c(-1, 0, 0, 1, 1, 0, 0, 0, 0, 0), "=", 0) # B
add.constraint(shot_path_10_6_2, c( 0,-1, 0, 0, 0, 1, 1, 0, 0, 0), "=", 0) # C
add.constraint(shot_path_10_6_2, c( 0, 0, 0,-1, 0,-1, 0, 1, 1, 0), "=", 0) # D
add.constraint(shot_path_10_6_2, c( 0, 0, 0, 0, 0, 0, 0,-1,-1,-1), "=", 0) # E
add.constraint(shot_path_10_6_2, c( 0, 0,-1, 0, 0, 0, 0, 0,-1,-1), "=",-1) # F

# Add names
dimnames(shot_path_10_6_2) <- list(c("A","B", "C", "D", "E", "F"), 
                      c("AB", "AC", "AF", "BD", "BE", "CD", "CE", "DE", "DF", "EF"))


# Write to view the algebraic formulation
write.lp(shot_path_10_6_2, "shot_path_10_6_2.lp",type = 'lp')

# Solve the model
solve(shot_path_10_6_2)
```

```
## [1] 0
```

```r
sensitivity_table(shot_path_10_6_2)
```

<table class="table table-striped table-bordered" style="margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:left;"> A </th>
   <th style="text-align:left;"> B </th>
   <th style="text-align:left;"> C </th>
   <th style="text-align:left;"> D </th>
   <th style="text-align:left;"> E </th>
   <th style="text-align:left;"> F </th>
   <th style="text-align:left;"> AB </th>
   <th style="text-align:left;"> AC </th>
   <th style="text-align:left;"> AF </th>
   <th style="text-align:left;"> BD </th>
   <th style="text-align:left;"> BE </th>
   <th style="text-align:left;"> CD </th>
   <th style="text-align:left;"> CE </th>
   <th style="text-align:left;"> DE </th>
   <th style="text-align:left;"> DF </th>
   <th style="text-align:left;"> EF </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> solution </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> -1 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> duals/coef </td>
   <td style="text-align:left;"> 9 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> -2 </td>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> -11 </td>
   <td style="text-align:left;"> 9 </td>
   <td style="text-align:left;"> 7 </td>
   <td style="text-align:left;"> 20 </td>
   <td style="text-align:left;"> 7 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:left;"> 6 </td>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> 6 </td>
   <td style="text-align:left;"> 9 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Sens From </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> -inf </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> -1 </td>
   <td style="text-align:left;"> -1 </td>
   <td style="text-align:left;"> 8 </td>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> 12 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> -inf </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> -5 </td>
   <td style="text-align:left;"> -inf </td>
   <td style="text-align:left;"> 8 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Sens Till </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> inf </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> -1 </td>
   <td style="text-align:left;"> inf </td>
   <td style="text-align:left;"> inf </td>
   <td style="text-align:left;"> inf </td>
   <td style="text-align:left;"> inf </td>
   <td style="text-align:left;"> inf </td>
   <td style="text-align:left;"> inf </td>
   <td style="text-align:left;"> inf </td>
   <td style="text-align:left;"> inf </td>
   <td style="text-align:left;"> inf </td>
   <td style="text-align:left;"> inf </td>
  </tr>
</tbody>
<tfoot>
<tr>
<td style = 'padding: 0; border:0;' colspan='100%'><sup></sup> Objective Value = 20</td>
</tr>
</tfoot>
</table>



