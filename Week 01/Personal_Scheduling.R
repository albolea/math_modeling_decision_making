# Course: 5260 Math models for decision making
# Title: Personal Scheduling - Exercise Book pages 57-59
# Purpose: Personal Scheduling
# Date: April, 4th, 2020
# Author: Renato Albolea 

# Clear environment of variables and functions
rm(list = ls(all = TRUE)) 

# Clear environmet of packages
if(is.null(sessionInfo()$otherPkgs) == FALSE)lapply(paste("package:", names(sessionInfo()$otherPkgs), sep=""), 
                                                    detach, character.only = TRUE, unload = TRUE)
# Load lpSolve package to demonstrate simple LP
library(lpSolveAPI)

# Defining the model: ps => Personal Schedule
ps <- make.lp(8, 5)

# Force only Integers on the solution
set.type(ps, 1:5, "int")

# Setting the problem as minimization:
lp.control(ps, sense="min")

# objective function
set.objfn(ps, c(170, 160, 175, 180, 195))

# Setting Constraints
set.row(ps, 1, c(1, 0, 0, 0, 0))
set.row(ps, 2, c(1, 1, 0, 0, 0))
set.row(ps, 3, c(1, 1, 1, 0, 0))
set.row(ps, 4, c(0, 1, 1, 0, 0))
set.row(ps, 5, c(0, 0, 1, 1, 0))
set.row(ps, 6, c(0, 0, 0, 1, 0))
set.row(ps, 7, c(0, 0, 0, 1, 1))
set.row(ps, 8, c(0, 0, 0, 0, 1))

# Seting the inequalities for the constraints
set.constr.type(ps, c(">=", ">=", ">=", ">=", ">=", ">=", ">=", ">="))

# Setting the resources contraint values
set.rhs(ps, c(48, 79, 87, 64, 82, 43, 52, 15))

# The Model:
ps

# Solving the model
solve(ps)

# Objective function Value:
get.objective(ps)

# THe optimal variables:
get.variables(ps)

# How much of the constraints we use?
get.constraints(ps)