# Course: 5260 Math models for decision making
# Title: Week 1 Practice Problem 2: The Phone Manufacturer 
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
phones <- make.lp(0, 2)

# Force only Integers on the solution
set.type(phones, 1:2, "int")

# Setting the problem as minimization:
lp.control(phones, sense="max")

# objective function
set.objfn(phones, c(150, 250))

# Setting Constraints
add.constraint(phones, c(1, 0), "<=", 7000)
add.constraint(phones, c(0, 1), "<=", 13500)
add.constraint(phones, c(4, 5), "<=", 90000)
add.constraint(phones, c(1, 1.5), "<=", 29000)

# The Model:
phones

# Solving the model
solve(phones)

# Objective function Value:
get.objective(phones)

# THe optimal variables:
get.variables(phones)

# How much of the constraints we use?
get.constraints(phones)