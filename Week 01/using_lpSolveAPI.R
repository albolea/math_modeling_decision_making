# Course: 5260 Math models for decision making
# Title: Week 1 - Constrained Optimization and Linear Programming
# Purpose: Basic demonstration of lpSolve and lpSolveAPI
# Date: April, 4th, 2020
# Author: Renato Albolea based on Gareth Green

# Clear environment of variables and functions
rm(list = ls(all = TRUE)) 

# Clear environmet of packages
if(is.null(sessionInfo()$otherPkgs) == FALSE)lapply(paste("package:", names(sessionInfo()$otherPkgs), sep=""), 
                                                    detach, character.only = TRUE, unload = TRUE)
# Load lpSolve package to demonstrate simple LP
library(lpSolveAPI)

# Defining the problem as 3 constraints and 2 Variables:
lpAPI <- make.lp(nrow = 3, ncol = 2)

# Setting the problem as Maximization:
lp.control(lpAPI, sense="max")

set.objfn(lpAPI, c(40, 20))

# Setting Constraints
set.row(lpAPI, 1, c(30, 20))
set.row(lpAPI, 2, c(10, 10))
set.row(lpAPI, 3, c(1, 0))

# Seting the inequalities for the constraints
set.constr.type(lpAPI, c("<=", "<=", "<="))

# Setting the resources contraint values
set.rhs(lpAPI, c(1200, 500, 30))

# The Model:
lpAPI

# Solving the model: IT NEED TO RETURN 0! It means that it found a solution
solve(lpAPI)

# Objective function Value:
get.objective((lpAPI))

# THe optimal variables:
get.variables(lpAPI)

# How much of the constraints we use?
get.constraints(lpAPI)


# Lets add a new constraint:
# Shoes needs to be more than 3x Boots => 3B - S <=0
add.constraint(lpAPI, c(3, -1), "<=", 0)

# Solve the model again
solve(lpAPI)

# Objective function Value:
get.objective((lpAPI))

# THe optimal variables:
get.variables(lpAPI)

# How much of the constraints we use?
get.constraints(lpAPI)