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
library(lpSolve)

# Defining parameters:

# Object Function:
obj_func <- c(40, 25)

# Defining constraint's Matrix
constrains <- matrix(c(30, 20,
                       10, 10,
                       1, 0), ncol = 2, byrow = TRUE)

# Seting the inequalities for the constraints
eq <- c("<=", "<=", "<=")

# Setting the resource constraint values (rhs => Right Hand Side)
rhs <- c(1200, 500, 30)

# Solve model
sol <-  lp("max", obj_func, constrains, eq, rhs, compute.sens = TRUE)

# View Constraints:
sol$constraints

# Optimal Decision variable values:
sol$solution

# Optimal Objective function Value:
sol$objval

# TO SOLVE AS A MINIMIZATION:

# Object Function:
obj_func_min <- c(-40, -25)

# Solve model
sol_min <-  lp("min", obj_func_min, constrains, eq, rhs, compute.sens = TRUE)

# Optimal Decision variable values:
sol_min$solution

# Optimal Objective function Value: REMEMBER THAT WE NEED TO USE THE NEGATIVE OF THIS VALUE!
sol_min$objval*(-1)



