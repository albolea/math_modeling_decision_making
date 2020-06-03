# load libraries
library(lpSolveAPI)



# Set up the cost minimization
supp_chain <- make.lp(0, 30)

# set objective function to min
lp.control(supp_chain, sense = "min")

# Build objective function and constraints
obj_fn <- c(2.4, 3.5, 4.8, 6.8, 5.75, 3.25, 2.3, 3.4, 5.25, 6.00, 4.05, 3.25, 2.85, 4.3, 4.75, 5.25, 6.05, 4.30, 3.25, 2.75, 6.95, 5.85, 4.8, 2.1, 3.5, 40000, 30000, 25000, 40000, 30000)
set.objfn(supp_chain, obj_fn)
# set.objfn(supp_chain, c(2.4,3.5,4.8,6.8,5.75,3.25,2.3,3.4,5.25,6,4.05,3.25,2.85,4.3,4.75,5.25,6.05,4.3,3.25,2.75,6.95,5.85,4.8,2.1,3.5,40000,30000,25000,40000,30000))



# the 5 WH site decision variables should be binary (1 = use the site, 0 = do not use the site)
set.type(supp_chain, c(26:30), "binary")

# Only have fixed cost if we use that Wearhpuse
add.constraint(supp_chain, c(rep(1, 5), rep(0,20), -20000, 0, 0, 0, 0), "<=", 0)
add.constraint(supp_chain, c(rep(0,5), rep(1,5), rep(0,15), 0, -20000, 0, 0, 0), "<=", 0)
add.constraint(supp_chain, c(rep(0, 10), rep(1, 5), rep(0,10), 0, 0, -15000, 0, 0), "<=", 0)
add.constraint(supp_chain, c(rep(0, 15), rep(1, 5), rep(0, 5), 0, 0, 0, -25000, 0), "<=", 0)
add.constraint(supp_chain, c(rep(0, 20), rep(1, 5), 0, 0, 0, 0, -15000), "<=", 0)




# Set Demand
add.constraint(supp_chain, c(rep(c(1, 0, 0, 0, 0),5), rep(0, 5)), "=", 8000)
add.constraint(supp_chain, c(rep(c(0, 1, 0, 0, 0),5), rep(0, 5)), "=", 12000)
add.constraint(supp_chain, c(rep(c(0, 0, 1, 0, 0),5), rep(0, 5)), "=", 9000)
add.constraint(supp_chain, c(rep(c(0, 0, 0, 1, 0),5), rep(0, 5)), "=", 14000)
add.constraint(supp_chain, c(rep(c(0, 0, 0, 0, 1),5), rep(0, 5)), "=", 17000)

# solve
solve(supp_chain)

get.objective(supp_chain)

get.variables(supp_chain)