#########################################################
########### Practical 03 - Programming Skills ###########
#########################################################


# # In this practical we will gain some experience using 
# 1 - Control statements (for loops, if/else statements etc.), 
# 2 - Functions, 
# 3 - Installing a package
# 4 - Sourcing other files

########## 1 - Control Statements  ##########
# Let's write a script to calculate the R0 for a range of infectiousness durations

# first, initialise variables
beta <- 0.2
infectiousness.duration <- 1:10
epidemic <- vector(length = length(infectiousness.duration))

# then, loop through infectious duration using index_i
for (index_i in 1:length(infectiousness.duration)) {
    # For each index_i, we can calculate R0
    R0 <- beta * infectiousness.duration[index_i]
    # Store this R0 value in the vector 'epidemic'
    epidemic[index_i] <- R0
}

# Questions:
#   a) Why do you initialise variables at the start of the script?
#   Answer: 


# Now let's write a script to calculate the R0 for a range of beta and
# infectiousness durations

# first, initialise variables
infectiousness.duration <- 1:10
beta <- seq(0.1, 0.5, by = 0.1)
epidemic <- matrix(NA,nrow = length(infectiousness.duration), ncol = length(beta))

# then, loop through infectious duration using index_i
for (index_i in 1:length(infectiousness.duration)) {
  # then, use a nested loop to loop through beta using index_j
  for (index_j in 1:length(beta)) {
    # For each index_j and index_i, we can calculate R0
    R0 <- beta[index_j] * infectiousness.duration[index_i]
    # Store this R0 value in the matrix 'epidemic'
    epidemic[index_i, index_j] <- R0
  }
}

# Questions:

#   b) We have indexed over 'index_i' and 'index_j'. Can you think of alternative indices?
#   Answer:
#   c) If we want to save whether there is an epidemic or not, rather than R0, 
#   can you add an if/else statement to capture this? 
#   Write in your answer in the following code:

infectiousness.duration <- 1:10
beta <- seq(0.1, 0.5, by = 0.1)
epidemic <- matrix(NA,nrow = length(infectiousness.duration), ncol = length(beta))

for (index.i in 1:length(infectiousness.duration)) {
  for (index.j in 1:length(beta)) {
    R0 <- beta[index.j] * infectiousness.duration[index.i]
    
    if(R0>1){
      # EPIDEMIC!
      epidemic[index.i, index.j] <- TRUE
    }else{
      # NOT AN EPIDEMIC!
      epidemic[index.i, index.j] <- FALSE
    }
    
    #### <YOUR CODE HERE> ####
    
  }
}

#   d) how do you know you have calculated this correctly?
# Answer: 

# Instead of accessing each element of a matrix via for loops, R can also apply operations to 
# matrices or dataframe as chunks. This can speed up the code, reduce the amount of code, and can make it easier to read

# here is an example of using "expand.grid" to enumerate all the combinations of the two parameters
epidemic <- expand.grid(beta.val = beta, id.val = infectiousness.duration)
# the '*' operator can then be used on the columns of the data.frame to create another column called 'R0'
epidemic$R0 <- epidemic$beta.val * epidemic$id.val
# we can also check the proportion of the combinations that give rise to epidemics
prop.epidemics <- sum(epidemic$R0 >= 1) / length(epidemic$R0)

# wow, that was much easier! 
# There are often multiple ways to calculate what you want - what you choose will be down to personal preference
# but there will be some guiding principles:
  # i) how fast is the code?
  # ii) how easy is it to write it - is it easy to debug?
  # iii) how easy is it to read - can other people understand what you've done?



########## 2 - Functions  ##########

# The expression for the final size of an SIR epidemic can be written as
# ln(S_inf) = R0*(S_inf - 1)
# That is,the natural log of the proportion susceptible at the end of the epidemic
# is equal to R0 multiplied by 1 minus the proportion susceptible at the end of the epidemic

# How do we find the solution? You'll notice that the S_inf appears on both the left and right sides of the equation
# That is, we cannot solve S_inf in terms of the other variables (i.e. R0)

# Let's turn this expression into our own R function so we can solve it

final.size.root <- function(s.inf) {
  final.size <- (R0 * (s.inf - 1) - log(s.inf))
  return(final.size)
}

# We have set the final.size.root such that whenever it evaluates 0, we have found a solution

# R has some in built functions to help you solve this equation
# Let's use 'uniroot' to find a solution. We want an answer bigger or equal to 0 and less than 1
# First have a look what uniroot does, by running
?uniroot
# Take a look at the 'Value' that uniroot provides: 'A list with at least four components'
# To display the value of the root, we need to tell R to use the output called 'root' of the function.
# We can do this by using the '$' notation below:

sol.root <- uniroot(final.size.root, c(0,0.9999))$root
print(sol.root)

#   a) Did you expect R to give you an answer? 
#   Answer:
#   b) What variables did it use to evaluate the function?
#   Answer:

# We have to make sure that we either 
# i) define all the variables that a function needs within the function itself or 
# ii) pass these variables as arguments. Let's try both ways.

# First, rewrite the function so that R0 is defined within the function:

final.size.root <- function(s.inf) {
  #### <YOUR CODE HERE> ####
}

# This looks like it could be a useful function, perhaps we don't want to have to 'hard code' R
# Let's try it a second way so that R0 is passed as an argument to the function

# Start by defining a function that takes both R0 and s.inf as arguments

final.size.root.twoargs <- function(R0, s.inf) {
  final.size <- R0 * (s.inf - 1) - log(s.inf)
  return(final.size)
}

#   c) What are the arguments of this new function?
#   Answer: 
#   d) What is the output of this new function?
#   Answer: 

# Now let's pick a number for R0 that we can easily change, let's call it rep.num
rep.num <- 2

#   e) Why have we called this rep.num and not R0?
#   Answer:

# Uniroot takes only one argument so we need to wrap our function inside another function that
# only has one argument. This is how we do it:

sol.root <- uniroot(function(s.inf){
                      return(final.size.root.twoargs(rep.num, s.inf))
                    }, 
                    c(0,0.9999))$root

# Notice that we have used the keyword 'function' without assigning the function a name (e.g. like we did with final.size)
# This type of function is called an 'anonymous function' and they are used when you can write a simple function on one line
# that you do not need to keep using. The output value of the function is the output value of the twoargs function.

#   f) What are all the functions that we have used to calculate the root of the equation 
#     and how many arguments do they have?
#   Answer: 

# Uniroot only gives us one root (unsurprisingly). rootSolve is a package that has functions to solve for multiple roots



########## 3 - Installing Packages  ##########

# EITHER INSTALL THE PACKAGE rootSolve by: Tools > InstallPackage > <type rootSolve>
# OR YOU CAN run:


install.packages("rootSolve")

# Now, load the package so your work environment has access to all its functions

library("rootSolve")

# We will now use the rootSolve function 'uniroot.all' to find all the solutions of the final size equation
# Let's use the same syntax as we did before, remembering to define rep.num again

rep.num <- 2

sol.all.roots <- uniroot.all(function(s.inf){
        return(final.size.root.twoargs(rep.num, s.inf))
      }, c(0,1))

print(sol.all.roots)
#   g) What is the epidemiological interpretation of these two roots?
#   Answer:

# Finally, let's see how our R0 changes our solution values. We can do this by employing a loop

# initialise our variables

root.matrix <- matrix(NA, nrow = 10, ncol = 2)
r0.vector <- 1:10

# Now loop through R0
for (r0 in r0.vector) {
  # find all the roots for the specific R0 value
  sol.all.roots <- uniroot.all(function(s.inf) final.size.root.twoargs(r0, s.inf), c(0,1))
  # save these root values in root.matrix
  root.matrix[r0,] <- sol.all.roots
}


# plot a new figure
par(new = FALSE)
# plot the first root as a function of R0
plot(r0.vector, root.matrix[,1], type = "b", xlim = c(1,10), ylim = c(0,1), ylab = "Root value", xlab = "R0")
# add the second root points to the plot
points(r0.vector, root.matrix[,2], type = "b")



########## 4 - Sourcing Other Files  ##########

# First let's delete all the variables on our work environment
rm(list = ls())

# check that this command has executed by looking in the Environment window - it should be empty

# In the 03_ProgrammingSkills folder, you will see a file called R0function.R. In it you will see the same script as above, but as a function
# where he argument is the R0 vector. Let's load the source file in

# First make sure the working directory is correct (i.e. in 03_ProgrammingSkills) by
getwd()
# if it's not correct then change it by
# setwd(<INSERT PATH HERE>)

# or by Session > Set Working Directory > To Source File Location

# Now load in our R file
source("R0function.R")

# Choose a range for R0 - use integer values

myR0range <- c(1.2, 1.7, 8)
# Now plot your function:

plot.final.size(myR0range)

