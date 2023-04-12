######################################################
#         Introduction to R: Section 2               #
######################################################

# A. read in a data file

myTBdata <- read.table("precourse/introR/TB_stats.txt", header=TRUE)

# What does the "header=TRUE" option mean?
# Answer:

# Now let's investigate the data file

# B. Have a look at the first few lines

head(myTBdata)

# How many rows can you see? What is the first row?
# Answer:

# C. What are the names of the columns?

names(myTBdata)

# Is this what you expected?
# Answer:

# D. How many rows and columns are there in your data ?

dim(myTBdata)

# What is the first number telling you? And the second?
# Answer:

# E. How are your data stored?

attributes(myTBdata)

# What new piece of information have you learned from the 'attributes()' function?
# Answer:

# F. Now take a look at some summary statistics for your data

summary(myTBdata)

# Let's extract some information from our data


# G. First, Calculate the total number of deaths across all countries

# The following two methods should give you the same answer
total_TB_mortality1 <- sum(myTBdata[,2:3]) # method 1
total_TB_mortality2 <- sum(myTBdata$HIV_pos_TB_mortality + myTBdata$HIV_neg_TB_mortality) # method 2

# Do you think one method is better than the other?
# Answer:

# H. Now let's check that both methods give the same answer. We'll use two ways to check this
# First, let's output both answers

total_TB_mortality1
total_TB_mortality2

# Now, let's ask R to check whether they are both equal

total_TB_mortality1==total_TB_mortality2 # logical expression which gives TRUE if equal and FALSE if not

# Why might you prefer to use the second check (using the logical expression) than the first?
# Answer:

# I. How different is the TB mortality rate in HIV positive persons in Lesotho compared to Zimbabwe?

# First, let's add "mortality rate" as another column in our data frame

myTBdata$Mortality_Per1000 <- 1000 * (myTBdata$HIV_pos_TB_mortality + myTBdata$HIV_neg_TB_mortality)/myTBdata$Population


# Now subset the dataset to extract the TB mortality rate for both Lesotho and Zimbabwe

Lesotho_mortalityrate <- myTBdata[myTBdata$Country=="Lesotho", "Mortality_Per1000"]
Zimbabwe_mortalityrate <- myTBdata[myTBdata$Country=="Zimbabwe", "Mortality_Per1000"]
Relative_Mortality_Rate <- Lesotho_mortalityrate / Zimbabwe_mortalityrate

# How many times higher is the mortality rate for TB in Lesotho as it is in Zimbabwe?

paste("The relative mortality rate is", round(Relative_Mortality_Rate, 2), sep=" ")

# J. Finally in this section, let's look at what can go wrong when reading
# in data files.


# (a) There is not an equal number of columns in each of the rows
readFile_a <- read.table("precourse/introR/readfileexample_1.txt", header=TRUE)

# How do you fix this error?
# Hint: set missing values in the data file to be 'Not Assigned' by adding them as NA in the original file
# Try running this line again with the updated file

# (b) The wrong delimiter is used

readFile_b <- read.table("precourse/introR/readfileexample_2.txt", header=TRUE)

# Is an error given? Check out 'readFile_b' - is it correct?
# Answer:

# How do you fix this? Ask R for help (?read.table)
# Which option do you need to specify?
# Answer:

# Is there another way of fixing this problem?
# Answer:


# (c) The names are read in as data rows rather than names
readFile_c <- read.csv("precourse/introR/readfileexample_2.txt", header=FALSE)

# Is an error given? Check out 'readFile_c' - is it correct?
# Answer:

# Type a new line of code to correct this problem (hint: copy-paste from above and change one of the options)
##### YOUR CODE GOES HERE #####

# (d) One of more of the columns contain different classes

readFile_d <- read.table("precourse/introR/readfileexample_3.txt", header=TRUE)

# Is an error given? Check out 'readFile_d' - is it correct?
# Answer:

# How do you fix this issue?
# Hint: check the 'class' of the problem column
# Try running this line again with an updated file





