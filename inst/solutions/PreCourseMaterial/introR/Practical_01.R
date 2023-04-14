######################################################
#         Introduction to R: Section 1               #
######################################################

# A. Create simple objects of different types in the workspace
item1 <- 1
item2 <- "a"
item3 <- 3.78901

# B. Objects with more than 1 entry are called vectors
# create some vectors using c() (short for concatenate)
# all items of a vector must be the same class
object1 <- c("a", "b", "c")
object2 <- 1:3
object3 <- c(1.3, -4.5, 6.99)

# you can create vectors using other named items or objects
object4 <- c(item1, item2, item3)

# take a look
object1
object2
object3
object4

# C. Look for your objects in the environment tab (upper right)

# D. What class of objects is each object?
# use class() to find this
class(object1)
class(object2)
class(object3)
class(object4)

# Answer:
# 1. character
# 2. integer
# 3. numeric
# 4. charachter

# Is object 4 the class you expected?
# Answer: No, it has been converted (cast) to a character
# this is because it is a combination of different types

# E. You can manipulate objects and make operations on them
object2 + 1
object3 * object3
object2 / object3
object1 + object2

# what happened at each of these commands? is it what you expected?
# Answer: there was an error for the last one: characters cannot be added

# F. You can view the history of commands that were run.
# check the history of commands in the history tab (upper right)

# G. Make some other objects
df1 <- data.frame(ID = 1:5,
                  animal = c("bear", "cat", "horse", "cat", "pig"),
                  weight = c(200, 5, 600, 8, 100))
mat1 <- matrix(data = 1:50, nrow = 10, ncol = 5)

# view these objects
df1
mat1
View(df1)
View(mat1)

# in R, what is the difference between a data.frame and a matrix?
# hint: google it.
# Answer: Matrices can be only of 1 class, e.g. numeric, integer, etc
# data frames can be a mixture of classes by column, e.g.
# col1 = numeric, col2 = character

# H. Calculate the mean weight of the animals in df1 (mean of column 3 of df1)
# you can either refer to a column by name or by index
# check what the names are
colnames(df1)
# reference a column by name
df1$weight
# check how to use the function "mean"
# ? allows you to view the help file of any inbuilt function
?mean
# calculate the mean
mean(df1$weight)
mean(df1[, 3])

# Answer: 182.6

# I. does mat1 have column names?
# Answer: no

# set column names for mat1
colnames(mat1) <- c("A", "B", "C", "D", "E")
mat1

# J. What is the value of the element row 5, column C in mat1?
# check by referencing the row, then column, either by name or number
mat1[5, "C"]
mat1[5, 3]

# Answer: 25

# what is the value of row 8, column E?
mat1[8, "E"]
# Answer: 48

# K. Discard all of mat1 except the 1st and 4th column.
# subset mat1 to keep only the 1st and 4th column by number or name
mat1[, c(1, 4)]
mat1[, c("A", "D")]

# Create a new object of the smaller matrix.
# assign the subsetted version as a new object, mat2
mat2 <- mat1[, c("A", "D")]
mat2 <- mat1[, c(1, 4)]
