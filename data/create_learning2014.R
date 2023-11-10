# Laura Häkkinen
# 07.11.2023
# Data wrangling excercise for chapter 2

# data from: http://www.helsinki.fi/~kvehkala/JYTmooc/JYTOPKYS3-meta.txt

# read data 
lrn14 <- read.table("http://www.helsinki.fi/~kvehkala/JYTmooc/JYTOPKYS3-data.txt", sep="\t", header=TRUE)

# check columns
names(lrn14)

# Create an analysis dataset with the variables gender, age, attitude, deep, stra, surf and points by combining questions
# first pick "Age"      "Attitude" "points"   "gender"  to a separate table "dataset"
dataset <- lrn14[, 57:60]
# change the names to lowercase, first check the order of columns
names(dataset)
colnames(dataset)[1] <- "age"
colnames(dataset)[2] <- "attitude"
colnames(dataset)[3] <- "points"

# define deep, surf, and stra and add means of those to the new dataset table
deep_questions <- c("D03", "D11", "D19", "D27", "D07", "D14", "D22", "D30","D06",  "D15", "D23", "D31")
dataset$deep <- rowMeans(lrn14[, deep_questions])
surface_questions <- c("SU02","SU10","SU18","SU26", "SU05","SU13","SU21","SU29","SU08","SU16","SU24","SU32")
dataset$surf <- rowMeans(lrn14[, surface_questions])
strategic_questions <- c("ST01","ST09","ST17","ST25","ST04","ST12","ST20","ST28")
dataset$stra <- rowMeans(lrn14[, strategic_questions])

# Scale all attitude to the original scales (by dividing with 10).
dataset$attitude <- dataset$attitude / 10

# Exclude observations where the exam points variable is zero.
dataset <- filter(dataset, points > 0)
# --> The data now has 166 observations and 7 variables)

# Set the working directory of your R session to the IODS Project folder
setwd("\\\\ad.helsinki.fi\\home\\l\\lehakkin\\Desktop\\KURSSIT\\Open_Data_Science\\IODS-project")
# check the working directory
getwd()

# Save the analysis dataset to the ‘data’ folder
write.csv2(dataset, "data\\learning2014.csv", row.names = FALSE)

# Demonstrate that I can read the data again
test <- read.csv2("data\\learning2014.csv")
# check that the structure of the data is correct.
str(test)
head(test)

