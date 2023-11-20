
# Laura Häkkinen
# 20.11.2023

# Student performance and alcohol consumption data

# data downloaded from: UCI Machine Learning Repository, Student Performance Data (incl. Alcohol consumption): https://www.archive.ics.uci.edu/dataset/320/student+performance

setwd("\\\\ad.helsinki.fi\\home\\l\\lehakkin\\Desktop\\KURSSIT\\Open_Data_Science\\IODS-project\\data")

math <- read.csv2(file = "student-mat.csv")

por <- read.csv2(file = "student-por.csv")

str(math)
dim(math)

str(por)
dim(por)

# Join the two data sets using all other variables than "failures", "paid", "absences", "G1", "G2", "G3" as (student) identifiers. Keep only the students present in both data sets. Explore the structure and dimensions of the joined data. (1 point)

free_cols <- c("failures", "paid", "absences", "G1", "G2", "G3")


# the rest of the columns are common identifiers used for joining the data sets
# setdiff(x, y) finds all rows in x that aren't in y

join_cols <- setdiff(colnames(por), free_cols)

# join the two data sets by the selected identifiers, and add the argument `suffix` to `inner_join()` and give it a vector of two strings: ".math" and ".por"
math_por <- inner_join(math, por, by = join_cols, suffix = c(".math", ".por"))

# look at the column names of the joined data set
names(math_por)

# glimpse at the joined data set
glimpse(math_por)

# Create alc data, where you get rid of the duplicate records in the joined data set by using the if-else structure" to combine the 'duplicated' answers in the joined data

# create a new data frame with only the joined columns
alc <- select(math_por, all_of(join_cols))

# for every column name not used for joining...
for(col_name in free_cols) {
  # select two columns from 'math_por' with the same original name
  two_cols <- select(math_por, starts_with(col_name))
  # select the first column vector of those two columns
  first_col <- select(two_cols, 1)[[1]]
  
  # then, enter the if-else structure!
  # if that first column vector is numeric...
  if(is.numeric(first_col)) {
    # take a rounded average of each row of the two columns and
    # add the resulting vector to the alc data frame
    alc[col_name] <- round(rowMeans(two_cols))
  } else { # else (if the first column vector was not numeric)...
    # add the first column vector to the alc data frame
    alc[col_name] <- first_col
  }
}

# glimpse at the new combined data
glimpse(alc)

# Take the average of the answers related to weekday and weekend alcohol consumption to create a new column 'alc_use' to the joined data.
alc <- mutate(alc, alc_use = (Dalc + Walc) / 2)

# Then use 'alc_use' to create a new logical column 'high_use' which is TRUE for students for which 'alc_use' is greater than 2 (and FALSE otherwise).
alc <- mutate(alc, high_use = alc_use > 2)

# Glimpse at the joined and modified data to make sure everything is in order. 
glimpse(alc)

# The joined data now has 370 observations.

# Save the joined and modified data set to the ‘data’ folder
write.csv2(alc, file ="alc.csv", row.names = FALSE)


