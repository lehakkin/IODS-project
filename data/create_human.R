
# Laura Häkkinen

# Data wrangling for Human data


## Data

#The 'human' dataset originates from the United Nations Development Programme. See their data page for more information.
#<https://hdr.undp.org/data-center/human-development-index#/indicies/HDI>

#For a nice overview see also the calculating the human development indices pdf:
#  <https://hdr.undp.org/system/files/documents/technical-notes-calculating-human-development-indices.pdf>
  
#Most of the variable names in the data have been shortened and two new variables have been computed. See the meta file for the modified data here for descriptions of the variables:
#  <https://github.com/KimmoVehkalahti/Helsinki-Open-Data-Science/blob/master/datasets/human_meta.txt>


# Get the data
library(readr)
human <- read_csv("https://raw.githubusercontent.com/KimmoVehkalahti/Helsinki-Open-Data-Science/master/datasets/human1.csv")

# set working dir
setwd("\\\\ad.helsinki.fi\\home\\l\\lehakkin\\Desktop\\KURSSIT\\Open_Data_Science\\IODS-project\\data")

#the structure and the dimensions of the 'human' data
str(human)
dim(human)


# Data describtion

## The data combines several indicators from most countries in the world

# "Country" = Country name

# **Health and knowledge**
  
## "GNI" = Gross National Income per capita
## "Life.Exp" = Life expectancy at birth
## "Edu.Exp" = Expected years of schooling 
## "Mat.Mor" = Maternal mortality ratio
## "Ado.Birth" = Adolescent birth rate

# **Empowerment**
  
## "Parli.F" = Percetange of female representatives in parliament
## "Edu2.F" = Proportion of females with at least secondary education
## "Edu2.M" = Proportion of males with at least secondary education
## "Labo.F" = Proportion of females in the labour force
## "Labo.M" " Proportion of males in the labour force

## "Edu2.FM" = Edu2.F / Edu2.M
## "Labo.FM" = Labo2.F / Labo2.M



# Exclude unneeded variables:

library(dplyr)
keep <- c("Country", "Edu2.FM", "Labo.FM", "Edu.Exp", "Life.Exp", "GNI", "Mat.Mor", "Ado.Birth", "Parli.F")

# select the 'keep' columns
human <- select(human, one_of(keep))


# Remove all rows with missing values with complete.cases() function

# filter out all rows with NA values
human_ <- filter(human, complete.cases(human))


# Remove the observations which relate to regions instead of countries:

# look at the last 10 observations of human
tail(human_, n=10L)

# Last 7 rows are regions. Define the last indice we want to keep
last <- nrow(human_) - 7

# choose everything until the last 7 observations
human_ <- human_[1:last, ]

# The data now has 155 observations and 9 variables (including the "Country" variable). Save the human data in your data folder:

write.csv2(human_, file = "human.csv", row.names = FALSE)
