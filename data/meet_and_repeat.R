
library(dplyr)
library(tidyr)
library(ggplot2)

BPRS <- read.table("https://raw.githubusercontent.com/KimmoVehkalahti/MABS/master/Examples/data/BPRS.txt", header=TRUE)

RATS <- read.table("https://raw.githubusercontent.com/KimmoVehkalahti/MABS/master/Examples/data/rats.txt", header=TRUE)

# BPRS data is 40 male subjects randomly assigned to one of two treatment groups,  rated on the brief psychiatric rating scale (BPRS) measured before treatment began (week 0) and then at weekly intervals for eight weeks. The BPRS assesses the level of 18 symptom constructs such as hostility, suspiciousness, hallucinations and grandiosity; each of these is rated from one (not present) to seven (extremely severe). The scale is used to evaluate patients suspected of having schizophrenia.

## View of the data

names(BPRS)
dim(BPRS)
str(BPRS)
summary(BPRS)
glimpse(BPRS)

# RATS is data from a nutrition study conducted in three groups of rats (Crowder and Hand, 1990). The three groups were put on different diets, and each animal’s body weight (grams) was recorded repeatedly (approximately weekly, except in week seven when two recordings were taken) over a 9-week period

names(RATS)
dim(RATS)
str(RATS)
summary(RATS)
glimpse(RATS)

## Convert the categorical variables of both data sets to factors

# Factor treatment & subject
BPRS$treatment <- factor(BPRS$treatment)
BPRS$subject <- factor(BPRS$subject)

# Factor variables ID and Group
RATS$ID <- as.factor(RATS$ID)
RATS$Group <- as.factor(RATS$Group)

## Convert the data sets to long form. Add a week variable to BPRS and a Time variable to RATS. 

# Convert to long form
BPRSL <-  pivot_longer(BPRS, cols = -c(treatment, subject),
                       names_to = "weeks", values_to = "bprs") %>%
  arrange(weeks) #order by weeks variable

# Extract the week number
BPRSL <-  BPRSL %>% 
  mutate(week = as.integer(substr(BPRSL$weeks, 5, 5)))


RATSL <- pivot_longer(RATS, cols = -c(ID, Group), 
                      names_to = "WD",
                      values_to = "Weight") %>% 
  mutate(Time = as.integer(substr(WD, 3, 4))) %>%
  arrange(Time)




## View the new data sets and compare them with their wide form versions

names(BPRSL)
dim(BPRSL)
str(BPRSL)
summary(BPRSL)
glimpse(BPRSL)

ggplot(BPRSL, aes(x = week, y = bprs, linetype = subject)) +
  geom_line() +
  scale_linetype_manual(values = rep(1:10, times=4)) +
  facet_grid(. ~ treatment, labeller = label_both) +
  theme(legend.position = "none") + 
  scale_y_continuous(limits = c(min(BPRSL$bprs), max(BPRSL$bprs)))

names(RATSL)
dim(RATSL)
str(RATSL)
summary(RATSL)
glimpse(RATSL)

# Plot the RATSL data
ggplot(RATSL, aes(x = Time, y = Weight, group = ID)) +
  geom_line(aes(linetype = Group)) + scale_x_continuous(name = "Time (days)", breaks = seq(0, 60, 10)) + scale_y_continuous(name = "Weight (grams)") + theme(legend.position = "top")


## Save data

setwd("\\\\ad.helsinki.fi\\home\\l\\lehakkin\\Desktop\\KURSSIT\\Open_Data_Science\\IODS-project\\data")

write.csv2(RATSL, file="RATSL.csv", row.names = FALSE)
write.csv2(BPRSL, file="BPRSL.csv", row.names = FALSE)
