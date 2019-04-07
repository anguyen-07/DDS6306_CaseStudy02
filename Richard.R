require("corrplot")
library(xlsx)
library(tidyverse)

# Load the CaseStudy2 Data set as a dataframe
jobsdf <- read.xlsx("CaseStudy2-data.xlsx", sheetIndex = 1, header = T) 

# Get information about the data frame jobsdf
class(jobsdf)
str(jobsdf)
head(jobsdf)
table(is.na(jobsdf))
summary(jobsdf)

# Change Attrition to values from Yes/No to 1 and 0
jobsdf <- jobsdf %>% mutate(Attrition=ifelse(Attrition=="Yes", 1, 0))

#Remove non-numberical columns for correlation.
jobcor <- jobsdf[-c(3,5,8:9,12,16,18,22,23,27)]

# Build Correlation Plot
JobsCorr <- cor(jobcor)
corrplot(JobsCorr, method = 'number', number.cex = 0.65, type = 'lower')


jobsdf <- jobsdf %>% mutate(BusinessTravel=ifelse(BusinessTravel=="Travel_Rarely", 2, ifelse(BusinessTravel=="Travel_Frequently", 3, 1)))
