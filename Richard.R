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


jobsdf <- jobsdf %>% mutate(BusinessTravel=ifelse(BusinessTravel=="Travel_Rarely", 2, 
                                                  ifelse(BusinessTravel=="Travel_Frequently", 3, 1)))

jobsdf <- jobsdf %>% mutate(Department=ifelse(Department=="Research & Development", 2, 
                                              ifelse(Department=="Sales", 3, 1)))

jobsdf <- jobsdf %>% mutate(EducationField=ifelse(EducationField=="Life Sciences", 2, 
                                              ifelse(EducationField=="Marketing", 3, 
                                                     ifelse(EducationField=="Medical", 4,
                                                            ifelse(EducationField=="Other", 5,
                                                                   ifelse(EducationField=="Technical Degree", 6, 1))))))

jobsdf <- jobsdf %>% mutate(Gender=ifelse(Gender=="Male", 1, 0))

jobsdf <- jobsdf %>% mutate(JobRole=ifelse(JobRole=="Healthcare Representative", 2,
                                    ifelse(JobRole=="Laboratory Technician", 3,
                                           ifelse(JobRole=="Manager", 4,
                                                  ifelse(JobRole=="Manufacturing Director", 5,
                                                         ifelse(JobRole=="Research Director", 6, 
                                                                ifelse(JobRole=="Research Scientist", 7,
                                                                       ifelse(JobRole=="Sales Executive", 8,
                                                                              ifelse(JobRole=="Sales Representative", 9, 0)))))))))

jobsdf <- jobsdf %>% mutate(MaritalStatus=ifelse(MaritalStatus=="Married", 2,
                                                 ifelse(MaritalStatus=="Divorced", 3, 1)))


jobsdf <- jobsdf %>% mutate(Over18=ifelse(Over18=="Y", 1, 0))

jobsdf <- jobsdf %>% mutate(OverTime=ifelse(OverTime=="Yes", 1, 0))

# Remove EmployeeCount, Over18, StandardHour due to the standard deviation is zero
jobsdf <- jobsdf[,-c(9,22,27)]
JobsCorr <- cor(jobsdf)

