---
title: "Overview"
author: "AMarkum"
date: "4/5/2019"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r att.df}
# Required packages
library(ggplot2)
library(tidyverse)
library(psych)
library(lattice)
library(corrplot)
library(vcd)
att.df <- read.csv(paste("CaseStudy2-singlesheet.csv", sep = ""))
attach(att.df)
```
# Talent Management: Predicting Employee Turnover
### Amy, Andy, Richard & Tom

## Introduction

### All organizations experience attrtion through the normal course of business. Natural attrition occurs in many ways, such as retirement, resignation, and termination of individual employees, or through downsizing or restructuring of the company. However, organizations should keep an eye on attrition to ensure they retain experienced talent and don't waste resources replacing good employees. Additionally, attrition can act as a barometer for the organization's overall health and corporate culture.

### So how does the organization know whether its turnover rate is "good" or "bad"? What should the organization's target attrition rate be? To answer that question, the organization needs know two things: 1) What is the organization's current turnover rate, and 2) What is the average turnover rate for the industry.

### According to the U.S. Bureau of Statistics (2017), the average turnover rate in the U.S. ranges from 12% - 15% percent. Looking deeper, according to a survey conducted by LinkedIn, turnover rates for the technology industry hovers around 13%, whith noticeably higher levels of attrition in the fields of Data analysis and software engineering, which is closer to 22%.

```{r att.df, echo=FALSE}
# First, determine what the organization's overall attrition rate is
attrition_table<- table(att.df$Attrition)
round(prop.table(attrition_table)*100)
```
