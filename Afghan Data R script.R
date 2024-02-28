library(tidyverse)
library(tidyr)
library(dplyr)
library(knitr)

rm(list = ls())

setwd("C:/Users/emory/Documents/Statistics Level 2")
Afghan_data =read.csv("Afghanistan Data Round 2.csv") |>
  slice(-(1:4)) |>
  rename(
    Country = Data.Source,
    Country.code = World.Development.Indicators,
    Indicator.name = X,
    Indicator.code = X.1,
    "1960" = X.2,
    "1961" = X.3,
    "1962" = X.4,
    "1963" = X.5,
    "1964" = X.6,
    "1965" = X.7, 
    "1966" = X.8,
    "1967" = X.9,
    "1968" = X.10,
    "1969" = X.11, 
    "1970" = X.12,
    "1971" = X.13,
    "1972" = X.14,
    "1973" = X.15,
    "1974" = X.16,
    "1975" = X.17,
    "1976" = X.18,
    "1977" = X.19,
    "1978" = X.20,
    "1979" = X.21,
    "1980" = X.22,
    "1981" = X.23,
    "1982" = X.24,
    "1983" = X.25,
    "1984" = X.26,
    "1985" = X.27,
    "1986" = X.28,
    "1987" = X.29,
    "1988" = X.30,
    "1989" = X.31,
    "1990" = X.32,
    "1991" = X.33,
    "1992" = X.34,
    "1993" = X.35,
    "1994" = X.36,
    "1995" = X.37,
    "1996" = X.38,
    "1997" = X.39,
    "1998" = X.40,
    "1999" = X.41,
    "2000" = X.42,
    "2001" = X.43,
    "2002" = X.44,
    "2003" = X.45,
    "2004" = X.46,
    "2005" = X.47,
    "2006" = X.48,
    "2007" = X.49,
    "2008" = X.50,
    "2009" = X.51,
    "2010" = X.52,
    "2011" = X.53,
    "2012" = X.54,
    "2013" = X.55,
    "2014" = X.56,
    "2015" = X.57,
    "2016" = X.58,
    "2017" = X.59,
    "2018" = X.60,
    "2019" = X.61,
    "2020" = X.62,
    "2021" = X.63,
    "2022" = X.64,
      ) 

# Replacing commas in variable names with periods
names(Afghan_data) <- gsub(",", ".", names(Afghan_data))

#Making data longer by combining all the years into one column. 
data_long =
  pivot_longer(Afghan_data, cols = matches("^(19|20|201|202)"), names_to = "Year",
               values_to = "Value") 

#Making data wider by creating by turning the Indicator.name values into 
# individual variable columns

wide_df =
  pivot_wider(data_long, 
                       id_cols = c(`Year`, `Country`, `Country.code`), 
                       names_from = Indicator.name, 
                       values_from = Value) |>
  

#Filtering for variables where 95% of the values are not NA, replacing NA values 
# with 0, and cleaning up the variable names

cleaned_data.= 
   wide_df[, colMeans(is.na(wide_df)) <= 0.05] %>%
   rename(
     `Urban.population` = `Urban population (% of total population)`,
     `Rural.population.growth` = `Rural population growth (annual %)`,
     `Population.total` = `Population, total`,
     `Population.largest.city` = `Population in the largest city (% of urban population)`,
     `Female.survival.65` = `Survival to age 65, female (% of cohort)`,
     `Male.survival.65` = `Survival to age 65, female (% of cohort)`,
     `Male.life.expectancy` = `Life expectancy at birth, male (years)`,
     `Female.life.expectancy` = `Life expectancy at birth, female (years)`,
     `Fertility.rate` = `Fertility rate, total (births per woman)`,
     `Net.migration` = `Net migration`,
   ) |> select(Year, Country, Population.total, Urban.population,
          Population.largest.city, Rural.population.growth, Fertility.rate,
          Net.migration, Male.life.expectancy, Female.life.expectancy) %>%
   mutate_all(~replace(., is.na(.), 0))
 
#Creating new factor variable based off Population.total variable

 cleaned_data.2$Population.category <- ifelse(cleaned_data.2$Population.total < 10000000, "Low Population",
                                            ifelse(cleaned_data.2$Population.total >= 10000000 & cleaned_data.2$Population.total < 20000000, 
                                                   "Medium Population",
                                                   ifelse(cleaned_data.2$Population.total >= 20000000, "High Population", NA)
                                            )
 )
 
 #Creating new factor variable based off Female.life.expectancy variable
 cleaned_data.2$F.life.expectancy.category <- ifelse(cleaned_data.2$Female.life.expectancy < 40, 
                                                   "Lowest life expectancy", 
                                                   ifelse(cleaned_data.2$Female.life.expectancy >= 40 & cleaned_data.2$Female.life.expectancy < 50,
                                                          "Lower life expectancy",
                                                          ifelse(cleaned_data.2$Female.life.expectancy >= 50 & cleaned_data.2$Female.life.expectancy < 60,
                                                                 "Medium life expectancy",
                                                                 ifelse(cleaned_data.2$Female.life.expectancy >= 60,
                                                                        "Highest life expectancy",
                                                                        NA
                                                                 )
                                                          )
                                                   )
 )


 #Creating new factor variable based off Male.life.expectancy variable    
 cleaned_data.2$M.life.expectancy.category <- ifelse(cleaned_data.2$Male.life.expectancy < 30, 
                                                   "Lowest life expectancy", 
                                                   ifelse(cleaned_data.2$Male.life.expectancy >= 30 & cleaned_data.2$Male.life.expectancy < 40,
                                                          "Lower life expectancy",
                                                          ifelse(cleaned_data.2$Male.life.expectancy >= 40 & cleaned_data.2$Male.life.expectancy <= 50,
                                                                 "Medium life expectancy",
                                                                 ifelse(cleaned_data.2$Male.life.expectancy >= 50 & cleaned_data.2$Male.life.expectancy <= 60,
                                                                        "Higher life expectancy",
                                                                ifelse(cleaned_data.2$Male.life.expectancy >= 60,
                                                                       "Highest life expectancy",
                                                                       NA)
                                                                 )
                                                          )
                                                   )
 )
 
 # I created a code to create a new CSV file for the cleaned data. 
 write.csv(cleaned_data.2, file = "cleaned_data.csv", row.names = TRUE)