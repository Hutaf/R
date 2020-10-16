library(tidyverse)
library(readxl)


# load observations.xlsx
url <- "https://github.com/mokainzi/R/blob/master/unicorns_on_unicycles/observations.xlsx?raw=true"
destfile <- "observations.xlsx"
curl::curl_download(url, destfile)
observations <- read_excel(destfile)
View(observations)

# load sales.xlsx
url <- "https://github.com/mokainzi/R/blob/master/unicorns_on_unicycles/sales.xlsx?raw=true"
destfile <- "sales.xlsx"
curl::curl_download(url, destfile)
sales <- read_excel(destfile)

# Print the first few rows in each dataset
head(observations)
head(sales)


#### Step 1: Cleaning and preprocessing #####

# Check if all the records are NA for columns 4 & 5 (Contains no relevent data)
sum(is.na(sales$...4))
sum(is.na(sales$...5))

# Same observations
identical(sales$name_of_country...1,sales$name_of_country...6)
identical(sales$year...2,sales$year...7)
identical(sales$year...2, observations$year)

# Same observations but the difference is upper case
identical(sales$name_of_country...1, observations$countryname)
table(observations$countryname)
table(sales$name_of_country...1)

# Drop unrelevent columns
drop <- c("name_of_country...6","year...7","...4", "...5")
sales <- sales[ , !(names(sales) %in% drop)]
str(sales)

# rename some columns
names(sales)<- c("country", "year", "bikes","total_turnover")
str(sales)
names(observations) <- c("country", "year", "pop")

# Convert country in observations to uppercase
observations$country <- toupper(observations$country)
# same observations
identical(sales$country, observations$country)

# full-join to combine 2 datasets
dataset <- observations %>%
  full_join(sales, by = c("country", "year"))
str(dataset)
View(dataset)

#### Step 2: Univariate visualization #####


# Plot unicorn population for each country
dataset %>% 
  ggplot() +
  geom_line(aes(year, pop, group = country))+
  geom_point(aes(year, pop, color = country))+
  labs(
    title = "Unicorns population for each country",
    x = "Year",
    y = "Unicorns Population")

# Plot total_turnover for each country in 1670s
dataset %>% 
  ggplot() +
  geom_line(aes(year, total_turnover, color = country))+
  labs(
    title = "Total turnover for each country",
    x = "Year",
    y = "Total Turnover")

# Plot the number of bikes sold in 1670s for each country
dataset %>% 
  ggplot() +
  geom_line(aes(year, bikes, color = country))+
  labs(
    title = "Bikes sold for each country in 1670s",
    x = "Year",
    y = "Bikes")

# Plot the years of observations for each country
dataset %>% 
  ggplot() +
  geom_line(aes(year, country))

## This shows that only Austria who has records in 1670, while other countrie's records are missing
## if the data is large enough I would drop this year to make better observation but the data is small.

#### Step 3: Bivariate visualization #####

## Q1: Is there a correlation between unicorns population and number of bikes sold in 1670s ?

# Plot a country wise disribution of unicorns
dataset %>%
  ggplot() +
  geom_boxplot(aes(country, pop,color = country))+
  labs(
    title = "country wise disribution of unicorn"
  )

# Plot a country wise disribution of bikes
dataset %>%
  ggplot() +
  geom_boxplot(aes(country, bikes,color = country))+
  labs(
    title = "country wise disribution of bikes"
  )

# Plot the relation between unicorns and unicycles
dataset %>%
  ggplot(aes(pop,bikes, color = country)) +
  geom_boxplot() +
  labs(
    title = "Country wise distbution of unicorns & bikes sold in 1670s",
    x = "Unicorns population",
    y = "Bikes"
  )  


dataset %>% 
  ggplot() +
  geom_line(aes(pop, bikes, color = country))+
  labs(
    title = "Country wise distbution of unicorns & bikes sold in 1670s",
    x = "Unicorns population",
    y = "Bikes"
  ) 


## Result: The correlation between unicorns and bikes exists 
## The increase in unicorns population couse a siginficant increase in bikes sold in 1670s


