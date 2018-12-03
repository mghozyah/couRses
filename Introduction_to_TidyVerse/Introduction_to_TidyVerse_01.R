#Introduction to the Tidyverse

library(gapminder)
library(tidyr)
library(dplyr) #pipeline
library(ggplot2)

#remember %>% <- ctrl + shift + m

#############################
#Chapter 1              
#DATA WRANGLING
#############################

####################FILTER###################
#filter 1 var
gapminder %>%
  filter(year == 2007)
gapminder %>%
  filter(country == "United States")

#filter 2 var, separate by comma
gapminder %>%
  filter(year == 2007, country == "United States")

####################ARRANGE###################
#ascending
gapminder %>%
  arrange(gdpPercap)
#descending
gapminder %>%
  arrange(desc(gdpPercap))
          
#Filter then arrange
gapminder %>%
  filter(year == 2007) %>%
  arrange(desc(gdpPercap))
 
# # Gather the month columns
# census2 <- gather(census, month, amount, -YEAR)
# 
# # Arrange rows by YEAR using dplyr's arrange
# census2 <- arrange(census2, YEAR)

####################MUTATE###################
gapminder %>%
  mutate(pop = pop / 1000000) %>% #per one million people
    arrange(desc(gdpPercap))

# Use mutate to change lifeExp to be in months
gapminder %>%
  mutate(lifeExp = lifeExp * 12)

# Use mutate to create a new column called lifeExpMonths
gapminder %>%
  mutate(lifeExpMonths = lifeExp * 12)

#List of highest gdp, gdp = gdpPercap*pop
gapminder %>%
  mutate(gdp = gdpPercap * pop) %>%
  filter(year == 2007) %>%
  arrange(desc(gdp))
#######################################

#############################
#Chapter 2
#DATA VISUALIZATION
#############################

####################VAR ASSIGNMENT###################
#assigning data in 2007 year
gapminder_2007 <- gapminder %>%
  filter(year == 2007)
gapminder_2007

#visualizing assigned var
gapminder_2007 %>%
  ggplot(aes(x = gdpPercap, y = lifeExp)) + geom_point()

####################LOG SCALES###################
ggplot(gapminder_2007, aes(x = gdpPercap, y = lifeExp)) +
  geom_point() +
  scale_x_log10()
ggplot(gapminder_2007, aes(x = gdpPercap, y = lifeExp)) +
  geom_point() +
  scale_y_log10()
ggplot(gapminder_2007, aes(x = gdpPercap, y = lifeExp)) +
  geom_point() +
  scale_x_log10() + scale_y_log10()

###################AESTHETIC#######################
#color aesthetic
ggplot(gapminder_2007, aes(x = gdpPercap, y = lifeExp, 
                           color = continent)) +
                            geom_point() +
                            scale_x_log10()
#size aesthetic
ggplot(gapminder_2007, aes(x = gdpPercap, y = lifeExp, 
                           color = continent,
                           size = pop)) +
                            geom_point() +
                            scale_x_log10()
# Add the size aesthetic to represent a country's gdpPercap
ggplot(gapminder, aes(x = pop, y = lifeExp, color = continent, size = gdpPercap)) +
  geom_point() +
  scale_x_log10()

###################FACETING#######################
#faceting <- sub plot
ggplot(gapminder_2007, aes(x = gdpPercap, y = lifeExp,
                           color = continent)) +
  geom_point() +
  scale_x_log10() +
  facet_wrap(~ continent)
# Scatter plot comparing gdpPercap and lifeExp, with color representing continent
# and size representing population, faceted by year
ggplot(gapminder, aes(gdpPercap, lifeExp, 
                      color = continent,
                      size = pop)) +
                  geom_point() + 
                  scale_x_log10() + 
                  facet_wrap(~year)

#############################
#Chapter 3
#GROUPING AND SUMMARIZING
#############################

################SUMMARIZE VERB####################
###function you can use to smmarize
  # Mean
  # Sum
  # Median
  # Min and Max
#Extracting Data
gapminder %>%
  filter(country == "United States", year == 2007)
#summarize() <- turns many rows into one
gapminder %>%
  summarize(meanLifeExp = mean(lifeExp))
#summarizing one year
gapminder %>%
  filter(year == 2007) %>%
  summarize(meanLifeExp = mean(lifeExp))
#Summarizing into multiple columns
gapminder %>%
  filter(year == 2007) %>%
  summarize(meanLifeExp = mean(lifeExp),
            totalPop = sum(as.numeric(pop)))

################GROUP_BY VERB#####################
#group_by() <- before summarize turn groups into one row each
#Summarizing by year
by_year <- gapminder %>%
  group_by(year) %>% 
  summarize(meanLifeExp = mean(lifeExp),
            totalPop = sum(as.numeric(pop)))
#summarizing by continent
by_continent <- gapminder %>% 
  group_by(continent) %>% 
  summarize(meanLifeExp = mean(lifeExp),
            totalPop = sum(as.numeric(pop)))
#summarizing combination of year and continent
by_year_continent <- gapminder %>%
  group_by(year, continent) %>%
  summarize(totalPop = sum(as.numeric(pop)),
            meanLifeExp = mean(lifeExp))
# Find median life expectancy and maximum GDP per capita in each continent in 1957
gapminder %>%
  filter(year == 1957) %>%
  group_by(continent) %>%
  summarize(medianLifeExp = median(lifeExp), 
            maxGdpPercap = max(gdpPercap))

##########VISUALIZING SUMMARIZED DATA#############
#visualizing pop over time (year)
ggplot(by_year, aes(x = year, y = totalPop)) +
  geom_point()
#start y-axis at zero
ggplot(by_year, aes(x = year, y = totalPop)) +
  geom_point() +
  expand_limits(y = 0)
#Visualizing population by year and continent
ggplot(by_year_continent, aes(x = year, y = totalPop, color = continent)) +
  geom_point() +
  expand_limits(y = 0)

# Summarize the median GDP and median life expectancy per continent in 2007
by_continent_2007 <- gapminder %>%
  filter(year==2007) %>%
  group_by(continent) %>%
  summarize(medianLifeExp = median(lifeExp), 
            medianGdpPercap = median(gdpPercap))

# Use a scatter plot to compare the median GDP and median life expectancy
ggplot(by_continent_2007, aes(x=medianGdpPercap, 
                              y = medianLifeExp, 
                              color = continent)) + 
  geom_point() + 
  expand_limits(y = 0)

#############################
#Chapter 4
#TYPE OF VISUALIZATIONS
#############################

################LINE####################
# Summarize the median gdpPercap by year & continent, save as by_year_continent
by_year_continent <- gapminder %>%
  group_by(year, continent) %>%
  summarize(medianGdpPercap = median(gdpPercap))
# Create a line plot showing the change in medianGdpPercap by continent over time
ggplot(by_year_continent, aes(x=year, 
                              y = medianGdpPercap, 
                              color = continent)) + 
  geom_line() + 
  expand_limits(y = 0)

################BAR####################
# Summarize the median gdpPercap by year and continent in 1952
by_continent <- gapminder %>%
  filter(year == 1952) %>%
  group_by( continent) %>%
  summarize(medianGdpPercap = median(gdpPercap))
# Create a bar plot showing medianGdp by continent
ggplot(by_continent, aes(continent, medianGdpPercap, color = continent)) +
  geom_col()
# Filter for observations in the Oceania continent in 1952
oceania_1952 <- gapminder %>%
  filter(continent == "Oceania", year == 1952)
# Create a bar plot of gdpPercap by country
ggplot(oceania_1952, aes(country, gdpPercap)) + 
  geom_col()

################HISTOGRAM####################
#basic
ggplot(gapminder_2007, aes(x = lifeExp)) +
  geom_histogram(binwidth = 5) 
# Create a histogram of population (pop)
gapminder_1952 <- gapminder %>%
  filter(year == 1952)
ggplot(gapminder_1952, aes(x=pop)) + 
  geom_histogram() +
  scale_x_log10()

################BOXPLOT####################
#giving title with ggtitle
ggplot(gapminder_2007, aes(x = continent, 
                           y = gdpPercap)) +
  geom_boxplot()+
  scale_y_log10()+
  ggtitle("Comparing GDP per capita across continents")


