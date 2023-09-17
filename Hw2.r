
library(fpp3)
library(dplyr)
library(ggplot2)
library(fabletools)

#3.1, 3.2, 3.3, 3.4, 3.5, 3.7, 3.8 and 3.9 

#glimpse works better
#glimpse(global_economy)

#calculate GDP per capita
gdpPerCapita <- global_economy %>%
  mutate(GDP_per_capita = GDP / Population) %>%
  select(Country, Year, GDP_per_capita)

#plot
gdpPlot <- ggplot(gdpPerCapita, aes(x = Year, y = GDP_per_capita, color = Country)) +
  geom_line() +
  labs(
    title = "GDP per Capita Over Time",
    x = "Year",
    y = "GDP per Capita"
  ) +
  theme(legend.position = "none")  #easier to read

#print(gdpPlot)

#finding the top country
latestYear <- max(gdpPerCapita$Year)

topCountry <- gdpPerCapita %>%
  filter(Year == latestYear) %>%
  arrange(desc(GDP_per_capita)) %>%
  top_n(1, GDP_per_capita)

print(topCountry)

#plotting the graph for it
selectedCountryPlot <- gdpPerCapita %>%
  filter(Country == topCountry$Country) %>%
  ggplot(aes(x = Year, y = GDP_per_capita)) +
  geom_line() +
  labs(
    title = paste("GDP per Capita Over Time for", topCountry$Country),
    x = "Year",
    y = "GDP per Capita"
  )

print(selectedCountryPlot)