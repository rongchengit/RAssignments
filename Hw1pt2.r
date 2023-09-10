#2.3

#install.packages("readr")
library(fpp3)
library(tsibble)
library(readr)
library(ggplot2)
library(dplyr)
library(tidyr) # For pivot_longer

#2.3
tute1 <- readr::read_csv("/Users/rongchen/Desktop/tute1.csv", show_col_types = FALSE)
# View(tute1)

mytimeseries <- tute1 |>
  mutate(Quarter = yearquarter(Quarter)) |>
  as_tsibble(index = Quarter)

timePlot <- mytimeseries |>
                pivot_longer(-Quarter) |>
                ggplot(aes(x = Quarter, y = value, colour = name)) +
                geom_line() +
                facet_grid(name ~ ., scales = "free_y")

# the facetgrid split the columns into 3 individual columns
print(timePlot)
