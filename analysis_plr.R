# analysis_WIP.R
# Followed (analysis.R) script from Bruno Rodrigues to carry on this analysis,
# plus added some comments for my own coding session. 

library(dplyr)
library(ggplot2)
library(purrr)
library(tidyr)

#Let’s load the datasets:

commune_level_data <- read.csv("datasets/commune_level_data.csv")
country_level_data <- read.csv("datasets/country_level_data.csv")

str(commune_level_data)
str(country_level_data)

# 1. Compute Laspeyeres index

#Let’s compute the Laspeyeres index for each commune:
# Analysis by locality: group_by(locality)
commune_level_data_analysis <- commune_level_data %>%
  group_by(locality) %>%
  mutate(p0 = ifelse(year == "2010", average_price_nominal_euros, NA)) %>%
  fill(p0, .direction = "down") %>%
  mutate(p0_m2 = ifelse(year == "2010", average_price_m2_nominal_euros, NA)) %>%
  fill(p0_m2, .direction = "down") %>%
  ungroup() %>%
  mutate(pl = average_price_nominal_euros/p0*100,
         pl_m2 = average_price_m2_nominal_euros/p0_m2*100)
commune_level_data_analysis

# Check how Laspeyeres index referenced to 2010 changes for one locality "Bech"
# See "analysis_plr_detailed.R"

#Let’s also compute it for the whole country:
# This time we do not group by locality, so we get the overall value for the whole country

country_level_data_analysis <- country_level_data %>%
  mutate(p0 = ifelse(year == "2010", average_price_nominal_euros, NA)) %>%
  fill(p0, .direction = "down") %>%
  mutate(p0_m2 = ifelse(year == "2010", average_price_m2_nominal_euros, NA)) %>%
  fill(p0_m2, .direction = "down") %>%
  mutate(pl = average_price_nominal_euros/p0*100,
         pl_m2 = average_price_m2_nominal_euros/p0_m2*100)
country_level_data_analysis

nrow(country_level_data_analysis)
# [1] 11

# country_level_data_analysis has 11 rows as many rows as years in the data set

# 2. CREATING OUTPUT PLOT

# We are going to create a plot for 5 communes and compare the price evolution 
# in the communes to the national price evolution. 
# Let’s first list the communes:

communes <- c("Luxembourg","Esch-sur-Alzette","Mamer","Schengen","Wincrange")
communes

# 2.1 Luxembourgh
# communes[1] 
filtered_data <- commune_level_data_analysis %>%  filter(locality == communes[1])

nrow(filtered_data)
# [1] 11

data_to_plot <- bind_rows(country_level_data_analysis,
                          filtered_data)
nrow(data_to_plot)
# [1] 22

# Total rows in data_to_plot for Luxembourg (22)
lux_plot <- ggplot(data_to_plot) +
            geom_line(aes(y = pl_m2,x = year,
                          group = locality,
                          colour = locality))
lux_plot
  
ggsave("plots/01_05_Luxembourg_country_price_ts.png", width = 6, height = 4) 

# 2.2 Esch-sur-Alzette
# communes[2] 
filtered_data <- commune_level_data_analysis %>%  filter(locality == communes[2])

nrow(filtered_data)
# [1] 11

data_to_plot <- bind_rows(country_level_data_analysis,
                          filtered_data)
nrow(data_to_plot)
# [1] 22

# Total rows in data_to_plot for Luxembourg (22)
esch_plot <- ggplot(data_to_plot) +
  geom_line(aes(y = pl_m2,x = year,
                group = locality,
                colour = locality))
esch_plot

ggsave("plots/02_05_Esch_sur_Alzette_country_price_ts.png", width = 6, height = 4) 

# 2.3 Mamer

filtered_data <- commune_level_data_analysis %>%  filter(locality == communes[3])

data_to_plot <- bind_rows(country_level_data_analysis,
                          filtered_data)

mamer_plot <- ggplot(data_to_plot) +
  geom_line(aes(y = pl_m2,x = year,
                group = locality,
                colour = locality))
mamer_plot 

ggsave("plots/03_05_Mamer_country_price_ts.png", width = 6, height = 4) 

# 2.4 Schengen
filtered_data <- commune_level_data_analysis %>%  filter(locality == communes[4])

data_to_plot <- bind_rows(country_level_data_analysis,
                          filtered_data)

schengen_plot <- ggplot(data_to_plot) +
  geom_line(aes(y = pl_m2,x = year,
                group = locality,
                colour = locality))
schengen_plot

ggsave("plots/04_05_Schengen_country_price_ts.png", width = 6, height = 4) 

# 2.5 Wincrange
filtered_data <- commune_level_data_analysis %>%  filter(locality == communes[5])

data_to_plot <- bind_rows(country_level_data_analysis,
                          filtered_data)

wincrange_plot <- ggplot(data_to_plot) +
  geom_line(aes(y = pl_m2,x = year,
                group = locality,
                colour = locality))
wincrange_plot

ggsave("plots/05_05_Luxembourg_country_price_ts.png", width = 6, height = 4) 

# Saving plot as in the original script
# Let’s save the plots
ggsave("plots/lux_plot.pdf", lux_plot) #
ggsave("plots/esch_plot.pdf", esch_plot) #
ggsave("plots/mamer_plot.pdf", mamer_plot)
ggsave("plots/schengen_plot.pdf", schengen_plot)
ggsave("plots/wincrange_plot.pdf", wincrange_plot)

 