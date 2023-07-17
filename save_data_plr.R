# save_data_WIP.R  
# Followed (save_data.R) script from Bruno Rodrigues to get the data for this analysis,

## 1. Download Luxembougrh housing data
# Load required libraries 
library(dplyr)
library(purrr)
library(readxl)
library(stringr)
library(janitor)
library(here)
library(tidyverse)

install.packages("rvest",dependencies = TRUE)
install.packages("tidyverse",dependencies = TRUE)


# Packages used in this section
#
# {dplyr} Basic data manipulation (10 most common data manipulations in R)
# {purrr} Package for functional programming
# {readxl} Read in Excel workbooks
# {stringr} Package for manipulating strings
# {janitor} Package providing set of functions to perform some common tasks like renaming every column of a data frame in snake case

# 1. Download data from book GitHub repo
url <- "https://is.gd/1vvBAc"
raw_data <- tempfile(fileext = ".xlsx")
download.file(url, raw_data,method = "auto",mode = "wb")
sheets <- excel_sheets(raw_data)

# He creates a function to read in excel file
read_clean <- function(...,sheet){
  read_excel(...,sheet = sheet) %>% 
    mutate(year = sheet)
}

# Now uses that function to read in the Excel file into R
# IMPORTANT: When using map function always include tilde "~"

# 2. Import data into R

# 2.1 The first half of the data ingestion returns a LIST
is.list(sheets)

raw_data_set <- map(sheets,
                    ~read_clean(raw_data,skip = 10, sheet = .)) %>%  # Map function returns a LIST
  bind_rows() %>% 
  

# 2.2 Then we clean initial column names using janitor package
# The map function applies function read_clean() to each element of the sheets list
is.list(sheets)

raw_data_set <- map(sheets,
                ~read_clean(raw_data,skip = 10, sheet = .)) %>%  # Map function returns a LIST
            bind_rows() %>% 
            clean_names()

# Rename several variables 
names(raw_data_set)
[1] "commune"                             "nombre_doffres"                     
[3] "prix_moyen_annonce_en_courant"       "prix_moyen_annonce_au_m2_en_courant"
[5] "year"                                "bech"                               
[7] "x12"                                 "x3"                                 
[9] "x4"  

# 2.3 Rename several variables from imported data set

data_set <- raw_data_set %>% 
            rename(
              locality = commune,
              n_offers = nombre_doffres,
              average_price_nominal_euros = prix_moyen_annonce_en_courant,
              average_price_m2_nominal_euros = prix_moyen_annonce_au_m2_en_courant
              )

# 2.4 Crate new variables
# Using starts_with() argument in the select() function allows to subset just average variables
# str_trim()
# str_trim() removes whitespace from start and end of string; 
#    str_squish() removes whitespace at the start and end, and replaces all internal whitespace with a single space.

data_set_new <- data_set %>% 
                mutate(locality_new = str_trim(locality)) %>% 
                select(year, locality_new, n_offers,starts_with("average"))
data_set_new

# 3. Fix some data transformations
# (page 49)
# But there is a problem: columns that should be of type numeric are character instead !!)

data_set_new <- data_set_new %>% 
                select(year,
                       locality = locality_new,
                       n_offers,
                       average_price_nominal_euros,
                       average_price_m2_nominal_euros)
data_set_new


# Using grepl() we quickly build freq tables searching strings
# grepl()
# Using grepl() inside filter() verb
# pattern: character string containing a regular expression (or character string for fixed = TRUE) to be matched in the given character vector. Coerced by as.character to a character string if possible. If a character vector of length 2 or more is supplied, the first element is used with a warning. Missing values are allowed except for regexpr, gregexpr and regexec.
# x      : text,a character vector where matches are sought, or an object which can be coerced by as.character to a character vector. Long vectors are supported.
# ignore.case: if FALSE, the pattern matching is case sensitive and if TRUE, case is ignored during matching.
filter_lux <- data_set_new %>% 
            filter(grepl("Luxembourg",locality)) %>% 
            count(locality)
filter_lux

> filter_lux
# A tibble: 2 × 2
locality             n
<chr>            <int>
  1 Luxembourg           9
2 Luxembourg-Ville     2

# the city of Louxembough is spelled in two different ways
# The same hapens with Petange
filter_ptange <- data_set_new %>% 
  filter(grepl("P.tange",locality)) %>% 
  count(locality)
filter_ptange

> raw_data
# A tibble: 2 × 2
locality     n
<chr>    <int>
  1 Petange      9
2 Pétange      2


# 3.1 write some code to fix this misspellings

# Recode these two localities
# "Luxembourg-Ville" > "Luxembourg"
#
raw_data_recode <- data_set_new %>% 
  mutate(
    locality = ifelse(
      grepl("Luxembourg-Ville",locality),"Luxembourg",locality),
    locality = ifelse(
      grepl("P.tange",locality),"Pétange",locality)
    
  )

# 3.1 Look for ALL Variables that start with "averge" turn them into numeric
raw_data_recode

# year  locality    n_offers average_price_nominal_euros average_price_m2_nominal_euros
# <chr> <chr>          <dbl> <chr>                       <chr>                         
#  1 2010  Bascharage       192 593698.31000000006          3603.57 

raw_data_num <- raw_data_recode %>% 
                mutate(across(starts_with("average"),
                              as.numeric))
raw_data_num

nrow(raw_data_num)
# [1] 1343

# Now we have numeric results
#> raw_data_num
# A tibble: 1,343 × 5
#year  locality    n_offers average_price_nominal_euros average_price_m2_nominal_euros
#<chr> <chr>          <dbl>                       <dbl>                          <dbl>
#  1 2010  Bascharage       192                     593698.                          3604.

Check_nulls <- raw_data_num  %>% filter(is.na(average_price_nominal_euros))
Check_nulls

# 4. THERE ARE SOME ROWS WE NEED TO REMOVE ( continue in underlined paragraph on page 52)

# 4.1 Remove rows stating the resources
# These are the rows where locality equals to "Source"
# Identify rows to be removed

Source_data <- raw_data_num %>% 
               filter(grepl("Source",locality))
Source_data

# There are 11 Rows containing just the following footnote:
# "Source : Ministère du Logement - Observatoire de l'Habitat (base pri… "
# One row for each year from 2010 to 2020

raw_data_num_full <- raw_data_num %>% filter(!grepl("Source",locality))
raw_data_num_full


# 4.2 Keep only the communes in our data
# Check records with nationale locality value
check_nationale <-  raw_data_num_full %>% 
                      select(locality) %>% 
                      group_by(locality)  %>%
                      summarize( freq = n()) 
check_nationale

# Check how fitler with negative grepl works
commune_level_data_check <- raw_data_num_full %>% 
                             filter(!grepl("nationale|offers",locality))
commune_level_data_check

commune_level_data <- raw_data_num_full %>% 
                      filter(!grepl("nationale|offers",locality),
                             !is.na(locality))
commune_level_data

  
# 4.3 National data set
# Using full_join() function
country_level <- raw_data_num_full %>% 
                  filter(grepl("nationale",locality))
country_level

# Remove "n_offers" variable from previous data set
country_level <- raw_data_num_full %>% 
                  filter(grepl("nationale",locality)) %>% 
                  select(-n_offers)
country_level
# (n = 11)

# And gather offers that just apply to cuntry level
offers_country <- raw_data_num_full %>% 
                  filter(grepl("Total d.offres", locality)) %>% 
                  select(year, n_offers)
offers_country
# (n = 11)

# Finally create country level data by joining both data sets
# dataset A: country_level ()
# dataset B: offers_coutry ()
country_level_data <- full_join(country_level,offers_country) %>% 
                      select(year,locality,n_offers,everything())
country_level_data
    
# 5. GET A LIST OF COMMUNES FROM WIKIPEDIA

# Let's scrape this content from wikipedia
# (page 54 in the book)

# Retrieve list of communes from Luxrmbourg
# Scrap them from Wikipedia
# Using these packages {rvest},{purr},{janitor}

# Scrap communes from wikipedia

current_communes <- "https://w.wiki/6nPu" %>% 
  rvest::read_html() %>% 
  rvest::html_table() %>% 
  purrr::pluck(1) %>% 
  janitor::clean_names()
  
  
# 5.1 Compare existing communes data from the Excel files with the list scrapped from wikipedia

# Using setdiff()function from base

# There are many communes in our coomune_level_data but not in the current_communes data set.
setdiff(unique(commune_level_data$locality),
        current_communes$commune)

Data_wiki_differences <- setdiff(unique(commune_level_data$locality),
                                 current_communes$commune)
Data_wiki_differences

# 5.2 Since 2010 several communes have merged into new ones. 
# Get data from 2010 onwards

former_communes <- "https://w.wiki/_wFe7" %>% 
  rvest::read_html() %>% 
  rvest::html_table() %>% 
  purrr::pluck(3) %>%
  janitor::clean_names() %>% 
  dplyr::filter(year_dissolved >2009)
former_communes 


communes <- unique(c(former_communes$name,current_communes$commune))
communes

# 5.3 compare again former against current communes


communes_chck_diff <- setdiff(unique(former_communes$name),
                    current_communes$commune)
communes_chck_diff

# 5.4 Solve the spelling differences present in the communes data set

Main_data_set_commune_names <- unique(commune_level_data$locality)
Main_data_set_commune_names

communes[which(communes == "Clemency")] <- "Clémency"
communes[which(communes == "Redange")] <- "Redange-sur-Attert"
communes[which(communes == "Erpeldange-sur-Sûre")] <- "Erpeldange"
communes[which(communes == "Luxembourg-City")] <- "Luxembourg"
communes[which(communes == "Käerjeng")] <- "Kaerjeng"
communes[which(communes == "Petange")] <- "Pétange"

# Let's run our test again
setdiff(unique(commune_level_data$locality),communes)

Unique_chck_locality <- commune_level_data %>% 
                        select(locality) %>% 
                        filter(locality == "Total d'offres") %>% 
                        distinct()
Unique_chck_locality

# remove
# Total d'offres frin tge fubak commune_level_data data set
# Total d'offres

commune_level_data_new <- commune_level_data %>% 
                          filter(!grepl("Total d'offres",locality))

commune_level_data_new_chck <- commune_level_data_new %>% 
  select(locality) %>% 
  filter(locality == "Total d'offres") %>% 
  distinct()
commune_level_data_new_chck

# Now our final community_level_data is cleansed and we can save it 
commune_level_data <- commune_level_data_new
commune_level_data_backup <- commune_level_data_new

# For the next section, he only uses two data sets 
# commune_level_data
# country_level_data

# So remove all other data sets except these two
rm(list=ls()[! ls() %in% c("commune_level_data","country_level_data")])


# commune_level_data <- read.csv("datasets/commune_level_data.csv")
# country_level_data <- read.csv("datasets/country_level_data.csv")

# write to csv the above to data sets into the /data folder
# commune_level_data data set 
write.csv(commune_level_data,here("data","commune_level_data.csv"), row.names = TRUE)
# country_level_data
write.csv(country_level_data,here("data","country_level_data.csv"), row.names = TRUE)

# End of this "save_data_plr.R" scrip