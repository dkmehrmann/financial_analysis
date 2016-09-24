library(dplyr)
library(lubridate)
library(ggplot2)
library(RecordLinkage)

### FINANCIAL ANALYSIS FOR CHASE TRANSACTION HISTORY ######
# This script reads in a file such as the one that is 
# available for download from Chase and summarizes 
# transaction amounts by location type. Its main
# contribution to the overall project is the R console 
# interface to manually categorize places into location
# types such as bars, restaurants, and grocers
#
# Future work includes time-series visualizations of
# spending by category as well as support for date
# filtering and different file types. 
###########################################################

work_dir <- '~/Documents/Financial/'
file_name <- 'Chase8805_Activity_20160924.CSV'

# function to categorize place types
categorize_places <- function(places, to_match){
  rx <- paste(to_match, collapse="|")
  return(grepl(rx, places, ignore.case=T))
}

# function to map locations to location types
get_location_types <- function(locations, 
                               categories=c('bar', 'grocer', 'online', 'restaurant')){
  
  places <- names(sort(table(locations),decreasing = T))
  res <- data.frame(row.names = places, 
                    category=rep('unknown', times=length(places)), 
                    stringsAsFactors = F)
  for (place in places){
    categories <- sort(categories)
    line_one <- sprintf("What kind of place is %s?\n", place)
    options <- paste(1:length(categories), categories, '\n')
    other_option <- paste(length(categories) + 1, 'other\n')
    quit_option <- 'Q: quit'
    prompt <- cat(line_one, options, other_option, quit_option)
    input <- readline(prompt=prompt)
    if (input == 'Q'){
      return(res)
    }
    input <- as.numeric(input)
    if (input == length(options) + 1) {
      userspec <- readline(prompt='What kind of place is it? ')
      categories <- c(categories, userspec)
      options <- c(options, cat(length(options)+1, userspec, '\n'))
      res[place, ] <- categories[input]
      
    }else if (1 <= input & input <= length(options)) {
      res[place, ] <- categories[input]
    }
  }
  return(res)
}

## START SCRIPT HERE ######################################

# READ IN DATA, FILTER TO SALES, AND PARSE DATE
f <- read.csv(paste0(work_dir, file_name)) %>%
  filter(Type == 'Sale') %>%
  mutate(Trans.Date=parse_date_time(Trans.Date, orders='mdy')) %>%
  mutate(week = as.Date(paste("1", week(Trans.Date), year(Trans.Date), sep = "-"), format = "%w-%W-%Y")) %>%
  mutate(clean_description = gsub('[^[:alpha:]]', '', Description)) %>%
  mutate(clean_description = gsub('^UBER.*', "UBER", clean_description))

# GET AND SAVE THE LOCATION TYPES 
classification_fname <- paste0(work_dir, 'location_map.RDS')
if (file.exists(classification_fname)){
  location_types <- readRDS(classification_fname)
  known_locs <- subset(location_types, category!='unknown')
  unknown_locs <- subset(location_types, category=='unknown')
  new_locs <- f$clean_description[! f$clean_description %in% rownames(known_locs)]
  location_types <- rbind(known_locs, get_location_types(c(rownames(unknown_locs), new_locs), categories = unique(known_locs$category)))
  saveRDS(location_types, file=classification_fname)
}else{
  location_types <- get_location_types(f$clean_description)
  saveRDS(location_types, file=classification_fname)
}


f <- f %>% merge(location_types, by.x='clean_description', by.y='row.names', all.x=T)

# SHOW SPENDING BY LOCATION TYPE
# f %>% group_by(category) %>% summarise(cost = sum(-Amount, na.rm=T)) %>% arrange(desc(cost))
# Plot Spending over Time
# f %>% group_by(week) %>% summarise(cost = sum(-Amount, na.rm=T)) %>% ggplot(aes(x=week, y=cost)) + geom_line()
# but missing dates need to be backfilled with 0 values for amount after aggregation
# f %>% group_by(week, category) %>% summarise(cost = sum(-Amount, na.rm=T)) %>% ggplot(aes(x=week, y=cost, group=category, color=category)) + geom_line() -> g

out_fname <- paste0(work_dir, 'clean_', file_name)
out_fname <- gsub('CSV', 'RDS', out_fname)
saveRDS(f, out_fname)
