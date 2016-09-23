library(dplyr)
library(lubridate)

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

# function to categorize place types
categorize_places <- function(places, to_match){
  rx <- paste(to_match, collapse="|")
  return(grepl(rx, places, ignore.case=T))
}

# function to map locations to location types
get_location_types <- function(locations, 
                               default_categories=c('bar', 'grocer', 'online', 'restaurant')){
  
  categories <- default_categories
  res <- vector('list', length(categories))
  names(res) <- categories
  places <- names(sort(table(locations),decreasing = T))
  for (place in places){
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
      userspec <- readline(prompt='What kind of place is it?')
      categories <- c(categories, userspec)
      options <- c(options, paste(length(options)+1, userspec, '\n'))
      res[[categories[input]]] <- place
      
    }else if (1 <= input & input <= length(options)) {
      res[[categories[input]]] <- c(res[[categories[input]]], place)
    }
  }
  return(res)
}

# READ IN DATA, FILTER TO SALES, AND PARSE DATE
f <- read.csv('~/Documents/Financial/Chase8805_Activity_20160923.CSV') %>%
  filter(Type == 'Sale') %>%
  mutate(Trans.Date=parse_date_time(Trans.Date, orders='mdy')) %>%
  mutate(Trans.Date = week(Trans.Date))

# GET AND SAVE THE LOCATION TYPES
location_types <- get_location_types(f$Description)
saveRDS(location_types, 
        file=sprintf('~/Documents/Financial/location_map-%s.RDS', strftime(today())))

# MAKE COLUMNS BASED ON LOCATION TYPE
for (i in names(location_types)){
  f[i] <- categorize_places(f$Description, location_types[[i]])
}

# SUMMARIZE INTO A SINGLE COLUMN
f$place <- as.character(apply(f[, 8:ncol(f)], 1, function(x) names(x)[x]))

# SHOW SPENDING BY LOCATION TYPE
f %>% group_by(place) %>% summarise(cost = sum(-Amount)) %>% arrange(desc(cost))


