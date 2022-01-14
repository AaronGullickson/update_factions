# The dates in our system_events.xml do not all have the same format of 
# year-month-day. This script will cycle through all events and fix dates for
# consistency

# Load libraries and functions ---------------------------------------------

source("check_packages.R")

# get all dates to use same format and fix a few february issues
fix_date <- function(x) {
  if(str_length(x)==4) {
    x <- paste(x, "01", "01", sep="-")
  }
  if(str_length(x)==7) {
    x <- paste(x, "01", sep="-")
  }
  if(str_length(x)==10) {
    # clean out all 02-30 and 02-29 to be safe
    if(str_sub(x, 6, 10)=="02-29" | str_sub(x, 6, 10)=="02-30") {
      str_sub(x, 6, 10) <- "02-28"
    }
  }
  return(x)
}

# Read data ---------------------------------------------------------------

# read the MekHQ systems data directly from GH repo to get up to date
system_events <- read_xml("https://raw.githubusercontent.com/MegaMek/mekhq/master/MekHQ/data/universe/planetary_systems/system_events.xml")

# Collect dates -----------------------------------------------------------

system_dates <- xml_find_all(
  xml_find_all(
    xml_children(system_events), "event"), "date")

planet_dates <- xml_find_all(
    xml_find_all(
      xml_find_all(
        xml_children(system_events), "planet"), "event"), "date")

all_dates <- c(system_dates, planet_dates)

#Check dates --------------------------------------------------------------

#get all events at the system level

dates_string <- c(xml_text(system_dates), xml_text(planet_dates))

# diagnostic

date_diagnostic <- tibble(date=dates_string, len=str_length(dates_string),
                          n_dash=str_count(dates_string, "-"))

# how many unique combinations of len and n_dash do I have?
unique(date_diagnostic[,c("len","n_dash")])

# are they all fixable?


for(i in 1:length(dates_string)) {
  as.Date(fix_date(dates_string[i]))
}

# getting a case of 2735-02-30. Did we do this with some kind of
# date randomization? Some of the 2-29 cases might also be a problem.
# There are no issues with 31 days in months with only 30 days, so not sure
# what happened here, but I think we can add a specific check for this to the
# convert date function

# Ok, after correcting these, it all seems to work.

# Fix dates ---------------------------------------------------------------

lapply(all_dates, function(x) {
  xml_replace(x, read_xml(paste("<date>", 
                             fix_date(xml_text(x)), 
                             "</date>", sep="")))
})


