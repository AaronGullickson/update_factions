# This script will add faction data to the system events data for a specified
# column from the SUCS data

# This value should be the column name to be added. Be sure to enter as a 
# character string
year <- "3151"

# Load libraries and functions ---------------------------------------------

source("check_packages.R")

# some dates are in years while others are in full year-month-day
# we need them all to be year-month-day
convert_date <- function(x) {
  if(str_length(x)==4) {
    x <- paste(x, "01", "01", sep="-")
  }
  if(str_length(x)==7) {
    x <- paste(x, "01", sep="-")
  }
  return(as.Date(x))
}

# return a data frame of type of events for a given system
get_latest_faction <- function(planet, date) {
  planet_events <- xml_find_all(planet, "event")
  faction <- NA
  for(event in planet_events) {
    event_faction <- xml_text(xml_find_first(event, "faction"))
    if(is.na(event_faction)) {
      next
    }
    event_date <- convert_date(xml_text(xml_find_first(event, "date")))
    if(event_date >= date) {
      break
    }
    faction <- event_faction
  }
  return(faction)
}

# Read data ---------------------------------------------------------------

# read SUCS from google sheet directly
sucs <- read_sheet("https://docs.google.com/spreadsheets/d/1x9bvFqSb4_or8JbvGj2LnezGkChWxEzRPf5FXvjonHE",
                   sheet="Systems", skip=1)[,c("iD",year)]
colnames(sucs) <- c("id","faction")

# read the MekHQ systems data directly from GH repo to get up to date
system_events <- read_xml("https://raw.githubusercontent.com/MegaMek/mekhq/master/MekHQ/data/universe/planetary_systems/system_events.xml")

# Clean up faction data in SUCS -------------------------------------------

# just get the first value before commas because the rest is subregion
sucs$faction <- sapply(str_split(sucs$faction, ","), function(x) { x[1] })

# Insert into XML ---------------------------------------------------------

date <- convert_date(year)

lapply(xml_children(system_events), function(x) {
  id <- as.numeric(xml_text(xml_find_first(x, "sucsId")))
  if(sum(sucs$id==id)==1) {
    faction <- subset(sucs, id==id)$faction[1]
    if(!is.na(faction)) {
      # faction events are nested within planets, although there is most
      # likely only one for each system, so we need to loop through planets
      for(planet in xml_find_all(x, "planet")) {
        faction_last <- get_latest_faction(planet, date)
        if(faction != faction_last) {
          child <- read_xml("<event></event>")
          xml_add_child(child, read_xml(paste("<date>", 
                                              as.character(date),
                                              "</date>", sep="")))
          xml_add_child(child, read_xml(paste('<faction source="sucs">',
                                              faction,
                                              "</faction>", sep="")))
          xml_add_child(planet, child)
        }
      }
    }
  }
})

cat(as.character(system_events), file = here("output","system_events.xml"))

       