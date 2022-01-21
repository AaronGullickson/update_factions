# This script will add faction data to the system events data for a specified
# column from the SUCS data

# This value should be the column name to be added. Be sure to enter as a 
# character string
year <- "3151"

# Load libraries and functions ---------------------------------------------

source("check_packages.R")

# return a data frame of type of events for a given system
get_latest_faction <- function(planet, date) {
  planet_events <- xml_find_all(planet, "event")
  faction <- NA
  for(event in planet_events) {
    event_faction <- xml_text(xml_find_first(event, "faction"))
    if(is.na(event_faction)) {
      next
    }
    event_date <- as.Date(xml_text(xml_find_first(event, "date")))
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
systems <- read_xml("https://raw.githubusercontent.com/MegaMek/mekhq/master/MekHQ/data/universe/systems.xml")
system_events <- read_xml("https://raw.githubusercontent.com/MegaMek/mekhq/master/MekHQ/data/universe/planetary_systems/system_events.xml")

# read the faction correspondence translator
code_translator <- read_sheet("https://docs.google.com/spreadsheets/d/117Mmhf7TtyumwCzB9bGKq05-SeTFKboZ7Ef2ZbyOEPk",
                              sheet="translator")

# Clean up faction data in SUCS -------------------------------------------

# just get the first value before commas because the rest is subregion
sucs$faction <- sapply(str_split(sucs$faction, ","), function(x) { x[1] })

# get rid of parentheticals
sucs$faction <- str_replace(sucs$faction, "\\([^\\)]+\\)", "")

# translate some faction codes
for(i in 1:nrow(code_translator)) {
  theirs <- code_translator$their_code[i]
  ours <- code_translator$our_code[i]
  sucs$faction[sucs$faction==theirs] <- ours
}

# Insert into XML ---------------------------------------------------------

date <- paste(year,"01","01",sep="-")

# first I need to parse systems to figure out the primary planet for each
# system as well as the distance to Terra
id <- xml_text(xml_find_all(xml_children(systems), "id"))
x <- as.numeric(xml_text(xml_find_all(xml_children(systems), "xcood")))
y <- as.numeric(xml_text(xml_find_all(xml_children(systems), "ycood")))
distance <- sqrt(x^2+y^2)
primary <- as.numeric(xml_text(xml_find_all(xml_children(systems), 
                                            "primarySlot")))
system_lookup <- tibble(id, primary, distance)


# ok for looping is not the most efficient, but will be easier for me to debug 
# and check
for(system in xml_children(system_events)) {
  our_name <- xml_text(xml_find_first(system, "id"))
  cat(our_name)
  distance <- subset(system_lookup, id==our_name)$distance[1]
  id <- as.numeric(xml_text(xml_find_first(system, "sucsId")))
  if(id %in% sucs$id) {
    # get the faction for this id
    faction <- as.character(sucs[which(id==sucs$id), "faction"])
    if(!is.na(faction)) {
      # in most cases only one planet should be in there, but lets just loop
      # through in case
      for(planet in xml_find_all(system, "planet")) {
        cat(xml_text(xml_find_first(planet, "sysPos")))
        # get the most recent faction based on our date
        faction_last <- get_latest_faction(planet, date)
        
        # convert to PIND if IND and previous was PIND
        if(faction_last == "PIND" & faction == "IND") {
          faction <- "PIND"
        }
        
        # this is far from perfect, but also change IND to PIND based on 
        # distance from Terra
        if(faction == "IND" & distance >=450) {
          faction <- "PIND"
        }
        
        # if this is missing or not the same, then put in a new entry
        if(is.na(faction_last) | faction != faction_last) {
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
  cat("\n")
}

cat(as.character(system_events), file = here("output","system_events.xml"))

       