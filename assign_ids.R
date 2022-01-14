# This script will read in the SUCS data and our data and then assign the id
# values from SUCS with a `sucsId` tag in our XML data. That will make it much
# easier to match to SUCS data in the future. However, we have to handle a few
# cases in the current data where names do not match.

## NOTE: This was a one-shot script that was used to add the sucsId to the 
#        the code base. That is now complete.

# Load libraries and other setup ------------------------------------------

source("check_packages.R")

# Read data sources -------------------------------------------------------

sucs <- read_sheet("https://docs.google.com/spreadsheets/d/1x9bvFqSb4_or8JbvGj2LnezGkChWxEzRPf5FXvjonHE",
                   sheet="Systems", skip=1)


system_events <- read_xml(here("input","system_events.xml"))
systems <- read_xml(here("input","systems.xml"))


# Match cases -------------------------------------------------------------

ids <- xml_text(xml_find_all(xml_children(systems), "id"))
x <- xml_text(xml_find_all(xml_children(systems), "xcood"))
y <- xml_text(xml_find_all(xml_children(systems), "ycood"))

# first pull out all the ids from our xml data and put them in a tibble
systems_ours <- tibble(systemName=ids, x=as.numeric(x), y=as.numeric(y))
sum(duplicated(systems_ours$systemName))

# now replace systemName in sucs with alternateName where present
sucs_short <- sucs %>%
  mutate(systemName=ifelse(is.na(alternateName), systemName, alternateName)) %>%
  select(iD, systemName, x, y)


# Ok first try to match on x and y
matches <- left_join(systems_ours, sucs_short, by=c("x", "y")) 
  
sum(is.na(matches$iD))

# clean up and then split the matched and unmatched cases
matches <- matches %>%
  select(systemName.x, iD) %>%
  rename(systemName = systemName.x) %>%
  group_by(is.na(iD)) %>%
  group_split()

# now match by name
matches[[2]] <- matches[[2]] %>%
  select(systemName) %>%
  left_join(sucs_short) %>%
  select(systemName, iD)

# now recombine
matches <- bind_rows(matches) %>%
  select(systemName, iD)

# who is still missing?
matches %>%
  filter(is.na(iD))

# Ok figured these out manually
matches$iD[matches$systemName=="Cambridge"] <- 3019
matches$iD[matches$systemName=="Mineite"] <- 1714
matches$iD[matches$systemName=="Murrain (Richmond's World 3025-)"] <- 3084
matches$iD[matches$systemName=="RWR Outpost #4"] <- 3085
matches$iD[matches$systemName=="RWR Outpost #7"] <- 3086
matches$iD[matches$systemName=="Riga (HL)"] <- 3003
matches$iD[matches$systemName=="Star Cluster 1108 (SW)"] <- 3089
matches$iD[matches$systemName=="T Cephei (The Devil's Eye)"] <- 3093

# The remaining cases are either nebulae which are tracked differently on SUCS
# or Pioche which appears to be missing in SUCS

# final check
matches %>%
  filter(is.na(iD))


# Insert tag in XML -------------------------------------------------------

# Now we need to go through the matches object and add a sucsId tag to each
# system. Then output the xml.

lapply(xml_children(systems), function(x) {
  id <- xml_text(xml_find_first(x, "id"))
  if(sum(matches$systemName==id)==1) {
    sucsId <- subset(matches, id==systemName)$iD[1]
    if(!is.na(sucsId)) {
      xml_add_child(x, read_xml(paste("<sucsId>", sucsId, "</sucsId>", sep="")),
                    .where=1)
    }
  }
  
})

cat(as.character(systems), file = here("output","systems.xml"))

# Lets also do system_events since we will work with this a lot directly
lapply(xml_children(system_events), function(x) {
  id <- xml_text(xml_find_first(x, "id"))
  if(sum(matches$systemName==id)==1) {
    sucsId <- subset(matches, id==systemName)$iD[1]
    if(!is.na(sucsId)) {
      xml_add_child(x, read_xml(paste("<sucsId>", sucsId, "</sucsId>", sep="")),
                    .where=1)
    }
  }
  
})

cat(as.character(system_events), file = here("output","system_events.xml"))
