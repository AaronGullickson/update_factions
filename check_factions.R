# Compare our faction codes to SUCS so we know what to change

# Load libraries and other setup ------------------------------------------

source("check_packages.R")

# Read data sources -------------------------------------------------------

sucs <- read_sheet("https://docs.google.com/spreadsheets/d/1x9bvFqSb4_or8JbvGj2LnezGkChWxEzRPf5FXvjonHE",
                   sheet="Factions")[,c("ID","factionName")]
colnames(sucs) <- c("id","faction_name_sucs")

factions_ours_xml <- read_xml("https://raw.githubusercontent.com/MegaMek/mekhq/master/MekHQ/data/universe/factions.xml")


# Convert XML to table ----------------------------------------------------

factions_ours <- tibble(
  id=xml_text(xml_find_all(xml_children(factions_ours_xml), "shortname")),
  faction_name_mhq=xml_text(xml_find_all(xml_children(factions_ours_xml), "fullname")))

factions <- full_join(factions_ours, sucs)
