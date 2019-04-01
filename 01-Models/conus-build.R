library(rsyncrosim)
library(raster)
library(sf)
library(tidyverse)
library(stringr)

######################################################
# Set the Syncro Sim program directory
######################################################
programFolder = "C:/Program Files/SyncroSim" # Set this to location of SyncroSim installation
mySession = session(programFolder) # Start a session with SyncroSim
setwd("D:/conus-assessment/") # Set the current working directory
getwd() # Show the current working directory

######################################################
# Create and setup a new Library
######################################################
myLibrary = ssimLibrary(name = "conus_v1", model="stsim", session=mySession, create=T)
module(mySession) # Get the current values for the Library's Modules
enableAddon(myLibrary, 'stsim-stockflow') # Enable to Stock and Flow module
addon(myLibrary) # Check if stock flow add-on is enabled for the library

######################################################
# Create or open a new project and set backup settings
######################################################
myProject = project(myLibrary, project="conus_v1", create=T) # Also creates a new project (if it doesn't exist already)
project(myLibrary, summary = TRUE)

# Get the current values for the Library's Backup Datasheet
sheetData = datasheet(myLibrary, name = "SSim_Backup", empty = T) # Get the current backup settings for the library
sheetData

# Modify the values for the Library's Backup Datasheet
sheetData = addRow(sheetData, data.frame(IncludeInput=TRUE, IncludeOutput=FALSE)) # Add a new row to this dataframe
saveDatasheet(myLibrary, data=sheetData, name="SSim_Backup") # Save the new dataframe back to the library
datasheet(myLibrary, "SSim_Backup")

# Modifiy any of the other Library settings (e.g. output settings, multiprocessing)
datasheet(myLibrary, summary = T) # Display internal names of all the library's datasheets

######################################################
# Set Project Properties
######################################################

projectSheetNames = datasheet(myProject, summary = T) # Get and Display Project Properties datasheets
projectSheetNames

# Terminology
sheetData = datasheet(myProject, "STSim_Terminology")
sheetData
sheetData$AmountLabel[1] = "Area"
sheetData$AmountUnits[1] = "Square Kilometers"
sheetData$StateLabelX[1] = "LULC"
sheetData$StateLabelY[1] = "Subclass"
sheetData$PrimaryStratumLabel[1] = "Ecoregion L3"
sheetData$SecondaryStratumLabel[1] = "State"
sheetData$TertiaryStratumLabel[1] = "Ecoregion L2"
sheetData$TimestepUnits[1] = "Year"
saveDatasheet(myProject, sheetData, "STSim_Terminology")
datasheet(myProject, "STSim_Terminology")

######################################################
# Define Model Strata
######################################################

# Define Primary Stratum (Level 3 Ecoregions) #####
ecol3 = st_read("J:/National_Assessment/2_Initial_Conditions/3_Vectors/Ecoregions/us_eco_l3/us_eco_l3.shp") # Get Level 3 Codes and Names from Shapefiles
ecol3_table = ecol3 %>% st_set_geometry(NULL) %>% dplyr::select(US_L3CODE, US_L3NAME) %>% unique() %>%
  rename("ID"="US_L3CODE", "Name"="US_L3NAME")

sheetData = datasheet(myProject, "STSim_Stratum", empty = T, optional = T) # Returns empty dataframe with only required column(s)
ecoregionsL3 = ecol3_table
saveDatasheet(myProject, ecoregionsL3, "STSim_Stratum", force = T, append = F)
datasheet(myProject, "STSim_Stratum", optional = T) # Returns entire dataframe, including optional columns

# Define Secondary Stratum (States) #####
statebnd = st_read("J:/National_Assessment/2_Initial_Conditions/3_Vectors/States/tl_2018_us_state.shp") # Get state names and FIPS codes from Tiger shapefile
statebnd = statebnd %>% st_set_geometry(NULL) %>% as.tibble() %>% dplyr::select(STATEFP, NAME) %>% unique() %>%
  rename("ID"="STATEFP", "Name"="NAME") 

sheetData = datasheet(myProject, "STSim_SecondaryStratum", empty = T, optional = T) # Returns empty dataframe with only required column(s)
states = statebnd
saveDatasheet(myProject, states, "STSim_SecondaryStratum", force = T, append = F)
datasheet(myProject, "STSim_SecondaryStratum", optional = T) # Returns entire dataframe, including optional columns
# Define Tertiary Stratum (Level 2 Ecoregions) #####
ecol3 = st_read("J:/National_Assessment/2_Initial_Conditions/3_Vectors/Ecoregions/us_eco_l3/us_eco_l3.shp") # Get Level 2 Codes and Names from Shapefiles
ecol2_table = ecol3 %>% st_set_geometry(NULL) %>% as.tibble() %>% dplyr::select(NA_L2CODE, NA_L2NAME) %>% unique() %>%
  filter(NA_L2NAME != "UPPER GILA MOUNTAINS (?)") %>% rename("ID"="NA_L2CODE", "Name"="NA_L2NAME")

ecol2_table$ID = as.numeric(as.character(ecol2_table$ID))
ecol2_table = mutate(ecol2_table, ID=ID*10)
ecol2_table$Name = str_to_title(ecol2_table$Name)

sheetData = datasheet(myProject, "STSim_TertiaryStratum", empty = T, optional = T) # Returns empty dataframe with only required column(s)
ecoregionsL2 = ecol2_table
saveDatasheet(myProject, ecoregionsL2, "STSim_TertiaryStratum", force = T, append = F)
datasheet(myProject, "STSim_TertiaryStratum", optional = T) # Returns entire dataframe, including optional columns




######################################################
# Define State Classes
######################################################

stateClass = read_csv("R inputs/project_definitions/state_class_legend.csv") %>%
  mutate(StateClass=paste(LULC,":",Subclass, sep=""))

# First State Class Label (LULC Class) ##############
sheetData = datasheet(myProject, "STSim_StateLabelX", empty = T, optional = T)
lulcTypes = unique(stateClass$LULC)
saveDatasheet(myProject, data.frame(Name=lulcTypes), "STSim_StateLabelX", force = T, append = F)

# Second State Class Label (Subclass) ##############
sheetData = datasheet(myProject, "STSim_StateLabelY", empty = T, optional = T)
subclassTypes = unique(stateClass$Subclass)
saveDatasheet(myProject, data.frame(Name = subclassTypes), "STSim_StateLabelY", force = T, append = F)
# Second State Class Label (Subclass) #############
sheetData = datasheet(myProject, "STSim_StateClass", empty = T, optional = T)
stateclassTypes = stateClass %>% rename("Name"="StateClass","StateLabelXID"="LULC", "StateLabelYID"="Subclass")
saveDatasheet(myProject, stateclassTypes, "STSim_StateClass", force = T, append = F)




######################################################
# Define Transition Types and Groups
######################################################



######################################################
# Define State Attributes
######################################################




######################################################
# Define Distributions and External Variables
######################################################




######################################################
# Define Carbon Stocks and Flows
######################################################




stateClass
projectSheetNames
