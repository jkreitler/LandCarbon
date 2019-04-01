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

# Transition Types ###################################
sheetData = datasheet(myProject, "STSim_TransitionType", empty=T, optional=T)

# Urbanization (Can only go to either Developed:Open or Developed:Low)
sheetData = addRow(sheetData, data.frame(Name="Urbanization"))

# Urban Intensity (Chagnes in Developed Intenisty)
sheetData = addRow(sheetData, data.frame(Name="URB: Open->Low"))
sheetData = addRow(sheetData, data.frame(Name="URB: Low->Medium"))
sheetData = addRow(sheetData, data.frame(Name="URB: Medium->High"))

# Ag Expansion
sheetData = addRow(sheetData, data.frame(Name="Ag Expansion"))

# Ag Contraction
sheetData = addRow(sheetData, data.frame(Name="Ag Contraction"))

# Forest Management
sheetData = addRow(sheetData, data.frame(Name="FORMGT: Clearcut"))
sheetData = addRow(sheetData, data.frame(Name="FORMGT: Selection"))
sheetData = addRow(sheetData, data.frame(Name="FORMGT: PresFire"))
sheetData = addRow(sheetData, data.frame(Name="FORMGT: ThinBelow"))

# Ag Management
sheetData = addRow(sheetData, data.frame(Name="AGMGT: Harvest"))
sheetData = addRow(sheetData, data.frame(Name="AGMGT: OrchRemoval"))
sheetData = addRow(sheetData, data.frame(Name="AGMGT: Grazing"))
sheetData = addRow(sheetData, data.frame(Name="AGMGT: NoTill"))
sheetData = addRow(sheetData, data.frame(Name="AGMGT: Fertilizer"))
sheetData = addRow(sheetData, data.frame(Name="AGMGT: Irrigation"))

# Wildfire
sheetData = addRow(sheetData, data.frame(Name="FIRE: HighSev"))
sheetData = addRow(sheetData, data.frame(Name="FIRE: MedSev"))
sheetData = addRow(sheetData, data.frame(Name="FIRE: LowSev"))

# Drought
sheetData = addRow(sheetData, data.frame(Name="DRGT: HighSev"))
sheetData = addRow(sheetData, data.frame(Name="DRGT: MedSev"))
sheetData = addRow(sheetData, data.frame(Name="DRGT: LowSev"))

# Insects
sheetData = addRow(sheetData, data.frame(Name="INSC: HighSev"))
sheetData = addRow(sheetData, data.frame(Name="INSC: MedSev"))
sheetData = addRow(sheetData, data.frame(Name="INSC: LowSev"))

# Wind
sheetData = addRow(sheetData, data.frame(Name="WIND: HighSev"))
sheetData = addRow(sheetData, data.frame(Name="WIND: MedSev"))
sheetData = addRow(sheetData, data.frame(Name="WIND: LowSev"))

# Successional
sheetData = addRow(sheetData, data.frame(Name="SUCC: Forest->Shrub"))
sheetData = addRow(sheetData, data.frame(Name="SUCC: Shrub->Forest"))

saveDatasheet(myProject, sheetData, "STSim_TransitionType")

# Transition Groups  #################################
sheetData = datasheet(myProject, "STSim_TransitionGroup", empty=T, optional=T)
sheetData = addRow(sheetData, data.frame(Name="Urbanization"))
sheetData = addRow(sheetData, data.frame(Name="Ag Expansion"))
sheetData = addRow(sheetData, data.frame(Name="Ag Contraction"))
sheetData = addRow(sheetData, data.frame(Name="Forest Harvest"))
sheetData = addRow(sheetData, data.frame(Name="Fire"))
sheetData = addRow(sheetData, data.frame(Name="Drought Mortality"))
sheetData = addRow(sheetData, data.frame(Name="Insect Mortality"))
sheetData = addRow(sheetData, data.frame(Name="Wind"))
saveDatasheet(myProject, sheetData, "STSim_TransitionGroup")

# Transition Types by Group  #########################
sheetData = datasheet(myProject, "STSim_TransitionTypeGroup", empty=T, optional=T)

sheetData = addRow(sheetData, data.frame(TransitionTypeID="Urbanization", TransitionGroupID="Urbanization"))
sheetData = addRow(sheetData, data.frame(TransitionTypeID="Ag Expansion", TransitionGroupID="Ag Expansion"))
sheetData = addRow(sheetData, data.frame(TransitionTypeID="Ag Contraction", TransitionGroupID="Ag Contraction"))

sheetData = addRow(sheetData, data.frame(TransitionTypeID="FORMGT: Clearcut", TransitionGroupID="Forest Harvest"))
sheetData = addRow(sheetData, data.frame(TransitionTypeID="FORMGT: Selection", TransitionGroupID="Forest Harvest"))
sheetData = addRow(sheetData, data.frame(TransitionTypeID="FORMGT: ThinBelow", TransitionGroupID="Forest Harvest"))

sheetData = addRow(sheetData, data.frame(TransitionTypeID="FIRE: HighSev", TransitionGroupID="Fire"))
sheetData = addRow(sheetData, data.frame(TransitionTypeID="FIRE: MedSev", TransitionGroupID="Fire"))
sheetData = addRow(sheetData, data.frame(TransitionTypeID="FIRE: LowSev", TransitionGroupID="Fire"))

sheetData = addRow(sheetData, data.frame(TransitionTypeID="DRGT: HighSev", TransitionGroupID="Drought Mortality"))
sheetData = addRow(sheetData, data.frame(TransitionTypeID="DRGT: MedSev", TransitionGroupID="Drought Mortality"))
sheetData = addRow(sheetData, data.frame(TransitionTypeID="DRGT: LowSev", TransitionGroupID="Drought Mortality"))

sheetData = addRow(sheetData, data.frame(TransitionTypeID="INSC: HighSev", TransitionGroupID="Insect Mortality"))
sheetData = addRow(sheetData, data.frame(TransitionTypeID="INSC: MedSev", TransitionGroupID="Insect Mortality"))
sheetData = addRow(sheetData, data.frame(TransitionTypeID="INSC: LowSev", TransitionGroupID="Insect Mortality"))

sheetData = addRow(sheetData, data.frame(TransitionTypeID="WIND: HighSev", TransitionGroupID="Wind"))
sheetData = addRow(sheetData, data.frame(TransitionTypeID="WIND: MedSev", TransitionGroupID="Wind"))
sheetData = addRow(sheetData, data.frame(TransitionTypeID="WIND: LowSev", TransitionGroupID="Wind"))

saveDatasheet(myProject, sheetData, "STSim_TransitionTypeGroup")

# Transition Simulation Group  #########################
sheetData = datasheet(myProject, "STSim_TransitionSimulationGroup", empty=T, optional=T)
sheetData = addRow(sheetData, data.frame(TransitionGroupID="Urbanization"))
sheetData = addRow(sheetData, data.frame(TransitionGroupID="Ag Expansion"))
sheetData = addRow(sheetData, data.frame(TransitionGroupID="Ag Contraction"))
sheetData = addRow(sheetData, data.frame(TransitionGroupID="Forest Harvest"))
sheetData = addRow(sheetData, data.frame(TransitionGroupID="Fire"))
sheetData = addRow(sheetData, data.frame(TransitionGroupID="Drought Mortality"))
sheetData = addRow(sheetData, data.frame(TransitionGroupID="Insect Mortality"))
sheetData = addRow(sheetData, data.frame(TransitionGroupID="Wind"))

saveDatasheet(myProject, sheetData, "STSim_TransitionSimulationGroup")


# Transition Multiplier Type #########################\
sheetData = datasheet(myProject, "STSim_TransitionMultiplierType", empty=T, optional=T)
sheetData = addRow(sheetData, data.frame(Name="Type 1"))
sheetData = addRow(sheetData, data.frame(Name="Type 2"))

saveDatasheet(myProject, sheetData, "STSim_TransitionMultiplierType")








######################################################
# Define State Attributes
######################################################

# State Attribute Groups  ##############################
sheetData = datasheet(myProject, "STSim_AttributeGroup", empty=T, optional=T)
sheetData = addRow(sheetData, data.frame(Name="LULC"))
sheetData = addRow(sheetData, data.frame(Name="Carbon Stocks"))
sheetData = addRow(sheetData, data.frame(Name="NPP"))
sheetData = addRow(sheetData, data.frame(Name="Albedo"))
sheetData = addRow(sheetData, data.frame(Name="Demographic"))

saveDatasheet(myProject, sheetData, "STSim_AttributeGroup", force=T, append=F)

# State Attribute Types ##############################
sheetData = datasheet(myProject, "STSim_StateAttributeType", empty=T, optional=T)
sheetData = tibble(Name=stateClass$StateClass, AttributeGroupID="LULC", Units="1km")
sheetData = addRow(sheetData, data.frame(Name="Developed:Open/Low", AttributeGroupID="LULC", Units="1km"))
sheetData = addRow(sheetData, data.frame(Name="Forest:All", AttributeGroupID="LULC", Units="1km"))
sheetData = addRow(sheetData, data.frame(Name="Agriculture:All", AttributeGroupID="LULC", Units="1km"))
sheetData = addRow(sheetData, data.frame(Name="Wetland:All", AttributeGroupID="LULC", Units="1km"))

sheetData = addRow(sheetData, data.frame(Name="NPP", AttributeGroupID="NPP", Units="kt/km2/yr"))
sheetData = addRow(sheetData, data.frame(Name="Stocks: Foliage", AttributeGroupID="Carbon Stocks", Units="kt/km2"))
sheetData = addRow(sheetData, data.frame(Name="Stocks: Branch", AttributeGroupID="Carbon Stocks", Units="kt/km2"))
sheetData = addRow(sheetData, data.frame(Name="Stocks: Stem", AttributeGroupID="Carbon Stocks", Units="kt/km2"))
sheetData = addRow(sheetData, data.frame(Name="Stocks: Fine Root", AttributeGroupID="Carbon Stocks", Units="kt/km2"))
sheetData = addRow(sheetData, data.frame(Name="Stocks: Coarse Root", AttributeGroupID="Carbon Stocks", Units="kt/km2"))
sheetData = addRow(sheetData, data.frame(Name="Stocks: DOM AGVF", AttributeGroupID="Carbon Stocks", Units="kt/km2"))
sheetData = addRow(sheetData, data.frame(Name="Stocks: DOM AGF", AttributeGroupID="Carbon Stocks", Units="kt/km2"))
sheetData = addRow(sheetData, data.frame(Name="Stocks: DOM AGM", AttributeGroupID="Carbon Stocks", Units="kt/km2"))
sheetData = addRow(sheetData, data.frame(Name="Stocks: DOM AGS", AttributeGroupID="Carbon Stocks", Units="kt/km2"))
sheetData = addRow(sheetData, data.frame(Name="Stocks: DOM BGVF", AttributeGroupID="Carbon Stocks", Units="kt/km2"))
sheetData = addRow(sheetData, data.frame(Name="Stocks: DOM BGF", AttributeGroupID="Carbon Stocks", Units="kt/km2"))
sheetData = addRow(sheetData, data.frame(Name="Stocks: DOM ABGS", AttributeGroupID="Carbon Stocks", Units="kt/km2"))

sheetData = addRow(sheetData, data.frame(Name="Albedo", AttributeGroupID="Albedo", Units="%"))
sheetData = addRow(sheetData, data.frame(Name="Demographic: Population", AttributeGroupID="Demographic", Units=""))
sheetData = addRow(sheetData, data.frame(Name="Demographic: Housing", AttributeGroupID="Demographic", Units=""))
sheetData = addRow(sheetData, data.frame(Name="Demographic: Buildings", AttributeGroupID="Demographic", Units=""))

saveDatasheet(myProject, sheetData, "STSim_StateAttributeType", force=T, append=F)






######################################################
# Define Distributions and External Variables
######################################################

# Distributions ######################################
sheetData = datasheet(myProject, "STime_DistributionType", empty=T, optional=T)
sheetData = addRow(sheetData, data.frame(Name="Dist: Urbanization"))
sheetData = addRow(sheetData, data.frame(Name="Dist: Ag Expansion"))
sheetData = addRow(sheetData, data.frame(Name="Dist: Ag Contraction"))
sheetData = addRow(sheetData, data.frame(Name="Dist: Fire"))
sheetData = addRow(sheetData, data.frame(Name="Dist: Clearcut"))
sheetData = addRow(sheetData, data.frame(Name="Dist: Selection"))
sheetData = addRow(sheetData, data.frame(Name="Dist: Harvest"))
sheetData = addRow(sheetData, data.frame(Name="Dist: Drought"))
sheetData = addRow(sheetData, data.frame(Name="Dist: Insects"))
sheetData = addRow(sheetData, data.frame(Name="Dist: Wind"))

saveDatasheet(myProject, sheetData, "STime_DistributionType", force=T, append=F)
# External Variables #################################
sheetData = datasheet(myProject, "STime_ExternalVariableType", empty=T, optional=T)
sheetData = addRow(sheetData, data.frame(Name="HistYear: Urbanization"))
sheetData = addRow(sheetData, data.frame(Name="HistYear: Ag Expansion"))
sheetData = addRow(sheetData, data.frame(Name="HistYear: Ag Contraction"))
sheetData = addRow(sheetData, data.frame(Name="HistYear: Land Use Change"))
sheetData = addRow(sheetData, data.frame(Name="HistYear: Harvest"))
sheetData = addRow(sheetData, data.frame(Name="HistYear: Fire"))
sheetData = addRow(sheetData, data.frame(Name="HistYear: Drought"))
sheetData = addRow(sheetData, data.frame(Name="HistYear: Insects"))
sheetData = addRow(sheetData, data.frame(Name="HistYear: Wind"))
sheetData = addRow(sheetData, data.frame(Name="HistYear: All Change"))

saveDatasheet(myProject, sheetData, "STime_ExternalVariableType", force=T, append=F)




######################################################
# Define Carbon Stocks and Flows
######################################################

# Carbon Stocks
sheetData = datasheet(myProject, "SF_StockType", empty=T, optional=T)
sheetData = addRow(sheetData, data.frame(Name="Live Foliage"))
sheetData = addRow(sheetData, data.frame(Name="Live Branch"))
sheetData = addRow(sheetData, data.frame(Name="Live Stem"))
sheetData = addRow(sheetData, data.frame(Name="Live Fine Root"))
sheetData = addRow(sheetData, data.frame(Name="Live Coarse Root"))
sheetData = addRow(sheetData, data.frame(Name="Snag Branch"))
sheetData = addRow(sheetData, data.frame(Name="Snag Stem"))
sheetData = addRow(sheetData, data.frame(Name="DOM AGVF"))
sheetData = addRow(sheetData, data.frame(Name="DOM AGF"))
sheetData = addRow(sheetData, data.frame(Name="DOM AGM"))
sheetData = addRow(sheetData, data.frame(Name="DOM AGS"))
sheetData = addRow(sheetData, data.frame(Name="DOM BGVF"))
sheetData = addRow(sheetData, data.frame(Name="DOM BGF"))
sheetData = addRow(sheetData, data.frame(Name="DOM BGS"))
sheetData = addRow(sheetData, data.frame(Name="Atmosphere"))
sheetData = addRow(sheetData, data.frame(Name="Ag Straw"))
sheetData = addRow(sheetData, data.frame(Name="Ag Grain"))
sheetData = addRow(sheetData, data.frame(Name="Aquatic"))
sheetData = addRow(sheetData, data.frame(Name="HWP"))

saveDatasheet(myProject, sheetData, "SF_StockType", force=T, append=F)

# Carbon Stock Groups
sheetData = datasheet(myProject, "SF_StockGroup", empty=T, optional=T)
sheetData = addRow(sheetData, data.frame(Name="Live"))
sheetData = addRow(sheetData, data.frame(Name="DOM"))
sheetData = addRow(sheetData, data.frame(Name="Litter"))
sheetData = addRow(sheetData, data.frame(Name="Soil"))
sheetData = addRow(sheetData, data.frame(Name="Standing Dead"))
sheetData = addRow(sheetData, data.frame(Name="Down Dead"))
sheetData = addRow(sheetData, data.frame(Name="Live Above Ground"))
sheetData = addRow(sheetData, data.frame(Name="Live Below Ground"))
sheetData = addRow(sheetData, data.frame(Name="Ag Products"))
sheetData = addRow(sheetData, data.frame(Name="HWP"))
sheetData = addRow(sheetData, data.frame(Name="Aquatic"))
sheetData = addRow(sheetData, data.frame(Name="Atmosphere"))

saveDatasheet(myProject, sheetData, "SF_StockGroup", force=T, append=F)

# Carbon Flow Types
sheetData = datasheet(myProject, "SF_FlowType", empty=T, optional=T)
sheetData = addRow(sheetData, data.frame(Name="Growth"))
sheetData = addRow(sheetData, data.frame(Name="Mortality"))
sheetData = addRow(sheetData, data.frame(Name="Litterfall"))
sheetData = addRow(sheetData, data.frame(Name="Decay"))
sheetData = addRow(sheetData, data.frame(Name="Humification"))
sheetData = addRow(sheetData, data.frame(Name="Emission"))
sheetData = addRow(sheetData, data.frame(Name="Leaching"))
sheetData = addRow(sheetData, data.frame(Name="Harvest"))













stateClass
projectSheetNames
