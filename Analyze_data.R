# Ensure the necessary libraries for building the GUI are loaded

require(gWidgets)
require(gWidgetsRGtk2)
require(RGtk2)

# Set the option for the GUI toolkit to use RGtK2. If this is not set
# the R console asks which toolkit to use.
options("guiToolkit"="RGtk2")

# Set the top level for the working directory
# This has the "Open CSV..." default to this folder
setwd("Z:/Programs/SGN-35/CMC/~~ADCETRIS Process Monitoring/R working directory")

# Load the functions in reverse order of use
source("scripts/PQR_analyses.R")      # PQR analyses has the raw functions
source("scripts/release_analysis.R")  # release analysis provides calls to PQR analyses
source("scripts/parse.PMDB.data.R")   # parse PMDB data reads the CSV file and creates the 
                                      # data table then calls release analysis for each parameter
source("scripts/GUI.functions.R")     # GUI functions provide the handlers for the GUI front end
source("scripts/GUI.start.R")         # Start the GUI build
