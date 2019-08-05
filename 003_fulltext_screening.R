
################################################################################
# Authors: 
# Alfredo Sanchez-Tojar (alfredo.tojar@gmail.com)
#   Affiliation: Dept. Evolutionary Biology, Bielefeld University, Germany
#   Profile: https://scholar.google.de/citations?user=Sh-Rjq8AAAAJ&hl=de

# Script first created on the 5th of August 2019


################################################################################
# Description of script and Instructions
################################################################################

# This script is to prepare the database coming from rayyan (i.e. the database
# with the title-and-abstract decisions) so that fulltext screening and data
# collection can be performed

################################################################################
# Packages needed
################################################################################

pacman::p_load(openxlsx)

# Clear memory
rm(list=ls())


################################################################################
# Functions needed
################################################################################

# none


################################################################################
# Import data
################################################################################

data <- read.xlsx("screening/ten_journals/ten_journals_fulltext_screening.xlsx",
                         colNames=T,sheet = 1)

##############################################################
# Creating output
##############################################################

write.xlsx(XXX,
           "screening/ten_journals/ten_journals_fulltext_screening_and_data_extraction.xlsx",
           sheetName="Sheet1",col.names=TRUE, row.names=F,
           append=FALSE, showNA=TRUE, password=NULL)

#remember to manually remove the quotes for the column names only in the .csv file

sink("literature_search/ten_journals/screening_process_Rpackages_session.txt")
sessionInfo()
sink()
