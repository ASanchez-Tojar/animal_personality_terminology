
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

pacman::p_load(openxlsx,stringr)

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

################################################################################
# Preparing dataset
################################################################################

# title-and-abstract decision
data$t.and.a_decision <- ifelse(str_detect(data$notes, "Included"),
                                "yes","no")
data$t.and.a_decision <- as.factor(data$t.and.a_decision)

# extracting exclusion reasons
t.and.a_exclusion_reason <- data$notes %>% str_match("RAYYAN-EXCLUSION-REASONS: (\\X+)")

data$t.and.a_exclusion_reason <- t.and.a_exclusion_reason[,2]


# creating variables for the fulltext screening and data extraction phase
data$fulltext_decision <- ""
data$fulltext_exclusion_reason <- ""
data$personality_definition <- ""
data$personality_definition_context <- ""
data$personality_interpretation <- "" #levels: among-individual, within-individual, both
data$repeatability_interpretation <- "" #levels: yes, no
data$repeatability_interpretation_context <- "" #copy interpretation sentence
data$individual_level_association <- "" #levels: yes, no
data$individual_level_association_method <- "" #levels: univariate, bivariate, correlation, among-subject centring
data$repetability_comparison <- "" #levels: yes, no
data$unstandardize_variance <- "" #levels: yes, no
data$comments <- ""


##############################################################
# Creating output
##############################################################

write.xlsx(data,
           "screening/ten_journals/ten_journals_fulltext_screening_and_data_extraction.xlsx",
           sheetName="Sheet1",col.names=TRUE, row.names=F,
           append=FALSE, showNA=TRUE, password=NULL)

#remember to manually remove the quotes for the column names only in the .csv file

sink("screening/ten_journals/screening_process_Rpackages_session.txt")
sessionInfo()
sink()
