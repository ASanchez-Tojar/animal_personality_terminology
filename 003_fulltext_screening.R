
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
data$fulltext_decision <- "" #levels: yes, no. Does it pass the fulltext screening or it is a non-empirical study that made it through the title-and-abstract screening?
data$fulltext_exclusion_reason <- "" #if fulltext_decision=no, give a reason for that
data$personality_definition <- "" #give the definition used by the authors, choose one among all the definitions we have already assembled, or, in case that it is a new one, provide it as it is
data$personality_definition_context <- "" #some context copied from the paper regarding the definition
data$personality_interpretation <- "" #levels: among-individual, within-individual, both. How is animal personality interpreted?
data$repeatability <- "" #levels: yes, no. Was repeatability measured?
data$repeatability_interpretation <- "" #levels: yes, no. Was repeatability interpreted as individual consistency and/or predictability?
data$repeatability_interpretation_context <- "" #some context copied from the paper regarding the definition
data$individual_level_association <- "" #levels: yes, no. Did the paper study individual level association between behaviour and some other traits?
data$individual_level_association_method <- "" #levels: univariate, bivariate, correlation, among-subject centring. If individual_level_association=yes, which method was used?
data$repetability_comparison <- "" #levels: yes, no. Did the authors compare repeatability estimates between different groups of animals?
data$unstandardize_variance <- "" #levels: yes, no. Are the raw variance estimates available or only the variance standardized (i.e. repeatability)?
data$comments <- ""


##############################################################
# Creating output
##############################################################

# we create three databases, one per observer, but first, let's
# sort by title as a way of "randomizing"

data <- data[order(data$title),]

# we then subset only the references that passed the title-and-
# abstract screening

data.t.and.a.passed <- data[data$t.and.a_decision=="yes",]

# subsets per observer=200refs/observer, with some overlapping
data.MM <- data.t.and.a.passed[c(1:200),]
data.MM$observer <- "MM"

data.AST <- data.t.and.a.passed[c(173:372),]
data.AST$observer <- "AST"

data.PN <- data.t.and.a.passed[c(344:543),]
data.PN$observer <- "PN"


# saving the databases
write.xlsx(data.MM,
           "screening/ten_journals/ten_journals_fulltext_screening_and_data_extraction_MM.xlsx",
           sheetName="Sheet1",col.names=TRUE, row.names=F,
           append=FALSE, showNA=TRUE, password=NULL)

write.xlsx(data.AST,
           "screening/ten_journals/ten_journals_fulltext_screening_and_data_extraction_AST.xlsx",
           sheetName="Sheet1",col.names=TRUE, row.names=F,
           append=FALSE, showNA=TRUE, password=NULL)

write.xlsx(data.PN,
           "screening/ten_journals/ten_journals_fulltext_screening_and_data_extraction_PN.xlsx",
           sheetName="Sheet1",col.names=TRUE, row.names=F,
           append=FALSE, showNA=TRUE, password=NULL)


# saving R session for reproducibility purposes
sink("screening/ten_journals/screening_process_Rpackages_session.txt")
sessionInfo()
sink()
