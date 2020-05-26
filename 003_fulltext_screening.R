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
data$fulltext_exclusion_reason <- "" #if fulltext_decision=no, give a reason for that, otherwise NA
# data$personality_definition <- "" #give the definition used by the authors, choose one among all the definitions we have already assembled, or, in case that it is a new one, provide it as it is; otherwise, NA
# data$personality_definition_context <- "" #some context copied from the paper regarding the definition, including the definition as stated in the paper; otherwise, NA  
# data$personality_definition_references <- "" #references cited for the definition of animal personality; otherwise, NA  
# data$personality_interpretation <- "" #levels: among-individual, within-individual, both,NA. How is animal personality interpreted?
# data$personality_trait_definition <- "" #give the definition used by the authors, choose one among all the definitions we have already assembled, or, in case that it is a new one, provide it as it is
# data$personality_trait_definition_context <- "" #some context copied from the paper regarding the definition, including the definition as estated in the paper
# data$personality_trait_definition_references <- "" #references cited for the definition of personality trait
# data$personality_trait_interpretation <- "" #levels: among-individual, within-individual, both. How is animal personality interpreted?
data$repeatability <- "" #levels: yes, no. Was repeatability measured?
data$repeatability_interpretation <- "" #levels: among-individual level, within- individual level, both, none, NA.
data$repeatability_interpretation_context <- "" #some context copied from the paper regarding the interpretation of repeatability; otherwise, NA    
data$repeatability_consist_predict <- "" #levels: individual consistency, individual predictability, both, none, NA.
data$repeatability_consist_predict_context <- "" #some context copied from the paper regarding the interpretation of repeatability specifically as individual predictability and/or consistency; otherwise, NA  
# data$repeatability_alternative <- "" #if repeatability was not estimated, what was estimated instead? (e.g. correlation?); otherwise, NA    
# data$individual_level_association <- "" #levels: yes, no. Did the paper study individual level association between behaviour and some other traits?
# data$individual_level_association_method <- "" #levels: univariate, bivariate, correlation, among-subject centring. If individual_level_association=yes, which method was used?
data$repetability_comparison <- "" #levels: yes (only applies within the study, i.e. if they measure repeatability in >1 group, and compare it), no, NA.
data$repetability_comparison_interpretation <- "" #levels: among-individual, within-individual, both, none, NA. How do the authors interpret the comparison between the repeatabilities of two or more groups?
data$repetability_comparison_interpretation_context <- "" #some context copied from the paper regarding the interpretation of comparing repeatabilities
data$unstandardize_variance <- "" #levels: yes, no, NA. Are the raw variance estimates available or only the variance standardized (i.e. repeatability)?
data$comments <- "" #comments  


##############################################################
# Creating output
##############################################################

# we create three databases, one per observer, but first, let's
# sort by title as a way of "randomizing"
data <- data[order(data$title),]

# we then subset only the references that passed the title-and-
# abstract screening
data.t.and.a.passed <- data[data$t.and.a_decision=="yes",]

# and then, we are only going to focus on papers published in 
# 2015 or later.
data.t.and.a.passed.2015on <- data.t.and.a.passed[data.t.and.a.passed$year>2014,]

# from this subset, we are only going to extract data from half
# of it, and that choice is going to be randome, using the
# function sample(). First, let's set.seed() to make this
# reproducible.
set.seed(135013)
data.t.and.a.passed.2015on.random <- data.t.and.a.passed.2015on[sample(1:nrow(data.t.and.a.passed.2015on),
                                                                       size = round(nrow(data.t.and.a.passed.2015on)/2,0)),]

# round((154*0.25)/3,0)
# round(154/3,0)

# subsets per observer ~ round(154/3,0) + round((154*0.25)/3,0) refs, with some overlapping
data.MM <- data.t.and.a.passed.2015on.random[c(1:64),]
data.MM$observer <- "MM"

data.AST <- data.t.and.a.passed.2015on.random[c(52:125),]
data.AST$observer <- "AST"

data.PN <- data.t.and.a.passed.2015on.random[c(1:13,113:154),]
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
