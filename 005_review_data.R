################################################################################
# Authors: 
# Alfredo Sanchez-Tojar (alfredo.tojar@gmail.com)
#   Affiliation: Dept. Evolutionary Biology, Bielefeld University, Germany
#   Profile: https://scholar.google.de/citations?user=Sh-Rjq8AAAAJ&hl=de

# Script first created on the 7th of July 2020


################################################################################
# Description of script and Instructions
################################################################################

# This script is to import each of the observer's dataset with the extracted
# data, put them together, and explore disagreement between observers.


################################################################################
# Packages needed
################################################################################

pacman::p_load(openxlsx,stringr,dplyr,tidyverse)

# Clear memory
rm(list=ls())


################################################################################
# Functions needed
################################################################################

# none


################################################################################
# Import data
################################################################################

# data extraction from Alfredo Sanchez-Tojar
AST <- read.xlsx("data/ten_journals/per_observer/ten_journals_fulltext_screening_and_data_extraction_AST.xlsx",
                 colNames=T,sheet = 1)

# data extraction from Maria Moiron
MM <- read.xlsx("data/ten_journals/per_observer/ten_journals_fulltext_screening_and_data_extraction_MM.xlsx",
                colNames=T,sheet = 1)

# data extraction from Petri Niemela
PN <- read.xlsx("data/ten_journals/per_observer/ten_journals_fulltext_screening_and_data_extraction_PN.xlsx",
                colNames=T,sheet = 1)


################################################################################
# Preparing dataset
################################################################################

# removing an extra column in PN with a couple of non-important comments
PN <- as.data.frame(PN %>% select(-X34))

# putting altogether
full <- rbind(AST,MM,PN)

# removing variables that we do not need or are non-informative
full <- select(full,-c(key,month,day,issn,language,publisher,location,notes))


head(full)
summary(full)

# formatting variables
full$studyID <- as.factor(full$studyID)
full$journal <- as.factor(full$journal)
full$t.and.a_decision <- as.factor(full$t.and.a_decision)
full$t.and.a_exclusion_reason <- as.factor(full$t.and.a_exclusion_reason)
full$fulltext_exclusion_reason <- as.factor(full$fulltext_exclusion_reason)
full$observer <- as.factor(full$observer)

# formatting variable levels and making them factors
full$fulltext_decision <- as.factor(tolower(full$fulltext_decision))
full$repeatability <- as.factor(tolower(full$repeatability))
full$repeatability_interpretation <- as.factor(tolower(full$repeatability_interpretation))
full$repeatability_consist_predict <- as.factor(tolower(full$repeatability_consist_predict))
full$repetability_comparison <- as.factor(tolower(full$repetability_comparison))
full$repetability_comparison_interpretation <- as.factor(tolower(full$repetability_comparison_interpretation))
full$unstandardize_variance <- as.factor(tolower(full$unstandardize_variance))

summary(full)


##############################################
# repeatability_interpretation

# "no" actually means "none" in this variable: 
# full[full$repeatability_interpretation=="no" & !(is.na(full$repeatability_interpretation)),]
table(full$repeatability_interpretation)
full$repeatability_interpretation  <- recode(full$repeatability_interpretation ,
                                             "among-individual" = "among",
                                             "among_individual" = "among",
                                             "within-individual" = "within",
                                             "within_individual level" = "within",
                                             "no" = "none",
                                             .default = levels(full$repeatability_interpretation ))

full$repeatability_interpretation <- factor(full$repeatability_interpretation)

table(full$repeatability_interpretation)


##############################################
# repeatability_consist_predict

table(full$repeatability_consist_predict)
full$repeatability_consist_predict  <- recode(full$repeatability_consist_predict ,
                                              "individual consistency" = "yes",
                                              .default = levels(full$repeatability_consist_predict ))

full$repeatability_consist_predict <- factor(full$repeatability_consist_predict)

table(full$repeatability_consist_predict)


##############################################
# repeatability_consist_predict

table(full$repetability_comparison_interpretation)
full$repetability_comparison_interpretation  <- recode(full$repetability_comparison_interpretation ,
                                                       "among-individual" = "among",
                                                       "within-individual" = "within",
                                                       "within_individual level" = "within",
                                                       "both " = "both",
                                                       "both (backet up nicely with actual variances)" = "both",
                                                       .default = levels(full$repetability_comparison_interpretation))

full$repetability_comparison_interpretation <- factor(full$repetability_comparison_interpretation)

table(full$repetability_comparison_interpretation)

# APR155: context placed on the wrong variable. Moving it to the right place
full[full$studyID=="APR155" & full$observer=="MM","repetability_comparison_interpretation_context"] <- full[full$studyID=="APR155" & full$observer=="MM","repetability_comparison_interpretation"]
full[full$studyID=="APR155" & full$observer=="MM","repetability_comparison_interpretation"] <- NA

full$repetability_comparison_interpretation <- factor(full$repetability_comparison_interpretation)

table(full$repetability_comparison_interpretation)


##############################################
# unstandardize_variance

table(full$unstandardize_variance)

# creating a new variable after recoding
full$unstandardize_variance.2 <- recode(full$unstandardize_variance ,
                                        "no access to supl" = "no",
                                        "yes (but has to be calculated from the sum of squares manually)" = "partially",
                                        "yes (but not id level estimates, only colony level)" = "partially",
                                        "yes (for one trait)" = "partially",
                                        "yes (in a figure)" = "yes",
                                        .default = levels(full$unstandardize_variance))

table(full$unstandardize_variance.2)


summary(full)


################################################################################
# checking data expectations to find error/missing data
################################################################################

##############################################
# fulltext_decision

# making sure that if fulltext_decision=="no" all the data variables are "NA"
full[full$fulltext_decision=="no" & !(is.na(full$fulltext_decision)),]


##############################################
# repeatability

# making sure that if repeatability=="no" all the data variables are "NA"
full[full$repeatability=="no" & !(is.na(full$repeatability)),c("studyID","repeatability",
                                                               "repeatability_interpretation",
                                                               "repeatability_consist_predict",
                                                               "repetability_comparison",
                                                               "repetability_comparison_interpretation",
                                                               "unstandardize_variance",
                                                               "unstandardize_variance.2",
                                                               "observer")]

# changing "unstandardize_variance" and "unstandardize_variance.2" to NA if repeatability == "no" (only PN filled this variable in when repeatability == "no")
full[full$repeatability=="no" & !(is.na(full$repeatability)),c("unstandardize_variance",
                                                               "unstandardize_variance.2")] <- NA



# making sure that if repeatability=="yes" there is no missing data
summary(full[full$repeatability=="yes" & !(is.na(full$repeatability)),c("studyID","repeatability",
                                                                        "repeatability_interpretation",
                                                                        "repeatability_consist_predict",
                                                                        "repetability_comparison",
                                                                        "unstandardize_variance.2",
                                                                        "observer")])

# needs double-checking: none!
full[full$repeatability=="yes" & !(is.na(full$repeatability)) & is.na(full$repeatability_consist_predict),
     c("studyID","repeatability",
       "repeatability_interpretation",
       "repeatability_consist_predict",
       "repetability_comparison",
       "unstandardize_variance.2",
       "observer")]


##############################################
# repetability_comparison

# making sure that if repetability_comparison=="no" repetability_comparison_interpretation is "NA"
full[full$repetability_comparison=="no" & !(is.na(full$repetability_comparison)),c("studyID",
                                                                                   "repetability_comparison",
                                                                                   "repetability_comparison_interpretation",
                                                                                   "observer")]


################################################################################
# sorting database to check observer agreement
################################################################################

counts <- as.data.frame(table(full$studyID))
names(counts) <- c("studyID","times.extracted")


# adding the counts to the database
full.counts <- merge(full,counts,by="studyID",all.x=T) 


# sorting by times.extracted to make it easy
full.counts <- arrange(full.counts,-times.extracted,studyID,observer)


############################################################
# exporting clean dataset
############################################################

# exporting data
write.csv(full.counts,"data/ten_journals/combined/ten_journals_fulltext_screening_and_data_extraction_combined.csv",row.names=FALSE)