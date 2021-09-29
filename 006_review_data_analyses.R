################################################################################
# Authors: 
# Alfredo Sanchez-Tojar (alfredo.tojar@gmail.com)
#   Affiliation: Dept. Evolutionary Biology, Bielefeld University, Germany
#   Profile: https://scholar.google.de/citations?user=Sh-Rjq8AAAAJ&hl=de

# Script first created on the 28th of October 2020


################################################################################
# Description of script and Instructions
################################################################################

# This script is to import the combined file that was reviewed and corrected
# outside R and that will be the final file to be used for the analyses of the 
# literature review.


################################################################################
# Packages needed
################################################################################

pacman::p_load(openxlsx,stringr,dplyr,tidyverse,binom)

# Clear memory
rm(list=ls())


################################################################################
# Functions needed
################################################################################

# none


################################################################################
# Import data
################################################################################

# combined and cleaned literature review data
combined.data <- read.xlsx("data/ten_journals/combined/ten_journals_fulltext_screening_and_data_extraction_combined_conflict_resolving.xlsx",
                           colNames=T,sheet = 1)


################################################################################
# Preparing dataset
################################################################################

# first we need to remove duplicates, which consists of those papers that were 
# screened twice and any conflicts discussed and agreed on. For any changes
# made to this dataset, please see the columns conflict_resolution_notes. The 
# excel file highlights those cells that had some changes to make them easier
# to spot. Be aware that contexts can differ between observers for the same
# paper, which is not important for the analyses, but will be kept as such in
# the non-duplicated dataset for the sake of transparency and reproducibility.

# for that, lets remove columns that are not of interest for the analyses, this
# will make everything else easier

combined.data.reduced <- as.data.frame(combined.data %>% 
                                         select(-title,-volume,-issue,-pages,
                                                -authors,-abstract,-t.and.a_exclusion_reason,
                                                -repeatability_interpretation_context,
                                                -repeatability_consist_predict_context,
                                                -repetability_comparison_interpretation_context,
                                                -unstandardize_variance,-comments,-observer,
                                                -conflict_resolution_notes,-times.extracted))

# formatting variables
combined.data.reduced$studyID <- as.factor(combined.data.reduced$studyID)
combined.data.reduced$journal <- as.factor(combined.data.reduced$journal)
combined.data.reduced$t.and.a_decision <- as.factor(combined.data.reduced$t.and.a_decision)
combined.data.reduced$fulltext_decision <- as.factor(combined.data.reduced$fulltext_decision)
combined.data.reduced$fulltext_exclusion_reason <- as.factor(combined.data.reduced$fulltext_exclusion_reason)
combined.data.reduced$repeatability <- as.factor(combined.data.reduced$repeatability)
combined.data.reduced$repeatability_interpretation <- as.factor(combined.data.reduced$repeatability_interpretation)
combined.data.reduced$repeatability_consist_predict <- as.factor(combined.data.reduced$repeatability_consist_predict)
combined.data.reduced$repetability_comparison <- as.factor(combined.data.reduced$repetability_comparison)
combined.data.reduced$repetability_comparison_interpretation <- as.factor(combined.data.reduced$repetability_comparison_interpretation)
combined.data.reduced$unstandardize_variance.2 <- as.factor(combined.data.reduced$unstandardize_variance.2)

summary(combined.data.reduced)

# deleting duplicated rows, which in principle should (and does) work to randomly
# delete those that were extracted twice for quality control
combined.data.reduced.dedup <- unique(combined.data.reduced)

summary(combined.data.reduced.dedup)

# excluding references that did not pass the fulltext screening or that did not
# estimate behavioural repeatability
final.database <- combined.data.reduced.dedup %>% filter(fulltext_decision=="yes", repeatability=="yes")


# saving dataset for creating figures in script 007_survey_vs_review.R
write.csv(final.database,
          "data/ten_journals/combined/ten_journals_fulltext_screening_and_data_extraction_combined_conflict_resolving_reformatted.csv",row.names=FALSE)


################################################################################
# Some summary statistics
################################################################################

# year distribution
table(final.database$year)


# journal distribution
table(final.database$journal)


# repeatability interpretation
table(final.database$repeatability_interpretation)

# both percentage manually plus 95%CI
binom.confint(table(final.database$repeatability_interpretation)[2], 
              sum(table(final.database$repeatability_interpretation)[1],
                  table(final.database$repeatability_interpretation)[2],
                  table(final.database$repeatability_interpretation)[5]), 
              method=c("agresti-coull"),type="central")

# within percentage manually plus 95%CI
binom.confint(table(final.database$repeatability_interpretation)[5], 
              sum(table(final.database$repeatability_interpretation)[1],
                  table(final.database$repeatability_interpretation)[2],
                  table(final.database$repeatability_interpretation)[5]), 
              method=c("agresti-coull"),type="central")

# among percentage manually plus 95%CI
binom.confint(table(final.database$repeatability_interpretation)[1], 
              sum(table(final.database$repeatability_interpretation)[1],
                  table(final.database$repeatability_interpretation)[2],
                  table(final.database$repeatability_interpretation)[5]), 
              method=c("agresti-coull"),type="central")


# is repeatability interpreted as individual predictability and/or consistency?
table(final.database$repeatability_consist_predict) 

# individual predictability and/or consistency percentage manually plus 95%CI
binom.confint(table(final.database$repeatability_consist_predict)[3], 
              sum(table(final.database$repeatability_consist_predict)[1],
                  table(final.database$repeatability_consist_predict)[3]), 
              method=c("agresti-coull"),type="central")

# repeatability of two or more groups compared
table(final.database$repetability_comparison) 

# when repeatability of two or more groups compared, what is the interpretation?
table(final.database$repetability_comparison_interpretation)

# do the authors report unstandardized variance components in addition to the repeatability values?
table(final.database$unstandardize_variance.2)

# how many of those comparing repeatability values provide unstandardized variance components?
table(final.database[final.database$repetability_comparison_interpretation %in% c("among","both","within"),"unstandardize_variance.2"])
