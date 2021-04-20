################################################################################
# Authors: 
# Alfredo Sanchez-Tojar (alfredo.tojar@gmail.com)
#   Affiliation: Dept. Evolutionary Biology, Bielefeld University, Germany
#   Profile: https://scholar.google.de/citations?user=Sh-Rjq8AAAAJ&hl=de

# Script first created on the 20th of April 2021


################################################################################
# Description of script and Instructions
################################################################################

# This script is to import the combined file that was reviewed and corrected
# outside R and that will be the final file to be used for the analyses of the 
# literature review. Importantly, this version of the file interprets 
# "consitent individual differences" (and synonyms) as referring to only the 
# among level, rather than both within- and among-level, as done for the results
# presented in the main text. The results of this script will be shown in 
# Appendix S3.


################################################################################
# Packages needed
################################################################################

pacman::p_load(openxlsx,stringr,dplyr,tidyverse,gt)

# Clear memory
rm(list=ls())


################################################################################
# Functions needed
################################################################################

# none

################################################################################
# Import data
################################################################################

# combined and cleaned literature review data with consistent individual differences interpreted as among only
combined.data <- read.xlsx("data/ten_journals/combined/ten_journals_fulltext_screening_and_data_extraction_combined_conflict_resolving_reformatted_SA.csv.xlsx",
                           colNames=T,sheet = 1)

