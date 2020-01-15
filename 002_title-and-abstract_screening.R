
################################################################################
# Authors: 
# Alfredo Sanchez-Tojar (alfredo.tojar@gmail.com)
#   Affiliation: Dept. Evolutionary Biology, Bielefeld University, Germany
#   Profile: https://scholar.google.de/citations?user=Sh-Rjq8AAAAJ&hl=de

# Script first created on the 6th of June 2019


################################################################################
# Description of script and Instructions
################################################################################

# This script is to import the reference data from a systematic search aimed at
# finding animal personality studies published in the 10 most representative
# journals in the field

################################################################################
# Packages needed
################################################################################

pacman::p_load(revtools,stringr)

# Clear memory
rm(list=ls())


################################################################################
# Functions needed
################################################################################

# none


################################################################################
# Import data
################################################################################

# importing .bib file
db.refs.1 <- read_bibliography("literature_search/ten_journals/animal_personality_1_subset.bib")
db.refs.2 <- read_bibliography("literature_search/ten_journals/animal_personality_2_subset.bib")


# reducing fields to the fields required by rayyan
reducing.fields <- c("label","title","author","journal","issn","volume",
                     "number","pages","year","publisher","doi","abstract")


db.refs.1.red <- db.refs.1[,reducing.fields]
db.refs.2.red <- db.refs.2[,reducing.fields]


# putting all references together

db.refs.full <- rbind(db.refs.1.red,db.refs.2.red)


##############################################################
# Formatting data for RAYYAN QCRI
##############################################################

# choose only the fields needed for creating a .csv file importable by: https://rayyan.qcri.org

# example of a valid .csv file
rayyan.example <- read.table("literature_search/rayyan_csv_example.csv",header=TRUE,sep=",")


# standardizing fields according to rayyan.example despite that some fields are missing from the wos output

# what's different between the two?
setdiff(names(rayyan.example),names(db.refs.full))
setdiff(names(db.refs.full),names(rayyan.example))


# rename columns in screening.ref.data so that they are as expected by rayyan
names(rayyan.example)
names(db.refs.full)

db.refs.rayyan <- plyr::rename(db.refs.full, 
                               c("label"="key", 
                                 "author"="authors", 
                                 "doi"="url",
                                 "number"="issue"))


# what's different now?
setdiff(names(rayyan.example),names(db.refs.rayyan))
setdiff(names(db.refs.rayyan),names(rayyan.example))


# reorder
db.refs.rayyan <- db.refs.rayyan[,names(rayyan.example)]


# finding authors with missing initial(s) as that causes an error when importing into rayyan
table(grepl(",  ",db.refs.rayyan$authors,fixed=T))

for(i in 1:nrow(db.refs.rayyan)){
  
  if(grepl(",  ",db.refs.rayyan$authors[i],fixed=T)){
    
    print(i)
  }
  
}


##############################################################
# Creating output
##############################################################

write.csv(db.refs.rayyan,
          "literature_search/ten_journals/animal_personality_subset_rayyan.csv",row.names=FALSE)

#remember to manually remove the quotes for the column names only in the .csv file


##############################################################
# Generating list of corresponding authors' emails
##############################################################

# importing .bib file
db.all.refs.1 <- read_bibliography("literature_search/all_journals/animal_personality_1.bib")
db.all.refs.2 <- read_bibliography("literature_search/all_journals/animal_personality_2.bib")
db.all.refs.3 <- read_bibliography("literature_search/all_journals/animal_personality_3.bib")

# creating vector with list of unique components
cor.aut <- unique(unlist(c(db.all.refs.1$author.email,db.all.refs.2$author.email,db.all.refs.3$author.email)))

# split multiple corresponding authors to create a full list of corresponding authors, and make it unique
cor.aut.1 <- unique(unlist(strsplit(cor.aut, " ")))

# all to lower to be able to find unique emails
cor.aut.2 <- unique(str_to_lower(cor.aut.1))

# remove NA
cor.aut.3 <- cor.aut.2[!is.na(cor.aut.2)]

# remove // from some emails
cor.aut.4 <- gsub(cor.aut.3, pattern="\\_", replacement="_", fixed=TRUE)

# sort
cor.aut.5 <- sort(cor.aut.4)

# printing each list with no more than 500 emails as that is the maximum allowed by gmail
paste(as.character(cor.aut.5[1:500]), collapse=",")
paste(as.character(cor.aut.5[501:length(cor.aut.5)]), collapse=",")

# R session
sink("literature_search/ten_journals/screening_process_Rpackages_session.txt")
sessionInfo()
sink()
