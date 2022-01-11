################################################################################
# Authors: 
# Alfredo Sanchez-Tojar (alfredo.tojar@gmail.com)
#   Affiliation: Dept. Evolutionary Biology, Bielefeld University, Germany
#   Profile: https://scholar.google.de/citations?user=Sh-Rjq8AAAAJ&hl=de

# Script first created on the 26th of May 2020


################################################################################
# Description of script and Instructions
################################################################################

# This script is to import the data set from a survey on animal personality
# terminology, clean it, and analyze it. 

################################################################################
# Packages needed
################################################################################

pacman::p_load(openxlsx,stringr,dplyr,tidyverse,gt,binom)

# Clear memory
rm(list=ls())


################################################################################
# Functions needed
################################################################################

# function obtained from: https://rstudio-pubs-static.s3.amazonaws.com/408658_512da947714740b99253228f084a08a9.html
# this function makes the first letter of a word capital to keep everything tidy and consistent
CapStr <- function(y) {
  c <- strsplit(y, " ")[[1]]
  paste(toupper(substring(c, 1,1)), substring(c, 2),
        sep="", collapse=" ")
}


################################################################################
# Import data
################################################################################

data <- read.xlsx("data/survey/understanding_personality_survey_results.xlsx",
                  colNames=T,sheet = 1)

################################################################################
# Preparing dataset
################################################################################

# transforming all characters to factors
data <- data %>%
  mutate_if(sapply(data, is.character), as.factor)

names(data)

##########################
# reducing dataset

data.red <- select (data,-c(Timestamp,Score,`PLEASE.CHECK:`))


##########################
# changing variable names #this stragegy of recoding variable names did not work due to the use of special characters in the orginal names
# data.red <- rename(data.red, 
#                    career.stage = `At.what.stage.of.your.research.career.are.you?`,
#                    personality.experience = `Have.you.ever.worked.or.published.in.questions.related.to."animal.personality"?`,
#                    country = `What.country.are.you.currently.working.in?.The.country.of.workplace.will.be.only.used.to.assess.the.spread.of.the.survey.`,
#                    personality.definition = `1..What.is.your.preferred.definition.of."animal.personality"?.Please,.select.the.option.that.applies.from.the.following.set.of.definitions.derived.from.the.literature..Otherwise,.please,.specify.yours.below.`,
#                    personality.interpretation = `2..What.does.the.term."animal.personality".represent.biologically?.If.none.of.the.descriptions.fits.yours,.please,.specify.yours.below.`,
#                    repeatability.interpretation = `3..What.does."repeatability".(i.e..the.proportion.of.phenotypic.variance.explained.by.individual.identity).commonly.represent.in.the.field.of.animal.personality?.If.none.of.the.descriptions.fits.yours,.please,.specify.yours.below.`,
#                    repeatability.consistency = `4..Does.repeatability.-.as.defined.in.question.3.".provide.an.estimate.of.individual.(behavioural).consistency.and/or.predictability?`,
#                    repeatability.comparison = `5..What.biological.interpretation.can.one.make.when.one.only.knows.that.a.group.of.individuals."A".expresses.higher.repeatability.than.a.group.of.individuals."B"?`,
#                    single.measurements = `6..Have.you.ever.used.a.single.measurement.of.behaviour(s).per.individual.when.studying.animal.personality?`,
#                    unpartitioned.data = `7..Have.you.ever.used.(unpartitioned).phenotypic.level.data.when.studying.animal.personality?`,
#                    personality.data = `8..Which.data.one.generally.needs.to.study.animal.personality?`,
#                    comments=`Please.let.us.know.if.you.have.any.other.comments:`)

names(data.red) <- c("career.stage","personality.experience","country",
                     "personality.definition","personality.interpretation",
                     "repeatability.interpretation","repeatability.consistency",
                     "repeatability.comparison","single.measurements",
                     "unpartitioned.data","personality.data","comments")

################################################################################
# career stage
################################################################################

table(data.red$career.stage)

# excluding career.stage identified as "I am not a researcher" from here on
data.red <- data.red[data.red$career.stage!="I am not a researcher",]


################################################################################
# personality experience
################################################################################
table(data.red$personality.experience) #think of excluding: "I am not sure" and "No"


################################################################################
# working country
################################################################################
table(data.red$country)

# standardizing by using only lower case
data.red$country <- tolower(data.red$country)

# removing final spaces
data.red$country <- str_trim(data.red$country)

# manual standardization of country names
data.red$country <- recode(data.red$country,
                           brasil = "brazil",
                           "czech republic" = "czechia",
                           deutschland = "germany",
                           netherlands = "the netherlands",
                           "united kingdom" = "UK",
                           "united states" = "USA",
                           "united states of america" = "USA",
                           usa = "USA",
                           uk = "UK",
                           us = "USA",
                           dublin = "ireland",
                           england = "UK",
                           .default = levels(data.red$country))

table(data.red$country) 
length(unique(data.red$country)) # 38 countries


################################################################################
# Figure S4
################################################################################

# sort of histogram showing countries of affiliations of participants in the survey

tiff("figures/FigureS4.tiff",
     height=18, width=36,
     units='cm', compression="lzw", res=600)

figureS4 <- 
  data.red %>%
  mutate(country = fct_infreq(country)) %>%
  group_by(country) %>% 
  summarise(n = n()) %>% 
  ggplot() + 
  geom_bar(aes(y = n, x = country), 
           stat="identity", position="dodge", colour = "white") +
  labs(y="Frequency") +
  scale_y_continuous(limits = c(0,75), breaks = seq(0,75,25),
                     expand = expand_scale(mult = c(0, 0.05))) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x=element_blank(),
        axis.title.y = element_text(size = 25,vjust = 9),
        axis.text.x = element_text(size = 15, color="black",angle = 90,hjust = 1,vjust=0.4),
        axis.text.y = element_text(size = 15),
        plot.margin = unit(c(0.5,1,0.75,1.25), "cm"))

figureS4

dev.off()

################################################################################
# generating a database with country data so that we can create an interactive map using eviatlas

# latitude and longitude obtained from: https://developers.google.com/public-data/docs/canonical/countries_csv
coordinates <- read.table("data/survey/latitude_and_longitude.csv",header=TRUE,sep=",")


# counting number of participants per country
country.counts <- as.data.frame(data.red %>% 
                                  group_by(country) %>% 
                                  summarise(n = n()))

# merging both databases and saving for eviatlas
country.counts.coordinates <- merge(country.counts,coordinates)

# changing variable names
country.counts.coordinates <- rename(country.counts.coordinates, 
                                     "participants" = n)

# making the first letter capital to make it look prettier
country.counts.coordinates$country <- sapply(as.character(country.counts.coordinates$country), CapStr)


# saving dataset for eviatlas
write.csv(country.counts.coordinates,
          "data/survey/country_coordinates.csv",row.names=FALSE)


# making the first letter capital to make it look prettier
data.red$country <- sapply(as.character(data.red$country), CapStr)


################################################################################
# personality definition
################################################################################
table(data.red$personality.definition)

# pre-defined answers
personality.definition.replies <- c("Between-individual differences in behavioural tendencies across contexts and within-individual consistency over time",
                                    "Consistent between-individual differences in behaviour across time and/or contexts",
                                    "Consistent between-individual differences in whole suites of correlated behaviours across time and/or contexts",
                                    "Variation among individuals in the intercept of their behavioural reaction norm",
                                    "Within-individual and between-individual consistency in behaviours across time and/or ecological contexts")


# creating a new variable so that additional replies are labelled as "others"
data.red$personality.definition.2 <- ifelse(data.red$personality.definition %in% personality.definition.replies,
                                            as.character(data.red$personality.definition),
                                            "other")

table(data.red$personality.definition.2)

# Consistent between-individual differences in behaviour across time and/or contexts percentage manually plus 95%CI
binom.confint(table(data.red$personality.definition.2)[2], 
              sum(table(data.red$personality.definition.2)), 
              method=c("agresti-coull"),type="central")

# Variation among individuals in the intercept of their behavioural reaction norm percentage manually plus 95%CI
binom.confint(table(data.red$personality.definition.2)[5], 
              sum(table(data.red$personality.definition.2)), 
              method=c("agresti-coull"),type="central")

# Within-individual and between-individual consistency in behaviours across time and/or ecological contexts percentage manually plus 95%CI
binom.confint(table(data.red$personality.definition.2)[6], 
              sum(table(data.red$personality.definition.2)), 
              method=c("agresti-coull"),type="central")


#table(data.red[!(data.red$personality.definition %in% personality.definition.replies),"personality.definition"])

# creating table 1 summarizing the definitions from the questionnaire
table1 <- data.red %>%
  group_by(personality.definition.2) %>%
  summarise(number=n()) %>% 
  mutate(percentage=round(number/sum(number)*100,1)) %>%
  arrange(desc(percentage)) %>%
  mutate(combined = paste0(number," (",percentage,"%)")) %>%
  select(-number,-percentage) %>%
  mutate(personality.definition.2 = fct_recode(personality.definition.2, "Others (see Table S1)" = "other")) %>%
  gt() %>%
  cols_label(personality.definition.2=md("**Animal personality definition**"),
             combined=md("**Number of times selected in the questionnaire (%)**")) %>%
  cols_align(align = "left", columns=vars(personality.definition.2)) %>%
  cols_align(align = "center", columns=vars(combined)) %>%
  tab_options(table.width=775)

table1

gtsave(table1,filename="table1.png", path="./tables/")


# creating table S1 containing all definitions contained in the others category
tableS1 <- data.red %>%
  filter(!(personality.definition %in% personality.definition.replies)) %>%
  select(personality.definition) %>%
  as.data.frame() %>%
  gt() %>%
  cols_label("personality.definition"=md("**Additional animal personality definitions provided by the participants**")) %>%
  cols_align(align = "left") %>%
  tab_options(table.width=775)

tableS1

gtsave(tableS1,filename="tableS1.png", path="./tables/")


################################################################################
# personality interpretation
################################################################################
table(data.red$personality.interpretation)

# pre-defined answers
personality.interpretation.replies <- c("a) Limited phenotypic plasticity in behavioural expression in a sample of individuals",
                                        "b) Individual differences in average behavioural expression in a sample of individuals",
                                        "c) Both a) and b) are correct",
                                        "d) I do not know the answer")

# after reviewing the "other" category, some do actually correspond to pre-defined categories
# therefore, we are adding those back to were they belong

data.red <- mutate(data.red, personality.interpretation = fct_recode(personality.interpretation,
                                                                     "c) Both a) and b) are correct" = "A and B combined",
                                                                     "d) I do not know the answer" = "a) is not correct because personality does not prevent plasticity; it only reduces I x E. b) is not very feasible as you need within-individual variance to know that individual differ in their average expression. So I do not really know what to answer here.",
                                                                     "d) I do not know the answer" = "I'm not sure what 'represent biologically' means. A mechanism? An evolutionary optimum? An evolutionary constraint? There are many hypotheses to explain personality."))

# creating a new variable so that additional replies are labelled as "others"
data.red$personality.interpretation.2 <- ifelse(data.red$personality.interpretation %in% personality.interpretation.replies,
                                                as.character(data.red$personality.interpretation),
                                                "other")

table(data.red$personality.interpretation.2)

# Individual differences in average behavioural expression in a sample of individuals percentage manually plus 95%CI
binom.confint(table(data.red$personality.interpretation.2)[2], 
              sum(table(data.red$personality.interpretation.2)), 
              method=c("agresti-coull"),type="central")

# c) Both a) and b) are correct percentage manually plus 95%CI
binom.confint(table(data.red$personality.interpretation.2)[3], 
              sum(table(data.red$personality.interpretation.2)), 
              method=c("agresti-coull"),type="central")

# among combined percentage manually plus 95%CI
binom.confint(sum(table(data.red$personality.interpretation.2)[2],table(data.red$personality.interpretation.2)[3]), 
              sum(table(data.red$personality.interpretation.2)), 
              method=c("agresti-coull"),type="central")

# within combined percentage manually plus 95%CI
binom.confint(sum(table(data.red$personality.interpretation.2)[1],table(data.red$personality.interpretation.2)[3]), 
              sum(table(data.red$personality.interpretation.2)), 
              method=c("agresti-coull"),type="central")

# Limited phenotypic plasticity in behavioural expression in a sample of individuals percentage manually plus 95%CI
binom.confint(table(data.red$personality.interpretation.2)[1], 
              sum(table(data.red$personality.interpretation.2)), 
              method=c("agresti-coull"),type="central")


# creating table 2 summarizing the interpreations from the questionnaire
table2 <- data.red %>%
  group_by(personality.interpretation.2) %>%
  summarise(number=n()) %>% 
  mutate(percentage=round(number/sum(number)*100,1)) %>%
  #arrange(desc(percentage)) %>%
  mutate(combined = paste0(number," (",percentage,"%)")) %>%
  select(-number,-percentage) %>%
  mutate(personality.interpretation.2 = fct_recode(personality.interpretation.2, "e) Others (see Table S2)" = "other")) %>%
  gt() %>%
  cols_label(personality.interpretation.2=md("**Biological interpretation of animal personality**"),
             combined=md("**Number of times selected in the questionnaire (%)**")) %>%
  cols_align(align = "left", columns=vars(personality.interpretation.2)) %>%
  cols_align(align = "center", columns=vars(combined)) %>%
  tab_options(table.width=775)

table2

gtsave(table2,filename="table2.png", path="./tables/")

#table(data.red[!(data.red$personality.interpretation %in% personality.interpretation.replies),"personality.interpretation"])

# creating table S2 containing all interpretations contained in the others category
tableS2 <- data.red %>%
  filter(!(personality.interpretation %in% personality.interpretation.replies)) %>%
  select(personality.interpretation) %>%
  as.data.frame() %>%
  gt() %>%
  cols_label("personality.interpretation"=md("**Additional biological interpretations of animal personality provided by the participants**")) %>%
  cols_align(align = "left") %>%
  tab_options(table.width=775)

tableS2

gtsave(tableS2,filename="tableS2.png", path="./tables/")


################################################################################
# repeatability interpretation
################################################################################
table(data.red$repeatability.interpretation)

# pre-defined answers
repeatability.interpretation.replies <- c("a) (Relative) amount of phenotypic plasticity in trait expression in a sample of individuals",
                                          "b) (Relative) amount of individual differences in average trait expression in a sample of individuals",
                                          "c) Both a) and b) are correct",
                                          "d) I do not know the answer")

# after reviewing the "other" category, some do actually correspond to pre-defined categories
# therefore, we are adding those back to were they belong
# for those that say that none of the asnwers are correct but do not provide an alternative we are marking them as "d) I do not know the answer"

data.red <- mutate(data.red, repeatability.interpretation = fct_recode(repeatability.interpretation,
                                                                       "b) (Relative) amount of individual differences in average trait expression in a sample of individuals" = "B, but at a specific value of the environment",
                                                                       "b) (Relative) amount of individual differences in average trait expression in a sample of individuals" = "Note that technically it's (b), but this means that regarding plasticity itself as a trait then (a) must also then apply.",
                                                                       "d) I do not know the answer" = "Neither a or b are correct",
                                                                       "d) I do not know the answer" = "None are correct. For the same reasons as quest 2."))


# creating a new variable so that additional replies are labelled as "others"
data.red$repeatability.interpretation.2 <- ifelse(data.red$repeatability.interpretation %in% repeatability.interpretation.replies,
                                                  as.character(data.red$repeatability.interpretation),
                                                  "other")

table(data.red$repeatability.interpretation.2)

# (Relative) amount of individual differences in average trait expression in a sample of individuals percentage manually plus 95%CI
binom.confint(table(data.red$repeatability.interpretation.2)[2], 
              sum(table(data.red$personality.interpretation.2)), 
              method=c("agresti-coull"),type="central")

# (Relative) amount of phenotypic plasticity in trait expression in a sample of individuals percentage manually plus 95%CI
binom.confint(table(data.red$repeatability.interpretation.2)[1], 
              sum(table(data.red$personality.interpretation.2)), 
              method=c("agresti-coull"),type="central")

# Both a) and b) are correct percentage manually plus 95%CI
binom.confint(table(data.red$repeatability.interpretation.2)[3], 
              sum(table(data.red$personality.interpretation.2)), 
              method=c("agresti-coull"),type="central")

# (Relative) amount of phenotypic plasticity in trait expression in a sample of individuals and Both a) and b) are correct percentage manually plus 95%CI
binom.confint(sum(table(data.red$repeatability.interpretation.2)[3],table(data.red$repeatability.interpretation.2)[1]), 
              sum(table(data.red$personality.interpretation.2)), 
              method=c("agresti-coull"),type="central")



# creating table 3 summarizing the interpreations from the questionnaire
table3 <- data.red %>%
  group_by(repeatability.interpretation.2) %>%
  summarise(number=n()) %>% 
  mutate(percentage=round(number/sum(number)*100,1)) %>%
  #arrange(desc(percentage)) %>%
  mutate(combined = paste0(number," (",percentage,"%)")) %>%
  select(-number,-percentage) %>%
  mutate(repeatability.interpretation.2 = fct_recode(repeatability.interpretation.2, "e) Others (see Table S3)" = "other")) %>%
  gt() %>%
  cols_label(repeatability.interpretation.2=md("**Biological interpretation of repeatability**"),
             combined=md("**Number of times selected in the questionnaire (%)**")) %>%
  cols_align(align = "left", columns=vars(repeatability.interpretation.2)) %>%
  cols_align(align = "center", columns=vars(combined)) %>%
  tab_options(table.width=775)

table3

gtsave(table3,filename="table3.png", path="./tables/")

# creating table S3 containing all interpretations contained in the others category
tableS3 <- data.red %>%
  filter(!(repeatability.interpretation %in% repeatability.interpretation.replies)) %>%
  select(repeatability.interpretation) %>%
  as.data.frame() %>%
  gt() %>%
  cols_label("repeatability.interpretation"=md("**Additional interpretations of repeatability provided by the participants**")) %>%
  cols_align(align = "left") %>%
  tab_options(table.width=950)

tableS3

gtsave(tableS3,filename="tableS3.png", path="./tables/")


################################################################################
# repeatability consistency
################################################################################
table(data.red$repeatability.consistency)

# repeatability.consistency yes percentage manually plus 95%CI
binom.confint(table(data.red$repeatability.consistency)[3], 
              sum(table(data.red$repeatability.consistency)), 
              method=c("agresti-coull"),type="central")

# repeatability.consistency no percentage manually plus 95%CI
binom.confint(table(data.red$repeatability.consistency)[2], 
              sum(table(data.red$repeatability.consistency)), 
              method=c("agresti-coull"),type="central")

# repeatability.consistency I dunno percentage manually plus 95%CI
binom.confint(table(data.red$repeatability.consistency)[1], 
              sum(table(data.red$repeatability.consistency)), 
              method=c("agresti-coull"),type="central")

################################################################################
# repeatability comparison
################################################################################
table(data.red$repeatability.comparison)

# # pre-defined answers
# repeatability.comparison.replies <- c("a) Group “A” expresses less plasticity in their trait expression than group “B”",
#                                       "b) Individuals differ more from each other in their average trait expression in group “A” than in group “B”",
#                                       "c) Both interpretations a) and b) can be made",
#                                       "d) Neither interpretations a) nor b) can be made",
#                                       "e) I do not know the answer")
# 
# 
# # creating a new variable so that additional replies are labelled as "others"
# data.red$repeatability.comparison.2 <- ifelse(data.red$repeatability.comparison %in% repeatability.comparison.replies,
#                                               as.character(data.red$repeatability.comparison),
#                                               "other")

data.red$repeatability.comparison.2 <- data.red$repeatability.comparison

table(data.red$repeatability.comparison.2)

# c) Both interpretations a) and b) can be made percentage manually plus 95%CI
binom.confint(table(data.red$repeatability.comparison.2)[3], 
              sum(table(data.red$repeatability.comparison.2)), 
              method=c("agresti-coull"),type="central")

# d) Neither interpretations a) nor b) can be made percentage manually plus 95%CI
binom.confint(table(data.red$repeatability.comparison.2)[4], 
              sum(table(data.red$repeatability.comparison.2)), 
              method=c("agresti-coull"),type="central")

# repeatability.consistency I dunno percentage manually plus 95%CI
binom.confint(table(data.red$repeatability.comparison.2)[5], 
              sum(table(data.red$repeatability.comparison.2)), 
              method=c("agresti-coull"),type="central")

# a) Group “A” expresses less plasticity in their trait expression than group “B” percentage manually plus 95%CI
binom.confint(table(data.red$repeatability.comparison.2)[1], 
              sum(table(data.red$repeatability.comparison.2)), 
              method=c("agresti-coull"),type="central")


# creating table 4 summarizing the interpreations from the questionnaire
table4 <- data.red %>%
  group_by(repeatability.comparison.2) %>%
  summarise(number=n()) %>% 
  mutate(percentage=round(number/sum(number)*100,1)) %>%
  #arrange(desc(percentage)) %>%
  mutate(combined = paste0(number," (",percentage,"%)")) %>%
  select(-number,-percentage) %>%
  gt() %>%
  cols_label(repeatability.comparison.2=md("**Biological interpretation of comparing repeatability estimates of two groups of animals**"),
             combined=md("**Number of times selected in the questionnaire (%)**")) %>%
  cols_align(align = "left", columns=vars(repeatability.comparison.2)) %>%
  cols_align(align = "center", columns=vars(combined)) %>%
  tab_options(table.width=775)

table4

gtsave(table4,filename="table4.png", path="./tables/")


################################################################################
# single measurements
################################################################################
table(data.red$single.measurements)

table(data.red[data.red$personality.experience %in% c("Yes, as a co-author","Yes, as a lead, corresponding or senior author"),
               "single.measurements"])

# used single measurements percentage manually plus 95%CI
binom.confint(table(data.red[data.red$personality.experience %in% c("Yes, as a co-author","Yes, as a lead, corresponding or senior author"),
                             "single.measurements"])[3], 
              sum(table(data.red[data.red$personality.experience %in% c("Yes, as a co-author","Yes, as a lead, corresponding or senior author"),
                                 "single.measurements"])[2],
                  table(data.red[data.red$personality.experience %in% c("Yes, as a co-author","Yes, as a lead, corresponding or senior author"),
                                 "single.measurements"])[3]), 
              method=c("agresti-coull"),type="central")


################################################################################
# unpartitioned data
################################################################################
table(data.red$unpartitioned.data)

table(data.red[data.red$personality.experience %in% c("Yes, as a co-author","Yes, as a lead, corresponding or senior author"),
               "unpartitioned.data"])


################################################################################
# personality data
################################################################################
table(data.red$personality.data)

# pre-defined answers
personality.data.replies <- c("a) A single behavioural measurement per individual",
                              "b) A single behavioural measurement per individual if the focal behaviour is repeatable",
                              "c) Repeated behavioural measurements per individual",
                              "d) Three previous options are correct",
                              "e) Both b) and c) are correct",
                              "f) I do not know")

# after reviewing the "other" category, some do actually correspond to pre-defined categories
# therefore, we are adding those back to were they belong

data.red <- mutate(data.red, personality.data = fct_recode(personality.data,
                                                           "a) A single behavioural measurement per individual" = "Opción 1",
                                                           "c) Repeated behavioural measurements per individual" = "Repeated behavioural measurements per individual"))



# # creating a new variable so that additional replies are labelled as "others"
# data.red$personality.data.2 <- ifelse(data.red$personality.data %in% personality.data.replies,
#                                       as.character(data.red$personality.data),
#                                       "other")
# 
# table(data.red$personality.data.2)

table(data.red$personality.data)

# repeatability.consistency no percentage manually plus 95%CI
binom.confint(sum(table(data.red$personality.data)[2],table(data.red$personality.data)[4],table(data.red$personality.data)[5]), 
              sum(table(data.red$personality.data)), 
              method=c("agresti-coull"),type="central")

# repeatability.consistency yes discussion percentage manually plus 95%CI
binom.confint(297,381, 
              method=c("agresti-coull"),type="central")

################################################################################
# personality data
################################################################################
table(data.red$comments)


################################################################################
# Further formatting
################################################################################

# converting all these variables to factors
variables.as.factors <- c("country","personality.definition.2","personality.interpretation.2",
                          "repeatability.interpretation.2","repeatability.comparison.2")

data.red[variables.as.factors] <- lapply(data.red[variables.as.factors], factor)

summary(data.red)


# saving dataset for creating figures in script 007_survey_vs_review.R
write.csv(data.red,
          "data/survey/understanding_personality_survey_results_reformatted.csv",row.names=FALSE)

# ################################################################################
# # Figures
# ################################################################################
# 
# # career stage
# career.stage.plot <- data.red %>% 
#   group_by(career.stage) %>% 
#   summarise(n = n()) %>% 
#   mutate(career.stage = fct_recode(career.stage, 
#                                    "ECR" = "Early-career researcher (i.e., have completed their doctorate degree within the past 5 years)",
#                                    "Non-researcher" = "I am not a researcher",
#                                    "Master" = "Master student (or earlier stage)",
#                                    "PhD" = "PhD researcher",
#                                    "Senior" = "Senior researcher (i.e., have completed their doctorate degree more than 5 years ago)")) %>%
#   mutate(career.stage = factor(career.stage, levels = c("Non-researcher",
#                                                         "Master",
#                                                         "PhD",
#                                                         "ECR",
#                                                         "Senior"))) %>% 
#   ggplot() + 
#   geom_bar(aes(y = n, x = career.stage), stat="identity",colour="black") +
#   labs(y="Number of participants") +
#   scale_y_continuous(breaks = seq(0,175,25),expand = expand_scale(mult = c(0, 0.05))) +
#   theme(panel.grid.major = element_blank(), 
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         axis.line = element_line(colour = "black"),
#         axis.title.x=element_blank())
# 
# 
# # experienced researching on animal personality
# personality.experience.plot <- data.red %>% 
#   group_by(personality.experience) %>% 
#   summarise(n = n()) %>% 
#   mutate(personality.experience = fct_recode(personality.experience, 
#                                              "Unsure" = "I am not sure",
#                                              "Yes (coauthor)" = "Yes, as a co-author",
#                                              "Yes" = "Yes, as a lead, corresponding or senior author")) %>%
#   mutate(personality.experience = factor(personality.experience, levels = c("No",
#                                                                             "Unsure",
#                                                                             "Yes (coauthor)",
#                                                                             "Yes"))) %>% 
#   ggplot() + 
#   geom_bar(aes(y = n, x = personality.experience), stat="identity",colour="black") +
#   labs(y="Number of participants") +
#   scale_y_continuous(breaks = seq(0,250,25),expand = expand_scale(mult = c(0, 0.05))) +
#   theme(panel.grid.major = element_blank(), 
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         axis.line = element_line(colour = "black"),
#         axis.title.x=element_blank())
# 
# 
# # personality definition
# personality.definition.2.plot <- data.red %>% 
#   group_by(personality.definition.2) %>% 
#   summarise(n = n()) %>% 
#   mutate(percentage = round((n/sum(n))*100,1)) %>%
#   mutate(personality.definition.2 = fct_recode(personality.definition.2, 
#                                                "A" = "Between-individual differences in behavioural tendencies across contexts and within-individual consistency over time",
#                                                "B" = "Consistent between-individual differences in behaviour across time and/or contexts",
#                                                "C" = "Consistent between-individual differences in whole suites of correlated behaviours across time and/or contexts",
#                                                "D" = "Variation among individuals in the intercept of their behavioural reaction norm",
#                                                "E" = "Within-individual and between-individual consistency in behaviours across time and/or ecological contexts")) %>%
#   mutate(personality.definition.2 = factor(personality.definition.2, levels = c("A",
#                                                                                 "B",
#                                                                                 "C",
#                                                                                 "D",
#                                                                                 "E",
#                                                                                 "other"))) %>% 
#   ggplot() + 
#   geom_bar(aes(y = percentage, x = personality.definition.2), stat="identity",colour="black") +
#   geom_text(aes(y = percentage, x = personality.definition.2,label=paste0("n = ",n)), position=position_dodge(width=0.9), vjust=-0.25) + 
#   labs(y="Percentage of participants") +
#   ggtitle("Personality definition") +
#   scale_y_continuous(limits = c(0,70), breaks = seq(0,70,10),expand = expand_scale(mult = c(0, 0.05))) +
#   theme(panel.grid.major = element_blank(), 
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         axis.line = element_line(colour = "black"),
#         axis.title.x=element_blank())
# 
# 
# 
# # personality interpretation
# personality.interpretation.2.plot <- data.red %>% 
#   group_by(personality.interpretation.2) %>% 
#   summarise(n = n()) %>% 
#   mutate(percentage = round((n/sum(n))*100,1)) %>%
#   mutate(personality.interpretation.2 = fct_recode(personality.interpretation.2, 
#                                                    "within level" = "a) Limited phenotypic plasticity in behavioural expression in a sample of individuals",
#                                                    "between level" = "b) Individual differences in average behavioural expression in a sample of individuals",
#                                                    "between and\nwithin level" = "c) Both a) and b) are correct",
#                                                    "NA" = "d) I do not know the answer")) %>%
#   mutate(personality.interpretation.2 = factor(personality.interpretation.2, levels = c("between level",
#                                                                                         "within level",
#                                                                                         "between and\nwithin level",
#                                                                                         "other",
#                                                                                         "NA"))) %>% 
#   ggplot() + 
#   geom_bar(aes(y = percentage, x = personality.interpretation.2), stat="identity",colour="black") +
#   geom_text(aes(y = percentage, x = personality.interpretation.2,label=paste0("n = ",n)), position=position_dodge(width=0.9), vjust=-0.25) + 
#   labs(y="Percentage of participants") +
#   ggtitle("Personality interpretation") +
#   scale_y_continuous(limits = c(0,70), breaks = seq(0,70,10),expand = expand_scale(mult = c(0, 0.05))) +
#   theme(panel.grid.major = element_blank(), 
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         axis.line = element_line(colour = "black"),
#         axis.title.x=element_blank())
# 
# 
# # repeatability interpretation
# repeatability.interpretation.2.plot <- data.red %>% 
#   group_by(repeatability.interpretation.2) %>% 
#   summarise(n = n()) %>% 
#   mutate(percentage = round((n/sum(n))*100,1)) %>%
#   mutate(repeatability.interpretation.2 = fct_recode(repeatability.interpretation.2, 
#                                                      "within level" = "a) (Relative) amount of phenotypic plasticity in trait expression in a sample of individuals",
#                                                      "between level" = "b) (Relative) amount of individual differences in average trait expression in a sample of individuals",
#                                                      "between and\nwithin level" = "c) Both a) and b) are correct",
#                                                      "NA" = "d) I do not know the answer")) %>%
#   mutate(repeatability.interpretation.2 = factor(repeatability.interpretation.2, levels = c("between level",
#                                                                                             "within level",
#                                                                                             "between and\nwithin level",
#                                                                                             "other",
#                                                                                             "NA"))) %>% 
#   ggplot() + 
#   geom_bar(aes(y = percentage, x = repeatability.interpretation.2), stat="identity",colour="black") +
#   geom_text(aes(y = percentage, x = repeatability.interpretation.2,label=paste0("n = ",n)), position=position_dodge(width=0.9), vjust=-0.25) + 
#   labs(y="Percentage of participants") +
#   ggtitle("Repeatability interpretation") +
#   scale_y_continuous(limits = c(0,70), breaks = seq(0,70,10),expand = expand_scale(mult = c(0, 0.05))) +
#   theme(panel.grid.major = element_blank(), 
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         axis.line = element_line(colour = "black"),
#         axis.title.x=element_blank())
# 
# 
# 
# # repeatability consistency
# repeatability.consistency.plot <- data.red %>% 
#   group_by(repeatability.consistency) %>% 
#   summarise(n = n()) %>% 
#   mutate(percentage = round((n/sum(n))*100,1)) %>%
#   mutate(repeatability.consistency = fct_recode(repeatability.consistency,
#                                                 "NA" = "I do not know")) %>%
#   mutate(repeatability.consistency = factor(repeatability.consistency, levels = c("No",
#                                                                                   "Yes",
#                                                                                   "NA"))) %>% 
#   ggplot() + 
#   geom_bar(aes(y = percentage, x = repeatability.consistency), stat="identity",colour="black") +
#   geom_text(aes(y = percentage, x = repeatability.consistency,label=paste0("n = ",n)), position=position_dodge(width=0.9), vjust=-0.25) + 
#   labs(y="Percentage of participants") +
#   ggtitle("Repeatability interpretated as individual (behavioural)\nconsistency and/or predictability") +
#   scale_y_continuous(limits = c(0,70), breaks = seq(0,70,10),expand = expand_scale(mult = c(0, 0.05))) +
#   theme(panel.grid.major = element_blank(), 
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         axis.line = element_line(colour = "black"),
#         axis.title.x=element_blank())
# 
# 
# 
# # repeatability comparison
# repeatability.comparison.2.plot <- data.red %>% 
#   group_by(repeatability.comparison.2) %>% 
#   summarise(n = n()) %>% 
#   mutate(percentage = round((n/sum(n))*100,1)) %>%
#   mutate(repeatability.comparison.2 = fct_recode(repeatability.comparison.2,
#                                                  "within level" = "a) Group “A” expresses less plasticity in their trait expression than group “B”",
#                                                  "between level" = "b) Individuals differ more from each other in their average trait expression in group “A” than in group “B”",
#                                                  "between and\nwithin level" = "c) Both interpretations a) and b) can be made",
#                                                  "other" = "d) Neither interpretations a) nor b) can be made",
#                                                  "NA" = "e) I do not know the answer")) %>%
#   mutate(repeatability.comparison.2 = factor(repeatability.comparison.2, levels = c("between level",
#                                                                                     "within level",
#                                                                                     "between and\nwithin level",
#                                                                                     "other",
#                                                                                     "NA"))) %>%
#   ggplot() + 
#   geom_bar(aes(y = percentage, x = repeatability.comparison.2), stat="identity",colour="black") +
#   geom_text(aes(y = percentage, x = repeatability.comparison.2,label=paste0("n = ",n)), position=position_dodge(width=0.9), vjust=-0.25) + 
#   labs(y="Percentage of participants") +
#   ggtitle("Group of individuals “A” expresses higher repeatability\nthan a group of individuals “B”") +
#   scale_y_continuous(limits = c(0,70), breaks = seq(0,70,10),expand = expand_scale(mult = c(0, 0.05))) +
#   theme(panel.grid.major = element_blank(), 
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         axis.line = element_line(colour = "black"),
#         axis.title.x=element_blank())
# 
# 
# 
# # # multipannel plot
# # library(ggpubr)
# # 
# # ggarrange(personality.experience.plot, personality.definition.2.plot, personality.interpretation.2.plot,
# #           repeatability.interpretation.2.plot, repeatability.consistency.plot, repeatability.comparison.2.plot,
# #           labels = c("a)","b)","c)",
# #                      "d)","e)","f)"),
# #           ncol = 2, nrow = 3)
# 
# 
# table(data.red$single.measurements)
# 
# 
# 
# # use of single measurements
# single.measurements.plot <- data.red %>% 
#   filter(personality.experience == "Yes, as a co-author" | personality.experience == "Yes, as a lead, corresponding or senior author") %>% #excluding participants that said "I am not sure" or "No" when asked about whether they published a study or more on animal personality
#   group_by(single.measurements) %>% 
#   summarise(n = n()) %>% 
#   mutate(percentage = round((n/sum(n))*100,1)) %>%
#   mutate(single.measurements = fct_recode(single.measurements,
#                                           "NA" = "N/A")) %>%
#   mutate(single.measurements = factor(single.measurements, levels = c("No",
#                                                                       "Yes",
#                                                                       "NA"))) %>%
#   ggplot() + 
#   geom_bar(aes(y = percentage, x = single.measurements), stat="identity",colour="black") +
#   geom_text(aes(y = percentage, x = single.measurements,label=paste0("n = ",n)), position=position_dodge(width=0.9), vjust=-0.25) + 
#   labs(y="Percentage of participants") +
#   ggtitle("Have you ever used a single measurement of behaviour(s)\nper individual when studying animal personality?") +
#   scale_y_continuous(limits = c(0,70), breaks = seq(0,70,10),expand = expand_scale(mult = c(0, 0.05))) +
#   theme(panel.grid.major = element_blank(), 
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         axis.line = element_line(colour = "black"),
#         axis.title.x=element_blank())
# 
# 
# # unpartitioned data
# unpartitioned.data.plot <- data.red %>% 
#   filter(personality.experience == "Yes, as a co-author" | personality.experience == "Yes, as a lead, corresponding or senior author") %>% #excluding participants that said "I am not sure" or "No" when asked about whether they published a study or more on animal personality
#   mutate(unpartitioned.data = fct_recode(unpartitioned.data,
#                                          "NA" = "N/A",
#                                          "NA" = "I do not know",
#                                          "Yes" = "Yes, but I was not aware of it when I used it")) %>%
#   group_by(unpartitioned.data) %>% 
#   summarise(n = n()) %>% 
#   mutate(percentage = round((n/sum(n))*100,1)) %>%
#   mutate(unpartitioned.data = factor(unpartitioned.data, levels = c("No",
#                                                                     "Yes",
#                                                                     "NA"))) %>%
#   ggplot() + 
#   geom_bar(aes(y = percentage, x = unpartitioned.data), stat="identity",colour="black") +
#   geom_text(aes(y = percentage, x = unpartitioned.data,label=paste0("n = ",n)), position=position_dodge(width=0.9), vjust=-0.25) + 
#   labs(y="Percentage of participants") +
#   ggtitle("Have you ever used (unpartitioned) phenotypic level data\nwhen studying animal personality?") +
#   scale_y_continuous(limits = c(0,70), breaks = seq(0,70,10),expand = expand_scale(mult = c(0, 0.05))) +
#   theme(panel.grid.major = element_blank(), 
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         axis.line = element_line(colour = "black"),
#         axis.title.x=element_blank())
# 
# 
# 
# # unpartitioned data
# personality.data.2.plot <- data.red %>% 
#   group_by(personality.data.2) %>% 
#   summarise(n = n()) %>% 
#   mutate(percentage = round((n/sum(n))*100,1)) %>%
#   mutate(personality.data.2 = fct_recode(personality.data.2,
#                                          "A" = "a) A single behavioural measurement per individual",
#                                          "B" = "b) A single behavioural measurement per individual if the focal behaviour is repeatable",
#                                          "C" = "c) Repeated behavioural measurements per individual",
#                                          "A,B,C" = "d) Three previous options are correct",
#                                          "B,C" = "e) Both b) and c) are correct",
#                                          "NA" = "f) I do not know")) %>%
#   ggplot() + 
#   geom_bar(aes(y = percentage, x = personality.data.2), stat="identity",colour="black") +
#   geom_text(aes(y = percentage, x = personality.data.2,label=paste0("n = ",n)), position=position_dodge(width=0.9), vjust=-0.25) + 
#   labs(y="Percentage of participants") +
#   ggtitle("Which data one generally needs to study animal personality?") +
#   scale_y_continuous(limits = c(0,70), breaks = seq(0,70,10),expand = expand_scale(mult = c(0, 0.05))) +
#   theme(panel.grid.major = element_blank(), 
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         axis.line = element_line(colour = "black"),
#         axis.title.x=element_blank())
