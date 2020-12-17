################################################################################
# Authors: 
# Alfredo Sanchez-Tojar (alfredo.tojar@gmail.com)
#   Affiliation: Dept. Evolutionary Biology, Bielefeld University, Germany
#   Profile: https://scholar.google.de/citations?user=Sh-Rjq8AAAAJ&hl=de

# Script first created on the 10th of December 2020


################################################################################
# Description of script and Instructions
################################################################################

# This script is to import the data set from a survey on animal personality
# terminology, clean it, and analyze it but only focused on responses from 
# animal personality researchers

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

data.red <- read.csv("data/survey/understanding_personality_survey_results_reformatted.csv",header=T,sep=",")

################################################################################
# Preparing dataset
################################################################################

# excluding non-animal personality researchers

data.red <- data.red[data.red$personality.experience %in% 
                       c("Yes, as a co-author","Yes, as a lead, corresponding or senior author"),]

################################################################################
# working country
################################################################################

table(data.red$country) 
length(unique(data.red$country)) # 38 countries

################################################################################
# Figure S5
################################################################################

# sort of histogram showing countries of affiliations of participants in the survey

tiff("figures/FigureS5.tiff",
     height=18, width=36,
     units='cm', compression="lzw", res=600)

figureS5 <- 
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

figureS5

dev.off()


################################################################################
# personality definition
################################################################################

# creating table S5 summarizing the definitions from the questionnaire
tableS5 <- data.red %>%
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

tableS5

gtsave(tableS5,filename="tableS5.png", path="./tables/")


################################################################################
# personality interpretation
################################################################################

# creating table S6 summarizing the interpreations from the questionnaire
tableS6 <- data.red %>%
  group_by(personality.interpretation.2) %>%
  summarise(number=n()) %>% 
  mutate(percentage=round(number/sum(number)*100,1)) %>%
  arrange(desc(percentage)) %>%
  mutate(combined = paste0(number," (",percentage,"%)")) %>%
  select(-number,-percentage) %>%
  mutate(personality.interpretation.2 = fct_recode(personality.interpretation.2, "e) Others (see Table S2)" = "other")) %>%
  gt() %>%
  cols_label(personality.interpretation.2=md("**Biological interpretation of animal personality**"),
             combined=md("**Number of times selected in the questionnaire (%)**")) %>%
  cols_align(align = "left", columns=vars(personality.interpretation.2)) %>%
  cols_align(align = "center", columns=vars(combined)) %>%
  tab_options(table.width=775)

tableS6

gtsave(tableS6,filename="tableS6.png", path="./tables/")


################################################################################
# repeatability interpretation
################################################################################

# creating table S7 summarizing the interpreations from the questionnaire
tableS7 <- data.red %>%
  group_by(repeatability.interpretation.2) %>%
  summarise(number=n()) %>% 
  mutate(percentage=round(number/sum(number)*100,1)) %>%
  arrange(desc(percentage)) %>%
  mutate(combined = paste0(number," (",percentage,"%)")) %>%
  select(-number,-percentage) %>%
  mutate(repeatability.interpretation.2 = fct_recode(repeatability.interpretation.2, "e) Others (see Table S3)" = "other")) %>%
  gt() %>%
  cols_label(repeatability.interpretation.2=md("**Biological interpretation of repeatability**"),
             combined=md("**Number of times selected in the questionnaire (%)**")) %>%
  cols_align(align = "left", columns=vars(repeatability.interpretation.2)) %>%
  cols_align(align = "center", columns=vars(combined)) %>%
  tab_options(table.width=775)

tableS7

gtsave(tableS7,filename="tableS7.png", path="./tables/")


################################################################################
# repeatability consistency
################################################################################
table(data.red$repeatability.consistency)


################################################################################
# repeatability comparison
################################################################################

# creating table 4 summarizing the interpreations from the questionnaire
tableS8 <- data.red %>%
  group_by(repeatability.comparison.2) %>%
  summarise(number=n()) %>% 
  mutate(percentage=round(number/sum(number)*100,1)) %>%
  arrange(desc(percentage)) %>%
  mutate(combined = paste0(number," (",percentage,"%)")) %>%
  select(-number,-percentage) %>%
  gt() %>%
  cols_label(repeatability.comparison.2=md("**Biological interpretation of comparing repeatability estimates of two groups of animals**"),
             combined=md("**Number of times selected in the questionnaire (%)**")) %>%
  cols_align(align = "left", columns=vars(repeatability.comparison.2)) %>%
  cols_align(align = "center", columns=vars(combined)) %>%
  tab_options(table.width=775)

tableS8

gtsave(tableS8,filename="tableS8.png", path="./tables/")


################################################################################
# single measurements
################################################################################
table(data.red$single.measurements)


################################################################################
# unpartitioned data
################################################################################

table(data.red$unpartitioned.data)


################################################################################
# personality data
################################################################################

table(data.red$personality.data)


################################################################################
# comments
################################################################################

# only from self-reported researchers in animal personality

# table(data.red$comments)
# 
# tableS9 <- data.red[!(is.na(data.red$comments)),"comments"] %>%
#   as.data.frame() %>%
#   gt() %>%
#   cols_label(.=md("**Comments from participants**")) %>%
#   cols_align(align = "left") %>%
#   tab_options(table.width=950)
# 
# tableS9
# 
# gtsave(tableS9,filename="tableS9.png", path="./tables/")

# for all self-reported researchers
data.red <- read.csv("data/survey/understanding_personality_survey_results_reformatted.csv",header=T,sep=",")

table(data.red$comments)

tableS9 <- data.red[!(is.na(data.red$comments)),"comments"] %>%
  as.data.frame() %>%
  gt() %>%
  cols_label(.=md("**Comments from participants**")) %>%
  cols_align(align = "left") %>%
  tab_options(table.width=950)

tableS9

gtsave(tableS9,filename="tableS9.png", path="./tables/")
