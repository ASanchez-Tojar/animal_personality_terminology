################################################################################
# Authors: 
# Alfredo Sanchez-Tojar (alfredo.tojar@gmail.com)
#   Affiliation: Dept. Evolutionary Biology, Bielefeld University, Germany
#   Profile: https://scholar.google.de/citations?user=Sh-Rjq8AAAAJ&hl=de

# Script first created on the 29th of October 2020


################################################################################
# Description of script and Instructions
################################################################################

# This script is to generate figures that compare the results of the literature
# review and those of the questionnaire for what repeatability represents and
# should be interpreted as.


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

# survey data after reformatting in script 004_survey.R
data.survey <- read.table("data/survey/understanding_personality_survey_results_reformatted.csv",
                          header=TRUE,sep=",")

# survey data after reformatting in script 006_review_data_analyses.R
data.review <- read.table("data/ten_journals/combined/ten_journals_fulltext_screening_and_data_extraction_combined_conflict_resolving_reformatted.csv",
                          header=TRUE,sep=",")


################################################################################
# Figure 2
################################################################################

################################################################################
# repeatability interpretation
################################################################################

# reformatting the variable of interest and subsetting the dataset to select only
# those for which the level is known
repeatability.1.survey <- as.data.frame(data.survey %>% 
                                          mutate(repeatability_interpretation = fct_recode(repeatability.interpretation.2, 
                                                                                           "\nWithin\n" = "a) (Relative) amount of phenotypic plasticity in trait expression in a sample of individuals",
                                                                                           "\nAmong\n" = "b) (Relative) amount of individual differences in average trait expression in a sample of individuals",
                                                                                           "Among\nand\nWithin" = "c) Both a) and b) are correct")) %>%
                                          mutate(source="questionnaire") %>%
                                          select(source,repeatability_interpretation) %>%
                                          filter(repeatability_interpretation %in% c("\nWithin\n","\nAmong\n","Among\nand\nWithin"))
)

repeatability.1.survey$repeatability_interpretation <- factor(repeatability.1.survey$repeatability_interpretation)

summary(repeatability.1.survey)


# reformatting the variable of interest and subsetting the dataset to select only
# those for which the level is known
repeatability.1.review <- as.data.frame(data.review %>% 
                                          mutate(repeatability_interpretation = fct_recode(repeatability_interpretation,
                                                                                           "\nWithin\n" = "within",
                                                                                           "\nAmong\n" = "among",
                                                                                           "Among\nand\nWithin" = "both")) %>%
                                          mutate(source="review") %>%
                                          select(source,repeatability_interpretation) %>%
                                          filter(repeatability_interpretation %in% c("\nWithin\n","\nAmong\n","Among\nand\nWithin"))
)

repeatability.1.review$repeatability_interpretation <- factor(repeatability.1.review$repeatability_interpretation)

summary(repeatability.1.review)

# binding both databases
repeatability.1 <- rbind(repeatability.1.review,repeatability.1.survey)
repeatability.1$source <- factor(repeatability.1$source)

summary(repeatability.1)


repeatability.1 <- repeatability.1 %>%
  group_by(source,repeatability_interpretation) %>% 
  summarise(n = n()) %>% 
  mutate(total.n = sum(n)) %>%
  mutate(percentage = round(binom.confint(n, total.n, method=c("agresti-coull"),type="central")$mean*100,1)) %>% #going for agresti-coull method, see https://casual-inference.com/post/binom-confint/
  mutate(lower = round(binom.confint(n, total.n, method=c("agresti-coull"),type="central")$lower*100,1)) %>%
  mutate(upper = round(binom.confint(n, total.n, method=c("agresti-coull"),type="central")$upper*100,1)) %>%
  ggplot(aes(y=percentage, x=repeatability_interpretation,ymin=lower, ymax=upper,fill = source)) + 
  geom_bar(position = position_dodge(), stat="identity") +
  geom_errorbar(position=position_dodge(.9), width = .2) +
  geom_text(aes(y = percentage, x = repeatability_interpretation, 
                group = source,label=paste0("n=",n)), position=position_dodge(width=0.9), vjust=-3.5, size=6, color="grey35") + 
  labs(y="% participants/articles") +
  #ggtitle("Repeatability interpretation") +
  scale_y_continuous(limits = c(0,100), breaks = seq(0,100,20),
                     expand = expand_scale(mult = c(0, 0.05))) +
  #scale_fill_manual(values = c("questionnaire" = "#f1a340", "review" = "#998ec3")) +
  #scale_fill_manual(values = c("questionnaire" = "#69b3a2", "review" = "#EFC000FF")) +
  scale_fill_manual(values = c("questionnaire" = "#EFC000FF", "review" = "#004D40")) +
  #scale_fill_manual(values = c("questionnaire" = "#f1a340", "review" = "#004D40")) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x=element_blank(),
        axis.title.y = element_text(size = 25,vjust = 4),
        axis.text.x = element_text(size = 18, color="black"),
        axis.text.y = element_text(size = 15),
        plot.margin = unit(c(0.7,0.25,0.25,2), "cm"),
        legend.position="none",
        plot.title = element_text(hjust = 0.5))


################################################################################
# repeatability as individual consistency/predictability
################################################################################

# reformatting the variable of interest and subsetting the dataset to select only
# those for which the level is known
repeatability.2.survey <- as.data.frame(data.survey %>% 
                                          mutate(repeatability.consistency = fct_recode(repeatability.consistency,
                                                                                        "\nYes\n" = "Yes",
                                                                                        "\nNo\n" = "No")) %>%
                                          mutate(source="questionnaire") %>%
                                          select(source,repeatability.consistency) %>%
                                          filter(repeatability.consistency %in% c("\nYes\n", "\nNo\n"))
)

repeatability.2.survey$repeatability.consistency <- factor(repeatability.2.survey$repeatability.consistency)

summary(repeatability.2.survey)


# reformatting the variable of interest and subsetting the dataset to select only
# those for which the level is known
repeatability.2.review <- as.data.frame(data.review %>% 
                                          mutate(repeatability.consistency = fct_recode(repeatability_consist_predict,
                                                                                        "\nYes\n" = "yes",
                                                                                        "\nNo\n" = "no")) %>%
                                          mutate(source="review") %>%
                                          select(source,repeatability.consistency) %>%
                                          filter(repeatability.consistency %in% c("\nYes\n", "\nNo\n"))
)

repeatability.2.review$repeatability.consistency <- factor(repeatability.2.review$repeatability.consistency)

summary(repeatability.2.review)

# binding both databases
repeatability.2 <- rbind(repeatability.2.review,repeatability.2.survey)
repeatability.2$source <- factor(repeatability.2$source)

summary(repeatability.2)


repeatability.2 <- repeatability.2 %>%
  group_by(source,repeatability.consistency) %>% 
  summarise(n = n()) %>% 
  mutate(total.n = sum(n)) %>%
  mutate(percentage = round(binom.confint(n, total.n, method=c("agresti-coull"),type="central")$mean*100,1)) %>%
  mutate(lower = round(binom.confint(n, total.n, method=c("agresti-coull"),type="central")$lower*100,1)) %>%
  mutate(upper = round(binom.confint(n, total.n, method=c("agresti-coull"),type="central")$upper*100,1)) %>%
  ggplot(aes(y=percentage, x=repeatability.consistency,ymin=lower, ymax=upper,fill = source)) + 
  geom_bar(position = position_dodge(), stat="identity") +
  geom_errorbar(position=position_dodge(.9), width = .2) +
  geom_text(aes(y = percentage, x = repeatability.consistency, 
                group = source,label=paste0("n=",n)), position=position_dodge(width=0.9), vjust=-4, size=6, color="grey35") + 
  labs(y="") +
  #ggtitle("Repeatability as individual consistency/predictability?") +
  scale_y_continuous(limits = c(0,100), breaks = seq(0,100,20),
                     expand = expand_scale(mult = c(0, 0.05))) +
  #scale_fill_manual(values = c("questionnaire" = "#f1a340", "review" = "#998ec3")) +
  #scale_fill_manual(values = c("questionnaire" = "#69b3a2", "review" = "#EFC000FF")) +
  scale_fill_manual(values = c("questionnaire" = "#EFC000FF", "review" = "#004D40")) +
  #scale_fill_manual(values = c("questionnaire" = "#f1a340", "review" = "#004D40")) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x=element_blank(),
        axis.title.y = element_text(),
        axis.text.x = element_text(size = 18, color="black"),
        axis.text.y = element_text(size = 15),
        plot.margin = unit(c(0.7,0.25,0.25,0.25), "cm"),
        legend.position="none",
        plot.title = element_text(hjust = 0.5))


################################################################################
# repeatability comparison across groups
################################################################################

# reformatting the variable of interest and subsetting the dataset to select only
# those for which the level is known
repeatability.3.survey <- as.data.frame(data.survey %>% 
                                          mutate(repetability_comparison_interpretation = fct_recode(repeatability.comparison.2,
                                                                                                     "\nWithin\n" = "a) Group “A” expresses less plasticity in their trait expression than group “B”",
                                                                                                     "\nAmong\n" = "b) Individuals differ more from each other in their average trait expression in group “A” than in group “B”",
                                                                                                     "Among\nand\nWithin" = "c) Both interpretations a) and b) can be made")) %>% 
                                          mutate(source="questionnaire") %>%
                                          select(source,repetability_comparison_interpretation) %>%
                                          filter(repetability_comparison_interpretation %in% c("\nWithin\n","\nAmong\n","Among\nand\nWithin"))
)

repeatability.3.survey$repetability_comparison_interpretation <- factor(repeatability.3.survey$repetability_comparison_interpretation)

summary(repeatability.3.survey)


# reformatting the variable of interest and subsetting the dataset to select only
# those for which the level is known
repeatability.3.review <- as.data.frame(data.review %>% 
                                          mutate(repetability_comparison_interpretation = fct_recode(repetability_comparison_interpretation,
                                                                                                     "\nWithin\n" = "within",
                                                                                                     "\nAmong\n" = "among",
                                                                                                     "Among\nand\nWithin" = "both")) %>%
                                          mutate(source="review") %>%
                                          select(source,repetability_comparison_interpretation) %>%
                                          filter(repetability_comparison_interpretation %in% c("\nWithin\n","\nAmong\n","Among\nand\nWithin"))
)

repeatability.3.review$repetability_comparison_interpretation <- factor(repeatability.3.review$repetability_comparison_interpretation)

summary(repeatability.3.review)

# binding both databases
repeatability.3 <- rbind(repeatability.3.review,repeatability.3.survey)
repeatability.3$source <- factor(repeatability.3$source)

summary(repeatability.3)


repeatability.3 <- repeatability.3 %>%
  group_by(source,repetability_comparison_interpretation) %>% 
  summarise(n = n()) %>% 
  mutate(total.n = sum(n)) %>%
  mutate(percentage = round(binom.confint(n, total.n, method=c("agresti-coull"),type="central")$mean*100,1)) %>%
  mutate(lower = round(binom.confint(n, total.n, method=c("agresti-coull"),type="central")$lower*100,1)) %>%
  mutate(upper = round(binom.confint(n, total.n, method=c("agresti-coull"),type="central")$upper*100,1)) %>%
  ggplot(aes(y=percentage, x=repetability_comparison_interpretation,ymin=lower, ymax=upper,fill = source)) + 
  geom_bar(position = position_dodge(), stat="identity") +
  geom_errorbar(position=position_dodge(.9), width = .2) +
  # mutate(percentage = round((n/sum(n))*100,1)) %>% 
  # ggplot() + 
  # geom_bar(aes(y = percentage, x = repetability_comparison_interpretation, fill = source), 
  #          stat="identity", position="dodge", colour = "white") +
  geom_text(aes(y = percentage, x = repetability_comparison_interpretation, 
                group = source,label=paste0("n=",n)), position=position_dodge(width=0.9), vjust=-8, size=6, color="grey35") + 
  labs(y="") +
  #ggtitle("Interpreting repeatability between groups") +
  scale_y_continuous(limits = c(0,100), breaks = seq(0,100,20),
                     expand = expand_scale(mult = c(0, 0.05))) +
  #scale_fill_manual(values = c("questionnaire" = "#f1a340", "review" = "#998ec3")) +
  #scale_fill_manual(values = c("questionnaire" = "#69b3a2", "review" = "#EFC000FF")) +
  scale_fill_manual(values = c("questionnaire" = "#EFC000FF", "review" = "#004D40")) +
  #scale_fill_manual(values = c("questionnaire" = "#f1a340", "review" = "#004D40")) +
  labs(fill = "Source") + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x=element_blank(),
        axis.title.y = element_text(),
        axis.text.x = element_text(size = 18, color="black"),
        axis.text.y = element_text(size = 15),
        plot.margin = unit(c(0.7,0.25,0.25,0.25), "cm"),
        legend.position = c(0.78, 0.90),
        legend.title = element_text(size = 21),
        legend.text = element_text(size = 17),
        legend.key.size = unit(1, "cm"),
        legend.background = element_rect(fill = "grey95"),
        legend.key = element_rect(fill = "grey95", color = NA),
        plot.title = element_text(hjust = 0.5))


################################################################################
# multipannel figure
################################################################################

library(ggpubr)

# exporting figure 1
tiff("figures/Figure2_v2.tiff",
     height=18, width=50,
     units='cm', compression="lzw", res=600)

ggarrange(repeatability.1, repeatability.2, repeatability.3,
          labels = c("a)","b)","c)"),
          font.label = list(size = 18),
          widths = c(1,0.9,0.9),
          ncol = 3, nrow = 1)

dev.off()

