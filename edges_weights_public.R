##################################################
## Project: FS Women Professional Matching Algorithm
## Script purpose: This script matches women in foreign affairs agencies with appropriate peer reviewers before submitting their annual review. 
##                Matches are based on responses to a survey to include rank, specialization, and willingness to review and/or be reviewed 
##                This is fully voluntary and not sponsored by any government agency.
## Date: This public-facing example version made 10/22/2024. Original script from May 2021
## Author: Sonnet Frisbie
##################################################

##########################
## 0.1 prep workspace  ###
##########################
 
rm(list=ls())

library(tidyverse)
library(igraph)
library(maxmatching)
library(readxl)


path_to_survey_results <- "The production version includes a path to the results from a survey that includes personal information"

test_survey <- read_csv(path_to_survey_results)


##########################
## 0.2 clean file      ###
##########################

##removing timestamp and comment box

test_survey <- test_survey[c(-1, -2, -3, -4, -5, -9, -18)]

colnames(test_survey)

## was having trouble matching long strings, so did so based on position
test_survey <- test_survey %>% 
  rename_with(~ "reviewer", 6) %>%
  rename_with(~ "choice_1", 7) %>%
  rename_with(~ "number_review", 10)

# renaming fields for ease
test_survey <- test_survey %>% rename("specialization" = "Please Select your Cone/Specialty",
                       "email" = "Please enter your official e-mail address",
                       "choice_2" = "Second Choice, if applicable",
                       "choice_3" = "Third Choice, if applicable",
                       "grade" = "Please Select your grade (or grade equivalent)",
                       "first" = "First Name",
                       "last" = "Last Name")

##remove duplicates based on same e-mail
test_survey <- test_survey[!duplicated(test_survey$email),]

unique(test_survey$reviewer)
test_survey <- test_survey %>% mutate(participation_code = if_else(reviewer == "Reviewed only: I want my EER reviewed, but I do not want to review anyone else's EER.", 0, 
                                          if_else(reviewer == "Both: I want to have my EER reviewed AND I want to review someone else's EER.", 1, 
                                                  if_else(reviewer == "Review only: I want to review another person(s) EER, but I do not need anyone to review my own EER.", 2, -1))))

test_survey$grade <- gsub("^.*-(0*)([1-9]\\d*)$", "\\2", test_survey$grade)

#finding those willing to review others
reviewers <- test_survey %>% 
  filter(participation_code == 1 | participation_code == 2)

reviewers <- reviewers %>% select(first, last, email, specialization, grade, number_review)

reviewers <- reviewers %>%
  uncount(number_review) 

## make unique names to capture people who review more than 1
reviewers$last <- with(reviewers, ave(as.character(last), first, FUN = make.unique))

##removing people who only wanted to review
test_survey <- test_survey %>% filter(participation_code != "2")

# making a matrix of all combinations
edges_weights <- tidyr::crossing(test_survey, reviewers, .name_repair = make.unique)

##adding _reviewer suffix to reviewer info
fix_names <- function(x) gsub("\\.1$", "_reviewer", x)
names(edges_weights) <- fix_names(names(edges_weights))

## changing sfs (senior foreign service) to numerical value 
edges_weights <- data.frame(lapply(edges_weights, function(x) {
                gsub("SFS", 0, x)
             }))

##adding column for weights
edges_weights$weights <- 0
##changing NA to 0 for logical operations
edges_weights <- edges_weights %>% replace_na(list(choice_1 = 0, 
                                      choice_2 = 0, 
                                      choice_3 = 0))

edges_weights$grade <- as.numeric(edges_weights$grade)
edges_weights$grade_reviewer <- as.numeric(edges_weights$grade_reviewer)

##########################
## 1.0 create weights  ###
##########################

##here is where I actually decide how much to weight different aspects

edges_weights$weights <- if_else(edges_weights$choice_1 == edges_weights$specialization_reviewer, edges_weights$weights + 3, 
                          if_else(edges_weights$choice_2 == edges_weights$specialization_reviewer, edges_weights$weights + 2, 
                                  if_else(edges_weights$choice_3 == edges_weights$specialization_reviewer, edges_weights$weights + 1, 0)))


#This is an edit based on feedback that makes OMSs always get someone 2 grades higher (due to their lower grades overall compared to TIS)

edges_weights$weights <- if_else(edges_weights$grade > edges_weights$grade_reviewer & edges_weights$specialization != "Specialist - OMS", edges_weights$weights + 3,
                                 if_else(edges_weights$grade == edges_weights$grade_reviewer & edges_weights$specialization != "Specialist - OMS", edges_weights$weights,
                                         if_else(edges_weights$grade < edges_weights$grade_reviewer & edges_weights$specialization != "Specialist - OMS", 0, edges_weights$weights)))

edges_weights$weights <-  if_else(edges_weights$grade - 1 > edges_weights$grade_reviewer & edges_weights$specialization == "Specialist - OMS", edges_weights$weights + 3,
        if_else(edges_weights$grade -1 == edges_weights$grade_reviewer & edges_weights$specialization == "Specialist - OMS", edges_weights$weights,
                if_else(edges_weights$grade -1 < edges_weights$grade_reviewer & edges_weights$specialization == "Specialist - OMS", 0, edges_weights$weights)))

##########################
## 2.0 fine tuning     ###
##########################

#this makes the algorithm not match people with themselves
edges_weights_test <- edges_weights %>% select(participation_code, first, last, email, first_reviewer, last_reviewer, email_reviewer, weights)

#this should prevent someone who doesn't need to have an EER reviewed from being assigned a reviewer
edges_weights_test <-  edges_weights_test %>%
  filter(participation_code != 2)

#edges_weights_test$V1 <- paste(edges_weights_test$first, edges_weights_test$last)

edges_weights_test$V1 <- edges_weights_test$email
edges_weights_test$V2 <- paste(edges_weights_test$first_reviewer, edges_weights_test$last_reviewer)

##This is a test ##
edges_weights_test <- edges_weights_test %>%
  filter(email != email_reviewer)


edges_weights_final <- edges_weights_test %>% select(V1, V2, weights)

edges_weights_final <- edges_weights_final %>% rename("weight" = "weights")

edges_weights_final$V2 <- gsub("$", "_R", edges_weights_final$V2)

edges_weights <- edges_weights_final %>% pivot_wider( 
                                   names_from = "V2",
                                   values_from = "weight")

##this now corrects for NAs as weights for people matching with themselves with 0 to make it
##as unlikely as possible to happen

edges_weights <- replace(edges_weights,is.na(edges_weights), 0)

write_csv(edges_weights, "edges_weights.csv")

# lapply(edges_weights, class)
# edges_weights[rowSums(edges[])>0,]

edges_weights_dropped <- edges_weights_final %>% 
  group_by(V1) %>% 
  mutate(sum = sum(weight)) %>% 
  filter(sum > 0) %>% 
  ungroup %>% 
  select (-sum)

##########################
## 3.0 matching        ###
##########################

g2 <- graph.data.frame(edges_weights_dropped, directed = TRUE)

matches <- maxmatching(g2, weighted = TRUE, maxcardinality = TRUE)

matches <- as.data.frame(matches)

matches_final <- matches %>% filter(str_detect(matching, "_R$"))

matches_final$email <- row.names(matches_final)  

matches_final <- matches_final %>% select(email, matching)

survey_matched <- test_survey %>% left_join(matches_final, by = "email")


###################################################
## 3.0 formatting results and exporting         ###
###################################################

##all done, now just merging in the reviewer e-mails to make it easier to send out the matches

reviewer_emails <- edges_weights_test %>% select(V2, email_reviewer)
reviewer_emails$V2 <- gsub("$", "_R", reviewer_emails$V2)
reviewer_emails <- reviewer_emails %>% distinct()

survey_matched <- survey_matched %>% left_join(reviewer_emails, by = c("matching" = "V2"))

# removing from the list the ones without a match (for any reason) into a separate. saving separately 

not_matched <- survey_matched %>%
  filter(is.na(matching))

survey_matched <- survey_matched %>%
  filter(!is.na(matching))

#export those who were matched
write_csv(survey_matched, "matched_survey_2024.csv")

#export those who were not matched
write_csv(not_matched, "not_matched_survey_2024.csv")


###########
## FIN  ###
###########
