library(tidyverse)
library(lme4)
library(lmerTest)

#######################################
## Section 3.3.4
#######################################

en_ic_data <- read.csv("results/English_IC_combined_flat.csv")

#######
# Sum Coding 
######

en_ic_data$IC <- ifelse(en_ic_data$IC=='subj', 1, -1)
en_ic_data$antecedent <- ifelse(en_ic_data$antecedent=='subj',1,-1)
en_ic_data$gender <- ifelse(en_ic_data$gender=='m',1,-1)
en_ic_data$verb <- as.factor(en_ic_data$verb)
en_ic_data$pairNum <- as.factor(en_ic_data$pairNum)


################
###GPT2-XL
###############
gpt2_ic_pron_model = en_ic_data %>%
  filter(model == 'gpt2-xl') %>%
  lmer(prob ~ IC*gender*antecedent + (1 + gender+antecedent || pairNum) + (1 + gender+antecedent || verb), 
       data = ., REML = FALSE,
       control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

summary(gpt2_ic_pron_model)

################
###LSTMS
###############
lstm_all_ic_pron_model = en_ic_data %>% 
  filter(grepl('lstm', tolower(model)) & model != 'lstm avg') %>%
  lmer(prob ~ IC*gender*antecedent + (1+gender+antecedent || model) + (1 + gender +antecedent || pairNum) + (1 + gender + antecedent || verb), 
       data = ., REML = FALSE,
       control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

summary(lstm_all_ic_pron_model)
anova(lstm_all_ic_pron_model)

################
###TFXL
###############
tfxl_ic_pron_model = en_ic_data %>%
  filter(model == 'transfo-xl-wt103') %>%
  lmer(prob ~ IC*gender*antecedent + (1 + gender +antecedent || pairNum) + (1 + gender + antecedent || verb), 
       data = ., REML = FALSE,
       control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

summary(tfxl_ic_pron_model)
anova(tfxl_ic_pron_model)

################
###BERT
###############
bert_ic_pron_model = en_ic_data %>%
  filter(model == 'bert-base-uncased') %>%
  lmer(prob ~ IC*gender*antecedent + (1 + gender +antecedent || pairNum) + (1 + gender + antecedent || verb), 
       data = ., REML = FALSE,
       control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

summary(bert_ic_pron_model)
anova(bert_ic_pron_model)

################
###RoBERTa
###############
roberta_ic_pron_model = en_ic_data %>%
  filter(model == 'roberta-base') %>%
  lmer(prob ~ IC*gender*antecedent + (1 + gender +antecedent || pairNum) + (1 + gender + antecedent || verb), 
       data = ., REML = FALSE,
       control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

summary(roberta_ic_pron_model)
anova(roberta_ic_pron_model)
