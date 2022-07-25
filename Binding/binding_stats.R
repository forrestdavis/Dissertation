library(tidyverse)
library(lme4)
library(lmerTest)

chow_1 <- read.csv("results/Chow_Exp1_combined_flat.csv")
nicol <- read.csv("results/Nicol_Exp_combined_flat.csv")
gompel_1 <- read.csv("results/vanGompel_Liversedge_Exp1_combined_flat.csv")
kush_1 <- read.csv("results/Kush_Dillon_Exp1_combined_flat.csv")
kush_2 <- read.csv("results/Kush_Dillon_Exp2_combined_flat.csv")


#######
# Sum Coding 
######

chow_1$matrix <- ifelse(chow_1$matrix=='Match', 1, -1)
chow_1$embedded <- ifelse(chow_1$embedded=='Match', 1, -1)
chow_1$item <- as.factor(chow_1$item)

nicol$top <- ifelse(nicol$top == 'Match', 1, -1)
nicol$middle <- ifelse(nicol$middle == 'Match', 1, -1)
nicol$bottom <- ifelse(nicol$bottom == 'Match', 1, -1)
nicol$item <- as.factor(nicol$item)

gompel_1$contrast <- ifelse(gompel_1$contrast=='Match', 1, -1)
gompel_1$pronoun <- ifelse(gompel_1$pronoun=='he', 1, -1)
gompel_1$item <- as.factor(gompel_1$item)

kush_1$contrast <- ifelse(kush_1$contrast=='Match', 1, -1)
kush_1$adjunct <- ifelse(kush_1$adjunct == 'Constraint', 1, -1)
kush_1$item <- as.factor(kush_1$adjunct )
kush_1$pronoun <- as.factor(kush_1$pronoun )


kush_2$contrast <- ifelse(kush_2$contrast=='Match', 1, -1)
kush_2$adjunct <- ifelse(kush_2$adjunct == 'Constraint', 1, -1)
kush_2$item <- as.factor(kush_2$adjunct )
kush_2$pronoun <- as.factor(kush_2$pronoun )


####
#BERT
##
bert_model = chow_1 %>%
  filter(model == 'bert-base-uncased' & pronoun=='him') %>%
  lmer(surp ~ matrix*embedded + (1+matrix+embedded || item), 
       data = ., REML = FALSE,
       control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

summary(bert_model)
anova(bert_model)

bert_model = chow_1 %>%
  filter(model == 'bert-base-uncased' & pronoun=='his') %>%
  lmer(surp ~ matrix*embedded + (1+matrix+embedded || item), 
       data = ., REML = FALSE,
       control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

summary(bert_model)
anova(bert_model)


bert_model = nicol %>%
  filter(model == 'bert-base-uncased') %>%
  lmer(surp ~ top*middle*bottom + (1+top+middle+bottom || item), 
       data = ., REML = FALSE,
       control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

summary(bert_model)
anova(bert_model)

bert_model = gompel_1 %>%
  filter(model == 'bert-base-uncased') %>%
  lmer(surp ~ contrast*pronoun + (1 | item), 
       data = ., REML = FALSE,
       control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

summary(bert_model)
anova(bert_model)


bert_model = gompel_3 %>%
  filter(model == 'bert-base-uncased') %>%
  lmer(surp ~ contrast*pronoun + (1 | item), 
       data = ., REML = FALSE,
       control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

summary(bert_model)
anova(bert_model)

bert_model = kush_2 %>%
  filter(model == 'bert-base-uncased') %>%
  lmer(surp ~ adjunct*contrast + (1| item), 
       data = ., REML = FALSE,
       control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

summary(bert_model)
anova(bert_model)

####
#RoBERTa
##
roberta_model = chow_1 %>%
  filter(model == 'roberta-base' & pronoun == 'him') %>%
  lmer(surp ~ matrix*embedded + (1+matrix+embedded || item), 
       data = ., REML = FALSE,
       control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

summary(roberta_model)
anova(roberta_model)

roberta_model = chow_1 %>%
  filter(model == 'roberta-base' & pronoun == 'his') %>%
  lmer(surp ~ matrix*embedded + (1+matrix+embedded || item), 
       data = ., REML = FALSE,
       control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

summary(roberta_model)
anova(roberta_model)


roberta_model = nicol %>%
  filter(model == 'roberta-base') %>%
  lmer(surp ~ top*middle*bottom + (1+top+middle+bottom || item), 
       data = ., REML = FALSE,
       control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

summary(roberta_model)
anova(roberta_model)

roberta_model = gompel_1 %>%
  filter(model == 'roberta-base') %>%
  lmer(surp ~ contrast*pronoun + (1 | item), 
       data = ., REML = FALSE,
       control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

summary(roberta_model)
anova(roberta_model)


roberta_model = kush_2 %>%
  filter(model == 'roberta-base') %>%
  lmer(surp ~ contrast*adjunct + (1 | item), 
       data = ., REML = FALSE,
       control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

summary(roberta_model)
anova(roberta_model)

####
#GPT2
##
gpt2_model = chow_1 %>%
  filter(model == 'gpt2-xl' & pronoun == 'him' ) %>%
  lmer(surp ~ matrix*embedded + (1+matrix+embedded || item), 
       data = ., REML = FALSE,
       control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

summary(gpt2_model)
anova(gpt2_model)

gpt2_model = chow_1 %>%
  filter(model == 'gpt2-xl' & pronoun == 'his' ) %>%
  lmer(surp ~ matrix*embedded + (1+matrix+embedded || item), 
       data = ., REML = FALSE,
       control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

summary(gpt2_model)
anova(gpt2_model)

gpt2_model = nicol %>%
  filter(model == 'gpt2-xl') %>%
  lmer(surp ~ top*middle*bottom + (1+top+middle+bottom || item), 
       data = ., REML = FALSE,
       control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

summary(gpt2_model)
anova(gpt2_model)

newdata <- data.frame(top=c(1, 1, 1, 1, -1, -1, -1, -1), middle=c(1, 1, -1, -1, 1, 1, -1, -1), bottom=c(1, -1, 1, -1, 1, -1, 1, -1))


gpt2_model = gompel_1 %>%
  filter(model == 'gpt2-xl') %>%
  lmer(surp ~ contrast*pronoun + (1 | item), 
       data = ., REML = FALSE,
       control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

summary(gpt2_model)
anova(gpt2_model)

gpt2_model = kush_2 %>%
  filter(model == 'gpt2-xl') %>%
  lmer(surp ~ contrast*adjunct + (1 | item), 
       data = ., REML = FALSE,
       control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

summary(gpt2_model)
anova(gpt2_model)

####
#TFXL
##
tfxl_model = chow_1 %>%
  filter(model == 'transfo-xl-wt103' & pronoun=='him') %>%
  lmer(surp ~ matrix*embedded + (1+matrix+embedded || item), 
       data = ., REML = FALSE,
       control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

summary(tfxl_model)
anova(tfxl_model)

tfxl_model = chow_1 %>%
  filter(model == 'transfo-xl-wt103' & pronoun=='his') %>%
  lmer(surp ~ matrix*embedded + (1+matrix+embedded || item), 
       data = ., REML = FALSE,
       control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

summary(tfxl_model)
anova(tfxl_model)


tfxl_model = nicol %>%
  filter(model == 'transfo-xl-wt103') %>%
  lmer(surp ~ top*middle*bottom + (1+top+middle+bottom || item), 
       data = ., REML = FALSE,
       control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

summary(tfxl_model)
anova(tfxl_model)

tfxl_model = gompel_1 %>%
  filter(model == 'transfo-xl-wt103') %>%
  lmer(surp ~ contrast*pronoun + (1 | item), 
       data = ., REML = FALSE,
       control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

summary(tfxl_model)
anova(tfxl_model)

tfxl_model = kush_2 %>%
  filter(model == 'transfo-xl-wt103') %>%
  lmer(surp ~ contrast*adjunct + (1 | pronoun), 
       data = ., REML = FALSE,
       control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

summary(tfxl_model)
anova(tfxl_model)
####
# LSTM
####
lstm_model = chow_1 %>% 
  filter(grepl('lstm', tolower(model)) & model != 'lstm avg' & pronoun=='him') %>%
  lmer(surp ~ matrix*embedded + (1+matrix || model) + (1 +embedded || item), 
       data = ., REML = FALSE,
       control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

summary(lstm_model)
anova(lstm_model)

lstm_model = chow_1 %>% 
  filter(grepl('lstm', tolower(model)) & model != 'lstm avg' & pronoun=='his') %>%
  lmer(surp ~ matrix*embedded + (1+matrix || model) + (1 +embedded || item), 
       data = ., REML = FALSE,
       control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

summary(lstm_model)
anova(lstm_model)

lstm_model = nicol %>% 
  filter(grepl('lstm', tolower(model)) & model != 'lstm avg') %>%
  lmer(surp ~ top*middle*bottom + (1+bottom || model) + (1+top+middle+bottom || item), 
       data = ., REML = FALSE,
       control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

summary(lstm_model)
anova(lstm_model)

lstm_model = gompel_1 %>% 
  filter(grepl('lstm', tolower(model)) & model != 'lstm avg') %>%
  lmer(surp ~ contrast*pronoun + (1 | model) + (1 | item), 
       data = ., REML = FALSE,
       control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

summary(lstm_model)
anova(lstm_model)

predict(lstm_model, data.frame(contrast=c(1, 1, -1, -1), pronoun=c(1, -1, 1, -1)), re.form=NA)

lstm_model = kush_2 %>% 
  filter(grepl('lstm', tolower(model)) & model != 'lstm avg') %>%
  lmer(surp ~ contrast*adjunct + (1 | model) + (1 | item), 
       data = ., REML = FALSE,
       control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

summary(lstm_model)
anova(lstm_model)
