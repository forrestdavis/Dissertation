library(tidyverse)
library(lme4)
library(lmerTest)

##################################################
#Fernandez (2003) and Cuetos and Mitchel (1988)
##################################################
en_data <- read.csv("results/FCM_English_combined_flat.csv")
es_data <- read.csv("results/FCM_Spanish_combined_flat.csv")

#######
# Sum Coding 
######

en_data$AttachType <- ifelse(en_data$AttachType=='high', 1, -1)
en_data$verbNum <- ifelse(en_data$verbNum=='pl', 1, -1)
en_data$item <- as.factor(en_data$item)

es_data$AttachType <- ifelse(es_data$AttachType=='high', 1, -1)
es_data$verbNum <- ifelse(es_data$verbNum=='pl', 1, -1)
es_data$item <- as.factor(es_data$item)


################
###BERT
###############
bert_en_model = en_data %>%
  filter(model == 'bert-base-uncased') %>%
  lmer(prob ~ AttachType*verbNum + (1+AttachType+verbNum || item), 
       data = ., REML = FALSE,
       control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

summary(bert_en_model)
anova(bert_en_model)

bert_es_model = es_data %>%
  filter(model == 'bert-base-spanish-wwm-uncased') %>%
  lmer(prob ~ AttachType*verbNum + (1+AttachType+verbNum || item), 
       data = ., REML = FALSE,
       control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

summary(bert_es_model)
anova(bert_es_model)


################
###ROBERTA
###############
roberta_en_model = en_data %>%
  filter(model == 'roberta-base') %>%
  lm(prob ~ AttachType*verbNum, 
       data = .)

summary(roberta_en_model)
anova(roberta_en_model)

roberta_es_model = es_data %>%
  filter(model == 'RuPERTa-base') %>%
  lmer(prob ~ AttachType*verbNum + (1+AttachType+verbNum || item), 
       data = ., REML = FALSE,
       control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

summary(roberta_es_model)
anova(roberta_es_model)


################
###GPT2-XL
###############
gpt_en_model = en_data %>%
  filter(model == 'gpt2-xl') %>%
  lmer(prob ~ AttachType*verbNum + (1+AttachType+verbNum || item), 
       data = ., REML = FALSE,
       control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

summary(gpt_en_model)
anova(gpt_en_model)

gpt_es_model = es_data %>%
  filter(model == 'gpt2-spanish') %>%
  lmer(prob ~ AttachType*verbNum + (1+AttachType+verbNum || item), 
       data = ., REML = FALSE,
       control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

summary(gpt_es_model)
anova(gpt_es_model)

es_gpt_model = es_data %>%
  filter(model == 'spanish-gpt2') %>%
  lmer(prob ~ AttachType*verbNum + (1+AttachType+verbNum || item), 
       data = ., REML = FALSE,
       control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

summary(es_gpt_model)
anova(es_gpt_model)

################
###LSTM
###############
lstm_en_model = en_data %>% 
  filter(grepl('lstm', tolower(model)) & model != 'lstm avg') %>%
  lmer(prob ~ AttachType*verbNum + (1+AttachType+verbNum || model) + (1 + AttachType+verbNum || item), 
       data = ., REML = FALSE,
       control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

summary(lstm_en_model)
anova(lstm_en_model)

lstm_es_model = es_data %>% 
  filter(grepl('lstm', tolower(model)) & model != 'lstm avg') %>%
  lmer(prob ~ AttachType*verbNum + (1+AttachType+verbNum || model) + (1 + AttachType+verbNum || item), 
       data = ., REML = FALSE,
       control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

summary(lstm_es_model)
anova(lstm_es_model)

##################################################
# Carreiras and Clifton (1993)
##################################################
adj_data <- read.csv("results/Carreiras_Clifton_1993_Spanish_combined_flat.csv")

adj_data$AttachType <- ifelse(adj_data$AttachType=='high', 1, -1)
adj_data$gender <- ifelse(adj_data$gender=='m', 1, -1)
adj_data$item <- as.factor(adj_data$item)

################
###BERT
###############

bert_adj_model = adj_data %>%
  filter(model == 'bert-base-spanish-wwm-uncased') %>%
  lmer(prob ~ AttachType*gender + (1 | item), 
       data = ., REML = FALSE,
       control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

summary(bert_adj_model)
anova(bert_adj_model)

################
###ROBERTA
###############
roberta_adj_model = adj_data %>%
  filter(model == 'RuPERTa-base') %>%
  lmer(prob ~ AttachType*gender + (1+gender || item), 
       data = ., REML = FALSE,
       control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

summary(roberta_adj_model)
anova(roberta_adj_model)

################
###GPT2-XL
###############
gpt_adj_model = adj_data %>%
  filter(model == 'gpt2-spanish') %>%
  lmer(prob ~ AttachType*gender + (1 | item), 
       data = ., REML = FALSE,
       control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

summary(gpt_adj_model)
anova(gpt_adj_model)

adj_gpt_model = adj_data %>%
  filter(model == 'spanish-gpt2') %>%
  lmer(prob ~ AttachType*gender + (1+gender || item), 
       data = ., REML = FALSE,
       control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

summary(adj_gpt_model)
anova(adj_gpt_model)

################
###LSTM
###############
lstm_adj_model = adj_data %>% 
  filter(grepl('lstm', tolower(model)) & model != 'lstm avg') %>%
  lmer(prob ~ AttachType*gender + (1+gender || model) + (1 + gender || item), 
       data = ., REML = FALSE,
       control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

summary(lstm_adj_model)
anova(lstm_adj_model)



##################################################
# Gilboy et al. (1995)
##################################################

gilboy_en <- read.csv("results/Gilboy_etal_1995_English_combined_flat.csv")
gilboy_es <- read.csv("results/Gilboy_etal_1995_Spanish_combined_flat.csv")


###
#English
###
model_names <- c()
cats <- c()
Ts <- c()
Ps <- c()
IsSig <- c()
Direction <- c()

threshold = 0.05 / 7

for (model_name in unique(gilboy_en$model)){
  for (cat in unique(gilboy_en$expCat)){
    human_data = gilboy_en %>% 
        filter(N2PropSource == 'human' & model == model_name & expCat == cat)
    
    model_data = gilboy_en %>% 
      filter(N2PropSource == 'model' & model == model_name & expCat == cat)
    
    result <- t.test(human_data$N2Prop, model_data$N2Prop)
    t = result$statistic
    p = result$p.value
    
    model_names <- c(model_names, model_name)
    cats <- c(cats, cat)
    Ts <- c(Ts, t)
    Ps <- c(Ps, p)
    
    if (p <= threshold){
      IsSig <- c(IsSig, 1)
    }else{
      IsSig <- c(IsSig, 0)
    }
    
    if (t <= 0){
      Direction <- c(Direction, "Lower")
    }else{
      Direction <- c(Direction, "Higher")
    }
  }
}

en_ttests <- data.frame(model_names, cats, Ts, Ps, Direction, IsSig)


###
#Spanish
###
model_names <- c()
cats <- c()
Ts <- c()
Ps <- c()
IsSig <- c()
Direction <- c()

threshold = 0.05 / 7

for (model_name in unique(gilboy_es$model)){
  for (cat in unique(gilboy_es$expCat)){
    human_data = gilboy_es %>% 
      filter(N2PropSource == 'human' & model == model_name & expCat == cat)
    
    model_data = gilboy_es %>% 
      filter(N2PropSource == 'model' & model == model_name & expCat == cat)
    
    result <- t.test(human_data$N2Prop, model_data$N2Prop)
    t = result$statistic
    p = result$p.value
    
    model_names <- c(model_names, model_name)
    cats <- c(cats, cat)
    Ts <- c(Ts, t)
    Ps <- c(Ps, p)
    
    if (p <= threshold){
      IsSig <- c(IsSig, 1)
    }else{
      IsSig <- c(IsSig, 0)
    }
    
    if (t <= 0){
      Direction <- c(Direction, "Lower")
    }else{
      Direction <- c(Direction, "Higher")
    }
    
  }
}

es_ttests <- data.frame(model_names, cats, Ts, Ps, Direction, IsSig)

##################################################
# Rohde et al. (2011)
##################################################
ic_rc_data <- read.csv("results/Rohde_etal_2011_English_combined_flat.csv")

#######
# Sum Coding 
######

ic_rc_data$hasIC <- ifelse(ic_rc_data$hasIC==1, 1, -1)
ic_rc_data$antecedent <- ifelse(ic_rc_data$antecedent=='NP1',1,-1)
ic_rc_data$verbNum <- ifelse(ic_rc_data$verbNum=='sg',1,-1)
ic_rc_data$item <- as.factor(ic_rc_data$item)


################
###GPT2-XL
###############
gpt2_ic_rc_model = ic_rc_data %>%
  filter(model == 'gpt2-xl') %>%
  lmer(prob ~ hasIC*antecedent*verbNum + (1+verbNum+antecedent| item), 
       data = ., REML = FALSE,
       control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

summary(gpt2_ic_rc_model)
anova(gpt2_ic_rc_model)

gpt2_nonIC_NP1 = ic_rc_data %>%
  filter(model == 'gpt2-xl' & hasIC == -1 & antecedent == 1)
gpt2_IC_NP1 = ic_rc_data %>%
  filter(model == 'gpt2-xl' & hasIC == 1 & antecedent == 1)

gpt2_nonIC_NP2 = ic_rc_data %>%
  filter(model == 'gpt2-xl' & hasIC == -1 & antecedent == -1)
gpt2_IC_NP2 = ic_rc_data %>%
  filter(model == 'gpt2-xl' & hasIC == 1 & antecedent == -1)

#IC/nonIC NP1
t.test(gpt2_IC_NP1$prob, gpt2_nonIC_NP1$prob)

#IC/nonIC NP2
t.test(gpt2_IC_NP2$prob, gpt2_nonIC_NP2$prob)

##Low Attachment is more likely after nonIC than IC, no other effects

################
###LSTMS
###############
lstm_all_ic_rc_model = ic_rc_data %>% 
  filter(grepl('LSTM', model)) %>%
  lmer(prob ~ hasIC*verbNum*antecedent + (1+verbNum*antecedent || model) + (1 + verbNum*antecedent || item), 
       data = ., REML = FALSE,
       control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

summary(lstm_all_ic_rc_model)
anova(lstm_all_ic_rc_model)


################
###BERT
###############
bert_ic_rc_model = ic_rc_data %>%
  filter(model == 'bert-base-uncased') %>%
  lmer(prob ~ hasIC*verbNum*antecedent + (1 + verbNum+antecedent || item), 
       data = ., REML = FALSE,
       control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

summary(bert_ic_rc_model)
anova(bert_ic_rc_model)

bert_nonIC_NP1 = ic_rc_data %>%
  filter(model == 'bert-base-uncased' & hasIC == -1 & antecedent == 1)
bert_IC_NP1 = ic_rc_data %>%
  filter(model == 'bert-base-uncased' & hasIC == 1 & antecedent == 1)

bert_nonIC_NP2 = ic_rc_data %>%
  filter(model == 'bert-base-uncased' & hasIC == -1 & antecedent == -1)
bert_IC_NP2 = ic_rc_data %>%
  filter(model == 'bert-base-uncased' & hasIC == 1 & antecedent == -1)

#IC/nonIC NP1
t.test(bert_IC_NP1$prob, bert_nonIC_NP1$prob)

#IC/nonIC NP2
t.test(bert_IC_NP2$prob, bert_nonIC_NP2$prob)

################
###RoBERTa
###############
roberta_ic_rc_model = ic_rc_data %>%
  filter(model == 'roberta-base') %>%
  lmer(prob ~ hasIC*verbNum*antecedent + (1 + verbNum+antecedent || item), 
       data = ., REML = FALSE,
       control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

summary(roberta_ic_rc_model)
anova(roberta_ic_rc_model)

roberta_nonIC_NP1 = ic_rc_data %>%
  filter(model == 'roberta-base' & hasIC == -1 & antecedent == 1)
roberta_IC_NP1 = ic_rc_data %>%
  filter(model == 'roberta-base' & hasIC == 1 & antecedent == 1)

roberta_nonIC_NP2 = ic_rc_data %>%
  filter(model == 'roberta-base' & hasIC == -1 & antecedent == -1)
roberta_IC_NP2 = ic_rc_data %>%
  filter(model == 'roberta-base' & hasIC == 1 & antecedent == -1)

#IC/nonIC NP1
t.test(roberta_IC_NP1$prob, roberta_nonIC_NP1$prob)


#IC/nonIC NP2
t.test(roberta_IC_NP2$prob, roberta_nonIC_NP2$prob)

