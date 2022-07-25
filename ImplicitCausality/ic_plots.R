library(ggplot2)
library(dplyr)
library(cowplot)
library(reshape2)
library(ggrepel)

#########################################
######## Implicit Causality #############
#########################################


###########################
###### LOAD DATA ##########
###########################
path = "/home/forrestdavis/Projects/Dissertation/ImplicitCausality/"

en_data <- read.csv(paste(path, "results/English_IC_combined_flat.csv", sep=''))
zh_data <- read.csv(paste(path, "results/IC_mismatch_ZH.csv", sep=''))
es_data <- read.csv(paste(path, "results/IC_mismatch_ES.csv", sep=''))
it_data <- read.csv(paste(path, "results/IC_mismatch_IT.csv", sep=''))

#Add verbose antecedent and IC info to data also flatten models
zh_data$antecedent <- ifelse(zh_data$isHigh == 1, "subj", "obj")
zh_data$IC <- ifelse(zh_data$bias > 50, "subj", "obj")
zh_data <- zh_data %>% select(exp, verb, item, sent, bias, antecedent, IC, gender, score_bert, score_bert_base, score_bert_pro, score_mbert, score_roberta, score_roberta_base, score_roberta_pro)
zh_data <- melt(zh_data, value.name = "prob", variable.name="model", id = c("exp", "verb", "item", "sent", "bias", "antecedent", "IC", "gender"))

zh_data <- zh_data %>% mutate(model = recode(model, "score_bert"="Chinese BERT", 
                                  "score_bert_base"="Chinese BERT base", 
                                  "score_bert_pro"="Chinese BERT pro", 
                                  "score_mbert" = "mBERT", 
                                  "score_roberta" = "Chinese RoBERTa", 
                                  "score_roberta_base" = "Chinese RoBERTa base", 
                                  "score_roberta_pro" = "Chinese RoBERTa pro"))

es_data$antecedent <- ifelse(es_data$isHigh == 1, "subj", "obj")
es_data$IC <- ifelse(es_data$bias > 50, "subj", "obj")
es_data <- es_data %>% select(exp, verb, item, sent, bias, antecedent, IC, gender, score_bert, score_bert_base, score_bert_pro, score_mbert, score_roberta, score_roberta_base, score_roberta_pro)
es_data <- melt(es_data, value.name = "prob", variable.name="model", id = c("exp", "verb", "item", "sent", "bias", "antecedent", "IC", "gender"))

es_data <- es_data %>% mutate(model = recode(model, "score_bert"="Spanish BERT", 
                                             "score_bert_base"="Spanish BERT base", 
                                             "score_bert_pro"="Spanish BERT pro", 
                                             "score_mbert" = "mBERT", 
                                             "score_roberta" = "Spanish RoBERTa", 
                                             "score_roberta_base" = "Spanish RoBERTa base", 
                                             "score_roberta_pro" = "Spanish RoBERTa pro"))

it_data$antecedent <- ifelse(it_data$isHigh == 1, "subj", "obj")
it_data$IC <- ifelse(it_data$bias > 50, "subj", "obj")
it_data <- it_data %>% select(exp, verb, item, sent, bias, antecedent, IC, gender, score_bert, score_bert_base, score_bert_pro, score_mbert, score_umberto, score_umberto_base, score_umberto_pro, score_gilberto, score_gilberto_base, score_gilberto_pro)
it_data <- melt(it_data, value.name = "prob", variable.name="model", id = c("exp", "verb", "item", "sent", "bias", "antecedent", "IC", "gender"))

it_data <- it_data %>% mutate(model = recode(model, "score_bert"="Italian BERT", 
                                             "score_bert_base"="Italian BERT base", 
                                             "score_bert_pro"="Italian BERT pro", 
                                             "score_mbert" = "mBERT", 
                                             "score_umberto" = "Italian UmBERTo", 
                                             "score_umberto_base" = "Italian UmBERTo base", 
                                             "score_umberto_pro" = "Italian UmBERTo pro", 
                                             "score_gilberto" = "Italian GilBERTo", 
                                             "score_gilberto_base" = "Italian GilBERTo base", 
                                             "score_gilberto_pro" = "Italian GilBERTo pro", 
                                             ))


########################
#### English Human #####
########################

human_bias <- en_data %>%
              filter(antecedent=='subj' & gender =='f' & pairNum == 0 & model == 'bert-base-uncased')

human_bias$pref <- (human_bias$bias)*.01

human_en_sum <- human_bias %>%
  group_by(IC) %>%
  summarise( 
    n=n(),
    mean=mean(pref),
    sd=sd(pref)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ci=se * qt((1-0.05)/2 + .5, n-1)) %>% as.data.frame()

########################
### English Models #####
########################

en_lstm_data <- en_data %>% filter(grepl("LSTM", model))

#Add item column (repeats per model)
en_lstm_data$item <- 1:13776

#Get lstm average by item
en_lstm_sum <- en_lstm_data %>%
  group_by(item) %>%
  summarise(
    n=n(), 
    mean = mean(prob), 
    sd = sd(prob)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ci=se * qt((1-0.05)/2 + .5, n-1)) %>% as.data.frame()

#Create data entry for LSTM avg
en_lstm_data <- en_lstm_data %>% filter(model=="wikitext-103_LSTM_400_1-d0.2.pt")
en_lstm_data$model <- c("LSTM avg")
en_lstm_data$prob <- en_lstm_sum$mean
en_lstm_data <- en_lstm_data %>% select(exp, verb, pairNum, mask_sents, auto_sents, target, bias, antecedent, gender, IC, sent, model, prob)

en_model_data <- en_data %>% filter(!grepl("LSTM", model)) %>% filter(!grepl('lstm', model))
en_model_data <- rbind(en_model_data, en_lstm_data)


model_en_subj <- subset(en_model_data, antecedent=='subj')
model_en_obj <- subset(en_model_data, antecedent=='obj')

#Remove non-informative columns for grouping
model_en <- model_en_subj %>% select(exp, verb, pairNum, sent, bias, gender, IC, model)

#Take difference between probability of subject pronoun - object pronoun
model_en$pref <- model_en_subj$prob - model_en_obj$prob

model_en_sum <- model_en %>% 
  group_by(IC, model) %>%
  summarise(
    n=n(), 
    mean = mean(pref), 
    sd = sd(pref)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ci=se * qt((1-0.05)/2 + .5, n-1)) %>% as.data.frame()

##Combine with human 
human_en_sum$model <- c("human")

en_sum <- rbind(human_en_sum, model_en_sum)

#######################################
#### English Plots for Figure 3.1 #####
#######################################

en_models_plot <- en_sum %>% filter(!model %in% c("ProBERT", "ProBERT_BASE", "ProRoBERTa", "ProRoBERTa_BASE")) %>%
  mutate(across(model, factor, levels=c("human", "bert-base-uncased", "roberta-base", "gpt2-xl", "transfo-xl-wt103", "LSTM avg"))) %>%
  mutate(model = recode(model, "gpt2-xl"="GPT-2 XL", 
                        "human"="Human",
                        "bert-base-uncased"="BERT", 
                        "roberta-base"="RoBERTa", 
                        "transfo-xl-wt103"="TransformerXL")) %>%
  ggplot(aes(x=IC, y=mean)) +
  geom_bar(stat="identity", fill="mediumpurple3", alpha=0.5)+
  scale_x_discrete(breaks=c("obj", "subj"),
                   labels=c("Object-IC", "Subject-IC")) + labs(y="Subject Preference", x='IC Verb Bias',title="Implicit Causality Verb Bias by Model") +
  ylim(-1, 1) +  theme(text = element_text(size=15)) +
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), position=position_dodge(.9), width=0.08, colour="black", alpha=0.9, size=.9) +
  facet_wrap(~model)

en_models_plot

ggsave(paste(path,"figures/English-IC.png", sep=""), width=8)

########################
#### Chinese Human #####
########################

human_bias <- zh_data %>%
  filter(antecedent=='subj' & gender =='f' & item == 0 & model == 'Chinese BERT')

#Make bias like ferstl bias
N1 <- human_bias$bias
N2 <- 100-N1
bias <- 100*(N1-N2)/(N1+N2)
human_bias$bias <- bias

human_bias$pref <- (human_bias$bias)*.01

human_zh_sum <- human_bias %>%
  group_by(IC) %>%
  summarise( 
    n=n(),
    mean=mean(pref),
    sd=sd(pref)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ci=se * qt((1-0.05)/2 + .5, n-1)) %>% as.data.frame()

########################
### Chinese Models #####
########################

zh_model_data <- zh_data 

model_zh_subj <- subset(zh_model_data, antecedent=='subj')
model_zh_obj <- subset(zh_model_data, antecedent=='obj')

#Remove non-informative columns for grouping
model_zh <- model_zh_subj %>% select(exp, verb, item, sent, bias, gender, IC, model)

#Take difference between probability of subject pronoun - object pronoun
model_zh$pref <- model_zh_subj$prob - model_zh_obj$prob

model_zh_sum <- model_zh %>% 
  group_by(IC, model) %>%
  summarise(
    n=n(), 
    mean = mean(pref), 
    sd = sd(pref)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ci=se * qt((1-0.05)/2 + .5, n-1)) %>% as.data.frame()

##Combine with human 
human_zh_sum$model <- c("Chinese Human")

zh_sum <- rbind(human_zh_sum, model_zh_sum)

#####################################
#### English/Chinese Figure 3.4 #####
#####################################

human_en_sum$model <- c("English Human")
en_sum <- rbind(human_en_sum, model_en_sum)

en_zh_sum <- rbind(en_sum, zh_sum)

#Filter out non-bert based english models
en_zh_sum <- en_zh_sum %>% filter(!model %in% c("LSTM avg", "gpt2-xl", "transfo-xl-wt103"))

en_zh_og_plots <-  en_zh_sum %>%
  filter(!model %in% c("Chinese BERT pro", "Chinese BERT base", "Chinese RoBERTa pro", "Chinese RoBERTa base",
                       "ProBERT", "ProBERT_BASE", "ProRoBERTa", "ProRoBERTa_BASE", "mBERT")) %>%
  mutate(across(model, factor, levels=c("English Human", "bert-base-uncased", "roberta-base", "Chinese Human", 
                                        "Chinese BERT", "Chinese RoBERTa"))) %>%
    mutate(model = recode(model, "bert-base-uncased"="English BERT", 
                          "roberta-base"="English RoBERTa")) %>%
  ggplot(aes(x=IC, y=mean)) +
  geom_bar(stat="identity", fill="mediumpurple3", alpha=0.5)+
  scale_x_discrete(breaks=c("obj", "subj"),
                   labels=c("Object-IC", "Subject-IC")) + labs(y="Subject Preference", x='IC Verb Bias',title="English and Chinese Implicit Causality Verb Bias by Model") +
  ylim(-1, 1) +  theme(text = element_text(size=15)) +
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), position=position_dodge(.9), width=0.08, colour="black", alpha=0.9, size=.9) +
  facet_wrap(~model)

en_zh_og_plots 

ggsave(paste(path,"figures/English-Chinese-og-IC.png", sep=""), width=8)

#####################################
#### English/Chinese Figure 3.7 #####
#####################################

human_en_sum$model <- c("English Human")
en_sum <- rbind(human_en_sum, model_en_sum)

en_zh_sum <- rbind(en_sum, zh_sum)

#Filter out non-bert based english models
en_zh_sum <- en_zh_sum %>% filter(!model %in% c("LSTM avg", "gpt2-xl", "transfo-xl-wt103"))

en_zh_pro_plots <-  en_zh_sum %>%
  filter(model %in% c("Chinese Human", "English Human","Chinese BERT pro", "Chinese RoBERTa pro",
                       "ProBERT", "ProRoBERTa")) %>%
  mutate(across(model, factor, levels=c("English Human", "ProBERT", "ProRoBERTa", "Chinese Human", 
                                        "Chinese BERT pro", "Chinese RoBERTa pro"))) %>%
  mutate(model = recode(model, "ProBERT"="English BERT", 
                        "ProRoBERTa"="English RoBERTa", 
                        "Chinese BERT pro"="Chinese BERT", 
                        "Chinese RoBERTa pro" = "Chinese RoBERTa")) %>%
  ggplot(aes(x=IC, y=mean)) +
  geom_bar(stat="identity", fill="mediumpurple3", alpha=0.5)+
  scale_x_discrete(breaks=c("obj", "subj"),
                   labels=c("Object-IC", "Subject-IC")) + labs(y="Subject Preference", x='IC Verb Bias',title="English and Chinese Implicit Causality Verb Bias by Model\nFine-Tuned with Pro Drop") +
  ylim(-1, 1) +  theme(text = element_text(size=15)) +
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), position=position_dodge(.9), width=0.08, colour="black", alpha=0.9, size=.9) +
  facet_wrap(~model)

en_zh_pro_plots  

ggsave(paste(path,"figures/English-Chinese-pro-IC.png", sep=""), width=8)


########################
#### Italian Human #####
########################

human_bias <- it_data %>%
  filter(antecedent=='subj' & gender =='f' & item == 0 & model == 'Italian BERT')

#Make bias like ferstl bias
N1 <- human_bias$bias
N2 <- 100-N1
bias <- 100*(N1-N2)/(N1+N2)
human_bias$bias <- bias

human_bias$pref <- human_bias$bias*.01

human_it_sum <- human_bias %>%
  group_by(IC) %>%
  summarise( 
    n=n(),
    mean=mean(pref),
    sd=sd(pref)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ci=se * qt((1-0.05)/2 + .5, n-1)) %>% as.data.frame()

########################
### Italian Models #####
########################

it_model_data <- it_data 

model_it_subj <- subset(it_model_data, antecedent=='subj')
model_it_obj <- subset(it_model_data, antecedent=='obj')

#Remove non-informative columns for grouping
model_it <- model_it_subj %>% select(exp, verb, item, sent, bias, gender, IC, model)

#Take difference between probability of subject pronoun - object pronoun
model_it$pref <- model_it_subj$prob - model_it_obj$prob

model_it_sum <- model_it %>% 
  group_by(IC, model) %>%
  summarise(
    n=n(), 
    mean = mean(pref), 
    sd = sd(pref)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ci=se * qt((1-0.05)/2 + .5, n-1)) %>% as.data.frame()

##Combine with human 
human_it_sum$model <- c("Italian Human")

it_sum <- rbind(human_it_sum, model_it_sum)

########################
#### Spanish Human #####
########################

human_bias <- es_data %>%
  filter(antecedent=='subj' & gender =='f' & item == 0 & model == 'Spanish BERT')

#Make bias like ferstl bias
N1 <- human_bias$bias
N2 <- 100-N1
bias <- 100*(N1-N2)/(N1+N2)
human_bias$bias <- bias


human_bias$pref <- (human_bias$bias)*.01

human_es_sum <- human_bias %>%
  group_by(IC) %>%
  summarise( 
    n=n(),
    mean=mean(pref),
    sd=sd(pref)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ci=se * qt((1-0.05)/2 + .5, n-1)) %>% as.data.frame()

########################
### Spanish Models #####
########################

es_model_data <- es_data 

model_es_subj <- subset(es_model_data, antecedent=='subj')
model_es_obj <- subset(es_model_data, antecedent=='obj')

#Remove non-informative columns for grouping
model_es <- model_es_subj %>% select(exp, verb, item, sent, bias, gender, IC, model)

#Take difference between probability of subject pronoun - object pronoun
model_es$pref <- model_es_subj$prob - model_es_obj$prob

model_es_sum <- model_es %>% 
  group_by(IC, model) %>%
  summarise(
    n=n(), 
    mean = mean(pref), 
    sd = sd(pref)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ci=se * qt((1-0.05)/2 + .5, n-1)) %>% as.data.frame()

##Combine with human 
human_es_sum$model <- c("Spanish Human")

es_sum <- rbind(human_es_sum, model_es_sum)

#####################################
#### Italian/Spanish Figure 3.5 #####
#####################################

it_es_sum <- rbind(it_sum, es_sum)

it_es_og_plots <- it_es_sum %>%
  filter(!model %in% c("Italian BERT pro", "Italian BERT base", 
                       "Italian UmBERTo base", "Italian UmBERTo pro","Italian GilBERTo base", "Italian GilBERTo pro", 
                       "Spanish BERT pro", "Spanish BERT base", "Spanish RoBERTa pro", "Spanish RoBERTa base", 'mBERT', "Spanish BERT")) %>%
  mutate(across(model, factor, levels=c("Italian Human", "Italian BERT", "Italian UmBERTo", "Italian GilBERTo", 
                                        "Spanish Human", "Spanish RoBERTa"))) %>%
  ggplot(aes(x=IC, y=mean)) +
  geom_bar(stat="identity", fill="mediumpurple3", alpha=0.5)+
  scale_x_discrete(breaks=c("obj", "subj"),
                   labels=c("Object-IC", "Subject-IC")) + labs(y="Subject Preference", x='IC Verb Bias',title="Spanish and Italian Implicit Causality Verb Bias by Model") +
  ylim(-1, 1) +  theme(text = element_text(size=15)) +
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), position=position_dodge(.9), width=0.08, colour="black", alpha=0.9, size=.9) +
  facet_wrap(~model)

it_es_og_plots 

ggsave(paste(path,"figures/Spanish-Italian-og-IC.png", sep=""), width=8)

#####################################
#### Italian/Spanish Figure 3.6 #####
#####################################

it_es_sum <- rbind(it_sum, es_sum)

it_es_pro_plots <- it_es_sum %>%
  filter(model %in% c("Italian Human", "Italian BERT pro", "Italian UmBERTo pro", "Italian GilBERTo pro", 
                       "Spanish RoBERTa pro", "Spanish Human")) %>%
  mutate(across(model, factor, levels=c("Italian Human", "Italian BERT pro", "Italian UmBERTo pro", "Italian GilBERTo pro", 
                                        "Spanish Human", "Spanish RoBERTa pro"))) %>%
    mutate(model = recode(model, "Italian BERT pro"="Italian BERT", 
                          "Italian UmBERTo pro"="Italian UmBERTo", 
                          "Italian GilBERTo pro"="Italian GilBERTo", 
                          "Spanish RoBERTa pro"="Spanish RoBERTa")) %>%
  ggplot(aes(x=IC, y=mean)) +
  geom_bar(stat="identity", fill="mediumpurple3", alpha=0.5)+
  scale_x_discrete(breaks=c("obj", "subj"),
                   labels=c("Object-IC", "Subject-IC")) + labs(y="Subject Preference ", x='IC Verb Bias',title="Spanish and Italian Implicit Causality Verb Bias by Model\nFine-Tuned with no Pro Drop") +
  ylim(-1, 1) +  theme(text = element_text(size=15)) +
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), position=position_dodge(.9), width=0.08, colour="black", alpha=0.9, size=.9) +
  facet_wrap(~model)

it_es_pro_plots 

ggsave(paste(path,"figures/Spanish-Italian-pro-IC.png", sep=""), width=8)

