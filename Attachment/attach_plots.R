library(ggplot2)
library(dplyr)
library(cowplot)
library(tidyverse)
library(ggrepel)


#########################################
######## Attachment Preferences #########
#########################################
path = "/home/forrestdavis/Projects/Dissertation/Attachment/"

en_FCM <- read.csv(paste(path, "results/FCM_English_combined_flat.csv", sep=''))
es_FCM <- read.csv(paste(path, "results/FCM_Spanish_combined_flat.csv", sep=''))

en_Gilboy <- read.csv(paste(path, "results/Gilboy_etal_1995_English_combined_flat.csv", sep=''))
es_Gilboy <- read.csv(paste(path, "results/Gilboy_etal_1995_Spanish_combined_flat.csv", sep=''))

en_Rohde <- read.csv(paste(path, "results/Rohde_etal_2011_English_combined_flat.csv", sep=''))
es_CC <- read.csv(paste(path, "results/Carreiras_Clifton_1993_Spanish_combined_flat.csv", sep=''))


###################################
###### Spanish Figure 4.1 #########
###################################

################
###Format Data##
################

es_lstm_data <- es_FCM %>% filter(grepl("lstm", model)) %>% filter(!grepl("lstm avg", model))

#Add item column (repeats per model)
es_lstm_data$groupItem <- 1:192

#Get lstm average by item
es_lstm_sum <- es_lstm_data %>%
  group_by(groupItem) %>%
  summarise(
    n=n(), 
    mean = mean(prob), 
    sd = sd(prob)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ci=se * qt((1-0.05)/2 + .5, n-1)) %>% as.data.frame()

#Create data entry for LSTM avg
es_lstm_data <- es_lstm_data %>% filter(model=="es_lstm_hidden650_batch64_dropout0.2_lr20_0.pt")
es_lstm_data$model <- c("LSTM avg")
es_lstm_data$prob <- es_lstm_sum$mean

es_lstm_data <- es_lstm_data %>% select(item, lang, exp, AttachType, verbNum, N1_changed, N2_changed, N1_full, N2_full, N1, N2, mask_sents, auto_sents, target, sent, model, prob)


es_model_data <- es_FCM %>% filter(!grepl("lstm", model))
es_model_data <- rbind(es_model_data, es_lstm_data)

#Get Low - High and add Bias of 1 if Low > High per item
es_low <- es_model_data %>% filter(AttachType=="low")
es_high <- es_model_data %>% filter(AttachType=="high")

es_model_data <- es_model_data %>% filter(AttachType =='low')
es_model_data$prob <- es_low$prob - es_high$prob
es_model_data$bias <- as.numeric(es_model_data$prob > 0)

#Summarize bias by model
es_sum <- es_model_data %>%
  group_by(model) %>%
  summarise(
    low = sum(bias)/length(bias), 
  ) %>% as.data.frame()

#Add Human
human_attach_data <- data.frame("model"= c("Spanish Human"),
                                "low" = c(0.40))

es_sum <- rbind(es_sum, human_attach_data)
es_sum$lang <- "Spanish"

######
#PLOT#
######
es_FCM_plot <- es_sum %>% 
  mutate(across(model, factor, levels=c("Spanish Human", "bert-base-spanish-wwm-uncased", "RuPERTa-base", "spanish-gpt2", "gpt2-spanish", "LSTM avg"))) %>%
  mutate(model = recode(model, "bert-base-spanish-wwm-uncased"="Spanish BERT", 
                        "gpt2-spanish"="GPT-2 Spanish",
                        "spanish-gpt2"="Spanish GPT-2", 
                        "RuPERTa-base"="Spanish RoBERTa")) %>%
  ggplot(aes(x=lang, y=low))+
         geom_bar(stat="identity", fill="mediumpurple3", alpha=0.5) +  
         ylim(0, 1) + 
  labs(y="Proporition of Low Attachment", x='',title="Spanish Attachment Preference by Model") +
  theme(axis.text.x=element_blank(), axis.ticks=element_blank()) +
  geom_hline(yintercept=.50,linetype=2)+ 
  theme(text=element_text(size=15)) +
  facet_wrap(~model)

es_FCM_plot

ggsave(paste(path,"figures/FCM-Spanish-Attach.png", sep=""))

###################################
###### English Figure 4.2 #########
###################################

################
###Format Data##
################

en_lstm_data <- en_FCM %>% filter(grepl("LSTM", model)) %>% filter(!grepl("lstm avg", model))

#Add item column (repeats per model)
en_lstm_data$groupItem <- 1:192

#Get lstm average by item
en_lstm_sum <- en_lstm_data %>%
  group_by(groupItem) %>%
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

en_lstm_data <- en_lstm_data %>% select(item, lang, exp, AttachType, verbNum, N1_changed, N2_changed, N1_full, N2_full, N1, N2, mask_sents, auto_sents, target, sent, model, prob)


en_model_data <- en_FCM %>% filter(!grepl("lstm", model)) %>% filter(!grepl("LSTM", model))
en_model_data <- rbind(en_model_data, en_lstm_data)

#Get Low - High and add Bias of 1 if Low > High per item
en_low <- en_model_data %>% filter(AttachType=="low")
en_high <- en_model_data %>% filter(AttachType=="high")

en_model_data <- en_model_data %>% filter(AttachType =='low')
en_model_data$prob <- en_low$prob - en_high$prob
en_model_data$bias <- as.numeric(en_model_data$prob > 0)

#Summarize bias by model
en_sum <- en_model_data %>%
  group_by(model) %>%
  summarise(
    low = sum(bias)/length(bias), 
  ) %>% as.data.frame()

#Add Human
human_attach_data <- data.frame("model"= c("English Human"),
                                "low" = c(0.63))

en_sum <- rbind(en_sum, human_attach_data)
en_sum$lang <- "English"

######
#PLOT#
######
en_FCM_plot <- en_sum %>% 
  mutate(across(model, factor, levels=c("English Human", "bert-base-uncased", "roberta-base", "gpt2-xl", "LSTM avg"))) %>%
  mutate(model = recode(model, "bert-base-uncased"="BERT", 
                        "gpt2-xl"="GPT-2 XL", 
                        "roberta-base"="RoBERTa")) %>%
  ggplot(aes(x=lang, y=low))+
  geom_bar(stat="identity", fill="mediumpurple3", alpha=0.5) +  
  ylim(0, 1) + 
  labs(y="Proporition of Low Attachment", x='',title="English Attachment Preference by Model") +
  theme(axis.text.x=element_blank(), axis.ticks=element_blank()) +
  geom_hline(yintercept=0.50,linetype=2)+ 
  theme(text=element_text(size=15)) +
  facet_wrap(~model)

en_FCM_plot

ggsave(paste(path,"figures/FCM-English-Attach.png", sep=""))


###################################
###### Spanish Figure 4.3 #########
###################################

################
###Format Data##
################

#Extract human results
es_human_data <- es_Gilboy %>% filter(model == 'lstm avg' & N2PropSource=='human')
es_human_data <- es_human_data %>% select(item, lang, expCat, expCat, sent, model, N2Prop)
es_human_data$model <- "Spanish Human"

#Extract model results
es_model_data <- es_Gilboy %>% filter(!grepl("lstm_", model)) %>% filter(N2PropSource!="human")
es_model_data <- es_model_data %>% select(item, lang, expCat, expCat, sent, model, N2Prop)

es_model_data <- rbind(es_model_data, es_human_data)

#Summarize bias by model
es_sum <- es_model_data %>% 
  group_by(model, expCat) %>%
  summarise(
    n=n(), 
    mean = mean(N2Prop), 
    sd = sd(N2Prop)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ci=se * qt((1-0.05)/2 + .5, n-1)) %>% as.data.frame()

######
#PLOT#
######
es_Gilboy_plot <- es_sum %>%
  mutate(across(model, factor, levels=c("Spanish Human", "bert-base-spanish-wwm-uncased", "RuPERTa-base", "spanish-gpt2", "gpt2-spanish", "lstm avg"))) %>%
  mutate(model = recode(model, "bert-base-spanish-wwm-uncased"="Spanish BERT", 
                        "gpt2-spanish"="GPT-2 Spanish",
                        "spanish-gpt2"="Spanish GPT-2", 
                        "RuPERTa-base"="Spanish RoBERTa", 
                          "lstm avg"="LSTM avg")) %>%
  ggplot(aes(x=expCat, y=mean)) +
  geom_bar(stat="identity", fill="mediumpurple3", alpha=0.5) + 
  labs(y="Proportion of Low Attachment", x='Experiment Category',title="Spanish Fine-Grained Attachment Preference by Model") +
  theme(text = element_text(size=15)) +
  scale_y_continuous(breaks=seq(0,1.2,by=.2))+
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), position=position_dodge(.9), width=0.08, colour="black", alpha=0.9, size=.9) +
  geom_hline(yintercept=0.50,linetype=2)+ 
  facet_wrap(~model)

es_Gilboy_plot
ggsave(paste(path,"figures/Gilboy-Spanish-Attach.png", sep=""))

###################################
###### English Figure 4.4 #########
###################################

################
###Format Data##
################

#Extract human results
en_human_data <- en_Gilboy %>% filter(model == 'lstm avg' & N2PropSource=='human')
en_human_data <- en_human_data %>% select(item, lang, expCat, expCat, sent, model, N2Prop)
en_human_data$model <- "English Human"

#Extract model results
en_model_data <- en_Gilboy %>% filter(!grepl("LSTM", model)) %>% filter(N2PropSource!="human")
en_model_data <- en_model_data %>% select(item, lang, expCat, expCat, sent, model, N2Prop)

en_model_data <- rbind(en_model_data, en_human_data)

#Summarize bias by model
en_sum <- en_model_data %>% 
  group_by(model, expCat) %>%
  summarise(
    n=n(), 
    mean = mean(N2Prop), 
    sd = sd(N2Prop)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ci=se * qt((1-0.05)/2 + .5, n-1)) %>% as.data.frame()

######
#PLOT#
######
en_Gilboy_plot <- en_sum %>%
  mutate(across(model, factor, levels=c("English Human", "bert-base-uncased", "roberta-base", "gpt2-xl", "lstm avg"))) %>%
  mutate(model = recode(model, "bert-base-uncased"="BERT", 
                        "gpt2-xl"="GPT-2 XL", 
                        "roberta-base"="RoBERTa",
                        "lstm avg" = "LSTM avg")) %>%
  ggplot(aes(x=expCat, y=mean)) +
  geom_bar(stat="identity", fill="mediumpurple3", alpha=0.5) + 
  labs(y="Proportion of Low Attachment", x='Experiment Category',title="English Fine-Grained Attachment Preference by Model") +
  theme(text = element_text(size=15)) +
  scale_y_continuous(breaks=seq(0,1.2,by=.2))+
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), position=position_dodge(.9), width=0.08, colour="black", alpha=0.9, size=.9) +
  geom_hline(yintercept=0.50,linetype=2)+ 
  facet_wrap(~model)

en_Gilboy_plot

ggsave(paste(path,"figures/Gilboy-English-Attach.png", sep=""))

###################################
###### English Figure 4.5 #########
###################################

################
###Format Data##
################
en_lstm_data <-  en_Rohde %>% filter(grepl("LSTM", model)) %>% filter(!grepl("lstm avg", model))

#Add item column (repeats per model)
en_lstm_data$groupItem <- 1:160

#Get lstm average by item
en_lstm_sum <- en_lstm_data %>%
  group_by(groupItem) %>%
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

en_lstm_data <- en_lstm_data %>% select(item, lang, exp, AttachType, antecedent, hasIC, verb, verbNum, N1_full, N2_full, N1, N2, mask_sents, auto_sents, target, sent, model, prob)

#Other models
en_model_data <- en_Rohde %>% filter(!grepl("lstm", model)) %>% filter(!grepl("LSTM", model))
en_model_data <- rbind(en_model_data, en_lstm_data)

#Get Low - High and add Bias of 1 if Low > High per item
en_low <- en_model_data %>% filter(AttachType=="low")
en_high <- en_model_data %>% filter(AttachType=="high")

en_model_data <- en_model_data %>% filter(AttachType =='low')
en_model_data$prob <- en_low$prob - en_high$prob
en_model_data$bias <- as.numeric(en_model_data$prob > 0)

en_model_data %>% filter(model == 'bert-base-uncased', hasIC == 1) %>% summarize(low = sum(bias)/100)

#Summarize bias by model
en_sum <- en_model_data %>%
  group_by(model, hasIC) %>%
  summarise(
    low = sum(bias)/length(bias), 
  ) %>% as.data.frame()

en_sum$hasIC <- as.factor(en_sum$hasIC)

######
#PLOT#
######
en_Rohde_plot <- en_sum %>%
  mutate(across(model, factor, levels=c("English Human", "bert-base-uncased", "roberta-base", "gpt2-xl", "LSTM avg"))) %>%
  mutate(model = recode(model, "bert-base-uncased"="BERT", 
                        "gpt2-xl"="GPT-2 XL", 
                        "roberta-base"="RoBERTa")) %>%
  ggplot(aes(x=hasIC, y=low))+
  geom_bar(stat="identity", fill="mediumpurple3", alpha=0.5) +  
  ylim(0, 1) +  
  scale_x_discrete(breaks=c(0, 1),
                                  labels=c("nonIC Verb", "IC Verb")) + 
  labs(y="Proporition of Low Attachment", x='Verb Type',title="English Attachment Preference and Implicit Causality by Model") +
  geom_hline(yintercept=0.50,linetype=2)+ 
  theme(text=element_text(size=15)) +
  facet_wrap(~model)

en_Rohde_plot

ggsave(paste(path,"figures/Rohde-English-Attach.png", sep=""), width=8)

###################################
###### Spanish Figure 4.6 #########
###################################

################
###Format Data##
################

es_lstm_data <- es_CC %>% filter(grepl("lstm", model)) %>% filter(!grepl("lstm avg", model))

#Add item column (repeats per model)
es_lstm_data$groupItem <- 1:96

#Get lstm average by item
es_lstm_sum <- es_lstm_data %>%
  group_by(groupItem) %>%
  summarise(
    n=n(), 
    mean = mean(prob), 
    sd = sd(prob)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ci=se * qt((1-0.05)/2 + .5, n-1)) %>% as.data.frame()

#Create data entry for LSTM avg
es_lstm_data <- es_lstm_data %>% filter(model=="es_lstm_hidden650_batch64_dropout0.2_lr20_0.pt")
es_lstm_data$model <- c("LSTM avg")
es_lstm_data$prob <- es_lstm_sum$mean

es_lstm_data <- es_lstm_data %>% select(item, lang, exp, AttachType, gender, N1_changed, N2_changed, N1, N2, mask_sents, auto_sents, target, sent, model, prob)


es_model_data <- es_CC %>% filter(!grepl("lstm", model))
es_model_data <- rbind(es_model_data, es_lstm_data)

#Get Low - High and add Bias of 1 if Low > High per item
es_low <- es_model_data %>% filter(AttachType=="low")
es_high <- es_model_data %>% filter(AttachType=="high")

es_model_data <- es_model_data %>% filter(AttachType =='low')
es_model_data$prob <- es_low$prob - es_high$prob
es_model_data$bias <- as.numeric(es_model_data$prob > 0)

#Summarize bias by model
es_sum <- es_model_data %>%
  group_by(model) %>%
  summarise(
    low = sum(bias)/length(bias), 
  ) %>% as.data.frame()


es_sum$lang <- "Spanish"

######
#PLOT#
######
es_CC_plot <- es_sum %>% 
  mutate(across(model, factor, levels=c("Spanish Human", "bert-base-spanish-wwm-uncased", "RuPERTa-base", "spanish-gpt2", "gpt2-spanish", "LSTM avg"))) %>%
  mutate(model = recode(model, "bert-base-spanish-wwm-uncased"="Spanish BERT", 
                        "gpt2-spanish"="GPT-2 Spanish",
                        "spanish-gpt2"="Spanish GPT-2", 
                        "RuPERTa-base"="Spanish RoBERTa")) %>%
  ggplot(aes(x=lang, y=low))+
  geom_bar(stat="identity", fill="mediumpurple3", alpha=0.5) +  
  ylim(0, 1) + 
  labs(y="Proporition of Low Attachment", x='',title="Spanish Gender Agreement Attachment Preference by Model") +
  theme(axis.text.x=element_blank(), axis.ticks=element_blank()) +
  geom_hline(yintercept=.50,linetype=2)+ 
  theme(text=element_text(size=15)) +
  facet_wrap(~model)

es_CC_plot

ggsave(paste(path,"figures/Carreiras-Clifton-Spanish-Attach.png", sep=""), width=8)
