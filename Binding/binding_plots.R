library(ggplot2)
library(dplyr)
library(cowplot)
library(tidyverse)
library(ggrepel)

path = "/home/forrestdavis/Projects/Dissertation/Binding/"

######################
## For Figure 5.1
######################

################
###Format Data##
################

exp2 <- read.csv(paste(path, "results/Chow_Exp1_combined_flat.csv", sep=''))

#Filter out individual LSTMs and focus on average
exp2 <- exp2 %>% filter(!grepl("LSTM", model))

exp2_him <- exp2 %>% filter(pronoun == "him")
exp2_his <- exp2 %>% filter(pronoun != "him")

exp2_him_MM <- exp2_him %>% filter(contrast=="Match_Match")
exp2_him_MF <- exp2_him %>% filter(contrast=="Match_Mismatch")
exp2_him_FM <- exp2_him %>% filter(contrast=="Mismatch_Match")
exp2_him_FF <- exp2_him %>% filter(contrast=="Mismatch_Mismatch")

exp2_his_MM <- exp2_his %>% filter(contrast=="Match_Match")
exp2_his_MF <- exp2_his %>% filter(contrast=="Match_Mismatch")
exp2_his_FM <- exp2_his %>% filter(contrast=="Mismatch_Match")
exp2_his_FF <- exp2_his %>% filter(contrast=="Mismatch_Mismatch")

exp2_him_Matrix_GMME <- c(exp2_him_FM$surp-exp2_him_MM$surp, exp2_him_FF$surp-exp2_him_MF$surp)
exp2_him_Embed_GMME <- c(exp2_him_FF$surp-exp2_him_FM$surp, exp2_him_MF$surp-exp2_him_MM$surp)

exp2_his_Matrix_GMME <- c(exp2_his_FM$surp-exp2_his_MM$surp, exp2_his_FF$surp-exp2_his_MF$surp)
exp2_his_Embed_GMME <- c(exp2_his_FF$surp-exp2_his_FM$surp, exp2_his_MF$surp-exp2_his_MM$surp)

#Get GMME for him for both Matrix and Embed in one dataframe
exp2_him_Matrix <- rbind(exp2_him_FM, exp2_him_FF) %>% select(exp, cond, contrast, matrix, embedded, pronoun, sent, target, model, surp)
exp2_him_Matrix$contrast = "Matrix"

if(length(exp2_him_Matrix$contrast) != length(exp2_him_Matrix_GMME)){
  print("Error Stop!!!!!!!!!!")
}else{
  exp2_him_Matrix$GMME = exp2_him_Matrix_GMME 
}

exp2_him_Embed <- rbind(exp2_him_FF, exp2_him_MF) %>% select(exp, cond, contrast, matrix, embedded, pronoun, sent, target, model, surp)
exp2_him_Embed$contrast = "Embedded"

if(length(exp2_him_Embed$contrast) != length(exp2_him_Embed_GMME)){
  print("Error Stop!!!!!!!!!!")
}else{
  exp2_him_Embed$GMME = exp2_him_Embed_GMME 
}

exp2_him <- rbind(exp2_him_Matrix, exp2_him_Embed)  

#Get GMME for his for both Matrix and Embed in one dataframe
exp2_his_Matrix <- rbind(exp2_his_FM, exp2_his_FF) %>% select(exp, cond, contrast, matrix, embedded, pronoun, sent, target, model, surp)
exp2_his_Matrix$contrast = "Matrix"

if(length(exp2_his_Matrix$contrast) != length(exp2_his_Matrix_GMME)){
  print("Error Stop!!!!!!!!!!")
}else{
  exp2_his_Matrix$GMME = exp2_his_Matrix_GMME 
}

exp2_his_Embed <- rbind(exp2_his_FF, exp2_his_MF) %>% select(exp, cond, contrast, matrix, embedded, pronoun, sent, target, model, surp)
exp2_his_Embed$contrast = "Embedded"

if(length(exp2_his_Embed$contrast) != length(exp2_his_Embed_GMME)){
  print("Error Stop!!!!!!!!!!")
}else{
  exp2_his_Embed$GMME = exp2_his_Embed_GMME 
}

exp2_his <- rbind(exp2_his_Matrix, exp2_his_Embed)  

exp2GMME <- rbind(exp2_him, exp2_his)

### Get summary info and plot
exp2GMME_sum <- exp2GMME %>%
  group_by(contrast, pronoun, model) %>%
  summarise( 
    n=n(),
    mean=mean(GMME),
    sd=sd(GMME)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ci=se * qt((1-0.05)/2 + .5, n-1)) %>% as.data.frame()

######
#PLOT#
######

chowplot <- exp2GMME_sum %>%  
  mutate(across(model, factor, levels=c("bert-base-uncased","roberta-base", "gpt2-xl", "transfo-xl-wt103", "lstm avg"))) %>%
  mutate(model = recode(model, "gpt2-xl"="GPT-2 XL", 
                        "bert-base-uncased"="BERT", 
                        "roberta-base"="RoBERTa", "lstm avg"="LSTM avg", 
                        "transfo-xl-wt103"="TransformerXL")) %>%
  mutate(pronoun = recode(pronoun, "him"="Object",
                          "his"="Poss")) %>%
  ggplot(aes(x=contrast, y=mean, fill=pronoun)) +
  scale_x_discrete(limits = c("Matrix", "Embedded"))+
  labs(y="Gender Mismatch Effect (bits)", x='Position in Sentence',title="Gender Mismatch Effect by Model and Position", fill="Pronoun") +
  geom_bar(position=position_dodge(), stat="identity", alpha=0.7) +
  ylim(-2, 5.5) + theme(text = element_text(size=20)) + 
  scale_fill_manual(values=c('mediumpurple3', 
                             "#E69F00"))+
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), position=position_dodge(.9), width=0.08, colour="black", alpha=0.9, size=.9)  +
  facet_wrap(~model)

chowplot

ggsave(paste(path,"figures/Chow.png", sep=""), width=8)

######################
## For Figure 5.2
######################

################
###Format Data##
################

exp3 <- read.csv(paste(path, "results/Nicol_Exp_combined_flat.csv", sep=''))

#Filter out individual LSTMs and focus on average
exp3 <- exp3 %>% filter(!grepl("LSTM", model))

exp3_him <- exp3 %>% filter(target == "him")

exp3_him_MMM <- exp3_him %>% filter(contrast=="Match_Match_Match")
exp3_him_MMF <- exp3_him %>% filter(contrast=="Match_Match_Mismatch")
exp3_him_MFM <- exp3_him %>% filter(contrast=="Match_Mismatch_Match")
exp3_him_MFF <- exp3_him %>% filter(contrast=="Match_Mismatch_Mismatch")
exp3_him_FMM <- exp3_him %>% filter(contrast=="Mismatch_Match_Match")
exp3_him_FMF <- exp3_him %>% filter(contrast=="Mismatch_Match_Mismatch")
exp3_him_FFM <- exp3_him %>% filter(contrast=="Mismatch_Mismatch_Match")
exp3_him_FFF <- exp3_him %>% filter(contrast=="Mismatch_Mismatch_Mismatch")

exp3_him_Matrix_GMME <- c(exp3_him_FMM$surp-exp3_him_MMM$surp, 
                          exp3_him_FMF$surp-exp3_him_MMF$surp, 
                          exp3_him_FFM$surp-exp3_him_MFM$surp, 
                          exp3_him_FFF$surp-exp3_him_MFF$surp)

exp3_him_Middle_GMME <- c(exp3_him_MFM$surp-exp3_him_MMM$surp, 
                          exp3_him_MFF$surp-exp3_him_MMF$surp, 
                          exp3_him_FFM$surp-exp3_him_FMM$surp, 
                          exp3_him_FFF$surp-exp3_him_FMF$surp)

exp3_him_Embed_GMME <- c(exp3_him_MMF$surp-exp3_him_MMM$surp, 
                         exp3_him_MFF$surp-exp3_him_MFM$surp, 
                         exp3_him_FMF$surp-exp3_him_FMM$surp, 
                         exp3_him_FFF$surp-exp3_him_FFM$surp)

#Get GMME for him for both Matrix and Embed in one dataframe
exp3_him_Matrix <- rbind(exp3_him_FMM, exp3_him_FMF, exp3_him_FFM, exp3_him_FFF) %>% select(item, exp, contrast, target, sent, model)
exp3_him_Matrix$contrast = "Matrix"

if(length(exp3_him_Matrix$contrast) != length(exp3_him_Matrix_GMME)){
  print("Error Stop!!!!!!!!!!")
}else{
  exp3_him_Matrix$GMME = exp3_him_Matrix_GMME 
}

exp3_him_Middle <- rbind(exp3_him_MMF, exp3_him_MFF, exp3_him_FMF, exp3_him_FFF) %>% select(item, exp, contrast, target, sent, model)
exp3_him_Middle$contrast = "Middle"

if(length(exp3_him_Middle$contrast) != length(exp3_him_Middle_GMME)){
  print("Error Stop!!!!!!!!!!")
}else{
  exp3_him_Middle$GMME = exp3_him_Middle_GMME 
}

exp3_him_Embed <- rbind(exp3_him_MFM, exp3_him_MFF, exp3_him_FFM, exp3_him_FFF) %>% select(item, exp, contrast, target, sent, model)
exp3_him_Embed$contrast = "Embedded"

if(length(exp3_him_Embed$contrast) != length(exp3_him_Embed_GMME)){
  print("Error Stop!!!!!!!!!!")
}else{
  exp3_him_Embed$GMME = exp3_him_Embed_GMME 
}

exp3GMME <- rbind(exp3_him_Matrix, exp3_him_Middle, exp3_him_Embed)  

### Get summary info and plot
exp3GMME_sum <- exp3GMME %>%
  group_by(contrast, model) %>%
  summarise( 
    n=n(),
    mean=mean(GMME),
    sd=sd(GMME)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ci=se * qt((1-0.05)/2 + .5, n-1)) %>% as.data.frame()


######
#PLOT#
######

nicolplot <- exp3GMME_sum %>%  
  mutate(across(model, factor, levels=c("bert-base-uncased","roberta-base", "gpt2-xl", "transfo-xl-wt103", "lstm avg"))) %>%
  mutate(model = recode(model, "gpt2-xl"="GPT-2 XL", 
                        "bert-base-uncased"="BERT", 
                        "roberta-base"="RoBERTa", "lstm avg"="LSTM avg", 
                        "transfo-xl-wt103"="TransformerXL")) %>%
  #mutate(pronoun = recode(target, "him"="Object")) %>%
  ggplot(aes(x=contrast, y=mean)) +
  scale_x_discrete(limits = c("Matrix", "Middle", "Embedded"), labels=c("Matrix", "Object", "Embedded"))+
  labs(y="Gender Mismatch Effect (bits)", x='Position in Sentence',title="Object Pronoun Gender Mismatch Effect by Model and Position", fill="Pronoun") +
  geom_bar(position=position_dodge(), stat="identity", alpha=0.7, fill="mediumpurple3") +
  ylim(-2, 5.5) + theme(text = element_text(size=20)) + 
  #scale_fill_manual(values=c('mediumpurple3'))+
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), position=position_dodge(.9), width=0.08, colour="black", alpha=0.9, size=.9)  +
  facet_wrap(~model)

nicolplot

ggsave(paste(path,"figures/Nicol.png", sep=""), width=10.5)

######################
## For Figure 5.3
######################

################
###Format Data##
################

exp5 <- read.csv(paste(path, "results/vanGompel_Liversedge_Exp1_combined_flat.csv", sep=''))


#Filter out individual LSTMs and focus on average
exp5 <- exp5 %>% filter(!grepl("LSTM", model))

exp5_he_Match <- exp5 %>% filter(pronoun == 'he' & contrast=="Match")
exp5_he_Mismatch <- exp5 %>% filter(pronoun == 'he' & contrast!="Match")
exp5_she_Match <- exp5 %>% filter(pronoun != 'he' & contrast=="Match")
exp5_she_Mismatch <- exp5 %>% filter(pronoun != 'he' & contrast!="Match")

exp5_he_GMME <- exp5_he_Mismatch$surp - exp5_he_Match$surp
exp5_she_GMME <- exp5_she_Mismatch$surp - exp5_she_Match$surp

exp5_he <- exp5_he_Mismatch %>% select(item, exp, pronoun, cond, contrast, sent, target, model)
exp5_she <- exp5_she_Mismatch %>% select(item, exp, pronoun, cond, contrast, sent, target, model)

exp5GMME <- rbind(exp5_he, exp5_she)  

exp5GMME$GMME = c(exp5_he_GMME, exp5_she_GMME)

exp5GMME_sum <- exp5GMME %>%
  group_by(model) %>%
  summarise( 
    n=n(),
    mean=mean(GMME),
    sd=sd(GMME)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ci=se * qt((1-0.05)/2 + .5, n-1)) %>% as.data.frame()

#add dummy variable for facet wrap
exp5GMME_sum$lang='EN'


######
#PLOT#
######

vanGompelplot <- exp5GMME_sum %>%   
  mutate(across(model, factor, levels=c("bert-base-uncased","roberta-base", "gpt2-xl", "transfo-xl-wt103", "lstm avg"))) %>%
  mutate(model = recode(model, "gpt2-xl"="GPT-2 XL", 
                        "bert-base-uncased"="BERT", 
                        "roberta-base"="RoBERTa", "lstm avg"="LSTM avg", 
                        "transfo-xl-wt103"="TransformerXL")) %>%
  ggplot(aes(x=lang, y=mean)) +
  geom_bar(position=position_dodge(), stat="identity", alpha=0.7, fill="mediumpurple3") +
  ylim(-2, 5) + theme(text = element_text(size=20)) +  
  theme(axis.text.x=element_blank(), axis.ticks=element_blank()) +
  labs(y="Gender Mismatch Effect (bits)", x='') +
  labs(y="Gender Mismatch Effect (bits)", x='',title="Gender Mismatch Effect for Subject Cataphora") +
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), position=position_dodge(.9), width=0.08, colour="black", alpha=0.9, size=.9)+
  facet_wrap(~model)

vanGompelplot

ggsave(paste(path,"figures/vanGompel_Exp1.png", sep=""), width=8)


######################
## For Figure 5.4
######################

################
###Format Data##
################

exp6 <- read.csv(paste(path, "results/Kush_Dillon_Exp1_combined_flat.csv", sep=''))


#Filter out individual LSTMs and focus on average
exp6 <- exp6 %>% filter(!grepl("LSTM", model))

exp6_Match <- exp6 %>% filter(contrast=="Match")
exp6_Mismatch <- exp6 %>% filter(contrast!="Match")

exp6_GMME <- exp6_Mismatch$surp - exp6_Match$surp

exp6GMME <- exp6_Mismatch %>% select(item, cond, adjunct, contrast, sent, target, model)

if(length(exp6GMME$contrast) != length(exp6_GMME)){
  print("Error Stop!!!!!!!!!!")
}else{
  exp6GMME$GMME = exp6_GMME 
}

exp6GMME_sum <- exp6GMME %>%
  group_by(adjunct, model) %>%
  summarise( 
    n=n(),
    mean=mean(GMME),
    sd=sd(GMME)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ci=se * qt((1-0.05)/2 + .5, n-1)) %>% as.data.frame()

######
#PLOT#
######

KD1plot <- exp6GMME_sum %>%
  mutate(across(model, factor, levels=c("bert-base-uncased","roberta-base", "gpt2-xl", "transfo-xl-wt103", "lstm avg"))) %>%
  mutate(model = recode(model, "gpt2-xl"="GPT-2 XL", 
                        "bert-base-uncased"="BERT", 
                        "roberta-base"="RoBERTa", "lstm avg"="LSTM avg", 
                        "transfo-xl-wt103"="TransformerXL")) %>%
  ggplot(aes(x=adjunct, y=mean)) +
  scale_x_discrete(breaks=c("No Constraint", "Constraint"),
                   labels=c("No Principle B", "Principle B"))+
  labs(y="Gender Mismatch Effect (bits)", x='Experiment Condition', title="Gender Mismatch Effect for Object Catphora") +
  geom_bar(position=position_dodge(), stat="identity",  fill="mediumpurple3",alpha=0.7) +
  ylim(-2, 5.5) + theme(text = element_text(size=20)) + 
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), position=position_dodge(.9), width=0.08, colour="black", alpha=0.9, size=.9)  +
  facet_wrap(~model)

KD1plot 

ggsave(paste(path,"figures/Kush-Dillon-Exp1.png", sep=""), width = 10)


######################
## For Figure 5.5
######################

################
###Format Data##
################

exp7 <- read.csv(paste(path, "results/Kush_Dillon_Exp2_combined_flat.csv", sep=''))


#Filter out individual LSTMs and focus on average
exp7 <- exp7 %>% filter(!grepl("LSTM", model))

exp7_Match <- exp7 %>% filter(contrast=="Match")
exp7_Mismatch <- exp7 %>% filter(contrast!="Match")

exp7_GMME <- exp7_Mismatch$surp - exp7_Match$surp

exp7GMME <- exp7_Mismatch %>% select(item, cond, adjunct, contrast, sent, target, model)

if(length(exp7GMME$contrast) != length(exp7_GMME)){
  print("Error Stop!!!!!!!!!!")
}else{
  exp7GMME$GMME = exp7_GMME 
}

exp7GMME_sum <- exp7GMME %>%
  group_by(adjunct, model) %>%
  summarise( 
    n=n(),
    mean=mean(GMME),
    sd=sd(GMME)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ci=se * qt((1-0.05)/2 + .5, n-1)) %>% as.data.frame()

######
#PLOT#
######
KD2plot <- exp7GMME_sum %>%
  mutate(across(model, factor, levels=c("bert-base-uncased","roberta-base", "gpt2-xl", "transfo-xl-wt103", "lstm avg"))) %>%
  mutate(model = recode(model, "gpt2-xl"="GPT-2 XL", 
                        "bert-base-uncased"="BERT", 
                        "roberta-base"="RoBERTa", "lstm avg"="LSTM avg", 
                        "transfo-xl-wt103"="TransformerXL")) %>%
  ggplot(aes(x=adjunct, y=mean)) +
  scale_x_discrete(breaks=c("No Constraint", "Constraint"),
                   labels=c("No Principle B", "Principle B"))+
  labs(y="Gender Mismatch Effect (bits)", x='Experiment Condition', title="Gender Mismatch Effect for Object Catphora") +
  geom_bar(position=position_dodge(), stat="identity",  fill="mediumpurple3",alpha=0.7) +
  ylim(-2, 5.5) + theme(text = element_text(size=20)) + 
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), position=position_dodge(.9), width=0.08, colour="black", alpha=0.9, size=.9)  +
  facet_wrap(~model)

KD2plot 

ggsave(paste(path,"figures/Kush-Dillon-Exp2.png", sep=""), width = 10)


######################
## For Figure 5.6
######################

#Focusing on Exp2
#For humans in Kush and Dillon (2021), in the Constraint condition there was a 
#gender mismatch effect of -21ms (that is, when the pronoun mismatched reading times were shorter)
#For the no Constraint condition the effect was +63ms

##Read in GPT2 xl Kush and Dillon experiment 2 results
kush_2 <- read.csv(paste(path, "results/Kush_Dillon_Exp2_combined_flat.csv", sep=""))

kush_2$contrast <- ifelse(kush_2$contrast=='Match', 1, -1)
kush_2$adjunct <- ifelse(kush_2$adjunct == 'Constraint', 1, -1)
kush_2$item <- as.factor(kush_2$adjunct )
kush_2$pronoun <- as.factor(kush_2$pronoun )


###### Get the ms/bit scalar term for GPT-2  (it's a bit slow)

modelfname = paste(path, "results/gpt2xl_surp_model.rda", sep="")
if(file.exists(modelfname)) {
  load(modelfname)
}else{
  gpt2_surp_model = surp_data %>%
    mutate(subj = as.factor(subj),
           length = as.integer(length),
           rt = as.integer(rt), 
           Text = as.factor(Text)) %>%
    lmer(rt ~ gpt2.xl_surp + length + (gpt2.xl_surp + length||subj) + (gpt2.xl_surp + length || Text), data = ., REML = FALSE,
         control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
  save(gpt2_surp_model, file=modelfname)
}

summary(gpt2_surp_model)

###get predicted effect sizes for gpt2 xl for exp 2
gpt2_model = kush_2 %>%
  filter(model == 'gpt2-xl') %>%
  lmer(surp ~ contrast*adjunct + (1 | item), 
       data = ., REML = FALSE,
       control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

Constraint = predict(gpt2_model, data.frame(adjunct=c(1), contrast=c(-1)), re.form=NA) - predict(gpt2_model, data.frame(adjunct=c(1), contrast=c(1)), re.form=NA)
NoConstraint = predict(gpt2_model, data.frame(adjunct=c(-1), contrast=c(-1)), re.form=NA) - predict(gpt2_model, data.frame(adjunct=c(-1), contrast=c(1)), re.form=NA)

#Map to RT
Constraint =  coef(summary(gpt2_surp_model))[2,1]*Constraint
NoConstraint =  coef(summary(gpt2_surp_model))[2,1]*NoConstraint


#Plot the humans and GPTxl
new_data <- data.frame("cond"= c("human", "human", "gpt2 xl", "gpt2 xl"), 
                       "pronoun" = c("No Constraint", "Principle B", "No Constraint", "Principle B"), 
                       "RT" = c(63, -21, NoConstraint, Constraint ))


GMME <- ggplot(new_data, aes(x=pronoun,y=RT, col=cond))+
  geom_point(shape = 21,fill = "white", size = 10, stroke = 2)+
  scale_color_manual(name="",values=c( "#00204E","#999999"),labels=c("GPT-2 XL","Human")) + ylim(-30, 70)+
  theme(text = element_text(size=20)) + 
  labs(y="Gender Mismatch Effect RT (ms)",x="Experimental Condition",title='Gender Mismatch Effect with and without\nPrinciple B violations') + 
  geom_hline(yintercept=0,linetype=2)

GMME

ggsave(paste(path,"figures/GMME.png", sep=""))

