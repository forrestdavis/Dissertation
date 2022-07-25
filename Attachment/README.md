# Ambiguous Relative Clause Attachment

Code for replicating the "Ambiguous Relative Clause Attachment" chapter of my
dissertation. See the README under ModelsAPI for information about
dependencies, models, and source code specifics.  LSTM models are used (and
trained) via [neural-complexity](https://github.com/vansky/neural-complexity).
The relevant files for ease of use (and to avoid issues in the future with any
changes to neural-complexity) are included under the directory
diss-neural-complexity, so you do not need to download neural-complexity.
Additionally, see Chapter 4 of my dissertation and the references below for
more information about the specific experiments. 

This chapter has 4 experiments: 

1. Based on [Cuetos and Mitchell (1988)](https://www.sciencedirect.com/science/article/abs/pii/0010027788900042?via%3Dihub) and [Fernández (2003)](https://benjamins.com/catalog/lald.29)
2. Based on [Gilboy et al. (1995)](https://www.sciencedirect.com/science/article/abs/pii/001002779400636Y?via%3Dihub)
3. Based on [Rohde et al. (2011)](https://www.sciencedirect.com/science/article/abs/pii/S0010027710002532?via%3Dihub)
4. Based on [Carreiras and Clifton (1993)](https://journals.sagepub.com/doi/10.1177/002383099303600401)

## Running the Experiments

Running experiments is separated by language (English as en and Spanish as es) and by model type: autoregressive models (GPT-2 XL), LSTM models, and non-autoregressive models (BERT and
RoBERTa). Config files specify the experiments, models, and some additional
parameters when passed to get\_results.py which will run the experiments. The
stimuli files for the 4 experiments are included under the directory stimuli. Additionally, 
verb lists for Spanish and English and an adjective list for Spanish are included under 
stimuli and are used to fill in wildcards used in the experiments. Experiment 3 is 
only English while experiment 4 is only Spanish.


For Spanish, run the below code snippet and results files (csv files) will be saved under results. 

```
python get_results.py configs/es_auto_config.yaml && python get_results.py configs/es_lstm_config.yaml && python get_results.py configs/es_bert_config.yaml
```

For English, run the below code snippet and the results files (csv files) will be saved under results.

```
python get_results.py configs/en_auto_config.yaml && python get_results.py configs/en_lstm_config.yaml && python get_results.py configs/en_bert_config.yaml
```

## Plotting the Experiments 

Plotting experiments is done via attach\_plots.R. But first, the results from
the experiments have to be compiled into csv files containing results from each
model. This is down by running compile.py. 

``` 
python compile.py
```

Then, open attach\_plots.R to recreate all the plots (saved under the directory
figures). 

## Statistics 

Statistical models and results are done via R with the script attach\_stats.R.

## References

Manuel Carreiras and Charles Clifton. 1993. [Relative Clause Interpretation Preferences in Spanish and English](https://journals.sagepub.com/doi/10.1177/002383099303600401). *Language and Speech*.

Fernando Cuetos and Don C. Mitchell. 1988. [Cross-linguistic differences in parsing: Restrictions on the use of the Late Closure strategy in Spanish](https://www.sciencedirect.com/science/article/abs/pii/0010027788900042?via%3Dihub). *Cognition*.

Eva M. Fernández. 2003. [Bilingual Sentence Processing: Relative Clause Attachment in English and Spanish](https://benjamins.com/catalog/lald.29). 

Elizabeth Gilboy, Josep-M Mariai Sopena, Charles Clifton, and Lyn Frazier. 1995. [Arugment structure and association preferences in Spanish and English complex NPs](https://www.sciencedirect.com/science/article/abs/pii/001002779400636Y?via%3Dihub). *Cognition*.

Hannah Rohde, Roger Levy, and Andrew Kehler. 2011. [Anticipating explanations in relative clause processing](https://www.sciencedirect.com/science/article/abs/pii/S0010027710002532?via%3Dihub). *Cognition*.
