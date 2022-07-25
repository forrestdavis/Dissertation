# Principle B and Coreference

Code for replicating the "Principle B and Coreference" chapter of my
dissertation. See the README under ModelsAPI for information about
dependencies, models, and source code specifics.  LSTM models are used (and
trained) via [neural-complexity](https://github.com/vansky/neural-complexity).
The relevant files for ease of use (and to avoid issues in the future with any
changes to neural-complexity) are included under the directory
diss-neural-complexity, so you do not need to download neural-complexity.
Additionally, see Chapter 5 of my dissertation and the references below for
more information about the specific experiments. 

This chapter has 4 experiments: 

1. Based on [Chow et al. (2014)](https://doi.org/10.3389/fpsyg.2014.00630)
2. Based on [Nicol and Swinney (1989)](https://doi.org/10.1007/BF01069043)
3. Based on [van Gompel and Liversedge (2003)](https://doi.apa.org/doiLanding?doi=10.1037%2F0278-7393.29.1.128)
4. Based on [Kush and Dillon (2021)](https://www.sciencedirect.com/science/article/pii/S0749596X21000371)

## Running the Experiments

Running experiments is separated by model type: autoregressive models (GPT-2 XL
and TransformerXL), LSTM models, and non-autoregressive models (BERT and
RoBERTa). Config files specify the experiments, models, and some additional
parameters when passed to get\_results.py which will run the experiments. The
stimuli files for the 4 experiments are included under the directory stimuli.


Run the below code snippet and results files (csv files) will be saved under results. 

```
python get_results.py configs/auto_config.yaml && python get_results.py configs/lstm_config.yaml && python get_results.py configs/bert_config.yaml
```

## Plotting the Experiments 

Plotting experiments is done via binding\_plots.R. But first, the results from
the experiments have to be compiled into csv files containing results from each
model. This is down by running compile.py.

``` 
python compile.py
```

Then, open binding\_plots.R to recreate all the plots (saved under the directory
figures). 

## Statistics 

Statistical models and results are done via R with the script binding\_stats.R.
In addition to replicating the statistical results reported in the chapter, the
figure in the general discussion comparing human reading times and GPT-2 XL
surprisal values for Kush and Dillon (2021) is created at the bottom of this
script.

## References

Wing-Yee Chow, Shevaun Lewis, and Colin Phillips. 2014. [Immediate sensitivity to structural constraints in pronoun resolution.](https://doi.org/10.3389/fpsyg.2014.00630). *Frontiers in Psychology*. 

Dave Kush and Brian Dillon. 2021. [Principle B constraints the processing of cataphora: Evidence for syntactic and discourse predictions.](https://www.sciencedirect.com/science/article/pii/S0749596X21000371) *Journal of Memory and Language*.

Janet L. Nicol and David A. Swinney. 2003. [The role of structure in coreference assignment during sentence comprehension](https://doi.org/10.1007/BF01069043). *Journal of Psycholinguistic Research*. 

Roger P.G. van Compel and Simon P. Liversedge. 2003. [The Influence of Morphological Information on Cataphoric Pronoun Assignment.](https://doi.apa.org/doiLanding?doi=10.1037%2F0278-7393.29.1.128). *Journal of Experimental Psychology: Learning, Memory, and Cognition*. 
