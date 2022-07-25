# Implicit Causality

Code for replicating the "Implicit Causality" chapter of my
dissertation. See the README under ModelsAPI for information about
dependencies, models, and source code specifics.  LSTM models are used (and
trained) via [neural-complexity](https://github.com/vansky/neural-complexity).
The relevant files for ease of use (and to avoid issues in the future with any
changes to neural-complexity) are included under the directory
diss-neural-complexity, so you do not need to download neural-complexity.
Additionally, see Chapter 3 of my dissertation and the references below for
more information about the specific experiments. 

This chapter draws on 4 psycholinguistic experiments: 

1. [Ferstl er al. (2011)](https://link.springer.com/article/10.3758/s13428-010-0023-2)
2. [Goikoetxea et al. (2008)](https://link.springer.com/article/10.3758/BRM.40.3.760)
3. [Mannetti and De Grada (1991)](https://onlinelibrary.wiley.com/doi/10.1002/ejsp.2420210506)
4. [Harsthorne et al. (2013)](https://econtent.hogrefe.com/doi/10.1027/1618-3169/a000187)


## Running the Experiments

The code for replicating the experiments is on my repository for my published work on IC verb biases
in neural models. You can find it [here](https://github.com/forrestdavis/ImplicitCausality). For 
ease of use, all necessary result files are included under the results directory. 

## Plotting the Experiments 

The bulk of the plotting of experiments is done via IC\_plots.R. But first, the results from
the English experiments have to be compiled into one csv file containing results from each
model. This is down by running compile.py. Additionally, compile.py creates the two 
correlation plots between models and humans for English IC in the chapter. 

``` 
python compile.py
```

Then, open IC\_plots.R to recreate all the plots (saved under the directory
figures). 

## Statistics 

Statistical models and results are done via R with the script IC\_stats.R.

## References

Evelyn C. Ferstl, Alan Garnham, and Christina Manouilidou. 2011. [Implicit causality bias in English: A corpus of 300 verbs](https://link.springer.com/article/10.3758/s13428-010-0023-2). *Behavior Research Methods*.

Joshua K. Hartshorne, Yasutada Sudo, and Miki Uruwashi. 2013. [Are implicit causality pronoun resolution biases consistent across languages and cultures?](https://econtent.hogrefe.com/doi/10.1027/1618-3169/a000187). *Experimental Psychology*. 

Edurne Goikoetxea, Gema Pascual, and Joana Acha. 2008. [Normative study of the implicit causlity of 100 interpersonal verbs in Spanish](https://link.springer.com/article/10.3758/BRM.40.3.760). *Behavior Research Methods*.

Lucia Mannetti and Eraldo De Grada. 1991. [Interpersonal verbs: Implicit causality of action verbs and contextual factors](https://onlinelibrary.wiley.com/doi/10.1002/ejsp.2420210506). *European Journal of Social Psychology*. 

