Next Word Prediction App
========================================================
author: Enrique Pérez Herrero
date: 22/Apr/2016
autosize: true
font-family: 'Helvetica'

### COURSERA - Data Science Specialization Capstone.

This lightweight App *predicts last word* of an uncompleted English sentence, 
with precalculated `n-gram` tables parsed with a dictionary. 

### APP LINKS:

* [Next Word Prediction Shiny App](https://kikesoft.shinyapps.io/NEXT_WORD/)
* [GitHub url](https://github.com/EnriquePH/NLP-Coursera-Swiftkey)
* [Data Exploration](https://rpubs.com/eph_1500/Swiftkey)

1. Introduction
========================================================

Natural Language Processing or
[NLP](https://en.wikipedia.org/wiki/Natural_language_processing)
is a field of computer science with the interaction between computers and human
languages.
One on the oldest NLP problem related with computer word prediction is
[Claude Shannon's](https://en.wikipedia.org/wiki/Claude_Shannon)
problem of assigning a probability to a word, _Shannon_ used
[n-grams](https://en.wikipedia.org/wiki/N-gram),
defined as a contiguous sequence of `n` items, from a given sequence of text or
speech, to compute probabilities of English sentences.

Markov assumption states that the probability of a word depends only on
the most recent $n-1$ tokens, thus `n-grams` can be used to predict next word
probability. 

***

In a given text or corpus the conditional probability of seen a word $w_n$ using
_Maximum Likelihood Estimation_ (MLE) is:

$$P(w_n | w_1...w_{n-1}) = \frac{c(w_1...w_n)}{c(w_1...w_{n-1})}$$

There are several smoothing algorithms that uses precomputed probabilities
and back-off weights, the _Stupid Backoff_ method is suitable for web applications
and does not calculates normalized probabilities but relative frequencies.


2. Text Preprocess and n-grams
========================================================

### 2.1 DATA:

The data source belongs to a series of text documents in several languages from
`blogs`, `news` and `twitter` content from [Swiftkey](https://swiftkey.com) but
the app only uses *English* data. This data is preprocessed with code [Swiftkey_app_ngrams.R](https://github.com/EnriquePH/NLP-Coursera-Swiftkey/blob/master/Swiftkey_app_ngrams.R)

### 2.2 CORPUS SAMPLES.

|       File      | Lines sampled |
|:---------------:|:-------------:|
|  en_US.news.txt |     10000     |
| en_US.blogs.txt |     10000     |
|  en_US_twitter  |       0       |

***

### 2.3 N-GRAMS

All `n-grams` tables but `unigrams` have three columns:  `sentence`, `prediction`
and `frequency`.

### 2.4 DICTIONARY

`N-gram` tables are parsed with dictionary `cracklib-small`,
included in _Ubuntu 15.10_ distribution. Words not in the dictionary 
([OOVs](http://festvox.org/bsv/x1407.html))
are sustituted by `UNK`. This step compacts `n-gram` tables and improve app
speed. All `n-gram` table with `UNK` in predicted column are deleted.


3. Algorithm
========================================================

Predicted word is found using a simplified version of
[Stupid Backoff algorithm](https://lagunita.stanford.edu/c4x/Engineering/CS-224N/asset/slp4.pdf),
where probabilities are not calculated but the more frequent `n-gram` of higher
order found is used for prediction if no `n-gram` is found the prediction function
returns the most probable unigram: `the`.

The main difference of this approach with the _Stupid Backoff_ smoothing is that,
for instance, in the App: if a trigram is found no bigram is used in the
prediction, but in the _Stupid Backoff_ you can get a bigram as the most probable
continuation if it has a higher score. The actial _Stupid Backoff_ method
penalizes lower order `n-grams` with an $\alpha$ weight that is often set to a
value of 0.4.

The main advantage is that the prediction is done faster this is important in
a _free online app_ with low computing resources. 


4. To Do
========================================================

## App improvements:

* Test the code with other languages distict than English. Dictionaries of
russian, german and finnish are needed but the code has a functional aproach
that must work with these other corpus.
* Use dictionary data to create an autocomplete feature.
* Add a control to switch on probabilities calculation.
* Add slide to change number of `n-grams` displayed in plots
* Add `n-grams` coverage plots.
* Use a external data base to expand space available for n-gram tables.

***
## Links:

### Packages:

* [Introduction to the tm Package, Text Mining in R, Ingo Feinerer, July 3, 2015](https://cran.r-project.org/web/packages/tm/vignettes/tm.pdf)
* [Getting Started with quanteda](https://cran.rstudio.com/web/packages/quanteda/vignettes/quickstart.html)

### Dictionaries:

* [cracklib-small included in Ubuntu 15.10 distribution](https://github.com/cracklib/cracklib/blob/master/src/dicts/cracklib-small)

### Books, articles and courses:

* [Large Language Models in Machine Translation](http://www.aclweb.org/anthology/D07-1090.pdf)
* [Speech and Language Processing](https://lagunita.stanford.edu/c4x/Engineering/CS-224N/asset/slp4.pdf). Daniel Jurafsky & James H. Martin.
* [Coursera Standford NLP](https://class.coursera.org/nlp/lecture)
