# shiny-caret
#### Cross-breeding of Shiny and Caret

The author is a lecturer in Applied Data Science at Canterbury University.

He has a long standing interest in Shiny and Caret. In this project he seeks to use Shiny to construct a visual and interactive application (in a web page) that follows the Caret concepts of best-practice in Classification and Regression. This project has been gestating for some time.

In creating this application the author is well aware that an impracticable number of libraries get loaded. None the less he is determined to have some fun exploring ideas relating to visualisation, workflow, automation and guided method selection.

#### Frustrations with/around *Caret* are:  
 - Dialogues relating to loading more libraries occurs in the consol which effectively hangs a Shiny session. 
 - When methods fail to train it is difficult to diagnose why. It is particularly difficult to catch any messages when methods are run in parallel mode.
 - The process of generating a production-ready model (combining train data with test data) has gone unrecognised as a workflow stage that needs automation, persistence, validation and risk-analysis regarding the extent of non-parametricness. For example comparing the residuals of the best, so called, "final" model to the "production-ready" model. 
 - Strategies for deploying models in high throughput scenarios. Sadly, *Plumbr* does not scale.
 - Cost based assessment of confusion matrices seems to be either impossible or too difficult. 
 - The inter-dependence of preprocessing and method selection. We need conditional preprocessing. Maybe *Recipes* need to be adaptive to a method. For example, a *step_dummyVar()* might be a null operation if the method is not Ordinary Least Squared based.
 - The tags returned by *getModelInfo()* need to be developed further. Perhaps they could describe whether the method is tolerant of co-linearity / (Near) Zero Variance / Rare categories. The author is still developing ideas in this area. Maybe method taxonomy is the key.
 - The complexity of a model cannot currently be quantified. There needs to be a generic function that reports the number of parameters that a model has fitted. The Obs/Params ratio gives an indication of the model's ability to generalise well. 

#### Planned enhancements

 * Convert charts to *plotly*
 * Redesign the nested tab-sets
 * Make the full set of recipe steps available
 * Retrofit hover text for the controls
 * (Maybe) Introduce data processing of text
 * (Maybe) Introduce data processing of images

I understand that Python is catching up to Shiny and that Scikit-Learn is maturing well. I fully expect that these technologies (or something new) will render this work redundant in the long term. In the meantime it is just good fun.
 
## Nicholas Ward, May 2019
