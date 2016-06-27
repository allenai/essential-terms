# Essential Term Selector 

## About 

This project contains a supervised learning classifier for identifying *essential* terms 
in a given a question (terms which are defintely needed for answering a question). 
Here is an example: 

TODO 

The main input for supervised training is collected via mechanical turk 
and is included in the project.  
 
To see more comprehensive analysis on our classifier and its usages 
in other QA systems, have a brief look at [1]. 


## Using it in your system 
Here we explain two popular ways of calling our this system in your code; 

### Using it as a service via `Injector` library: 
  TODO 
  
### Using it directly as library dependency: 
  
  If you want to use it in an external project (non-Aristo), you'd have 
   to first add it as a dependency. 
  ```sbt
      TODO 
  ```
  
  Next we have to initialize the classifiers and make predictions on a given 
  question 
  ```scala 
     TODO 
  ```
  
## Further Reading 
Checkout the following paper for more details and analysis 

[1] TODO 
