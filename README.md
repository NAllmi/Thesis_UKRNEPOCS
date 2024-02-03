# Masters Thesis - Technical University of Munich

## Political information fact-checking behavior in the context of the Russia-Ukraine war

Built using R version 4.2.3

The main research questions of the thesis are:

RQ1: How does the consumption of political information through different media outlets affect the levels
of concern in individuals regarding the Russia-Ukraine war?

RQ2: How do anxiety and the importance assigned to the Russia-Ukraine war affect the amount of factchecking performed by individuals?


DATA: Not publicly available.

CODE:

Before running the models run the master.R file that loads libraries and sets working directory.

master.R file runs:

-data_preprocessing.R: load and prepare data (This code also runs data_collection_output.R : print plots and tables for data collection section) 
 
-rq1_models.R: runs models for the 1st research question

-rq2_glm_models.R: runs models for the 2nd research question

-efa.R : exploratory factory analysis for "concern for the war" scale 

RESULTS:

plots and tables are in the \output folder.

Final thesis is available in pdf format (Thesis_Allmi.pdf)
