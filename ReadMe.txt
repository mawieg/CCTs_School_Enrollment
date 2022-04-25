The original for my paper "Do early-ending conditional cash transfer programs discourage continued school enrollment?" 
comes from the website https://evaluacion.prospera.gob.mx/en/eval_cuant/p_bases_cuanti.php, which was no longer 
available at the time of writing. 

To merge the various survey rounds, define relevant variables, and construct datasets for analysis in R, open 
"MakeDataset.do" in Stata, change the directory to the adress of the Progresa folder, and run. The location-specific 
variables are constructed in a separate file, "MakeLocationData.do". To run this, change the directory to the
adress of the Progresa folder first. 

To obtain summary statistics, open "SummaryStats.do" in Stata, change the directory to the adress of the Progresa folder, 
and run.

To obtain the DML results, open "Run.R" in R. Change the directory to the adress of the Progresa folder. Change the 
variables "set", "status", and "outcomename" according to the subset and dependent variable of interest. Adjust the 
settings for parallel computing accordingly (in lines 78-80). Before running the program, make sure all required packages 
are installed. The results of the DML estimation are saved in the matrix "result". The vectors of relative importance 
are saved in the vector "delta.0". 


Martin Wiegand, 05. January 2020