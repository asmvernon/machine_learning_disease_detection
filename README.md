---------------------------------------------------------------------------------------------------------
Andrea S. Martinez-Vernon & Richard S. Savage, University of Warwick, UK

Last modified 2018/05/04

---------------------------------------------------------------------------------------------------------
*** PLEASE NOTE:  ALL SCRIPTS AND FUNCTIONS ARE DISTRIBUTED UNDER VERSION 3 OF THE GNU PUBLIC LICENCE ***
---------------------------------------------------------------------------------------------------------
R scripts were used to generate the supporting information tables. The data in the main tables are a subset of these.
	
	> NOTE: CHANGE THE DIRECTORY PATH BEFORE RUNNING SCRIPTS!


**FOLDER STRUCTURE AND CONTENT**
```
  analysis.R                      - R script to generate all Supporting Information Tables, except Tables 10 and 16
  analysis_features.R             - R script to generate Supporting Information Table 10
  analysis_features_forced.R      - R script to generate Supporting Information Table 16


  src/
    ClassifierModels.R
    CrossValidation.R
    FeatureSelection.R
    PlotALLRocCurve.R
    PlotFaimsData.R
    PlotRocCurve.R
    ReadInFaimsData.R
    RunData_Diabetes_ensemble.R
    RunData_Diabetes_runs_function_RunSubtract.R
    RunData_Diabetes_runs_function.R
    WaveletTransform.R
    WaveletTransform2D.R
```
<!----
data/ 
	  DiabetesLonestar/             - Directory containing 115 subfolders that hold the FAIMS sample runs
		  <DATE> ID/					        - Naming convention for subfolders (e.g. 020614 DM145)
			  export_matrix_0001.txt 	  - Text file containing Lonestar instrument data for first sample run
			  export_matrix_0002.txt	  - Text file containing Lonestar instrument data for second sample run
			  export_matrix_0003.txt	  - Text file containing Lonestar instrument data for third sample run
	  demo_data.rda				 	        - R object containing the demographic variables associated with the  
---->
