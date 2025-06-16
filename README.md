# Code-Master-Thesis-Pauline---Analysis-of-PFIC-data

This repository consists of the R codes used in my master thesis: Retrospective analysis of PFIC data. An electronic version of this thesis is available at http://repository.tudelft.nl/. 

The file r_code_latent_class_linear_mixed_models_sba_platelet.R is the code for the latent class linear mixed model, explained in Chapter 4 of the thesis. 

The rest of the files are needed to perform the weighted survival analysis, explained in Chapter 5 of the thesis. The programs should be executed in the following order:
1. r_code_prepare_data.R: prepares the data for performing the weighted survival analysis.
3. r_code_different_index_times.R: creates the dataset used in the weighted survival analysis, including the survival times which are calculated using different options of choosing the index time.
4. r_code_ps_model.R: performs the inverse probability treatment weighting (for the different index times).
5. r_code_weighted_survival_results.R: performs the weighted survival analysis: creates the adjusted kaplan-meier curve, and performs the weighted cox model.
6. r_code_permutation_test.R: performs the permutation test. 
