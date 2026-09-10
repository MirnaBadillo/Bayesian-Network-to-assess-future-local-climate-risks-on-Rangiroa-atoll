# Bayesian-Network-to-assess-future-risk-to-habitability-in-Rangiroa-Atoll-using-heterogeneous-information
The objective of this project is to share a program that builds a Bayesian Network (BN) model based on heterogenous data and knowledge. This program also allows for Bayesian inference using the Likelihood Weighting method. 

This project consists of a series of scripts that should be executed as follows:

1) Configure files paths: Run [1_Path_config](./1_Path_config.R) to set up the file pathways.
2) Conditional Probability tables (CPTs) for all nodes are provided in the repertory CPT_BN_Rangiroa
3) Build the BN model and perform inference:  Run [BN_to_Assess_Future_Risk_to_Habitability_in_Rangiroa_Atoll](./BN_to_Assess_Future_Risk_to_Habitability_in_Rangiroa_Atoll.R)
   
   This BN model is applied to Rangiroa Atoll. The example queries provided in this project allow for risk assessment across the study islands and identification of severe risk conditions (inverse analysis).
   
4) The functions used to perform queries are in the [Queries_BN_LW_method_Rangiroa](./Queries_BN_LW_method_Rangiroa.R) script.

Necessary packages:
- Bayesian Network development [bnlearn](https://www.bnlearn.com/) and [bnviewer](https://cran.r-project.org/web/packages/bnviewer/bnviewer.pdf)
