# Repository contents
<b>analysis.R</b> is contains all the code used for the research project. It mostly runs on R version 4.1.1.

It's split into the following sections. Some sections require files as input, and sometimes analysis decisions were made based on descriptive graphs of the data. I.e. the code isn't designed to run from beginning to end non-stop & will probably need adapting to different datasets.

Contents:
1. Identify a list of CRC symptoms
2. Find UKBB participants with symptoms and make a table of earliest symptom for each participant
3. Find earliest occurence of CRC for participants (identify cases & controls)
4. Add all lifestyle/sympton/health variables to participant data frame
5. Split cohort into 1. unrelated European individuals and 2. remaining cohort
6. Generate the polygenic/genetic risk score for all participants and work out quintiles, distributions in cases v controls, and Cox proportional hazards cumulative incidence plot (note: part of this section ideally requires a more up-to-date R version than 4.1.1. to prevent errors)
7. Logistic regression analysis and ROCAUC for individual variables
8. Building the integrated risk model based on increases in ROCAUC
9. Building the integrated risk model based on AIC
10. Calculate ROCAUC for the final risk model and GRS/PRS alone

The <b>find-read-codes</b> folder contains an R function built for this analysis & a descriptive README file.

<b>read_codes_list</b> contains the 59 read codes for CRC symptoms which I identified through use of the function. It does not contain read codes provided by other researchers at the University of Exeter and University College London and used in this study.

<b>nonsignificant_variables.csv</b> are the results of logistic regression and ROCAUC testing for the variables which were not significantly associated with CRC diagnosis, in terms of p-value and odds ratio.
