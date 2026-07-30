# Sub-national Tailoring in PNG

R scripts for sub national tailoring of malaria intervention in PNG. This repository contains the R code used to reproduce the analyses presented in the manuscript: 
> **Malaria risk stratification and sub-national tailoring of interventions in Papua New Guinea**

## Overview:

Malaria remains a persistent public health challenge in Papua New Guinea (PNG), characterized by substantial heterogeneity in transmission intensity. Following a period of stagnation in malaria control progress, the National Malaria Control Program (NMCP) revised its strategic framework by adopting a sub-national tailoring (SNT) approach. This strategy aims to optimize the allocation of limited resources and accelerate progress toward malaria elimination. The SNT process in PNG, guided by a dedicated local task force, involved conducting district-level malaria risk stratification, defining eligibility criteria for the range of interventions under consideration, and geographically allocating and strategically prioritizing interventions based on epidemiological and contextual factors. Epidemiological stratification was conducted at the district level using annual parasite incidence (API) data from 2022–2024. Parasite prevalence estimates from the malaria indicator survey were used to define API thresholds. The stratification classified 18 districts as high transmission, 23 as moderate, 19 as low, 9 as very low, and 18 as pre-elimination strata. Additional quantifiable determinants of transmission including access to health facilities, intervention coverage, population distribution, and altitude were stratified and incorporated as complementary layers to inform intervention eligibility criteria. Interventions were then geographically allocated based on these established criteria. The outcomes of the SNT process informed the development of PNG’s National Strategic Plan for Malaria (NSPM) 2026–2030. The SNT process represents the first comprehensive, data-driven approach to locally tailored malaria control in PNG that explicitly accounts for epidemiological heterogeneity. It marks a significant programmatic shift toward evidence-based and strategically targeted malaria interventions.

## Data:

The analyses use multiple data sources including:
•	Routine malaria surveillance data from the electronic national health information system (eNHIS)
•	Population distribution
•	Elevation 
•	Travel time to health facility
•	Prevalence data from MIS/DHS survey

Data from routine eNHIS are not publicly available and were obtained with request from the NMCP of PNG. Restrictions apply to the availability of these data and permission can be obtained with reasonable request from NDoH.

## Analysis workflow:

1.	Data assembly and processing
2.	Aggregation of health facility data to council level
3.	Assembly of environmental and health system datasets for determining eligibility criteria
4.	Epidemiological stratification
5.	Quantifying malaria risk heterogeneity within provinces
6.	Determining eligibility criteria
7.	Intervention targeting and strategic prioritization 
8.	Generation of manuscript figures and tables (ManuscriptFigures.R)
Reproducibility
Because the surveillance data cannot be shared publicly, this repository provides:
•	documentation of the analytical workflow
•	code used to generate the manuscript figures and tables

## Software
Analyses were performed in R (version 4.5.1)

## Citation

If you use this code, please cite:
Thawer SG, Seidahmed O, Timbi D, et al. Malaria risk stratification and sub-national tailoring of interventions in Papua New Guinea. (Manuscript under review.)

## Funding

Funding was provided by the Global Fund to Fight AIDS, Tuberculosis and Malaria, with co-funding from the Swiss Network for International Studies. Diana Timbi was supported by a PhD scholarship from the Canton of Basel-Stadt and Vincent Minconetti by the Forlen Stiftung. The funders had no role in study design, data collection and analysis, decision to publish, or manuscript preparation.

## Contact

For questions regarding the analysis or repository, please contact:
Sumaiyya G. Thawer
Swiss Tropical and Public Health Institute, Allschwil, Switzerland

