# meta_analysis_CogVar
Meta-analysis of the known-group validity (Hedges g) of various cognitive tests, i.e. the capacity of these tests to discriminate patients from controls. 
The study data will be included upon publication. 
More information: marie.pittet@chuv.ch or marie.pittet93@gmail.com

The project contains two R scripts: 
- Risk_of_bias.R: a risk of bias assessment using ROBVIS
- ForestPlot_FunnelPlot_NonIndependence.R: meta-analysis with forest plots, funnel plots, and non-independence testing

  
The meta-analysis is separated in various subcomponents of the construct of interest (here social cognition). It also calculates separate summary indices for different clinical populations. 
The random-effect model allows for study-to-study differences in the discrimination power, which is expected since different tests are used to target the same construct. Following recommendations by Cheung (2019), we have performed Likelihood Ratio Tests to assess the non-independence of some effect sizes extracted from the same study.
