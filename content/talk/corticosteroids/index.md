---
abstract: BACKGROUND We illustrate the correct use of modern causal inference methods and compare to standard research practice to estimate the effect of corticosteroids on mortality in hospitalized COVID-19 patients in an observational dataset. We use several large RCTs to benchmark our results. METHODS Our retrospective data contains 3,293 COVID-19 patients hospitalized at NewYork Presbyterian March 1-May 15, 2020. Lack of clinical knowledge on COVID-19 at this time meant high variability existed in the administration of steroids, providing a unique opportunity in terms of experimentation. We design our study using the Target Trial Emulation framework. We estimate an intervention of 6 days of steroids administered at the time of severe hypoxia and contrast with an intervention of no steroids. Data include dozens of longitudinal confounders and estimation is non-parametric using a Sequentially Double Robust estimator where the probabilities of treatment, outcome, and censoring are estimated using flexible regressions via superlearning. We compare these analyses to standard practice in clinical research, i.e. Cox models for point- and time-varying treatments with variations in patient population, time zero, and treatment windows, with and without inverse probability weighting.  RESULTS The effect in our target trial emulation is qualitatively identical to RCT results, estimated to reduce 28-day mortality from 32% (31,34) to 23% (21,24). Estimates from standard clinical practice vary widely in directionality and significance, and are sensitive to study design, especially choice in time window for treatment protocol. Widening the time frame for treatment increases selection bias (patients die before they have an opportunity to receive steroids), but is necessary for adequate sample size. CONCLUSION In an opportunity to study observational clinical data during a time when RCTs were simultaneously occurring, modern causal inference methods outperform standard research practice.
all_day: false
authors: []
date: "2022-05-23"
event: American Causal Inference Conference 2022
event_url: 
featured: false
#links:
#
# - icon: twitter
#   icon_pack: fab
#   name: Follow
#   url: https://twitter.com/Rkatlady
location: University of California, Berkeley
math: true
publishDate: "2019-06-17T19:00:00Z"
summary:  A case study of modern causal inference methodology and a comparison to standard clinical research practice
tags: []
title: Corticosteroids in COVID-19
url_code: ""
url_pdf: ""
url_slides: ""
url_video: ""
---
