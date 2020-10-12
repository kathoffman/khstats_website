# G-COMPUTATION/SUBSTITUTION ESTIMATOR

# prespecified parametric model
outcome_fit <- glm(outcome ~ trt + x1 + x2 + x3, data = obs)

# make data sets where everyone was either treated or not treated
obs_trt <- obs %>% mutate(trt = 1)
obs_cntrl <- obs %>% mutate(trt = 0)

# get the predictions using outcome fit
preds_trt <- predict(outcome_fit, obs_trt)
preds_cntrl <- predict(outcome, obs_cntrl)

# avg risk difference is the difference in E[Y|A=1,X] - E[Y|A=0,X]
avg_risk_diff <- preds_trt - preds_cntrl
avg_risk_diff





# IPTW

# que tragique !

# enter machine learning

# TMLE 
