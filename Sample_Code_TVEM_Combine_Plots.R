##############################################################
# This part is to simulate the datasets -- don't worry if you
# don't understand it.
set.seed(12345)
dataset1 <- simulate_tvem_example(n_subjects=200,simulate_binary=TRUE)
dataset2 <- simulate_tvem_example(n_subjects=200,simulate_binary=TRUE)
##############################################################
# FIRST PLOT
##############################################################
# Fit intercept-only models
tvem_model1 <- tvem(data=dataset1,
                   formula=y~1,
                   family=binomial(),
                   id=subject_id,
                   time=time)
tvem_model2 <- tvem(data=dataset2,
                    formula=y~1,
                    family=binomial(),
                    id=subject_id,
                    time=time)
# Plot probabilities
time_grid <- tvem_model1$time_grid
log_odds_1 <- tvem_model1$grid_fitted_coefficients$`(Intercept)`$estimate
probs_1 <- plogis(log_odds_1) # plogis is the inverse logit function;
log_odds_2 <- tvem_model2$grid_fitted_coefficients$`(Intercept)`$estimate
probs_2 <- plogis(log_odds_2)
plot(x=time_grid,
     y=probs_1,
     main="Time-Specific Fitted Response Probability",
     type="l",  # plot a line;
     xlab="Day of Study",
     ylab="Probability",
     lwd=2,  # nice thick line so that we can see the color;
     col="blue")
lines(x=time_grid,
      y=probs_2,
      lwd=2,
      col="black")
# Add confidence limits to plot of probabilities
upper_log_odds_1 <- tvem_model1$grid_fitted_coefficients$`(Intercept)`$upper
upper_log_odds_2 <- tvem_model2$grid_fitted_coefficients$`(Intercept)`$upper
lower_log_odds_1 <- tvem_model1$grid_fitted_coefficients$`(Intercept)`$lower
lower_log_odds_2 <- tvem_model2$grid_fitted_coefficients$`(Intercept)`$lower
upper_probs_1 <- plogis(upper_log_odds_1)
upper_probs_2 <- plogis(upper_log_odds_2)
lower_probs_1 <- plogis(lower_log_odds_1)
lower_probs_2 <- plogis(lower_log_odds_2)
lines(x=time_grid,
      y=upper_probs_1,
      lwd=1,   # thin line for plotting confidence limit
      lty=2,   # dashed line for plotting confidence limit 
      col="blue")
lines(x=time_grid,
      y=lower_probs_1,
      lwd=1,
      lty=2,
      col="blue")
lines(x=time_grid,
      y=upper_probs_2,
      lwd=1,
      lty=2,
      col="black")
lines(x=time_grid,
      y=lower_probs_2,
      lwd=1,
      lty=2,
      col="black")
legend(x="bottomright",
       lwd=2,
       col=c("blue","black"),
       legend=c("Dataset 1","Dataset 2"))
##############################################################
# SECOND PLOT PART ONE: INTERCEPT
##############################################################
# Now fit models with covariate
tvem_model3 <- tvem(data=dataset1,
                    formula=y~x1,
                    family=binomial(),
                    id=subject_id,
                    time=time)
tvem_model4 <- tvem(data=dataset2,
                    formula=y~x1,
                    family=binomial(),
                    id=subject_id,
                    time=time)
time_grid <- tvem_model3$time_grid 
plot(x=time_grid,
     y=tvem_model3$grid_fitted_coefficients$`(Intercept)`$estimate,
     main="Log Odds Given Covariate=0",
     xlab="Day of Study",
     ylab="Intercept Log Odds",
     type="l",
     lwd=2,
     ylim=c(0,2),
     col="blue")
lines(x=time_grid,
      y=tvem_model3$grid_fitted_coefficients$`(Intercept)`$upper,
      lwd=1,
      lty=2,
      col="blue")
lines(x=time_grid,
      y=tvem_model3$grid_fitted_coefficients$`(Intercept)`$lower,
      lwd=1,
      lty=2,
      col="blue")
lines(x=time_grid,
      y=tvem_model4$grid_fitted_coefficients$`(Intercept)`$estimate,
      lwd=2,
      col="black")
lines(x=time_grid,
      y=tvem_model4$grid_fitted_coefficients$`(Intercept)`$upper,
      lwd=1,
      lty=2,
      col="black")
lines(x=time_grid,
      y=tvem_model4$grid_fitted_coefficients$`(Intercept)`$lower,
      lwd=1,
      lty=2,
      col="black")

##############################################################
# SECOND PLOT PART TWO: TIME-VARYING EFFECT
##############################################################
plot(x=time_grid,
     y=tvem_model3$grid_fitted_coefficients$x1$estimate,
     main="Time-Varying Effect on Log Odds Scale",
     xlab="Day of Study",
     ylab="Time-Specific Regression Coefficient",
     type="l",
     lwd=2,
     ylim=c(-.5,.5),
     col="blue")
lines(x=time_grid,
      y=tvem_model3$grid_fitted_coefficients$x1$upper,
      lwd=1,
      lty=2,
      col="blue")
lines(x=time_grid,
      y=tvem_model3$grid_fitted_coefficients$x1$lower,
      lwd=1,
      lty=2,
      col="blue")
lines(x=time_grid,
      y=tvem_model4$grid_fitted_coefficients$x1$estimate,
      lwd=2,
      col="black")
lines(x=time_grid,
      y=tvem_model4$grid_fitted_coefficients$x1$upper,
      lwd=1,
      lty=2,
      col="black")
lines(x=time_grid,
      y=tvem_model4$grid_fitted_coefficients$x1$lower,
      lwd=1,
      lty=2,
      col="black")
