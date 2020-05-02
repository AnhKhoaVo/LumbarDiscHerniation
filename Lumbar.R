plot(ctree(Postop_grade_paresis_3m~Preop_grading_paresis+Myotoma+Duration_paresis+Duration_initial+Age+Levels, data=subset(lumbar_disc_herniation, !is.na(Postop_grade_paresis_3m))))

plot(ctree(Postop_muscle_strength_3m~Preop_muscle_strength+Duration_initial+Myotoma+Duration_paresis+Age+Levels, data=subset(lumbar_disc_herniation, !is.na(Postop_muscle_strength_3m))))


ggplot(data=lumbar_disc_herniation, aes(x=Postop_muscle_strength_3m, y=Postop_muscle_strength_last, colour=Group_paresis))+
  geom_jitter(alpha=.6)+
  geom_smooth(method="lm")+
  scale_colour_manual(name = "Groups of Paresis", values=c("mediumseagreen", "tan2", "red")
                      , labels = c("1" = "<48h", "2" = "2-7 days", "3" = ">7 days"))+
  theme_bw()+
  facet_grid(.~OPwithin48h, scales = "free_x")
xlab("Pre Operative Muscle Strength")+
  ylab("Post Operative Muscle Strength 3 months")+
  ggtitle("Correlation between pre and post op muscle strength across groups of paresis")

last3m <- ggplot(data=lumbar_disc_herniation, aes(x=Postop_muscle_strength_3m, y=Postop_muscle_strength_last))+
  geom_jitter(alpha=.6)+
  geom_smooth(method="lm")+
  theme_bw()+
  xlab("Post Operative Muscle Strength 3 months")+
  ylab("Post Operative Muscle Strength last follow up")+
  ggtitle("Relationship between 3 month and last follow up muscle strength")


outlierKD <- function(dt, var) {
  var_name <- eval(substitute(var),eval(dt))
  na1 <- sum(is.na(var_name))
  m1 <- mean(var_name, na.rm = T)
  par(mfrow=c(2, 2), oma=c(0,0,3,0))
  boxplot(var_name, main="With outliers")
  hist(var_name, main="With outliers", xlab=NA, ylab=NA)
  outlier <- boxplot.stats(var_name)$out
  mo <- mean(outlier)
  var_name <- ifelse(var_name %in% outlier, NA, var_name)
  boxplot(var_name, main="Without outliers")
  hist(var_name, main="Without outliers", xlab=NA, ylab=NA)
  title("Outlier Check", outer=TRUE)
  na2 <- sum(is.na(var_name))
  cat("Outliers identified:", na2 - na1, "n")
  cat("Propotion (%) of outliers:", round((na2 - na1) / sum(!is.na(var_name))*100, 1), "n")
  cat("Mean of the outliers:", round(mo, 2), "n")
  m2 <- mean(var_name, na.rm = T)
  cat("Mean without removing outliers:", round(m1, 2), "n")
  cat("Mean if we remove outliers:", round(m2, 2), "n")
  response <- readline(prompt="Do you want to remove outliers and to replace with NA? [yes/no]: ")
  if(response == "y" | response == "yes"){
    dt[as.character(substitute(var))] <- invisible(var_name)
    assign(as.character(as.list(match.call())$dt), dt, envir = .GlobalEnv)
    cat("Outliers successfully removed", "n")
    return(invisible(dt))
  } else{
    cat("Nothing changed", "n")
    return(invisible(var_name))
  }
}

outlierKD(lumbar_disc_herniation, Duration_paresis)

#Changes of muscle strength over time based on LMER
Lumbar_50 <- head(lumbar_disc_herniation, 50)

Lumbar_long<- melt(lumbar_disc_herniation, id.vars=c("PatID", "Levels", "Age", "Gender", "Group_paresis"),
                   measure.vars=c("Preop_muscle_strength", "Postop_muscle_strength", "Postop_muscle_strength_6w",
                                  "Postop_muscle_strength_3m", "Postop_muscle_strength_last"),
                   variable.name ="Time", 
                   value.name ="Muscle_strength")

Lumbar_long_3mlast <- subset(Lumbar_long, !Time=="5")

lumbar_long_fig <- ggplot(data = Lumbar_long_3mlast, aes(x=as.numeric(Time), y=Muscle_strength, group=PatID)) +
  geom_jitter()+
  geom_line()

geom_smooth(method="lm",formula = y~poly(x,3), se=FALSE,colour="red")

Lumbar_lmer <- lmer(Muscle_strength ~ Time + (1+Time|PatID), REML = FALSE, 
                    data = Lumbar_long_3mlast)

Lumbar_long_50<- melt(Lumbar_50, id.vars=c("PatID", "Levels", "Age", "Gender", "Group_paresis"),
                   measure.vars=c("Preop_muscle_strength", "Postop_muscle_strength", "Postop_muscle_strength_6w",
                                  "Postop_muscle_strength_3m", "Postop_muscle_strength_last"),
                   variable.name ="Time", 
                   value.name ="Muscle_strength")
Lumbar_long_50$Time <- as.numeric(Lumbar_long_50$Time)

lattice_plot<- xyplot(Muscle_strength~Time|PatID, col.line = "red", layout = c(10,5),
       grid = TRUE, type = c("p", "r"), data = Lumbar_long_50)

coef_lumbar <- coef(Lumbar_lmer)$PatID[1:2]
setDT(coef_lumbar, keep.rownames = TRUE)[]
colnames(coef_lumbar) <- c("PatID","Intercept", "Slope")

lumbar_coef <- merge(lumbar_disc_herniation, coef_lumbar, by="PatID")

slope <- ctree(Slope~Paresis_group+Age+Levels+Preop_muscle_strength+Gender+BMI, data=lumbar_coef)
predicted_ctree <- predict(slope, lumbar_coef)
actuals_preds <- data.frame(cbind(lumbar_coef$Slope, predicted_ctree))
cor.test(actuals_preds$V1, actuals_preds$Slope)


rmse(lumbar_coef$Slope, predicted)
R2(lumbar_coef$Slope, predicted)

a <- train(Slope~Duration_initial+Myotoma+Paresis_group+Age+Levels+Preop_muscle_strength, 
           data=lumbar_coef, method = "lm", na.action = na.omit,
  trControl = trainControl("cv", number = 10)
)


slope_lm <- lm(Slope~Duration_initial+Myotoma+Paresis_group+Age+Levels+Preop_muscle_strength, data=lumbar_coef, na.action = na.omit)
predicted_lm <- predict(slope_lm, lumbar_coef)

actuals_preds_lm <- data.frame(cbind(actuals=lumbar_coef$Slope, predicteds=predicted_lm))
cor.test(actuals_preds_lm$actuals, actuals_preds_lm$predicteds)

rmse(actuals_preds_lm$actuals, actuals_preds_lm$predicteds)
R2(predicted_lm, lumbar_coef$Slope)

#plot for cross validation for lm
library(DAAG)
cv.lm(data=lumbar_coef, form.lm=formula(Slope~Myotoma+Paresis_group+Age+Levels+Preop_muscle_strength, data=lumbar_coef), m=10)

summary(aov(Muscle_strength ~ as.factor(Time)*Group_paresis, data = Lumbar_long))


jmv::ANOVA(
  formula = Muscle_strength ~ Gender*Group_paresis,
  data = Lumbar_long,
  effectSize = "eta",
  postHoc = ~ Gender*Group_paresis,
  postHocCorr = c("none", "bonf"),
  emmPlotData = TRUE,
  emmTables = TRUE)

#Change of muscle strength based on substraction 
lumbar_coef$Change_prelast <- lumbar_coef$Postop_muscle_strength_3m - lumbar_coef$Preop_muscle_strength

plot(ctree(Change_prelast~Duration_initial+Myotoma+Paresis_group+Age+Levels, data=subset(lumbar_coef, !is.na(Change_prelast))))

#Factor-ing of muscle strength?

lumbar_coef$Muscle_Factor <- as.factor(ifelse(lumbar_coef$Postop_muscle_strength_3m<3, "severe", 
                                              ifelse(lumbar_coef$Postop_muscle_strength_3m == 5, "recovered", "moderate")))

lumbar_coef$Paresis_group <- as.factor(ifelse(lumbar_coef$Group_paresis == "1", "early", 
                                              ifelse(lumbar_coef$Group_paresis == "2", "late", "very late")))

plot(ctree(Muscle_Factor~Preop_muscle_strength+Duration_paresis+Age+Levels+Gender, data=subset(lumbar_coef, !is.na(Muscle_Factor))))

lumbar_coef$Muscle_Factor2 <- relevel(lumbar_coef$Muscle_Factor, ref = "severe")
lumbar_coef$Group_paresis2 <- relevel(lumbar_coef$Group_paresis, ref = "3")
lumbar_log <- multinom(Muscle_Factor2 ~ Preop_muscle_strength + Group_paresis, data=lumbar_coef)
summary(lumbar_log)

z_log <- summary(lumbar_log)$coefficients/summary(lumbar_log)$standard.errors
p_log <- (1 - pnorm(abs(z_log), 0, 1)) * 2

exp(coef(lumbar_log))

head(prob <- fitted(lumbar_log))

prob_paresis <- data.frame(Group_paresis = c("1", "2", "3"), Preop_muscle_strength = mean(lumbar_coef$Preop_muscle_strength))
predict(lumbar_log, newdata = prob_paresis, "probs")

prob_muscle <- data.frame(Group_paresis = rep(c("1", "2", "3")), Preop_muscle_strength = rep(c(0:5),
                                                                                             3))
prob_both <- cbind(prob_muscle, predict(lumbar_log, newdata =prob_muscle, type = "probs", se = TRUE))

prob_long <- melt(prob_both, id.vars = c("Group_paresis", "Preop_muscle_strength"), value.name = "probability")

ggplot(prob_long, aes(x = Preop_muscle_strength, y = probability, colour = Group_paresis)) + geom_line() + facet_grid(variable ~
                                                                                                                        ., scales = "free")
