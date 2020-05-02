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

Lumbar_long_50<- melt(Lumbar_50, id.vars=c("PatID", "Levels", "Age", "Gender", "Group_paresis"),
                      measure.vars=c("Preop_muscle_strength", "Postop_muscle_strength", "Postop_muscle_strength_6w",
                                     "Postop_muscle_strength_3m", "Postop_muscle_strength_last"),
                      variable.name ="Time", 
                      value.name ="Muscle_strength")

Lumbar_long_50$Time <- as.numeric(Lumbar_long_50$Time)

Lumbar_long<- melt(lumbar_disc_herniation, id.vars=c("PatID", "Levels", "Age", "Gender", "Group_paresis"),
                              measure.vars=c("Preop_muscle_strength", "Postop_muscle_strength", "Postop_muscle_strength_6w",
                                             "Postop_muscle_strength_3m", "Postop_muscle_strength_last"),
                              variable.name ="Time", 
                              value.name ="Muscle_strength")

Lumbar_long_3mlast <- subset(Lumbar_long, !Time=="5")

lattice_plot<-xyplot(Muscle_strength~Time|PatID, col.line = "red", layout = c(10,5),
       grid = TRUE, type = c("p", "r"), data = Lumbar_long_50, ylab="Muscle Strength", 
       main="Trellis Plot of Muscle Strength of the First 50 Individuals Over Time")

plot(lattice_plot)

ggplot(data = Lumbar_long_50, aes(x=Time, y=Muscle_strength)) +
  geom_jitter()+
  geom_line()+
  facet_grid(.~PatID)
  
  geom_smooth(method="lm",formula = y~poly(x,3), se=FALSE,colour="red")

Lumbar_lmer <- lmer(Muscle_strength ~ Time + (1+Time|PatID), REML = FALSE, 
                      data = Lumbar_long)

coef_lumbar <- coef(Lumbar_lmer)$PatID[1:2]
setDT(coef_lumbar, keep.rownames = TRUE)[]
colnames(coef_lumbar) <- c("PatID","Intercept", "Slope")

lumbar_coef <- merge(lumbar_disc_herniation, coef_lumbar, by="PatID")

lumbar_coef$Group_paresis <- as.factor(ifelse(lumbar_coef$Group_paresis=="1", "<48h(early)", 
                                              ifelse(lumbar_coef$Group_paresis=="2" ,"2-7d(late)", "7d(very late)")))

lumbar_coef$Group_paresis2 <- relevel(lumbar_coef$Group_paresis, ref = "<48h(early)")


lumbar_coef = apply_labels(lumbar_coef, Preop_muscle_strength="Pre-operation muscle strength", 
                           Group_paresis="Groups of Paresis",  Duration_paresis="Duration of Paresis")

URP_slope<-use_labels(lumbar_coef,ctree(Slope~Duration_initial+Myotoma+Duration_paresis+Age+Levels+Preop_muscle_strength+BMI+Gender, 
                 data=..data,controls=ctree_control(testtype = "Bonferroni", maxdepth = 2)))

plot(URP_slope, main="Decision Tree for Slope of Recovery")

lumbar_coef$Preop_muscle_strength_factor <- as.factor(ifelse(lumbar_coef$Preop_muscle_strength>2, "Pre-operation muscle strength>2", "Pre-operation muscle strength=<2"))

#the library (expss) will FUCK UP the where() function, so have to clarify which package where() comes from
lumbar_coef$node_slope <- party::where(URP_slope)
lumbar_coef %>% group_by(node_slope) %>% summarise(mean=mean(Slope), median=median(Slope))

lumbar_coef$node_slope2 <- as.factor(ifelse(lumbar_coef$node_slope==3,3,
                                            ifelse(lumbar_coef$node_slope==4,4,7)))

lumbar_coef$Group_paresis3 <- as.factor(ifelse(lumbar_coef$Duration_paresis>3, ">3d", "=<3d"))
                                              
Slope_boxplot <-ggboxplot(data=subset(lumbar_coef, !is.na(Group_paresis3)), x="node_slope", y="Slope", fill="Group_paresis3")+
  ylab("Slope of Recovery")+
  facet_grid(.~Preop_muscle_strength_factor, scales = "free_x")+
  scale_fill_manual(values=c("steelblue4", "lightblue3"))+
  labs(fill='Timing of Surgery')+
  theme(
    axis.text = element_text(size=12),
    axis.text.y = element_text(size=10),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x=element_blank())+
  ggtitle("Association of Slope of Recovery between Muscle Strength and Time of Surgery")

ggboxplot(data=subset(lumbar_coef, node_slope==c(3, 4)), x="node_slope", y="Slope")+
  xlab("Time of Surgery")+
  scale_x_discrete(labels=c("3" = "Early", "4" = "Late/Very Late"))+
  ylab("Slope of Recovery")+
  ggtitle("Association between Time of Surgery and Slope of Recovery")

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

plot(ctree(Change_prelast~Duration_initial+Myotoma+Group_paresis+Age+Levels, data=subset(lumbar_coef, !is.na(Change_prelast))))

#Factor-ing of muscle strength?

lumbar_coef$Muscle_Factor <- as.factor(ifelse(lumbar_coef$Postop_muscle_strength_3m<3, "severe", 
                                              ifelse(lumbar_coef$Postop_muscle_strength_3m == 5, "recovered", "moderate")))

lumbar_coef_noNA <-subset(lumbar_coef, !is.na(Muscle_Factor2))

URP_muscle <- use_labels(lumbar_coef_noNA, ctree(Muscle_Factor2~Preop_muscle_strength+Duration_initial+Myotoma+Duration_paresis+Age+Levels+BMI+Gender, 
                                             data=..data, controls = ctree_control(testtype = "Bonferroni", maxdepth = 2)))

plot(URP_muscle, main="Decision Tree for Muscle Groups")

#the library (expss) will FUCK UP the where() function!!!!!!!!!!!
lumbar_coef_noNA$node_muscle <- party::where(URP_muscle)
URP_muscle_node <-lumbar_coef_noNA %>% group_by(node_muscle) %>% count(Muscle_Factor2)
URP_muscle_node$sum <- ifelse(URP_muscle_node$node_muscle==2, 74, 
                              ifelse(URP_muscle_node$node_muscle==4,153,103)) 
                                     
URP_muscle_node$percent <- (URP_muscle_node$n/URP_muscle_node$sum)*100
URP_muscle_node$Preop_muscle_strength_factor <- as.factor(ifelse(URP_muscle_node$node_muscle=="4"|URP_muscle_node$node_muscle=="5","Pre-operation muscle strength>2",
                                                                 "Pre-operation muscle strength=<2"))

URP_muscle_node$Group_paresis <- as.factor(ifelse(URP_muscle_node$node_muscle==2, "Not Significant",
                                                  ifelse(URP_muscle_node$node_muscle==4, "=<3d", ">3d")))
                                                                
Muscle_barplot<-ggplot(subset(URP_muscle_node, !(node_muscle==2)), aes(x=Muscle_Factor2, y=percent, fill=Group_paresis))+
  geom_bar(stat="identity", position = position_dodge(width = 0.95, preserve = "total"), width = 0.94)+
  scale_fill_manual(values=c("steelblue4", "lightblue3"))+
  theme_bw() +
  facet_grid(.~Preop_muscle_strength_factor, scales = "free_x")+
  theme(axis.line = element_line(colour = "black"),
        axis.text = element_text(size=12),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  labs(title="Association between Muscle Strength and Time of Surgery", 
       x="Groups of Muslce Strength at 3 months", y = "Percentage of Individuals", 
       fill="Timing of Surgery")

ggarrange(Outcomes_corplot, ggarrange(Muscle_barplot, Slope_boxplot,labels=c("B", "C"), legend="bottom", ncol=2), nrow=2, labels="A")

ggarrange(Muscle_barplot, Slope_boxplot,labels=c("A", "B"), legend="bottom", ncol=2, common.legend = TRUE)


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

#Descriptive Stats
library(table1)
descrip <- table1(~as.factor(Levels)+as.factor(Gender)+Age+BMI+Preop_muscle_strength+Postop_muscle_strength_3m+Slope|Group_paresis3, data=lumbar_coef)
write.table(descrip, file="descrip.txt")

#Correlation matrix 
Outcomes <- select(lumbar_disc_herniation, starts_with("Preop_muscle"), starts_with("Postop_muscle"))
colnames(Outcomes)=c("Pre-op Muscle Strength","Post-op Muscle Strength","Post-op Muscle Strength 6w","Post-op Muscle Strength 3m","Post-op Muscle Strength last")

Cor_outcomes <- cor(Outcomes, use = "complete.obs")

Outcomes_corplot<-ggcorrplot(Cor_outcomes, type="lower", lab = TRUE)
# New dataset for surgery 
Surgery$`time of injury` <- format(as.POSIXct(Surgery$`time of injury` ,format="%H:%M:%S"),"%H:%M")
Surgery$`Time of surgery` <- format(as.POSIXct(Surgery$`Time of surgery` ,format="%H:%M:%S"),"%H:%M")

Surgery$Date_time_injury <- as.POSIXct(paste(Surgery$`Date of injury`, Surgery$`time of injury`), format="%Y-%m-%d %H:%M")
Surgery$Date_time_surgery <- as.POSIXct(paste(Surgery$`Surgery`, Surgery$`Time of surgery`), format="%Y-%m-%d %H:%M")

Surgery$Time_diff_surgery <- as.numeric(difftime(Surgery$Date_time_surgery, Surgery$Date_time_injury, units="mins"))

Surgery$Time_diff_discharge <- as.numeric(difftime(Surgery$Discharge, Surgery$Admission, units="days"))

Surgery$Time_diff_EMSCI <- as.numeric(difftime(Surgery$`Last EMSCI`, Surgery$`First EMSCI`, units="days"))

Last_LEMS<-ctree(`Last LEMS`~Time_diff_surgery+Time_diff_discharge+Time_diff_EMSCI+Levels+`1_LEMS`, data=subset(Surgery, !is.na(`Last LEMS`)), controls = ctree_control(testtype = "Bonferroni"))
plot(Last_LEMS, main="Last LEMS")

Surgery$node_LEMS <- where(Last_LEMS)
Surgery %>% group_by(node_LEMS) %>% summarise(mean=mean(`Last LEMS`), median=median(`Last LEMS`))

Last_UEMS<-ctree(`Last UEMS`~Time_diff_surgery+Time_diff_discharge+Time_diff_EMSCI+`1_UEMS`, data=subset(Surgery, !is.na(`Last UEMS`)&Levels==1))
plot(Last_UEMS, main="Last UEMS (Cervicals)")

Last_TMS<-ctree(`Last TMS`~Time_diff_surgery+Time_diff_discharge+Time_diff_EMSCI+Levels+`1_TMS`, data=subset(Surgery, !is.na(`Last TMS`)))
plot(Last_TMS, main="Last TMS")

Last_PP<-ctree(`Last PP`~Time_diff_surgery+Time_diff_discharge+Time_diff_EMSCI+Levels+`1_PP`, data=subset(Surgery, !is.na(`Last PP`)))
plot(Last_PP, main="Last PP")

Last_LT<-ctree(`Last LT`~Time_diff_surgery+Time_diff_discharge+Time_diff_EMSCI+Levels+`1_LT`, data=subset(Surgery, !is.na(`Last LT`)))
plot(Last_LT, main="Last LT")

Last_AIS<-ctree(`last AIS`~Time_diff_surgery+Time_diff_discharge+Time_diff_EMSCI+Levels+`first AIS`, data=subset(Surgery, !is.na(`last AIS`)))
plot(Last_AIS, main="Last AIS")

Last_SCIM<-ctree(`Last SCIM`~Time_diff_surgery+Time_diff_discharge+Time_diff_EMSCI+Levels+`1_SCIM`, data=subset(Surgery, !is.na(`Last SCIM`)))
plot(Last_SCIM, main="Last SCIM")

