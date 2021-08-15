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

lumbar_coef$Group_paresis <- as.factor(ifelse(lumbar_coef$Group_paresis=="1", "<48h(ultra early)", 
                                              ifelse(lumbar_coef$Group_paresis=="2" ,"2-7d(early)", "7d(delayed)")))

lumbar_coef$Group_paresis2 <- relevel(lumbar_coef$Group_paresis, ref = "<48h(ultra early)")


lumbar_coef = apply_labels(lumbar_coef, Preop_muscle_strength="Pre-operation muscle strength", 
                           Group_paresis="Groups of Paresis",  Duration_paresis="Duration of Paresis")

URP_slope<-use_labels(lumbar_coef, party::ctree(Slope~Duration_initial+Myotoma+Duration_paresis+Age+Levels+Preop_muscle_strength+BMI+Gender, 
                 data=..data,controls=ctree_control(testtype = "Bonferroni", maxdepth = 2)))

plot(URP_slope, main="Decision Tree for Slope of Recovery")

lumbar_coef$Preop_muscle_strength_factor <- as.factor(ifelse(lumbar_coef$Preop_muscle_strength>2, "Pre-operation muscle strength>2", "Pre-operation muscle strength=<2"))

#the library (expss) will FUCK UP the where() function, so have to clarify which package where() comes from
lumbar_coef$node_slope <- party::where(URP_slope)
lumbar_coef %>% group_by(node_slope) %>% summarise(mean=mean(Slope), median=median(Slope))

lumbar_coef$node_slope2 <- as.factor(ifelse(lumbar_coef$node_slope==2,2,
                                            ifelse(lumbar_coef$node_slope==4,4,5)))
lumbar_coef$Group_paresis2 <- relevel(lumbar_coef$Group_paresis, ref = "3")


lumbar_coef$Group_paresis3 <- as.factor(ifelse(lumbar_coef$Duration_paresis>3, ">3d", "=<3d"))
                                              
Slope_boxplot <-ggboxplot(data=subset(lumbar_coef, !is.na(Group_paresis3)), x="node_slope", y="Slope", fill="Group_paresis3")+
  ylab("Slope of Recovery")+
  facet_grid(.~Preop_muscle_strength_factor, scales = "free_x")+
  scale_fill_manual(values=c("steelblue4", "lightblue3"))+
  labs(fill='Timing of Surgery')+
  theme(
    axis.text = element_text(size=18),
    axis.text.y = element_text(size=16),
    legend.text=element_text(size=16),
    legend.title = element_text(size=18),
    strip.text.x = element_text(size = 16),
    axis.title.y = element_text(size=18),
    strip.background = element_rect(fill="grey60", colour="grey60"),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x=element_blank())

  ggtitle("Association of Slope of Recovery between Muscle Strength and Time of Surgery")

ggboxplot(data=subset(lumbar_coef, !is.na(Group_paresis2)), x="node_slope", y="Slope", fill="Preop_muscle_strength_factor")+
  ylab("Slope of Recovery")+
  facet_grid(.~Group_paresis2, scales = "free_x")+
  scale_fill_manual(values=c("steelblue4", "lightblue3"))+
  labs(fill='Pre-operation Muscle Strength')+
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

lumbar_coef$Muscle_Factor2 <- relevel(lumbar_coef$Muscle_Factor, ref = "severe")

lumbar_coef$Muscle_Factor3 <- as.factor(ifelse(lumbar_coef$Postop_muscle_strength_3m<4, "deteriorate", 
                                               ifelse(lumbar_coef$Postop_muscle_strength_3m == 5, "recovered", "unchanged")))


lumbar_coef_noNA <-subset(lumbar_coef, !is.na(Muscle_Factor2))

URP_muscle <- use_labels(lumbar_coef_noNA, ctree(Muscle_Factor2~Preop_muscle_strength+Duration_initial+Myotoma+Duration_paresis+Age+Levels+BMI+Gender, 
                                             data=..data, controls = ctree_control(testtype = "Bonferroni", maxdepth = 2)))

plot(URP_muscle, main="Decision Tree for Muscle Groups at 3 months")

# Muscle factor last 
lumbar_coef$Muscle_Factor_last <- as.factor(ifelse(lumbar_coef$Postop_muscle_strength_last<3, "severe", 
                                              ifelse(lumbar_coef$Postop_muscle_strength_last == 5, "recovered", "moderate")))

lumbar_coef$Muscle_Factor_last <- relevel(lumbar_coef$Muscle_Factor_last, ref = "severe")

lumbar_coef$Muscle_Factor_last2 <- as.factor(ifelse(lumbar_coef$Postop_muscle_strength_last<4, "deteriorate", 
                                               ifelse(lumbar_coef$Postop_muscle_strength_last == 5, "recovered", "unchanged")))


lumbar_coef_noNA_last <-subset(lumbar_coef, !is.na(Muscle_Factor_last))

URP_muscle_last <- use_labels(lumbar_coef_noNA_last, ctree(Muscle_Factor_last~Preop_muscle_strength+Duration_initial+Myotoma+Duration_paresis+Age+Levels+BMI+Gender, 
                                                 data=..data, controls = ctree_control(testtype = "Bonferroni")))

plot(URP_muscle_last, main="Decision Tree for Muscle Groups at LAST follow-up")

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


Muscle_barplot<-ggplot(subset(URP_muscle_node, !(node_muscle==2)), aes(x=Group_paresis, y=percent, fill=Muscle_Factor2))+
  geom_bar(colour="black", stat="identity", position = "stack" )+
  scale_fill_manual(values=c("darkolivegreen3", "darkseagreen4"))+
  theme_bw() +
  facet_grid(.~Preop_muscle_strength_factor, scales = "free_x")+
  theme(axis.line = element_line(colour = "black"),
        axis.text.x = element_text(size=16, colour = "black"),
        axis.text.y = element_text(size=16, colour = "black"),
        legend.text=element_text(size=16),
        legend.title = element_text(size=18),
        strip.text.x = element_text(size = 16),
        axis.title.y = element_text(size=18),
        axis.title.x = element_text(size=18),
        text=element_text(family="NimbusRom"),
        strip.background = element_rect(fill="grey60",colour="grey60"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  labs(fill="Groups of Muslce Strength at 3 months", y = "Percentage of Individuals", 
       x="Timing of Surgery")

#Create barplot for muscle factor at last time point
lumbar_coef_noNA_last$node_muscle <- party::where(URP_muscle_last)
URP_muscle_node_last <-lumbar_coef_noNA_last %>% group_by(node_muscle) %>% count(Muscle_Factor_last)
URP_muscle_node_last$sum <- ifelse(URP_muscle_node_last$node_muscle==3, 57, 
                              ifelse(URP_muscle_node_last$node_muscle==4,24,
                                     ifelse(URP_muscle_node_last$node_muscle==6, 191,117)))

URP_muscle_node_last$percent <- (URP_muscle_node_last$n/URP_muscle_node_last$sum)*100
URP_muscle_node_last$Preop_muscle_strength_factor <- as.factor(ifelse(URP_muscle_node_last$node_muscle==3|URP_muscle_node_last$node_muscle==4,"Pre-operation muscle strength=<2",
                                                                      "Pre-operation muscle strength>2"))

URP_muscle_node_last$Group_paresis <- as.factor(ifelse(URP_muscle_node_last$node_muscle==6|URP_muscle_node_last$node_muscle==7, "NS",
                                                       ifelse(URP_muscle_node_last$node_muscle==3, "=<6d", ">6d")))

ggplot(subset(URP_muscle_node_last, !(Preop_muscle_strength_factor=="Pre-operation muscle strength>2")), aes(x=Group_paresis, y=percent, fill=Muscle_Factor_last))+
  geom_bar(stat="identity", position = position_stack(), width = 0.94)+
  scale_fill_manual(values=c("steelblue4", "lightblue3", "palegreen1"))+
  theme_bw() +
  facet_grid(.~Preop_muscle_strength_factor, scales = "free_x")+
  theme(axis.line = element_line(colour = "black"),
        axis.text = element_text(size=14),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        legend.text=element_text(size=12),
        strip.text.x = element_text(size = 12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  labs(title="Association between Muscle Strength and Time of Surgery", 
       fill="Groups of Muscle Strength at last follow-up", y = "Percentage of Individuals", 
       x="Timing of Surgery")
  


ggarrange(Outcomes_corplot, ggarrange(Muscle_barplot, Slope_boxplot,labels=c("B", "C"), legend="bottom", ncol=2), nrow=2, labels="A")

ggarrange(Muscle_barplot, Slope_boxplot,labels=c("A", "B"), legend="bottom", ncol=2)

#Subset data to create another barplot!!!!
Lumbar_3m <- select(lumbar_coef, "PatID", "Duration_paresis", "Group_paresis3", "Preop_muscle_strength", "Postop_muscle_strength_3m", "Postop_muscle_strength_last")

#3m vs last diff scores
Lumbar_3m$Diff_scores <- Lumbar_3m$Postop_muscle_strength_3m-Lumbar_3m$Preop_muscle_strength

Lumbar_3m$Diff_scores_last <- Lumbar_3m$Postop_muscle_strength_last-Lumbar_3m$Preop_muscle_strength

#3m vs last change scores
Lumbar_3m$Change_scores <- as.factor(ifelse(Lumbar_3m$Diff_scores>=1&Lumbar_3m$Postop_muscle_strength_3m==5, "recovered", 
                                           ifelse(Lumbar_3m$Diff_scores>=1&Lumbar_3m$Postop_muscle_strength_3m==4,"improved", "unchanged")))

Lumbar_3m$Change_scores_last <- as.factor(ifelse(Lumbar_3m$Diff_scores_last>=1&Lumbar_3m$Postop_muscle_strength_last==5, "recovered", 
                                            ifelse(Lumbar_3m$Diff_scores_last>=1&Lumbar_3m$Postop_muscle_strength_last==4,"improved", "unchanged")))

Lumbar_3m <- na.omit(Lumbar_3m)

Lumbar_3m$Muscle_Factor_pre <- as.factor(ifelse(Lumbar_3m$Preop_muscle_strength<3, "severe", 
                                              ifelse(Lumbar_3m$Preop_muscle_strength== 4, "mild", "moderate")))

#New data to create graph for change scores of 3m 
Lumbar_3m_factor <-Lumbar_3m %>% group_by(Group_paresis3, Muscle_Factor_pre) %>% count(Change_scores)

Lumbar_3m_factor$percent <- ifelse(Lumbar_3m_factor$Group_paresis3=="=<3d"&Lumbar_3m_factor$Muscle_Factor_pre=="severe", (Lumbar_3m_factor$n/38)*100,
                                   ifelse(Lumbar_3m_factor$Group_paresis3=="=<3d"&Lumbar_3m_factor$Muscle_Factor_pre=="mild",(Lumbar_3m_factor$n/44)*100,
                                          ifelse(Lumbar_3m_factor$Group_paresis3=="=<3d"&Lumbar_3m_factor$Muscle_Factor_pre=="moderate", (Lumbar_3m_factor$n/109)*100,
                                                 ifelse(Lumbar_3m_factor$Group_paresis3==">3d"&Lumbar_3m_factor$Muscle_Factor_pre=="severe", (Lumbar_3m_factor$n/36)*100,
                                                        ifelse(Lumbar_3m_factor$Group_paresis3==">3d"&Lumbar_3m_factor$Muscle_Factor_pre=="mild", (Lumbar_3m_factor$n/36)*100, (Lumbar_3m_factor$n/67)*100
                                                        )))))
                                   
Lumbar_3m_factor$percent <- round(Lumbar_3m_factor$percent, digits = 1)

Lumbar_3m_factor$Change_scores <- relevel(Lumbar_3m_factor$Change_scores, ref = "unchanged")

#new data to create graph for change scores at last follow up
Lumbar_last <- subset(Lumbar_3m, !is.na(Postop_muscle_strength_last))

Lumbar_last_factor <-Lumbar_last %>% group_by(Group_paresis3, Muscle_Factor_pre) %>% count(Change_scores_last)

Lumbar_last_factor$percent <- ifelse(Lumbar_last_factor$Group_paresis3=="=<3d"&Lumbar_last_factor$Muscle_Factor_pre=="severe", (Lumbar_last_factor$n/41)*100,
                                   ifelse(Lumbar_last_factor$Group_paresis3=="=<3d"&Lumbar_last_factor$Muscle_Factor_pre=="mild",(Lumbar_last_factor$n/56)*100,
                                          ifelse(Lumbar_last_factor$Group_paresis3=="=<3d"&Lumbar_last_factor$Muscle_Factor_pre=="moderate", (Lumbar_last_factor$n/115)*100,
                                                 ifelse(Lumbar_last_factor$Group_paresis3==">3d"&Lumbar_last_factor$Muscle_Factor_pre=="severe", (Lumbar_last_factor$n/40)*100,
                                                        ifelse(Lumbar_last_factor$Group_paresis3==">3d"&Lumbar_last_factor$Muscle_Factor_pre=="mild", (Lumbar_last_factor$n/61)*100, (Lumbar_last_factor$n/76)*100
                                                        )))))

Lumbar_last_factor$percent <- round(Lumbar_last_factor$percent, digits = 1)

Lumbar_last_factor$Change_scores_last <- relevel(Lumbar_last_factor$Change_scores_last, ref = "unchanged")

#graph for 3m change scores
ggplot(Lumbar_3m_factor, aes(x=Muscle_Factor_pre, y=percent, fill=Change_scores))+
  geom_bar(colour="black", stat="identity", position = "stack" )+
  scale_fill_manual(values=c("coral1", "goldenrod2", "seagreen4"))+
  theme_bw() +
  facet_grid(.~Group_paresis3, scales = "free_x")+
  theme(axis.line = element_line(colour = "black"),
        axis.text.x = element_text(size=14, colour = "black"),
        axis.text.y = element_text(size=14, colour = "black"),
        legend.text=element_text(size=14),
        legend.title = element_text(size=16),
        legend.position="top",
        strip.text.x = element_text(size = 14),
        axis.title.y = element_text(size=16),
        axis.title.x = element_text(size=16),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  geom_text(aes(label=paste0((percent))),
            position=position_stack(vjust=0.5),size=3)+
  labs(fill="Change of Muscle Strength at 3 months", y = "Percentage of Individuals", 
       x="Muscle Groups Pre-operation")

#graph for last follow up change scores
ggplot(Lumbar_last_factor, aes(x=Muscle_Factor_pre, y=percent, fill=Change_scores_last))+
  geom_bar(colour="black", stat="identity", position = "stack" )+
  scale_fill_manual(values=c("coral1", "goldenrod2", "seagreen4"))+
  theme_bw() +
  facet_grid(.~Group_paresis3, scales = "free_x")+
  theme(axis.line = element_line(colour = "black"),
        axis.text.x = element_text(size=14, colour = "black"),
        axis.text.y = element_text(size=14, colour = "black"),
        legend.text=element_text(size=14),
        legend.title = element_text(size=16),
        legend.position="top",
        strip.text.x = element_text(size = 14),
        axis.title.y = element_text(size=16),
        axis.title.x = element_text(size=16),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  geom_text(aes(label=paste0((percent))),
            position=position_stack(vjust=0.5),size=3)+
  labs(fill="Change of Muscle Strength at last follow up", y = "Percentage of Individuals", 
       x="Muscle Groups Pre-operation")

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
descrip <- table1(~as.factor(Levels)+as.factor(Gender)+Age+BMI+Preop_muscle_strength+Muscle_Factor2+Slope+Muscle_Factor_last|Group_paresis3, data=lumbar_coef)
write.table(descrip, file="descrip.txt")

#Correlation matrix 
Lumbar_330 = filter(lumbar_coef, PatID %in% c(1:330))

Outcomes <- select(Lumbar_330, starts_with("Preop_muscle"), starts_with("Postop_muscle"))
colnames(Outcomes)=c("Pre-op Muscle Strength","Post-op Muscle Strength","Post-op Muscle Strength 6w","Post-op Muscle Strength 3m","Post-op Muscle Strength last")

Cor_outcomes <- cor(Outcomes, use = "complete.obs")

Outcomes_corplot<-ggcorrplot(Cor_outcomes, type="lower", lab = TRUE, outline.color = "black")
# New dataset for surgery 
Surgery$`time of injury` <- format(as.POSIXct(Surgery$`time of injury` ,format="%H:%M:%S"),"%H:%M")
Surgery$`Time of surgery` <- format(as.POSIXct(Surgery$`Time of surgery` ,format="%H:%M:%S"),"%H:%M")

Surgery$Date_time_injury <- as.POSIXct(paste(Surgery$`Date of injury`, Surgery$`time of injury`), format="%Y-%m-%d %H:%M")
Surgery$Date_time_surgery <- as.POSIXct(paste(Surgery$`Surgery`, Surgery$`Time of surgery`), format="%Y-%m-%d %H:%M")

Surgery$Time_diff_surgery <- as.numeric(difftime(Surgery$Date_time_surgery, Surgery$Date_time_injury, units="mins"))

Surgery$Time_diff_discharge <- as.numeric(difftime(Surgery$Discharge, Surgery$Admission, units="days"))

Surgery$Time_diff_EMSCI <- as.numeric(difftime(Surgery$`Last EMSCI`, Surgery$`First EMSCI`, units="days"))

Last_LEMS<-ctree(`Last LEMS`~Time_diff_surgery+Time_diff_discharge+Time_diff_EMSCI+`1_LEMS`, data=subset(Surgery, !is.na(`Last LEMS`)), controls = ctree_control(testtype = "Bonferroni"))
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




