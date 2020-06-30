library(reshape2)
library(lattice)
library(lme4)
library(expss)
library(party)
library(ggpubr)

#To perfrom LMER, I transformed wide to long data format 
Lumbar_long<- melt(lumbar_disc_herniation, id.vars=c("PatID", "Levels", "Age", "Gender", "Group_paresis"),
                   measure.vars=c("Preop_muscle_strength", "Postop_muscle_strength", "Postop_muscle_strength_6w",
                                  "Postop_muscle_strength_3m", "Postop_muscle_strength_last"),
                   variable.name ="Time", 
                   value.name ="Muscle_strength")

Lumbar_long$Time <- as.numeric(Lumbar_long$Time)

#This is based on the first 50 observations only 
Lumbar_50 <- head(lumbar_disc_herniation, 50)

Lumbar_long_50<- melt(Lumbar_50, id.vars=c("PatID", "Levels", "Age", "Gender", "Group_paresis"),
                      measure.vars=c("Preop_muscle_strength", "Postop_muscle_strength", "Postop_muscle_strength_6w",
                                     "Postop_muscle_strength_3m", "Postop_muscle_strength_last"),
                      variable.name ="Time", 
                      value.name ="Muscle_strength")

Lumbar_long_50$Time <- as.numeric(Lumbar_long_50$Time)

#Lattice plot for the first 50 observations 
lattice_plot<-xyplot(Muscle_strength~Time|PatID, col.line = "red", layout = c(10,5),
                     grid = TRUE, type = c("p", "r"), data = Lumbar_long_50, ylab="Muscle Strength", 
                     main="Trellis Plot of Muscle Strength of the First 50 Individuals Over Time")

plot(lattice_plot)

#Performing LMER of muscle strength over time
Lumbar_lmer <- lmer(Muscle_strength ~ Time + (1+Time|PatID), REML = FALSE, 
                    data = Lumbar_long)

#Extract intercept and slope from LMER
coef_lumbar <- coef(Lumbar_lmer)$PatID[1:2]
setDT(coef_lumbar, keep.rownames = TRUE)[]
colnames(coef_lumbar) <- c("PatID","Intercept", "Slope")

#Then merge them with original data 
lumbar_coef <- merge(lumbar_disc_herniation, coef_lumbar, by="PatID")

#Performing URP on this Slope as outcome
lumbar_coef = apply_labels(lumbar_coef, Preop_muscle_strength="Pre-operation muscle strength", 
                           Group_paresis="Groups of Paresis",  Duration_paresis="Duration of Paresis")

URP_slope<-use_labels(lumbar_coef, party::ctree(Slope~Duration_initial+Myotoma+Duration_paresis+Age+Levels+Preop_muscle_strength+BMI+Gender, 
                                                data=..data,controls=ctree_control(testtype = "Bonferroni", maxdepth = 2)))

plot(URP_slope, main="Decision Tree for Slope of Recovery")

#Creating boxplot based on the URP tree above:
lumbar_coef$Preop_muscle_strength_factor <- as.factor(ifelse(lumbar_coef$Preop_muscle_strength>2, "Pre-operation muscle strength>2", "Pre-operation muscle strength=<2"))
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

#Categorizing muscle outcome at 3m into 3 categories: 
lumbar_coef$Muscle_Factor <- as.factor(ifelse(lumbar_coef$Postop_muscle_strength_3m<3, "severe", 
                                              ifelse(lumbar_coef$Postop_muscle_strength_3m == 5, "recovered", "moderate")))

lumbar_coef$Muscle_Factor2 <- relevel(lumbar_coef$Muscle_Factor, ref = "severe")

lumbar_coef$Muscle_Factor3 <- as.factor(ifelse(lumbar_coef$Postop_muscle_strength_3m<4, "deteriorate", 
                                               ifelse(lumbar_coef$Postop_muscle_strength_3m == 5, "recovered", "unchanged")))

#Removing all NA in this outcome for URP: 
lumbar_coef_noNA <-subset(lumbar_coef, !is.na(Muscle_Factor2))

#URP for Muscle factor at 3m
URP_muscle <- use_labels(lumbar_coef_noNA, ctree(Muscle_Factor2~Preop_muscle_strength+Duration_initial+Myotoma+Duration_paresis+Age+Levels+BMI+Gender, 
                                                 data=..data, controls = ctree_control(testtype = "Bonferroni", maxdepth = 2)))

plot(URP_muscle, main="Decision Tree for Muscle Groups at 3 months")

#Subset data to create another plot
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
Muscle_factor_last_barplot <-ggplot(Lumbar_last_factor, aes(x=Muscle_Factor_pre, y=percent, fill=Change_scores_last))+
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

#Figure 1:
ggarrange(Slope_boxplot,Muscle_factor_last_barplot,labels=c("A", "B"), legend="bottom", ncol=2)

#Sensitivity Analysis (Outcomes for only pre-op muscle of 4):
plot(use_labels(lumbar_coef_noNA,ctree(Muscle_Factor3~Duration_paresis,
                                       data=subset(..data, Preop_muscle_strength==4))), 
     main="Time of Surgery in Mild Pre-op Muscle")

plot(use_labels(lumbar_coef_noNA,ctree(Slope~Duration_paresis,
                                       data=subset(..data, Preop_muscle_strength==4))), 
     main="Time of Surgery in Mild Pre-op Muscle")


