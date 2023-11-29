packages <- c("dplyr", "survival", "survminer","fastDummies", "gridExtra")
if ( length(missing_pkgs <- setdiff(packages, rownames(installed.packages()))) > 0) {
  message("Installing missing package(s): ", paste(missing_pkgs, collapse = ", "))
  install.packages(missing_pkgs)
}
lapply(packages, library, character.only = TRUE)

data <- read.csv("C:/Users/ASUS/Downloads/dataset_heart_reduced.csv")



model1 <- coxph(Surv(Age, HeartDisease) ~ Sex+ChestPainType
                +Cholesterol+MaxHR+ExerciseAngina+ST_Slope, data=df)

summary(model1)

model2 <- coxph(Surv(Age, HeartDisease) ~ Sex
                +Cholesterol+MaxHR, data=df)

summary(model2)

model3 <- coxph(Surv(Age, HeartDisease) ~
                Cholesterol+MaxHR+ExerciseAngina+ST_Slope, data=df)

summary(model3)

model4 <- model1 <- coxph(Surv(Age, HeartDisease) ~ MaxHR+ExerciseAngina+ST_Slope
                          , data=df)

summary(model4)

model4 <- coxph(Surv(Age, HeartDisease) ~ MaxHR+ExerciseAngina
                          , data=df)

summary(model4)

model5 <- coxph(Surv(Age, HeartDisease) ~ ExerciseAngina+ST_Slope
                          , data=df)

summary(model5)

model6 <- coxph(Surv(Age, HeartDisease) ~ ST_Slope
                , data=df)

summary(model6)


surfitnew <- survfit(Surv(Age, HeartDisease)~ST_Slope, data=df)
summary(surfitnew)
plot(surfitnew)
ggsurvplot(surfitnew, conf.int=TRUE, pval=TRUE, risk.table=TRUE, 
           legend.labs=c("Up", "Flat", "Down"), legend.title="ST_Slope", 
           main="Kaplan-Meier Curve for Heart Failure", 
           risk.table.height=.25)


surfitnew <- survfit(Surv(Age, HeartDisease)~ST_Slope, data=df)
summary(surfitnew)
plot(surfitnew)

ggsurvplot(surfitnew, conf.int=TRUE, pval=TRUE, risk.table=TRUE, 
           legend.labs=c("Down", "Flat", "Up"), legend.title="ST_Slope", 
           main="Kaplan-Meier Curve for Heart Failure", 
           risk.table.height=.25)

ggsurvplot(surfitnew, conf.int=TRUE, pval=TRUE, risk.table=TRUE, 
           legend.labs=c("Down", "Flat", "Up"), legend.title="ST_Slope", 
           main="Kaplan-Meier Curve for Heart Failure", 
           risk.table.height=.25, fun = "cumhaz")


###Asumsi PH
surfitnew2 <- survfit(model6)

cumulative_hazard <- -log(surfitnew2$surv)

plot(log(surfitnew2$time), log(cumulative_hazard), type = "s",
     xlab = "Log(Time)", ylab = "Log(Cumulative Hazard)",
     main = "Log Cumulative Hazard Curve")


###Statistik Uji PH
# Perform the proportional hazards test
proportional_hazards_test <- cox.zph(model6)
print(proportional_hazards_test)





