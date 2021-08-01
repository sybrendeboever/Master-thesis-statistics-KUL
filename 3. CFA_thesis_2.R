# ----------------------------------------------------------------------------------------
# --------------------------- Confirmatory factor analysis ------------------------------- #
library(lavaan)
library(ggplot2)
library(dplyr)
# Final dataset
data = read.csv("D:/Unief/Master Statistiek/Jaar 2/Thesis/6. Data/all_NA_2019_2020_NoMiss_OmgedraaideScales_CorrecteQ22.csv")
data$id = paste(data$Student_ID,data$YEAR,sep="_")
data2 = read.csv("D:/Unief/Master Statistiek/Jaar 2/Thesis/6. Data/Results/CFA/dataq_NA.csv") # Dataq_NA bevat de first attempt van students
data2$id = paste(data2$Student_ID,data2$YEAR,sep="_")
data = data[data$id %in% data2$id,]; rm(data2)
data$Q5[data$Q5==3] = NA

# Subset data
set.seed(1234)
sample = sample(c(1:nrow(data)),
                size=round(0.40*nrow(data)), # 40% of the data
                replace=FALSE) # Obtain unique row numbers
test = data[-sample,]

# ----------------------------------- Multiple imputation ----------------------------------------
library(mice)
library(semTools)
# Order de survey itmes
items = c("Q5","Q8","Q9","Q14_1","Q14_2","Q14_3","Q14_4","Q16","Q17_2","Q17_3","Q17_4","Q17_5","Q17_6",
          "Q20","Q23","Q24_1","Q24_2","Q24_3","Q24_4","Q24_5","Q25_2","Q25_3","Q25_4","Q25_5",
          "Q30","Q32_1","Q32_2","Q32_3","Q32_4","Q32_5","Q32_6","Q32_7",
          "Gender","Age","Fase")
test2 = test[,items]
test2 = data.frame(apply(test2,2,factor))
test2$Age = as.numeric(as.character(test2$Age))

# Predictor matrix
predictormatrix = quickpred(test2, 
                            include=c("Gender","Age","Fase","Q5"), # standard background predictors
                            exclude=NULL,
                            mincor = 0.25,
                            method = "kendall")
# MI
m = 20
imp_gen = mice(data=test2,
               predictorMatrix = predictormatrix,
               method = c("logreg",rep("polr",12),"logreg",rep("polr",18),"logreg","pmm","polr"), 
               m = m,
               maxit = 20,            
               diagnostics=TRUE)
mice.imp <- NULL
for (i in 1:m) {
  mice.imp[[i]] <- complete(imp_gen, action=i,inc=FALSE)
}

# -------------------------------- CFA model -------------------------------
## Base models
model = ' # New measurement model
        CE =~ Q32_1 + Q32_2 + Q32_3 + Q32_4 + Q32_5 + Q32_6 + Q32_7 + Q30
        CFC =~ Q17_2 + Q17_3 + Q17_4 + Q17_5 + Q17_6 + Q16
        CC =~ Q25_2 + Q25_3 + Q25_4 + Q23
        RU =~ Q14_3 + Q14_2 + Q14_1
        CU =~ Q24_1 + Q24_3 + Q24_4 + Q24_5 # no 14_4
'

## Modification indices taken into account
model = ' # Remove Q24_2 + add cor(Q16,Q23) + cor(Q24_4,Q24_5) + cor(Q30,32_4)
        CE =~ Q32_1 + Q32_2 + Q32_3 + Q32_4 + Q32_5 + Q32_6 + Q32_7 + Q30
        CFC =~ Q17_2 + Q17_3 + Q17_4 + Q17_5 + Q17_6 + Q16
        CC =~ Q25_2 + Q25_3 + Q25_4 + Q23
        RU =~ Q14_3 + Q14_2 + Q14_1
        CU =~ Q24_1 + Q24_3 + Q24_4 + Q24_5 # no 14_4
        
        Q16 ~~ Q23
        Q24_4	~~	Q24_5
        Q30	~~	Q32_4
'
## Model run
out = runMI(model,
            data=mice.imp,
            fun="cfa",
            estimator="DWLS",
            ordered=TRUE,
            seed=1234,
            std.lv=TRUE)
summary(out,standardized=TRUE,fit.measures=TRUE)

## Model fit indices per imputed dataset
df = c()
for (i in 1:length(mice.imp)){
  fit = lavaan::cfa(model,data=mice.imp[[i]],
                    estimator="DWLS",
                    ordered=TRUE,
                    std.lv=TRUE)
  df = rbind(df,fitmeasures(fit)[c("cfi","tli","rmsea","srmr")])
};df

## Modification indices
a = modindices.mi(out)
a$label = paste(a$lhs,a$op,a$rhs,sep="")
### plot mi and epc
ggplot(data = a[a$mi > 5,], aes(x=mi))+
  geom_histogram(binwidth = 1,fill="grey",col="black")+
  theme_bw()+
  scale_y_continuous(expand=c(0, 0.1))+
  scale_x_continuous(expand=c(0, 5),name="Modification index",breaks = seq(0,250,by=10))+
  geom_vline(xintercept = c(5,20),col="navyblue")+
  theme(axis.text.x = element_text(angle = 30, hjust=1))
ggplot(data=a[a$mi > 5,],aes(x=mi,y=sepc.lv,col=op))+
  geom_vline(xintercept=c(5,20),colour="gray75")+
  geom_hline(yintercept = c(0.3,-0.3),colour="gray75")+
  geom_point(alpha=0.5)+
  scale_x_log10()+ # log transform for clear visibility
  theme_bw()+
  scale_colour_manual(values=c("gray25","blue"),name=NULL)+
  theme(legend.position = c(0.12,0.91),panel.grid = element_blank())+
  xlab("Modification index")+ylab("Standardized expected parameter change (sepc.lv)")+
  ggrepel::geom_text_repel(data = a[a$mi>20 & (a$sepc.lv>0.3|a$sepc.lv< -0.3),], aes(label = label),
                           box.padding = 0.55,cex=3)
### Investigate the MIs
length(which(a$mi >= 5 & a$mi <=20)) # 74
length(which(a$mi>=20)) # 14
View(doBy::orderBy(~ -mi,a[a$mi >= 20,]))
sort(table(a$rhs[a$mi >= 20 & a$op=="=~"]))
View(doBy::orderBy(~ -mi,a[a$mi > 20 & abs(a$sepc.lv)>0.3,]))
a$mi_cut = 5
a$mi_cut[a$mi>=20] = 20
table(a[a$mi>5,]$op,a[a$mi>5,]$mi_cut)
table(a[a$mi>5 & a$op=="=~",]$lhs,a[a$mi>5 & a$op=="=~",]$rhs,a[a$mi>5 & a$op=="=~",]$mi_cut)

## Fit measures
fitMeasures(fit)

## Model test
pchisq(q=attributes(fit)$test$standard$stat,
       df=attributes(fit)$test$standard$df,lower.tail=F)


# ----------------------------------- confirmatory measures ----------------------------------------
# LRT
chi = 743.722-577.84
pchisq(chi,df=3,lower.tail=F)
anova(fit1,fit2)
# NPD
eigen(lavInspect(fit, "cov.lv"))$values
eigen(lavInspect(fit, "cor.lv"))$values
eigen(lavInspect(fit, "cov.ov"))$values
eigen(lavInspect(fit, "cor.ov"))$values
eigen(lavInspect(fit, "cov.all"))$values
eigen(lavInspect(fit, "cor.all"))$values
cor = psych::polychoric(test[,order]) # Of input correlation matrix
eigen(cor$rho)$values

# Cronbach's alpha
## CE: 0.83
psych::alpha(test[,c("Q30","Q32_1","Q32_2","Q32_3","Q32_4","Q32_5","Q32_6","Q32_7")],check.keys = TRUE)$total[1]
## CFC: 0.76
psych::alpha(test[,c("Q17_2","Q17_3","Q17_4","Q17_5","Q17_6","Q16")],check.keys = TRUE)$total[1]
## CC: 0.56
psych::alpha(test[,c("Q25_2","Q25_3","Q25_4","Q23")],check.keys = TRUE)$total[1]
## RU: 0.54
psych::alpha(test[,c("Q14_3","Q14_2","Q14_1")],check.keys = TRUE)$total[1]
## CU: 0.51
psych::alpha(test[,c("Q24_1","Q24_3","Q24_4","Q24_5")],check.keys = TRUE)$total[1]

# MSA
## CE: 0.86
psych::KMO(test[,c("Q30","Q32_1","Q32_2","Q32_3","Q32_4","Q32_5","Q32_6","Q32_7")])$MSA
## CFC: 0.80
psych::KMO(test[,c("Q17_2","Q17_3","Q17_4","Q17_5","Q17_6","Q16")])$MSA
## CC: 0.67
psych::KMO(test[,c("Q25_2","Q25_3","Q25_4","Q23")])$MSA
## RU: 0.62
psych::KMO(test[,c("Q14_3","Q14_2","Q14_1")])$MSA
## CU: 0.60
psych::KMO(test[,c("Q24_1","Q24_3","Q24_4","Q24_5")])$MSA

# Variance explained per factor
a = summary(fit,standardized=TRUE,fit.measures=TRUE)
parms = a$PE[a$PE$op=="=~",c("lhs","op","rhs","std.lv")]
library(dplyr)
parms %>%
  as_tibble() %>%
  group_by(lhs) %>%
  summarise(sum(std.lv^2)/n())

# Averge correlation (Clark & Watson 1995)
## CE
a = psych::polychoric(test[,c("Q30","Q32_1","Q32_2","Q32_3","Q32_4","Q32_5","Q32_6","Q32_7")])$rho
mean(abs(a[lower.tri(a)])) # 0.42
## CFC
a = psych::polychoric(test[,c("Q17_2","Q17_3","Q17_4","Q17_5","Q17_6","Q16")])$rho
mean(abs(a[lower.tri(a)])) # 0.44
## CC
a = psych::polychoric(test[,c("Q25_2","Q25_3","Q25_4","Q23")])$rho
mean(abs(a[lower.tri(a)])) # 0.32
## RU
a = psych::polychoric(test[,c("Q14_3","Q14_2","Q14_1")])$rho
mean(abs(a[lower.tri(a)])) # 0.36
## CU
a = psych::polychoric(test[,c("Q24_1","Q24_3","Q24_4","Q24_5")])$rho
mean(abs(a[lower.tri(a)])) # 0.27




# --------------------------- Multigroup analysis: original data (0 cells) (don't) ---------------------
# Normal
model = ' # Remove Q24_2 + add cor(Q16,Q23) + cor(Q24_4,Q24_5) + cor(Q30,32_4)
        CE =~ Q32_1 + Q32_2 + Q32_3 + Q32_4 + Q32_5 + Q32_6 + Q32_7 + Q30
        CFC =~ Q17_2 + Q17_3 + Q17_4 + Q17_5 + Q17_6 + Q16
        CC =~ Q25_2 + Q25_3 + Q25_4 + Q23
        RU =~ Q14_3 + Q14_2 + Q14_1
        CU =~ Q24_1 + Q24_3 + Q24_4 + Q24_5 # no 14_4
        
        Q16 ~~ Q23
        Q24_4	~~	Q24_5
        Q30	~~	Q32_4
'
w = which(!is.na(test$Q14_1) & test$Gender=="V") # To remove zero cells
test$Q14_1[w[1]] = 1
w = which(!is.na(test$Q24_3) & test$Gender=="V")
test$Q24_3[w[1]] = 4

model_config = cfa(model,data=test,
                   estimator="DWLS",
                   std.lv=TRUE,
                   missing="pairwise",
                   ordered=TRUE,
                   group="Gender")
summary(model_config,standardized=TRUE,fit.measures=TRUE)

# With MI: bad convergence: negative variances and out of bound fit measures
config = cfa.mi(model,data=mice.imp,
                estimator="DWLS",
                ordered=TRUE,
                group="Gender")
summary(config,standardized=TRUE,fit.measures=TRUE)


# ------------------------------ Multigroup analysis: remove 0 cell-ness -------------------------------
items = c("Q14_1","Q14_2","Q14_3","Q16","Q17_2","Q17_3","Q17_4","Q17_5","Q17_6",
          "Q23","Q24_1","Q24_3","Q24_4","Q24_5","Q25_2","Q25_3","Q25_4",
          "Q30","Q32_1","Q32_2","Q32_3","Q32_4","Q32_5","Q32_6","Q32_7")
### Gender
# Cell count
for (i in items){
  print(i)
  print(table(test$Gender,test[,i]))
}
c = c("Q14_1", # 1 (merge 1-2)
      "Q24_3") # 4 (merge 3-4)

test2 = test
test2$Q14_1[test2$Q14_1 %in% c(1,2)] = 2
test2$Q24_3[test2$Q24_3 %in% c(3,4)] = 3
# MI
# Order de survey itmes
items = c("Q5","Q8","Q9","Q14_1","Q14_2","Q14_3","Q14_4","Q16","Q17_2","Q17_3","Q17_4","Q17_5","Q17_6",
          "Q20","Q23","Q24_1","Q24_2","Q24_3","Q24_4","Q24_5","Q25_2","Q25_3","Q25_4","Q25_5",
          "Q30","Q32_1","Q32_2","Q32_3","Q32_4","Q32_5","Q32_6","Q32_7",
          "Gender","Age","Fase")
test2 = test2[,items]
test2 = data.frame(apply(test2,2,factor))
test2$Age = as.numeric(as.character(test2$Age))

# Predictor matrix
predictormatrix = quickpred(test2, 
                            include=c("Gender","Age","Fase","Q5"), # standard background predictors
                            exclude=NULL,
                            mincor = 0.25,
                            method = "kendall")
# MI
m = 20
imp_gen = mice(data=test2,
               predictorMatrix = predictormatrix,
               method = c("logreg",rep("polr",12),"logreg",rep("polr",18),"logreg","pmm","polr"), 
               m = m,
               maxit = 20,            
               diagnostics=TRUE)
mice.imp <- NULL
for (i in 1:m) {
  mice.imp[[i]] <- complete(imp_gen, action=i,inc=FALSE)
}

model = ' # Remove Q24_2 + add cor(Q16,Q23) + cor(Q24_4,Q24_5) + cor(Q30,32_4)
        CE =~ Q32_1 + Q32_2 + Q32_3 + Q32_4 + Q32_5 + Q32_6 + Q32_7 + Q30
        CFC =~ Q17_2 + Q17_3 + Q17_4 + Q17_5 + Q17_6 + Q16
        CC =~ Q25_2 + Q25_3 + Q25_4 + Q23
        RU =~ Q14_3 + Q14_2 + Q14_1
        CU =~ Q24_1 + Q24_3 + Q24_4 + Q24_5 # no 14_4
        
        Q16 ~~ Q23
        Q24_4	~~	Q24_5
        Q30	~~	Q32_4
'
# Configural equivalence
config = cfa.mi(model,data=mice.imp[c(1:7,9,11,14:16,18:20)], # 8,10,12,13,17 gave negative variances
                estimator="DWLS",
                std.lv=TRUE,
                ordered=TRUE,
                group="Gender")
summary(config,standardized=TRUE,fit.measures=TRUE)
# Compare fit
lavTestLRT.mi(config)
modindices.mi(config)
# Metric equivalence
metric = cfa.mi(model,data=mice.imp[c(1:7,9,11,14:16,18:20)], # 8,10,12,13,17 gave negative variances
                estimator="DWLS",
                std.lv=TRUE,
                ordered=TRUE,
                group="Gender",
                group.equal=c("thresholds","loadings"))
summary(metric,standardized=TRUE,fit.measures=TRUE)
# Compare fit
lavTestLRT.mi(metric) # Idk, maar dit lijkt niet te kloppen (lijkt dezelfde waarde als config te hebben, en de parameters voor de twee models zijn alleshebalve hetzelfde (cor(CFC,CU) is zwaar verschillende voor de twee groepen + cor is negatief))
modindices.mi(metric)


