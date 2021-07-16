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

# -------------------------------- CFA model -------------------------------
## Base models
# 1. First-order model
model = ' # New measurement model
        CE =~ Q32_1 + Q32_2 + Q32_3 + Q32_4 + Q32_5 + Q32_6 + Q32_7 + Q30
        CFC =~ Q17_2 + Q17_3 + Q17_4 + Q17_5 + Q17_6 + Q16
        CC =~ Q25_2 + Q25_3 + Q25_4 + Q23
        RU =~ Q14_3 + Q14_2 + Q14_1
        CU =~ Q24_1 + Q24_3 + Q24_4 + Q24_5 # no 14_4
'

# 2. Second-order model
model = ' # New measurement model + 2nd-order PRA
       CE =~ Q30 + Q32_1 + Q32_2 + Q32_3 + Q32_4 + Q32_5 + Q32_6 + Q32_7
        CFC =~ Q17_2 + Q17_3 + Q17_4 + Q17_5 + Q16 + Q17_6
        CC =~ Q25_2 + Q25_3 + Q25_4 + Q23
        
        RU =~ Q14_1 + Q14_2 + Q14_3
        CU =~ Q24_1 + Q24_4 + Q14_4 + Q24_3 + Q24_5
        PRA =~ a*RU + a*CU
'
## Modification indices taken into account
# 1. First-order model
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
# 2. Second-order model
model = ' # New measurement model + 2nd-order PRA
        
        Q16 ~~ Q23
        Q24_4	~~	Q24_5
        Q30	~~	Q32_4
'


## Model investigation
# 1. fit the models
fit = cfa(model,data=test,
          estimator="DWLS",
          std.lv=TRUE,
          missing = "pairwise",
          ordered = TRUE) # Declares all observed endogenous variables as ordered
summary(fit,standardized=TRUE,fit.measures=TRUE)
semTools::discriminantValidity(fit,cutoff = 0.80) # check discriminant validity of constructs

## 2. MI
a = modificationindices(fit)
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

## Standardized residuals
View(lavResiduals(fit,zstat=TRUE)$cov.z)
offdiag = data.frame(lavResiduals(fit,zstat=TRUE)$cov.z[lower.tri(lavResiduals(fit,zstat=TRUE)$cov.z)])
names(offdiag) = "st.res"
length(offdiag$st.res[abs(offdiag$st.res) >2.58]) # 54
### Histogram
ggplot(offdiag,aes(x=st.res))+
  geom_histogram(binwidth = 0.25,fill="gray",col="black")+
  theme_bw()+
  geom_vline(xintercept = c(-2.85,2.85))+
  scale_y_continuous(expand=c(0, 0.1),name="Count")+
  scale_x_continuous(expand=c(0, 5),name="Standardized residuals")
### Heatmap
heat = data.frame(lavResiduals(fit,zstat=TRUE)$cov.z)
names = row.names(heat)
heat2 = stack(heat)
heat2$x = rep(names)
heat2$values[heat2$values <= 2.58 & heat2$values >= -2.58] = 0 # for clearer visualization
pal = colorRampPalette(c("navyblue","lightblue","white","coral","red"))
order = c("Q30","Q32_1","Q32_2","Q32_3","Q32_4","Q32_5","Q32_6","Q32_7",
          "Q17_2","Q17_3","Q17_4","Q17_5","Q16","Q17_6",
          "Q25_2","Q25_3","Q25_4","Q23",
          "Q14_1","Q14_2","Q14_3",
          "Q24_1","Q14_4","Q24_3","Q24_4","Q24_5")
heat2$ind = factor(heat2$ind, levels=order)
heat2$x = factor(heat2$x, levels=order)
ggplot(heat2,aes(x=x,y=ind,fill=values))+
  geom_tile(col="white")+
  scale_fill_gradientn(colours = pal(50),name="St. res.")+
  ylab(NULL)+xlab(NULL)+
  theme(axis.text.x = element_text(angle = 30, hjust=1))+
  annotate("text",x = heat2$x[abs(heat2$values)>5],y=heat2$ind[abs(heat2$values)>5],
           label = round(heat2$values[abs(heat2$values)>5],1),
           colour="white",cex=2.5)

# Plot the model
semPlot::semPaths(fit, "std", style="lisrel",
                  residuals=FALSE,
                  intercepts = FALSE,
                  thresholds = FALSE,
                  theme="colorblind",
                  curvePivot=TRUE,
                  rotation=1,
                  structural=FALSE,
                  layout="tree2")

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
out = runMI(model,
            data=mice.imp,
            fun="cfa",
            estimator="DWLS",
            ordered=TRUE,
            seed=1234,
            std.lv=TRUE)
summary(out,standardized=TRUE,fit.measures=TRUE)

# Factor scores
# https://stats.stackexchange.com/questions/217786/how-to-predict-factor-scores-in-lavaan
# plausibleValues(fit,method="EBM") # ERROR: Plausible values not available (yet) for categorical data
df = c()
for (i in 1:length(mice.imp)){
  fit1=sem(model,data=mice.imp[[i]],
           estimator="DWLS",
           ordered=TRUE,
           std.lv=TRUE)
  a = data.frame(lavPredict(fit1,method="EBM")) # Enkel voor non-MI data method; for cat indicator (EBM or ML, see help)
  a$type=paste("imp",i,sep="_")
  df = rbind(df,a)
}
df$id = rep(seq(1,nrow(mice.imp[[1]])))
means = df %>% as_tibble() %>%
  group_by(id) %>%
  summarise(across(c("CE","CFC","CC","RU","CU"), ~mean(.)))
means$SOE = test$SOE # Run eerst script 4.SEM
boxplot(means$CE~means$SOE)
boxplot(means$RU~means$SOE)
boxplot(means$CU~means$SOE)
boxplot(means$CFC~means$SOE)
boxplot(means$CC~means$SOE)

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


