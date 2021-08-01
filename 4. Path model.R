# ----------------------------------------------------------------------------------------
# ------------------------------ DIRECTIONAL PATH MODEL -------------------------------- #
library(lavaan)
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


# ------------------------------------ SEM - Construct connectivity ----------------------------------- #
model = ' # 1. Measurement model:
        CE =~ Q32_1 + Q32_2 + Q32_3 + Q32_4 + Q32_5 + Q32_6 + Q32_7 + Q30
        CFC =~ Q17_2 + Q17_3 + Q17_4 + Q17_5 + Q17_6 + Q16
        CC =~ Q25_2 + Q25_3 + Q25_4 + Q23
        RU =~ Q14_3 + Q14_2 + Q14_1
        CU =~ Q24_1 + Q24_3 + Q24_4 + Q24_5 # no 14_4
        
        Q16 ~~ Q23
        Q24_4	~~	Q24_5
        Q30	~~	Q32_4
        
        # 2. Structural model:
        CFC ~ c*CE + e*RU + g*CU
        CC ~ d*CE + f*RU + h*CU
        RU ~ a*CE
        CU ~ b*CE
        
        # Correlatie tussen niet-gelinkte constructen:
        CFC ~~ CC
        RU ~~ CU
        
        # 3. Direct & indirect effects:
        CE_RU_CFC := a*e
        CE_RU_CC  := a*f
        CE_CU_CFC := b*g
        CE_CU_CC  := b*h
        
        CE_RU_CFC_halftotal := (a*e) + c
        CE_RU_CC_halftotal  := (a*f) + d
        CE_CU_CFC_halftotal := (b*g) + c
        CE_CU_CC_halftotal  := (b*h) + d
        
        CE_CFC_total := (a*e) + c + (b*g)
        CE_CC_total  := (a*f) + d + (b*h)
'
# Run model
fit = runMI(model,data=mice.imp,
            fun="sem",
            estimator="DWLS",
            ordered=TRUE,
            std.lv=TRUE, # Does not change result, sometimes convergence
            seed=1234)
summary(fit,standardized=TRUE,fit.measures=TRUE)

# Model fit indices per impted dataset
df = c()
for (i in 1:length(mice.imp)){
  fit = sem(model,data=mice.imp[[i]],
            estimator="DWLS",
            ordered=TRUE,
            std.lv=TRUE)
  df = rbind(df,fitmeasures(fit)[c("cfi","tli","rmsea","srmr")])
};df

