# ----------------------------------------------------------------------------------------
# -------------------------------------- MIMIC MODEL ----------------------------------- #
library(lavaan)
library(mice)
library(semTools)
# Final dataset
data = read.csv("D:/Unief/Master Statistiek/Jaar 2/Thesis/6. Data/all_NA_2019_2020_NoMiss_OmgedraaideScales_CorrecteQ22.csv")
data$id = paste(data$Student_ID,data$YEAR,sep="_")
data2 = read.csv("D:/Unief/Master Statistiek/Jaar 2/Thesis/6. Data/Results/CFA/dataq_NA.csv") # Dataq_NA bevat de first attempt van students
data2$id = paste(data2$Student_ID,data2$YEAR,sep="_")
data = data[data$id %in% data2$id,]; rm(data2)
data$Q5[data$Q5==3] = NA
data$Q29_20[data$Q29_20==0] = NA

# Subset data
set.seed(1234)
sample = sample(c(1:nrow(data)),
                size=round(0.40*nrow(data)), # 40% of the data
                replace=FALSE) # Obtain unique row numbers
test = data[-sample,]

# Rearrange Herkomst variable
test$SOE[test$Herkomst %in% c("1/Migratie-achtergrond (EU1)","2/Migratie-achtergrond (niet-EU1)")] = "1" # Migration background
test$SOE[test$Herkomst == "5/Overige"] = "2" # 'Other'
test$SOE[test$Herkomst %in% c("3/Geen migratie-achtergrond","4/Niet toegewezen")] = "3" # No migration background: ref

# MI for 'Q15' and 'Q31' and 'Migration' 
## reform Q15 into one variable to impute
for (i in 1:nrow(test)){
  row = test[i,c("Q15_1", "Q15_3", "Q15_2","Q15_5", "Q15_6", "Q15_7", "Q15_8")] # remark the order of Qs!
  if (anyNA(row)){
    test$Q15[i] = NA
  } else{
    if (row[1]==1){ # PL
      test$Q15[i] = 1 # Number of question
    }
    if (row[2]==1){ # OE
      test$Q15[i] = 3
    }
    if (row[3]==1){ # CI
      test$Q15[i] = 2
    }
    if (row[4]==1){ # PL+OE+CI
      test$Q15[i] = 5
    }
    if (row[5]==1){ # PL+OE
      test$Q15[i] = 6
    }
    if (row[6]==1){ # PL+CI
      test$Q15[i] = 7
    }
    if (row[7]==1){ # OE+CI
      test$Q15[i] = 8
    }
    if (test[i,c("Q15_4")]==1){
      test$Q15[i] = 0
    }
  }
}
## MI
set.seed(1234)
items = c("Q5","Q8","Q9","Q14_1","Q14_2","Q14_3","Q14_4","Q15","Q16","Q17_2","Q17_3","Q17_4","Q17_5","Q17_6",
          "Q20","Q23","Q24_1","Q24_2","Q24_3","Q24_4","Q24_5","Q25_2","Q25_3","Q25_4","Q25_5",
          "Q30","Q31","Q32_1","Q32_2","Q32_3","Q32_4","Q32_5","Q32_6","Q32_7",
          "Gender","Age","Fase","SOE")
test2 = test[,items]
test2 = data.frame(apply(test2,2,factor))
test2$Age = as.numeric(as.character(test2$Age))

# Predictor matrix
predictormatrix = quickpred(test2, 
                            include=c("Gender","Age","Fase","Q5"), # standard background predictors
                            exclude=NULL,
                            mincor = 0.25,
                            method = "kendall")
# Multiple imputation
m = 20
imp_gen = mice(data=test2,
               predictorMatrix = predictormatrix,
               method = c("logreg",rep("polr",6),"polyreg",rep("polr",6),"logreg",rep("polr",19),"logreg","pmm","polr","polyreg"), 
               m = m,
               maxit = 20,            
               diagnostics=TRUE)
mice.imp <- NULL
for (i in 1:m) {
  mice.imp[[i]] <- complete(imp_gen, action=i,inc=FALSE)
}

for (i in 1:length(mice.imp)){
  # 1. Fase (remark: no NAs anymore)
  mice.imp[[i]]$Fase1 = ifelse(mice.imp[[i]]$Fase==" 1",1,0) # God knows why, but a space occurred after imputation
  mice.imp[[i]]$Fase2 = ifelse(mice.imp[[i]]$Fase==" 2",1,0)
  mice.imp[[i]]$Fase3 = ifelse(mice.imp[[i]]$Fase==" 3",1,0)
  mice.imp[[i]]$Fase4 = ifelse(mice.imp[[i]]$Fase==" 4",1,0) # ref = Fase5 (=MA)
  # 2. Q5: Parents
  mice.imp[[i]]$Parents = ifelse(mice.imp[[i]]$Q5==" 1",1,0) # ref= parents not engineer
  # 3. Gender
  mice.imp[[i]]$sex = ifelse(mice.imp[[i]]$Gender=="M",1,0) # ref= female
  # 4. Q31: considering other field than engineering?
  ## ref = "nee" = 'denkt niet aan een andere loopbaan dan ingenieur'
  mice.imp[[i]]$indec = 0
  mice.imp[[i]]$indec[mice.imp[[i]]$Q31==" 2"] = 1 # "yes" (= 'idecisive')
  mice.imp[[i]]$quest = 0
  mice.imp[[i]]$quest[mice.imp[[i]]$Q31==" 1"] = 1 # sometimes (= 'questionable')
  # 5. Professional role preference
  ## Separate
  # (PL=1, CI=2, OE=3, idk=0, PL+OE+CI=ref(5), PL+OE=6, PL+CI=7, OE+CI=8; see line 27)
  # 1) Ref = PL+OE+CI
  mice.imp[[i]]$PL_refPLOECI = ifelse(mice.imp[[i]]$Q15==" 1",1,0)
  mice.imp[[i]]$CI_refPLOECI = ifelse(mice.imp[[i]]$Q15==" 2",1,0)
  mice.imp[[i]]$OE_refPLOECI = ifelse(mice.imp[[i]]$Q15==" 3",1,0)
  mice.imp[[i]]$idk_refPLOECI = ifelse(mice.imp[[i]]$Q15==" 0",1,0)
  mice.imp[[i]]$PL_OE_refPLOECI = ifelse(mice.imp[[i]]$Q15==" 6",1,0)
  mice.imp[[i]]$PL_CI_refPLOECI = ifelse(mice.imp[[i]]$Q15==" 7",1,0)
  mice.imp[[i]]$OE_CI_refPLOECI = ifelse(mice.imp[[i]]$Q15==" 8",1,0) # Ref = PL+OE+CI
  # 2) Ref = PL
  mice.imp[[i]]$CI_refPL = ifelse(mice.imp[[i]]$Q15==" 2",1,0)
  mice.imp[[i]]$OE_refPL = ifelse(mice.imp[[i]]$Q15==" 3",1,0)
  mice.imp[[i]]$idk_refPL = ifelse(mice.imp[[i]]$Q15==" 0",1,0)
  mice.imp[[i]]$PL_OE_refPL = ifelse(mice.imp[[i]]$Q15==" 6",1,0)
  mice.imp[[i]]$PL_CI_refPL = ifelse(mice.imp[[i]]$Q15==" 7",1,0)
  mice.imp[[i]]$OE_CI_refPL = ifelse(mice.imp[[i]]$Q15==" 8",1,0)
  mice.imp[[i]]$PL_OE_CI_refPL = ifelse(mice.imp[[i]]$Q15==" 5",1,0)
  # 3) Ref = OE
  mice.imp[[i]]$CI_refOE = ifelse(mice.imp[[i]]$Q15==" 2",1,0)
  mice.imp[[i]]$PL_refOE = ifelse(mice.imp[[i]]$Q15==" 1",1,0)
  mice.imp[[i]]$idk_refOE = ifelse(mice.imp[[i]]$Q15==" 0",1,0)
  mice.imp[[i]]$PL_OE_refOE = ifelse(mice.imp[[i]]$Q15==" 6",1,0)
  mice.imp[[i]]$PL_CI_refOE = ifelse(mice.imp[[i]]$Q15==" 7",1,0)
  mice.imp[[i]]$OE_CI_refOE = ifelse(mice.imp[[i]]$Q15==" 8",1,0)
  mice.imp[[i]]$PL_OE_CI_refOE = ifelse(mice.imp[[i]]$Q15==" 5",1,0)
  # 4) Ref = CI
  mice.imp[[i]]$OE_refCI = ifelse(mice.imp[[i]]$Q15==" 3",1,0)
  mice.imp[[i]]$PL_refCI = ifelse(mice.imp[[i]]$Q15==" 1",1,0)
  mice.imp[[i]]$idk_refCI = ifelse(mice.imp[[i]]$Q15==" 0",1,0)
  mice.imp[[i]]$PL_OE_refCI = ifelse(mice.imp[[i]]$Q15==" 6",1,0)
  mice.imp[[i]]$PL_CI_refCI = ifelse(mice.imp[[i]]$Q15==" 7",1,0)
  mice.imp[[i]]$OE_CI_refCI = ifelse(mice.imp[[i]]$Q15==" 8",1,0)
  mice.imp[[i]]$PL_OE_CI_refCI = ifelse(mice.imp[[i]]$Q15==" 5",1,0)
  # 5) Ref = PL+OE
  mice.imp[[i]]$OE_refPLOE = ifelse(mice.imp[[i]]$Q15==" 3",1,0)
  mice.imp[[i]]$PL_refPLOE = ifelse(mice.imp[[i]]$Q15==" 1",1,0)
  mice.imp[[i]]$idk_refPLOE = ifelse(mice.imp[[i]]$Q15==" 0",1,0)
  mice.imp[[i]]$CI_refPLOE = ifelse(mice.imp[[i]]$Q15==" 2",1,0)
  mice.imp[[i]]$PL_CI_refPLOE = ifelse(mice.imp[[i]]$Q15==" 7",1,0)
  mice.imp[[i]]$OE_CI_refPLOE = ifelse(mice.imp[[i]]$Q15==" 8",1,0)
  mice.imp[[i]]$PL_OE_CI_refPLOE = ifelse(mice.imp[[i]]$Q15==" 5",1,0)
  # 6) Ref = PL+CI
  mice.imp[[i]]$OE_refPLCI = ifelse(mice.imp[[i]]$Q15==" 3",1,0)
  mice.imp[[i]]$PL_refPLCI = ifelse(mice.imp[[i]]$Q15==" 1",1,0)
  mice.imp[[i]]$idk_refPLCI = ifelse(mice.imp[[i]]$Q15==" 0",1,0)
  mice.imp[[i]]$CI_refPLCI = ifelse(mice.imp[[i]]$Q15==" 2",1,0)
  mice.imp[[i]]$PL_OE_refPLCI = ifelse(mice.imp[[i]]$Q15==" 6",1,0)
  mice.imp[[i]]$OE_CI_refPLCI = ifelse(mice.imp[[i]]$Q15==" 8",1,0)
  mice.imp[[i]]$PL_OE_CI_refPLCI = ifelse(mice.imp[[i]]$Q15==" 5",1,0)
  # 7) Ref = OE+CI
  mice.imp[[i]]$OE_refOECI = ifelse(mice.imp[[i]]$Q15==" 3",1,0)
  mice.imp[[i]]$PL_refOECI = ifelse(mice.imp[[i]]$Q15==" 1",1,0)
  mice.imp[[i]]$idk_refOECI = ifelse(mice.imp[[i]]$Q15==" 0",1,0)
  mice.imp[[i]]$CI_refOECI = ifelse(mice.imp[[i]]$Q15==" 2",1,0)
  mice.imp[[i]]$PL_OE_refOECI = ifelse(mice.imp[[i]]$Q15==" 6",1,0)
  mice.imp[[i]]$PL_CI_refOECI = ifelse(mice.imp[[i]]$Q15==" 7",1,0)
  mice.imp[[i]]$PL_OE_CI_refOECI = ifelse(mice.imp[[i]]$Q15==" 5",1,0)
  # 8) Ref = idk
  mice.imp[[i]]$OE_refidk = ifelse(mice.imp[[i]]$Q15==" 3",1,0)
  mice.imp[[i]]$PL_refidk = ifelse(mice.imp[[i]]$Q15==" 1",1,0)
  mice.imp[[i]]$OE_CI_refidk = ifelse(mice.imp[[i]]$Q15==" 8",1,0)
  mice.imp[[i]]$CI_refidk = ifelse(mice.imp[[i]]$Q15==" 2",1,0)
  mice.imp[[i]]$PL_OE_refidk = ifelse(mice.imp[[i]]$Q15==" 6",1,0)
  mice.imp[[i]]$PL_CI_refidk = ifelse(mice.imp[[i]]$Q15==" 7",1,0)
  mice.imp[[i]]$PL_OE_CI_refidk = ifelse(mice.imp[[i]]$Q15==" 5",1,0)
  # Combined
  mice.imp[[i]]$OE_overall = 0
  mice.imp[[i]]$OE_overall[mice.imp[[i]]$Q15 %in% c(" 3"," 5"," 6"," 8")] = 1
  mice.imp[[i]]$CI_overall = 0
  mice.imp[[i]]$CI_overall[mice.imp[[i]]$Q15 %in% c(" 2"," 5"," 7"," 8")] = 1 # Ref = PL_overall
  # Know vocational interest: yes/no
  mice.imp[[i]]$voc_int[mice.imp[[i]]$Q15 %in% c(" 1"," 2"," 3"," 5"," 6"," 7"," 8")] = 1
  mice.imp[[i]]$voc_int[mice.imp[[i]]$Q15 == " 0"] = 0 # idk = ref 
  # 6. Migration background
  mice.imp[[i]]$Migration = ifelse(mice.imp[[i]]$SOE=="1",1,0)
  mice.imp[[i]]$Migration_other = ifelse(mice.imp[[i]]$SOE=="2",1,0) # ref = no migration background
  ## PREFER
  mice.imp[[i]]$Q29_20 = test$Q29_20
  mice.imp[[i]]$Role = ifelse(mice.imp[[i]]$Q29_20==1,1,0)
  mice.imp[[i]]$Role[which(is.na(mice.imp[[i]]$Q29_20))] = NA
  mice.imp[[i]]$Comp = ifelse(mice.imp[[i]]$Q29_20==2,1,0)
  mice.imp[[i]]$Comp[which(is.na(mice.imp[[i]]$Q29_20))] = NA
  mice.imp[[i]]$Both = ifelse(mice.imp[[i]]$Q29_20==3,1,0)
  mice.imp[[i]]$Both[which(is.na(mice.imp[[i]]$Q29_20))] = NA # ref = 4 (= geen prefer deelgenomen)
}

# --------------------------------------- MIMIC model ---------------------------------------------
model = ' # Measurement model
        CE =~ Q32_1 + Q32_2 + Q32_3 + Q32_4 + Q32_5 + Q32_6 + Q32_7 + Q30
        CFC =~ Q17_2 + Q17_3 + Q17_4 + Q17_5 + Q17_6 + Q16
        CC =~ Q25_2 + Q25_3 + Q25_4 + Q23
        RU =~ Q14_3 + Q14_2 + Q14_1
        CU =~ Q24_1 + Q24_3 + Q24_4 + Q24_5 # no 14_4
        
        Q16 ~~ Q23
        Q24_4	~~	Q24_5
        Q30	~~	Q32_4
        
        # Structural model (run the correct regressions for the correct reference group for vocational interest)
        # CE ~ Fase1 + Fase2 + Fase3 + Fase4 + sex + Parents + indec + quest + PL_refPLOECI + CI_refPLOECI + OE_refPLOECI + PL_CI_refPLOECI + PL_OE_refPLOECI + OE_CI_refPLOECI +idk_refPLOECI + Migration + Migration_other
        # CFC ~ Fase1 + Fase2 + Fase3 + Fase4 + sex + Parents + indec + quest + PL_refPLOECI + CI_refPLOECI + OE_refPLOECI + PL_CI_refPLOECI + PL_OE_refPLOECI + OE_CI_refPLOECI +idk_refPLOECI + Migration + Migration_other
        # CC ~ Fase1 + Fase2 + Fase3 + Fase4 + sex + Parents + indec + quest + PL_refPLOECI + CI_refPLOECI + OE_refPLOECI + PL_CI_refPLOECI + PL_OE_refPLOECI + OE_CI_refPLOECI +idk_refPLOECI + Migration + Migration_other
        # RU ~ Fase1 + Fase2 + Fase3 + Fase4 + sex + Parents + indec + quest + PL_refPLOECI + CI_refPLOECI + OE_refPLOECI + PL_CI_refPLOECI + PL_OE_refPLOECI + OE_CI_refPLOECI +idk_refPLOECI + Migration + Migration_other
        # CU ~ Fase1 + Fase2 + Fase3 + Fase4 + sex + Parents + indec + quest + PL_refPLOECI + CI_refPLOECI + OE_refPLOECI + PL_CI_refPLOECI + PL_OE_refPLOECI + OE_CI_refPLOECI +idk_refPLOECI + Migration + Migration_other

        # CE ~ Fase1 + Fase2 + Fase3 + Fase4 + sex + Parents + indec + quest + CI_refPL + OE_refPL + PL_CI_refPL + PL_OE_refPL + OE_CI_refPL + PL_OE_CI_refPL +idk_refPL + Migration + Migration_other
        # CFC ~ Fase1 + Fase2 + Fase3 + Fase4 + sex + Parents + indec + quest + CI_refPL + OE_refPL + PL_CI_refPL + PL_OE_refPL + OE_CI_refPL + PL_OE_CI_refPL +idk_refPL + Migration + Migration_other
        # CC ~ Fase1 + Fase2 + Fase3 + Fase4 + sex + Parents + indec + quest + CI_refPL + OE_refPL + PL_CI_refPL + PL_OE_refPL + OE_CI_refPL + PL_OE_CI_refPL +idk_refPL + Migration + Migration_other        
        # RU ~ Fase1 + Fase2 + Fase3 + Fase4 + sex + Parents + indec + quest + CI_refPL + OE_refPL + PL_CI_refPL + PL_OE_refPL + OE_CI_refPL + PL_OE_CI_refPL +idk_refPL + Migration + Migration_other
        # CU ~ Fase1 + Fase2 + Fase3 + Fase4 + sex + Parents + indec + quest + CI_refPL + OE_refPL + PL_CI_refPL + PL_OE_refPL + OE_CI_refPL + PL_OE_CI_refPL +idk_refPL + Migration + Migration_other

        # CE ~ Fase1 + Fase2 + Fase3 + Fase4 + sex + Parents + indec + quest + PL_refCI + OE_refCI + PL_CI_refCI + PL_OE_refCI + OE_CI_refCI + PL_OE_CI_refCI +idk_refCI + Migration + Migration_other
        # CFC ~ Fase1 + Fase2 + Fase3 + Fase4 + sex + Parents + indec + quest + PL_refCI + OE_refCI + PL_CI_refCI + PL_OE_refCI + OE_CI_refCI + PL_OE_CI_refCI +idk_refCI + Migration + Migration_other
        # CC ~ Fase1 + Fase2 + Fase3 + Fase4 + sex + Parents + indec + quest + PL_refCI + OE_refCI + PL_CI_refCI + PL_OE_refCI + OE_CI_refCI + PL_OE_CI_refCI +idk_refCI + Migration + Migration_other
        # RU ~ Fase1 + Fase2 + Fase3 + Fase4 + sex + Parents + indec + quest + PL_refCI + OE_refCI + PL_CI_refCI + PL_OE_refCI + OE_CI_refCI + PL_OE_CI_refCI +idk_refCI + Migration + Migration_other
        # CU ~ Fase1 + Fase2 + Fase3 + Fase4 + sex + Parents + indec + quest + PL_refCI + OE_refCI + PL_CI_refCI + PL_OE_refCI + OE_CI_refCI + PL_OE_CI_refCI +idk_refCI + Migration + Migration_other

        # CE ~ Fase1 + Fase2 + Fase3 + Fase4 + sex + Parents + indec + quest + PL_refOE + CI_refOE + PL_CI_refOE + PL_OE_refOE + OE_CI_refOE + PL_OE_CI_refOE +idk_refOE + Migration + Migration_other
        # CFC ~ Fase1 + Fase2 + Fase3 + Fase4 + sex + Parents + indec + quest + PL_refOE + CI_refOE + PL_CI_refOE + PL_OE_refOE + OE_CI_refOE + PL_OE_CI_refOE +idk_refOE + Migration + Migration_other
        # CC ~ Fase1 + Fase2 + Fase3 + Fase4 + sex + Parents + indec + quest + PL_refOE + CI_refOE + PL_CI_refOE + PL_OE_refOE + OE_CI_refOE + PL_OE_CI_refOE +idk_refOE + Migration + Migration_other
        # RU ~ Fase1 + Fase2 + Fase3 + Fase4 + sex + Parents + indec + quest + PL_refOE + CI_refOE + PL_CI_refOE + PL_OE_refOE + OE_CI_refOE + PL_OE_CI_refOE +idk_refOE + Migration + Migration_other
        # CU ~ Fase1 + Fase2 + Fase3 + Fase4 + sex + Parents + indec + quest + PL_refOE + CI_refOE + PL_CI_refOE + PL_OE_refOE + OE_CI_refOE + PL_OE_CI_refOE +idk_refOE + Migration + Migration_other

        # CE ~ Fase1 + Fase2 + Fase3 + Fase4 + sex + Parents + indec + quest + PL_refPLOE + CI_refPLOE + OE_refPLOE + PL_CI_refPLOE + OE_CI_refPLOE +  PL_OE_CI_refPLOE +idk_refPLOE + Migration + Migration_other
        # CFC ~ Fase1 + Fase2 + Fase3 + Fase4 + sex + Parents + indec + quest + PL_refPLOE + CI_refPLOE + OE_refPLOE + PL_CI_refPLOE + OE_CI_refPLOE +  PL_OE_CI_refPLOE +idk_refPLOE + Migration + Migration_other
        # CC ~ Fase1 + Fase2 + Fase3 + Fase4 + sex + Parents + indec + quest + PL_refPLOE + CI_refPLOE + OE_refPLOE + PL_CI_refPLOE + OE_CI_refPLOE +  PL_OE_CI_refPLOE +idk_refPLOE + Migration + Migration_other
        # RU ~ Fase1 + Fase2 + Fase3 + Fase4 + sex + Parents + indec + quest + PL_refPLOE + CI_refPLOE + OE_refPLOE + PL_CI_refPLOE + OE_CI_refPLOE +  PL_OE_CI_refPLOE +idk_refPLOE + Migration + Migration_other
        # CU ~ Fase1 + Fase2 + Fase3 + Fase4 + sex + Parents + indec + quest + PL_refPLOE + CI_refPLOE + OE_refPLOE + PL_CI_refPLOE + OE_CI_refPLOE +  PL_OE_CI_refPLOE +idk_refPLOE + Migration + Migration_other

        # CE ~ Fase1 + Fase2 + Fase3 + Fase4 + sex + Parents + indec + quest + PL_refPLCI + CI_refPLCI + OE_refPLCI + PL_OE_refPLCI + OE_CI_refPLCI +  PL_OE_CI_refPLCI +idk_refPLCI + Migration + Migration_other
        # CFC ~ Fase1 + Fase2 + Fase3 + Fase4 + sex + Parents + indec + quest + PL_refPLCI + CI_refPLCI + OE_refPLCI + PL_OE_refPLCI + OE_CI_refPLCI +  PL_OE_CI_refPLCI +idk_refPLCI + Migration + Migration_other
        # CC ~ Fase1 + Fase2 + Fase3 + Fase4 + sex + Parents + indec + quest + PL_refPLCI + CI_refPLCI + OE_refPLCI + PL_OE_refPLCI + OE_CI_refPLCI +  PL_OE_CI_refPLCI +idk_refPLCI + Migration + Migration_other
        # RU ~ Fase1 + Fase2 + Fase3 + Fase4 + sex + Parents + indec + quest + PL_refPLCI + CI_refPLCI + OE_refPLCI + PL_OE_refPLCI + OE_CI_refPLCI +  PL_OE_CI_refPLCI +idk_refPLCI + Migration + Migration_other
        # CU ~ Fase1 + Fase2 + Fase3 + Fase4 + sex + Parents + indec + quest + PL_refPLCI + CI_refPLCI + OE_refPLCI + PL_OE_refPLCI + OE_CI_refPLCI +  PL_OE_CI_refPLCI +idk_refPLCI + Migration + Migration_other

        # CE ~ Fase1 + Fase2 + Fase3 + Fase4 + sex + Parents + indec + quest + PL_refOECI + CI_refOECI + OE_refOECI + PL_OE_refOECI + PL_CI_refOECI + PL_OE_CI_refOECI +idk_refOECI + Migration + Migration_other
        # CFC ~ Fase1 + Fase2 + Fase3 + Fase4 + sex + Parents + indec + quest + PL_refOECI + CI_refOECI + OE_refOECI + PL_OE_refOECI + PL_CI_refOECI + PL_OE_CI_refOECI +idk_refOECI + Migration + Migration_other
        # CC ~ Fase1 + Fase2 + Fase3 + Fase4 + sex + Parents + indec + quest + PL_refOECI + CI_refOECI + OE_refOECI + PL_OE_refOECI + PL_CI_refOECI + PL_OE_CI_refOECI +idk_refOECI + Migration + Migration_other
        # RU ~ Fase1 + Fase2 + Fase3 + Fase4 + sex + Parents + indec + quest + PL_refOECI + CI_refOECI + OE_refOECI + PL_OE_refOECI + PL_CI_refOECI + PL_OE_CI_refOECI +idk_refOECI + Migration + Migration_other
        # CU ~ Fase1 + Fase2 + Fase3 + Fase4 + sex + Parents + indec + quest + PL_refOECI + CI_refOECI + OE_refOECI + PL_OE_refOECI + PL_CI_refOECI + PL_OE_CI_refOECI +idk_refOECI + Migration + Migration_other
         
        CE ~ Fase1 + Fase2 + Fase3 + Fase4 + sex + Parents + indec + quest + PL_refidk + CI_refidk + OE_refidk + PL_OE_refidk + PL_CI_refidk + OE_CI_refidk + PL_OE_CI_refidk + Migration + Migration_other
        CFC ~ Fase1 + Fase2 + Fase3 + Fase4 + sex + Parents + indec + quest + PL_refidk + CI_refidk + OE_refidk + PL_OE_refidk + PL_CI_refidk + OE_CI_refidk + PL_OE_CI_refidk + Migration + Migration_other
        CC ~ Fase1 + Fase2 + Fase3 + Fase4 + sex + Parents + indec + quest + PL_refidk + CI_refidk + OE_refidk + PL_OE_refidk + PL_CI_refidk + OE_CI_refidk + PL_OE_CI_refidk + Migration + Migration_other
        RU ~ Fase1 + Fase2 + Fase3 + Fase4 + sex + Parents + indec + quest + PL_refidk + CI_refidk + OE_refidk + PL_OE_refidk + PL_CI_refidk + OE_CI_refidk + PL_OE_CI_refidk + Migration + Migration_other
        CU ~ Fase1 + Fase2 + Fase3 + Fase4 + sex + Parents + indec + quest + PL_refidk + CI_refidk + OE_refidk + PL_OE_refidk + PL_CI_refidk + OE_CI_refidk + PL_OE_CI_refidk + Migration + Migration_other

'
# Run the model
fit = runMI(model,data=mice.imp,
            fun="sem",
            estimator="DWLS",
            ordered=TRUE,
            #std.lv=TRUE,
            seed=1234)
summary(fit,standardized=TRUE,fit.measures=TRUE)
modindices.mi(fit)

# Model fit indices for each imputed dataset
df = c()
for (i in 1:length(mice.imp)){
  fit = sem(model,data=mice.imp[[i]],
            estimator="DWLS",
            ordered=TRUE,
            std.lv=TRUE)
  df = rbind(df,fitmeasures(fit)[c("cfi","tli","rmsea","srmr")])
};df

# Multiple comparison for vocation interest (PL/OE/CI)
# The p-values for the pairwise comparisons were created by rerunning the MIMIC model with different reference categories
# and saving all the results in one csv-file.
multicomp = read.csv("D:/Unief/Master Statistiek/Jaar 2/Thesis/5. Thesis/3. Results/3. CFA/4.MIMIC_multiple comp PL_OE_CI.csv")
# Summary output contains all pairwise comparisons, incl. A <> B and B <> A. Remove doubles:
multicomp = multicomp[! multicomp$rm=="yes",]
for (i in levels(factor(multicomp$Factor))){
  p = multicomp$P_value[multicomp$Factor==i]
  multicomp$p_fdr[multicomp$Factor==i] = p.adjust(p,"fdr")
  multicomp$p_holm[multicomp$Factor==i] = p.adjust(p,"holm")
}
multicomp$sign_fdr = ifelse(multicomp$p_fdr>=0.05,".",
                            ifelse(multicomp$p_fdr<0.05 & multicomp$p_fdr>=0.01,"*",
                                   ifelse(multicomp$p_fdr<0.01 & multicomp$p_fdr>=0.001,"**",
                                          ifelse(multicomp$p_fdr<0.001,"***",NA))))
multicomp$sign_holm = ifelse(multicomp$p_holm>=0.05,".",
                             ifelse(multicomp$p_holm<0.05 & multicomp$p_holm>=0.01,"*",
                                    ifelse(multicomp$p_holm<0.01 & multicomp$p_holm>=0.001,"**",
                                           ifelse(multicomp$p_holm<0.001,"***",NA))))
#write.csv(multicomp,file="D:/Unief/Master Statistiek/Jaar 2/Thesis/5. Thesis/3. Results/3. CFA/4.multicomp_R.csv")

















