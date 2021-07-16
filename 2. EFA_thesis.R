# ----------------------------------------------------------------------------------------
# ------------------------------ EFA on all variables ---------------------------------- #
library(psych)
library(ggplot2)
library(grid)
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
training = data[sample,]
subj_train = paste(training$Student_ID,training$YEAR,sep="_")
test = data[-sample,]
subj = paste(test$Student_ID,test$YEAR,sep="_")

# -------------------------------- COMPARE TRAINING AND TEST DATASET ----------------------
## 1. Training
for (i in 1:nrow(training)){
  row = training[i,c("Q15_1", "Q15_3", "Q15_2","Q15_5", "Q15_6", "Q15_7", "Q15_8")] # remark the order of Qs!
  if (anyNA(row)){
    training$Q15[i] = NA
  } else{
    if (row[1]==1){ # PL
      training$Q15[i] = 1 # Number of question
    }
    if (row[2]==1){ # OE
      training$Q15[i] = 3
    }
    if (row[3]==1){ # CI
      training$Q15[i] = 2
    }
    if (row[4]==1){ # PL+OE+CI
      training$Q15[i] = 5
    }
    if (row[5]==1){ # PL+OE
      training$Q15[i] = 6
    }
    if (row[6]==1){ # PL+CI
      training$Q15[i] = 7
    }
    if (row[7]==1){ # OE+CI
      training$Q15[i] = 8
    }
    if (training[i,c("Q15_4")]==1){
      training$Q15[i] = 0
    }
  }
}
# Migration background
training$SOE[training$Herkomst %in% c("1/Migratie-achtergrond (EU1)","2/Migratie-achtergrond (niet-EU1)")] = "1" # Migration background
training$SOE[training$Herkomst == "5/Overige"] = "2" # 'Other'
training$SOE[training$Herkomst %in% c("3/Geen migratie-achtergrond","4/Niet toegewezen")] = "3" # No migration background: ref
# Count frequencies per level
factors = c("Gender","Fase","Q5","Q15","SOE","Q31")
training2 = data.frame(apply(training,2,factor))
list=list()
for (i in factors){
  n = nlevels(training2[,i])
  groups = data.frame(var=character(n),levels=character(n),summary=character(n),percent=c(n))
  groups$var = i
  groups$levels = levels(training2[,i])
  groups$summary = paste("n = ",as.numeric(table(training2[,i])),sep="")
  groups$percent = round(as.numeric(table(training2[,i]))*100/sum(!is.na(training2[,i])),2)
  list[[i]] = groups
}
summary_1=do.call(rbind,list)
#write.csv(summary_1,file="D:/Unief/Master Statistiek/Jaar 2/Thesis/5. Thesis/3. Results/1. Descriptive statistics/summary_training.csv")
## 2. Test
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
# Migration background
test$SOE[test$Herkomst %in% c("1/Migratie-achtergrond (EU1)","2/Migratie-achtergrond (niet-EU1)")] = "1" # Migration background
test$SOE[test$Herkomst == "5/Overige"] = "2" # 'Other'
test$SOE[test$Herkomst %in% c("3/Geen migratie-achtergrond","4/Niet toegewezen")] = "3" # No migration background: ref
# Count frequencies per level
factors = c("Gender","Fase","Q5","Q15","SOE","Q31")
test2 = data.frame(apply(test,2,factor))
list=list()
for (i in factors){
  n = nlevels(test2[,i])
  groups = data.frame(var=character(n),levels=character(n),summary=character(n),percent=c(n))
  groups$var = i
  groups$levels = levels(test2[,i])
  groups$summary = paste("n = ",as.numeric(table(test2[,i])),sep="")
  groups$percent = round(as.numeric(table(test2[,i]))*100/sum(!is.na(test2[,i])),2)
  list[[i]] = groups
}
summary_2=do.call(rbind,list)
#write.csv(summary_2,file="D:/Unief/Master Statistiek/Jaar 2/Thesis/5. Thesis/3. Results/1. Descriptive statistics/summary_test.csv")

# 3. Check differences between the dataset per variable
## Gender
d = data.frame(training=as.numeric(table(training$Gender)),
               test = as.numeric(table(test$Gender)))
row.names(d)=c("M","V")
chisq.test(d)
## Phase of study
d = data.frame(training=as.numeric(table(training$Fase)),
               test = as.numeric(table(test$Fase)))
row.names(d)=c("1","2","3","4","5")
chisq.test(d)
## Q5
d = data.frame(training=as.numeric(table(training$Q5)),
               test = as.numeric(table(test$Q5)))
row.names(d)=c("yes","no")
chisq.test(d)
## Vocational interest
d = data.frame(training=as.numeric(table(training$Q15)),
               test = as.numeric(table(test$Q15)))
row.names(d)=c("idk","PL","OE","CI","PL+OE+CI","PL+OE","PL+CI","OE+CI")
chisq.test(d)
## Engineering persistence
d = data.frame(training=as.numeric(table(training$Q31)),
               test = as.numeric(table(test$Q31)))
row.names(d)=c("0","1","2")
chisq.test(d)
## Migration status
d = data.frame(training=as.numeric(table(training$SOE)),
               test = as.numeric(table(test$SOE)))
row.names(d)=c("1","2","3")
chisq.test(d) # p=0.3652

# -------------------------------- HORNS PARALLEL ANALSYSIS -------------------------------

# Variables to include
include = c("Q30","Q32_1","Q32_2","Q32_3","Q32_4","Q32_5","Q32_6","Q32_7", # CE
            "Q8","Q9","Q16","Q17_6","Q20","Q23","Q25_5", # SA
            "Q14_1","Q14_2","Q14_3","Q14_4","Q24_1","Q24_2","Q24_3","Q24_4","Q24_5", # PRA
            "Q17_2","Q17_3","Q17_4","Q17_5","Q25_2","Q25_3","Q25_4") # PRC
# Polychoric correlations
cor = polychoric(training[,include])
round(eigen(cor$rho)$values, 3)
# cor = cor(training[,include],use="pairwise")
# round(eigen(cor)$values, 3)
# Scree plot
eigenvalues_pc = nFactors::eigenComputes(x=cor$rho,cor=TRUE,model="components")
eigenvalues_fa = nFactors::eigenComputes(x=cor$rho,cor=TRUE,model="factors")
# nFactors::eigenComputes(x=cor(training[,include],use="pairwise"),cor=TRUE,model="components") # gives the same eigenvalues as the pc in parallel analysis which is based on the pearson correlations
# nFactors::eigenComputes(x=cor(training[,include],use="pairwise"),cor=TRUE,model="factors")
plot1 = data.frame(eigenvalues = c(eigenvalues_pc,eigenvalues_fa),
                   type = c(rep("pc",length(eigenvalues_pc)),
                            rep("fa",length(eigenvalues_fa))),
                   i = rep(c(1:length(eigenvalues_pc)),2))
g2=ggplot(data = plot1,aes(group=type,col=type)) +
  scale_color_manual(values=c("black","gray45"),guide=F)+
  geom_hline(yintercept = 1, col="gray55")+
  geom_point(aes(x = i, y = eigenvalues),shape=18,size=2) + 
  geom_line(aes(x = i, y = eigenvalues),alpha=0.5) + 
  xlab(NULL)+ ylab("Eigenvalues")+ 
  scale_x_continuous(breaks = seq(from = 1, to = 30, by = 1),
                     labels = c(1,"",3,"","",6,"","",9,"","",12,"","",15,"","",18,"","",21,"","",24,"","",27,"","",30))+
  scale_y_continuous(breaks = seq(from = 0, to = 13, by = 1))+
  theme(panel.border = element_rect(color="black",fill=NA),
        panel.background = element_blank())

# Horn's parallel analysis
pa = psych::fa.parallel(training[,include],
                   n.iter=1500,
                   SMC=TRUE,
                   use="pairwise",
                   fm="uls",
                   fa="both",
                   cor="cor" # Garrido 2013: polychoric, however, Timmerman 2011: Pearson in case polychoric leads to nonconvergence
)
plot = data.frame(value = c(pa$fa.values,pa$fa.sim,pa$fa.simr,
                            pa$pc.values,pa$pc.sim,pa$pc.simr),
                  type = c(rep("fa",3*31),rep("pc",3*31)),
                  id = c(rep("actual",31),rep("sim",31),rep("simr",31),
                         rep("actual",31),rep("sim",31),rep("simr",31)),
                  n = rep(c(1:31),2))
plot$type_id = paste(plot$type,plot$id,sep="_")
g1 = ggplot(data=plot,aes(x=n,y=value,group=type_id,colour=type))+
  geom_line(lwd=1,lty=c(1,1,2)[plot$id],alpha=0.45)+
  geom_point(data=plot[plot$id=="actual",],shape=18,size=3)+
  theme_bw()+
  theme(panel.grid=element_blank(),legend.position = 'none')+
  ylab("Eigenvalues of pricipal components \n and factor analysis") + xlab("Factor/Component number")+
  scale_x_continuous(n.breaks=length(unique(plot$n)))+
  scale_y_continuous(n.breaks=6)+
  scale_colour_manual(values=c("black","gray45"))

# % variance explained by the extracted factors
# PC: 5 components (based on polychoric scree plot and Horn), based on polychoric
eigenvalues_pc = nFactors::eigenComputes(x=cor$rho,cor=TRUE,model="components")
PC_5_poly = sum(eigenvalues_pc[1:5]/sum(eigenvalues_pc))*100 # 47%
# PC: 9 components
PC_9_poly = sum(eigenvalues_pc[1:9]/sum(eigenvalues_pc))*100 # 62%
# FA: 5 factors
FA_5_poly = sum(eigenvalues_fa[1:5]/sum(eigenvalues_fa[eigenvalues_fa>0]))*100 # 73%
# FA: 9 factors
FA_9_poly = sum(eigenvalues_fa[1:9]/sum(eigenvalues_fa[eigenvalues_fa>0]))*100 # 90%
## Create a barplot
PC5poly = eigenvalues_pc[1:5]/sum(eigenvalues_pc)*100
PC9poly = eigenvalues_pc[1:9]/sum(eigenvalues_pc)*100
FA5poly = eigenvalues_fa[1:5]/sum(eigenvalues_fa[eigenvalues_fa>0])*100
FA9poly = eigenvalues_fa[1:9]/sum(eigenvalues_fa[eigenvalues_fa>0])*100
# plot
eig = data.frame(value=c(PC9poly,0,0,0,0,PC_5_poly,0,0,0,PC_9_poly,
                         FA9poly,0,0,0,0,FA_5_poly,0,0,0,FA_9_poly),
                 type = c(rep("PC",2*9),rep("FA",2*9)),
                 layer = rep(c(rep("front",9),rep("back",9)),2),
                 x = c(rep(seq(1,9),4)))
eig$xaxs = paste(eig$type,eig$x,sep="_")
eig$xaxs = factor(eig$xaxs,levels=levels(factor(eig$xaxs)))
g3=ggplot(data=eig[eig$layer=="back",],aes(x=xaxs,y=value,fill=type))+
  theme_bw()+
  theme(panel.grid=element_blank(),
        legend.position = c(0.9,0.9),
        legend.title = element_blank(),
        panel.grid.major.y = element_line(colour="gray97"),
        panel.grid.minor.y = element_line(colour="gray97"))+
  geom_col(alpha=0.35,lty=2,col=c("gray45"))+
  geom_col(data=eig[eig$layer=="front",])+
  scale_fill_manual(values=c("black","gray45"),guide=FALSE)+
  scale_x_discrete(expand=c(0,0),labels=rep(c(1:9),2),name="Factor/Component number")+
  scale_y_continuous(expand=c(0,0),limits=c(0,100),name="Variance (%)")

# Combine plots
pushViewport(viewport(layout=grid.layout(3,5)))
print(g1,vp=viewport(layout.pos.row=c(1:3),layout.pos.col=c(1:3)))
print(g2,vp=viewport(layout.pos.row=c(1:2),layout.pos.col=c(4:5)))
print(g3,vp=viewport(layout.pos.row=3,layout.pos.col=c(4:5)))

#export::graph2ppt(file="D:/Unief/Master Statistiek/Jaar 2/Thesis/5. Thesis/3. Results/2. EFA/Horn.ppt",append=TRUE)

  
# -------------------------------- EFA -------------------------------

# Factor analysis
# 9 factors
include = c("Q30","Q32_1","Q32_2","Q32_3","Q32_4","Q32_5","Q32_6","Q32_7", # CE
            "Q8","Q9","Q16","Q17_6","Q20","Q23","Q25_5", # SA
            "Q14_1","Q14_2","Q14_3","Q14_4","Q24_1","Q24_2","Q24_3","Q24_4","Q24_5", # PRA
            "Q17_2","Q17_3","Q17_4","Q17_5","Q25_2","Q25_3","Q25_4") # PRC
fit = psych::fa(r = training[,include], 
                max.iter=100,
                SMC = TRUE,
                residuals = TRUE,
                use="pairwise",
                nfactors = 9, 
                cor = "poly", 
                fm = "uls",
                rotate = "Promax"); fit
# 5 factors
include = c("Q30","Q32_1","Q32_2","Q32_3","Q32_4","Q32_5","Q32_6","Q32_7", # CE
            "Q8","Q9","Q16","Q17_6","Q20","Q23","Q25_5", # SA
            "Q14_1","Q14_2","Q14_3","Q14_4","Q24_1","Q24_2","Q24_3","Q24_4","Q24_5", # PRA
            "Q17_2","Q17_3","Q17_4","Q17_5","Q25_2","Q25_3","Q25_4") # PRC
fit = psych::fa(r = training[,include], 
                max.iter=100,
                SMC = TRUE,
                residuals = TRUE,
                use="pairwise",
                nfactors = 5, 
                cor = "poly", 
                fm = "uls",
                rotate = "Promax"); fit


