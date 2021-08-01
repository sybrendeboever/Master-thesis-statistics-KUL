# -------------------------------------------------------------- # 
# -------------------- 1. DESCRIPTIVE STATS -------------------- #
# -------------------------------------------------------------- # 

### Final dataset
data = read.csv("D:/Unief/Master Statistiek/Jaar 2/Thesis/6. Data/all_NA_2019_2020_NoMiss_OmgedraaideScales_CorrecteQ22.csv")
data$id = paste(data$Student_ID,data$YEAR,sep="_")
data2 = read.csv("D:/Unief/Master Statistiek/Jaar 2/Thesis/6. Data/Results/CFA/dataq_NA.csv") # Dataq_NA bevat de first attempt van students
data2$id = paste(data2$Student_ID,data2$YEAR,sep="_")
data = data[data$id %in% data2$id,]; rm(data2)
# Parents
data$Q5[data$Q5==3] = NA
# Vocational interest
for (i in 1:nrow(data)){
  row = data[i,c("Q15_1", "Q15_3", "Q15_2","Q15_5", "Q15_6", "Q15_7", "Q15_8")] # remark the order of Qs!
  if (anyNA(row)){
    data$Q15[i] = NA
  } else{
    if (row[1]==1){ # PL
      data$Q15[i] = 1 # Number of question
    }
    if (row[2]==1){ # OE
      data$Q15[i] = 3
    }
    if (row[3]==1){ # CI
      data$Q15[i] = 2
    }
    if (row[4]==1){ # PL+OE+CI
      data$Q15[i] = 5
    }
    if (row[5]==1){ # PL+OE
      data$Q15[i] = 6
    }
    if (row[6]==1){ # PL+CI
      data$Q15[i] = 7
    }
    if (row[7]==1){ # OE+CI
      data$Q15[i] = 8
    }
    if (data[i,c("Q15_4")]==1){
      data$Q15[i] = 0
    }
  }
}
# Migration background
data$SOE[data$Herkomst %in% c("1/Migratie-achtergrond (EU1)","2/Migratie-achtergrond (niet-EU1)")] = "1" # Migration background
data$SOE[data$Herkomst == "5/Overige"] = "2" # 'Other'
data$SOE[data$Herkomst %in% c("3/Geen migratie-achtergrond","4/Niet toegewezen")] = "3" # No migration background: ref

###  1. Descriptive tables of (1) survey questions & (2) used background variables
# ---------------------------------------------------------------------------------
########## (1) Survey questions
### a. Likert responses for the questions
## The relevant question in survey order
items = c("Q5","Q8","Q9","Q14_1","Q14_2","Q14_3","Q14_4","Q16","Q17_2","Q17_3","Q17_4","Q17_5","Q17_6",
          "Q20","Q23","Q24_1","Q24_2","Q24_3","Q24_4","Q24_5","Q25_2","Q25_3","Q25_4","Q25_5",
          "Q30","Q31","Q32_1","Q32_2","Q32_3","Q32_4","Q32_5","Q32_6","Q32_7")
## Create count table
# start
counts = t(table(data$Q5))
counts = cbind(counts,"3"=NA,"4"=NA,"5"=NA) # Create a table with 5 columns (= max nr of responses in the survey)
row.names(counts) = c("Q5")
# add others
for (i in items[2:length(items)]){ # Create a table for each survey item
  c = t(table(data[,i]))
  row.names(c) = i # indicate the item
  if (ncol(c)<5){ 
    n = ncol(c)
    for (a in 1:(5-n)){ # add NA columns until 5 columns are reached
      col = as.character(n+a)
      c = cbind(c,col=NA)
    }
    
  }
  counts = rbind(counts,c) # add the new count to the previous one
}
### b. add total number of responses per question
counts = data.frame(counts)
counts$Total = apply(counts,1,function(x){sum(x,na.rm=TRUE)})
### c. add % missingness
counts$Missing = round(100-counts$Total*100/nrow(data),2)
### d. add KMO values
cor = psych::polychoric(data[,items[! items %in% c("Q5","Q31")]]) # remove Q5 and Q31 bc they are bgvars
kmo = data.frame(psych::KMO(cor$rho)$MSAi)
names(kmo) = c("KMO")
# join the counts and kmo dataset
counts$item = row.names(counts)
kmo$item = row.names(kmo)
counts = dplyr::left_join(counts,kmo,by="item")
counts$KMO = round(counts$KMO,2)
row.names(counts) = counts$item
counts$item = NULL
# write.csv(counts, file="D:/Unief/Master Statistiek/Jaar 2/Thesis/5. Thesis/Results/1. Descriptive statistics/counts_surveyquestions.csv")

########## (2) Background variables
factors = c("Gender","Fase","Q5","Q15","SOE","Q31")
data2 = data.frame(apply(data,2,factor))
list=list()
for (i in factors){
  n = nlevels(data2[,i])
  groups = data.frame(var=character(n),levels=character(n),summary=character(n),percent=c(n),NAs=numeric(n))
  groups$var = i
  groups$levels = levels(data2[,i])
  groups$summary = paste("n = ",as.numeric(table(data2[,i])),sep="")
  groups$percent = round(as.numeric(table(data2[,i]))*100/sum(!is.na(data2[,i])),2)
  groups$NAs = sum(is.na(data2[,i]))
  groups$NA_percent = sum(is.na(data2[,i]))*100/nrow(data2)
  list[[i]] = groups
}
summary_1=do.call(rbind,list)
# write.csv(summary_1,file="D:/Unief/Master Statistiek/Jaar 2/Thesis/5. Thesis/3. Results/1. Descriptive statistics/bgvars_table2.csv")

# Dependency between vocational interest and engineering persistence
data$voc_int = ifelse(data$Q15=="0",0,1)
data$voc_int[is.na(data$Q15)] = NA
chisq.test(table(data$Q31,data$voc_int))
fisher.test(table(data$Q31,data$Q15))

# --------------------------------------------------------------------------
s = list()
s[[1]] = summary[,c("var","summary","levels","percent")]

names(summary)[names(summary)=="summary"] = c("summary_test")
names(summary)[names(summary)=="percent"] = c("percent_test")
s[[2]] = summary[,c("percent_test")]

names(summary)[names(summary)=="summary"] = c("summary_train")
names(summary)[names(summary)=="percent"] = c("percent_train")
s[[3]] = summary[,c("percent_train")]
s = do.call(cbind,s)
# write.csv(s,file="D:/Unief/Master Statistiek/Jaar 2/Thesis/6. Data/Results/Data exploration/Percent_bgvar_data_test_train.csv")



###  2. Polychoric correlations for all items (without Q5 and Q31)
# ------------------------------------------------------------------
items = c("Q8","Q9","Q14_1","Q14_2","Q14_3","Q14_4","Q16","Q17_2","Q17_3","Q17_4","Q17_5","Q17_6",
          "Q20","Q23","Q24_1","Q24_2","Q24_3","Q24_4","Q24_5","Q25_2","Q25_3","Q25_4","Q25_5",
          "Q30","Q32_1","Q32_2","Q32_3","Q32_4","Q32_5","Q32_6","Q32_7")
cor = psych::polychoric(data[,items])
corrplot::corrplot.mixed(cor$rho,number.cex = 0.45,
                         tl.cex = 0.45,lower.col="black")
#export::graph2ppt(file="D:/Unief/Master Statistiek/Jaar 2/Thesis/5. Thesis/3. Results/1. Descriptive statistics/corrplot.ppt",append=TRUE)


###  3. Missing data
# -------------------
library(tidyverse)
library(grid)
# a. Count the number of missing values per student
# 859 completers and 181 non-completers
tel = data.frame(t(data[,items]))
names(tel) = data$id
nas = tel %>%
  as_tibble() %>%
  summarize(across(everything(),
                   ~sum(is.na(.)))) %>%
  gather()
plot = data.frame(table(nas$value))
ggplot(data=plot,aes(x=factor(Var1),y=Freq))+
  geom_col()+
  theme_bw()+
  ylim(0,75)+
  ylab("Frequency")+xlab("Number of missing values")
#export::graph2ppt(file="D:/Unief/Master Statistiek/Jaar 2/Thesis/5. Thesis/3. Results/1. Descriptive statistics/NAs.ppt",append=TRUE)

# b. Check the dropout patterns
items = c("Q8","Q9","Q14_1","Q14_2","Q14_3","Q14_4","Q16","Q17_2","Q17_3","Q17_4","Q17_5","Q17_6",
          "Q24_1","Q24_2","Q24_3","Q24_4","Q24_5","Q20","Q23","Q25_2","Q25_3","Q25_4","Q25_5",
          "Q30","Q32_1","Q32_2","Q32_3","Q32_4","Q32_5","Q32_6","Q32_7") # Correct survey order
## Dropout patterns
pattern = data[,items]
pattern[!is.na(pattern)] = 1
pattern[is.na(pattern)] = 0
pattern$pattern = apply(pattern,MARGIN =1,FUN = function(x){paste(x,sep="",collapse = "")}) # dropout pattern
pattern$id = data$id # get the ids to match the number of NAs per subject
pattern = left_join(pattern,nas,by=c("id"="key"))
names(pattern)[names(pattern)=="value"] = c("nas")
pattern = doBy::orderBy(~nas,pattern)
## Count the frequency per pattern (because you only want to plot the unique ones)
pat_freq = data.frame(table(pattern$pattern))
pattern = left_join(pattern,pat_freq,by=c("pattern"="Var1"))
## Only keep the observations with missingness
pattern_miss = pattern[pattern$nas>0,]
## Remove unnecessary variables (c("pattern","id","nas"))
pattern_miss[,c("pattern","id","nas")] = NULL
## Only keep the unique missingness patterns
pattern_miss =unique(pattern_miss)
plot = stack(pattern_miss[,items])
plot$id = rep(seq(nrow(pattern_miss),1)) # reverse, for plotting reasons
## plot
plot1 = ggplot(data=plot,aes(x=ind,y=factor(id)))+
  geom_tile(aes(fill = factor(values)))+
  scale_fill_manual(values=c("brown4","antiquewhite1"),guide=F)+
  theme_test()+
  scale_x_discrete(position = "top",guide=guide_axis(angle=75))+
  theme(plot.background =element_blank()) + xlab(NULL)+ ylab("Missingness pattern")+
  geom_vline(xintercept = seq(1.5,30.5,by=1),alpha=0.025)+
  geom_hline(yintercept = seq(1.5,31.5,by=1),alpha=0.25)+
  scale_y_discrete(labels=factor(seq(32,1))) # overwrite the numbers to have a better order
freqs = data.frame(pattern_miss$Freq,id=seq(nrow(pattern_miss),1))
names(freqs) = c("freq","id")
freqs$x = factor(rep(1))
plot2 = ggplot(data=freqs,aes(x=x,y=factor(id),fill=freq))+
  geom_tile(col="gray45")+
  scale_fill_gradient(low="white",high="brown4",guide = FALSE)+
  geom_text(aes(label=freq),col="gray25",size=3)+
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank())+
  ggtitle("Freq \n ")+ylab(NULL)+xlab(NULL)
pushViewport(viewport(layout=grid.layout(1,15)))
print(plot1,vp=viewport(layout.pos.row=1,layout.pos.col=c(1:14)))
print(plot2,vp=viewport(layout.pos.row=1,layout.pos.col=c(15)))
#export::graph2ppt(file="D:/Unief/Master Statistiek/Jaar 2/Thesis/5. Thesis/3. Results/1. Descriptive statistics/NAs.ppt",append=TRUE)

## c. MCAR
# 1. Little's MCAR test: "Geen MCAR (p<0.05, H0="missingness is MCAR"), opm: geldt niet voor categorical data
library(naniar)
test = data.frame(apply(data[,order_qs],2,factor))
mcar_test(test)
# 2. Zelf missingness checken: mbv 'prev' (voor monotome miss (hier zijn slecht 28 studenten niet monotoom))
# a. drie aparte logistic regressions (ss was groot genoeg)
order_qs = c("Q8","Q9","Q14_1","Q14_2","Q14_3","Q14_4","Q16","Q17_2","Q17_3","Q17_4","Q17_5","Q17_6",
             "Q24_1","Q24_2","Q24_3","Q24_4","Q24_5","Q20","Q23","Q25_2","Q25_3","Q25_4","Q25_5",
             "Q30","Q32_1","Q32_2","Q32_3","Q32_4","Q32_5","Q32_6","Q32_7") # Correct survey order
# [ 0 = MISSING ; 1 = OBSERVED ]
# Per variable
sub = data[,c("Gender","Age","Fase","Q5",order_qs)]
sub = data.frame(apply(sub,2,factor))
sub$Q9 = relevel(sub$Q9,ref=1)
sub$Age = as.numeric(as.character(sub$Age))
list = list()
for (i in names(sub)[! names(sub) %in% c("Q8","Gender","Age","Fase","Q5","D")]){ # exclude Q8 because this one is complete
  # Copy dataset for safety reasons
  a = sub
  # Create missingness indicator
  a[,i] = factor(ifelse(is.na(a[,i]),0,1)) # missing=0, observed=1
  # Remove all variables following variable i (remark that the bgvars are the first variables so they are always included)
  nr = which(names(sub)==i)
  a = data.frame(a[,c(1:nr)])
  list[[i]] = a
}
# Q9
fit = glm(Q9 ~ ., # "." can be used because it includes all the remaining vars in the data, which is here restrict to only the necessary ones
          family = binomial(link = "logit"), 
          data=list[["Q9"]]) 
summary(fit)
car::vif(fit)
# Q14_1
fit = glm(Q14_1 ~ Gender + Age + Fase + Q5 + Q9, 
          family = binomial(link = "logit"), 
          data=list[["Q14_1"]]) 
alias(fit)
summary(fit)
car::vif(fit)
# Q17_2
fit = glm(Q17_2 ~ Gender + Age + Fase + Q5 + Q9 + Q14_1 +Q14_2 +Q14_3 +Q14_4 +Q16, 
          family = binomial(link = "logit"), 
          data=list[["Q17_2"]]) 
summary(fit)
car::vif(fit)
# b. 5 delen in dataset
# create one dataset for each survey part that includes only students who reached that part
p1 = c("Q9", "Q14_1", "Q14_2", "Q14_3", "Q14_4") # +/- RU
p2 = c("Q16", "Q17_2","Q17_3", "Q17_4", "Q17_5", "Q17_6") # = CFC
p3 = c("Q24_1", "Q24_2", "Q24_3","Q24_4", "Q24_5") # +/- CU
p4 = c("Q20", "Q23", "Q25_2", "Q25_3", "Q25_4", "Q25_5") # +/- CC
p5 = c("Q30", "Q32_1", "Q32_2", "Q32_3", "Q32_4", "Q32_5", "Q32_6","Q32_7") # = CE
incl = c("Gender","Age","Fase","Q5","Q8")

data$p1 = apply(data[,p1],1,function(x){ifelse(anyNA(x),0,1)}) # 1=observed ; 0=NA
data$p2 = apply(data[,p2],1,function(x){ifelse(anyNA(x),0,1)})
data$p3 = apply(data[,c(p3)],1,function(x){ifelse(anyNA(x),0,1)})
data$p4 = apply(data[,c(p4)],1,function(x){ifelse(anyNA(x),0,1)})
data$p5 = apply(data[,p5],1,function(x){ifelse(anyNA(x),0,1)})

part1 = data[,c(incl,"p1")]
part2 = data[!(data$p1==0),c(incl,p1,"p2")] # exclude the previous students who dropped out in part1
part3 = data[!(data$p1==0 | data$p2==0),c(incl,p1,p2,"p3")]
part4 = data[!(data$p1==0 | data$p2==0 | data$p3==0),c(incl,p1,p2,p3,"p4")]
part5 = data[!(data$p1==0 | data$p2==0 | data$p3==0 | data$p4==0),c(incl,p1,p2,p3,p4,"p5")]
list = list(part1=part1,part2=part2,part3=part3,part4=part4,part5=part5)
for (i in 1:5){
  list[[i]] = data.frame(apply(list[[i]],2,factor))
  if (i>1){
    list[[i]]$Q9 = NULL # delete this question and use Q8 instead because of singularity
  }
  list[[i]]$Age = as.numeric(as.character(list[[i]]$Age))
}
# Part1
fit = glm(p1 ~ ., 
          family = binomial(link = "logit"), 
          data=list[["part1"]]) 
summary(fit)
car::vif(fit)
# Part2
fit = glm(p2 ~ ., 
          family = binomial(link = "logit"), 
          data=list[["part2"]]) 
summary(fit)
car::vif(fit)
# Part3 (does not work, neither does combining part3+part4+part5)
library(brglm2)
library(detectseparation)
# - separation issue because of zero count for certain response categories
fit = glm(p3 ~ ., 
          family = binomial(link = "logit"), 
          data=list[["part3"]],
          method="detect_separation");fit
# - investigate separation issue
a = list[["part3"]]
for (i in names(a)){ # separated variables
  print(table(a[,c("p3",i)])) # Check wheter the categories have 0 count for either completers or dropouts
}
a$Q14_2 = relevel(a$Q14_2,ref = "2") # The reference level had 0 count
a$Q17_2 = relevel(a$Q17_2,ref = "2")
a$Q17_4 = relevel(a$Q17_4,ref = "2")
fit = glm(p3 ~ ., 
          family = binomial(link = "logit"), 
          data=a,
          method="detect_separation");fit

#
fit = glm(p3 ~ ., 
          family = binomial(link = "logit"), 
          data=list[["part3"]],
          method="brglmFit")
summary(fit)
fit = glm(p4 ~ ., 
          family = binomial(link = "logit"), 
          data=list[["part4"]],
          method="detect_separation");fit
fit = glm(p5 ~ ., 
          family = binomial(link = "logit"), 
          data=list[["part5"]],
          method="detect_separation");fit
# - ignoring the categorical nature of the responses ignores the separation problem (but also gives completely different results)
a = data.frame(apply(list[["part3"]],2,function(x){as.numeric(as.character(x))}))
a$Gender = list[["part3"]]$Gender
fit = glm(p3 ~ ., 
          family = binomial(link = "logit"), 
          data=a)
summary(fit)
car::vif(fit)
# Part4
fit = glm(p4 ~ ., 
          family = binomial(link = "logit"), 
          data=list[["part4"]],
          method="detect_separation");fit
summary(fit)
a = data.frame(apply(list[["part4"]],2,function(x){as.numeric(as.character(x))}))
a$Gender = list[["part4"]]$Gender
fit = glm(p4 ~ ., 
          family = binomial(link = "logit"), 
          data=a)
summary(fit)
car::vif(fit)
# Part5
a = data.frame(apply(list[["part5"]],2,function(x){as.numeric(as.character(x))}))
a$Gender = list[["part5"]]$Gender
fit = glm(p5 ~ ., 
          family = binomial(link = "logit"), 
          data=a)
summary(fit)
car::vif(fit)

# ---------------  ------------------
sub = data[,c("Gender","Age","Fase","Q5",order_qs)]
sub = data.frame(apply(sub,2,factor))
sub$Age = as.numeric(as.character(sub$Age))
sub$miss = apply(sub[,order_qs],1,function(x){sum(is.na(x))})

fit = glm(miss ~ Gender + Age + Fase + Q5 + Q8, 
          family = poisson(link = "log"), data=sub[sub$miss>0,]) 
summary(fit)

# --------------- pairwise comparisons ------------------
p1 = c("Q9", "Q14_1", "Q14_2", "Q14_3", "Q14_4") # +/- RU
p2 = c("Q16", "Q17_2","Q17_3", "Q17_4", "Q17_5", "Q17_6") # = CFC
p3 = c("Q24_1", "Q24_2", "Q24_3","Q24_4", "Q24_5") # +/- CU
p4 = c("Q20", "Q23", "Q25_2", "Q25_3", "Q25_4", "Q25_5") # +/- CC
p5 = c("Q30", "Q32_1", "Q32_2", "Q32_3", "Q32_4", "Q32_5", "Q32_6","Q32_7") # = CE
incl = c("Gender","Fase","Q5","Q8")

data$p1 = apply(data[,p1],1,function(x){ifelse(anyNA(x),0,1)}) # 1=observed ; 0=NA
data$p2 = apply(data[,p2],1,function(x){ifelse(anyNA(x),0,1)})
data$p3 = apply(data[,c(p3)],1,function(x){ifelse(anyNA(x),0,1)})
data$p4 = apply(data[,c(p4)],1,function(x){ifelse(anyNA(x),0,1)})
data$p5 = apply(data[,p5],1,function(x){ifelse(anyNA(x),0,1)})

part1 = data[,c(incl,"p1")]
part2 = data[!(data$p1==0),c(incl,p1,"p2")] # exclude the previous students who dropped out in part1
part3 = data[!(data$p1==0 | data$p2==0),c(incl,p1,p2,"p3")]
part4 = data[!(data$p1==0 | data$p2==0 | data$p3==0),c(incl,p1,p2,p3,"p4")]
part5 = data[!(data$p1==0 | data$p2==0 | data$p3==0 | data$p4==0),c(incl,p1,p2,p3,p4,"p5")]
list = list(part1=part1,part2=part2,part3=part3,part4=part4,part5=part5)
for (i in 1:5){
  list[[i]] = data.frame(apply(list[[i]],2,factor))
}
# 1. P-values
# p1
vars = c("Gender","Fase","Q5","Q8")
c_p1 =c()
for (i in vars){
  name = paste("p1",i,sep="_")
  c_p1[name] = fisher.test(table(data[,c("p1",i)]))$p
}
# p2
vars = c("Gender","Fase","Q5","Q8",p1)
c_p2 =c()
for (i in vars){
  name = paste("p2",i,sep="_")
  c_p2[name] = fisher.test(table(data[,c("p2",i)]))$p
}; round(c_p2,3)
# p3
vars = c("Gender","Fase","Q5","Q8",p1,p2)
c_p3 =c()
for (i in vars){
  name = paste("p3",i,sep="_")
  c_p3[name] = fisher.test(table(data[,c("p3",i)]))$p
}; round(c_p3,3)
# p4
vars = c("Gender","Fase","Q5","Q8",p1,p2,p3)
c_p4 =c()
for (i in vars){
  name = paste("p4",i,sep="_")
  c_p4[name] = fisher.test(table(data[,c("p4",i)]))$p
}; round(c_p4,3)
# p5
vars = c("Gender","Fase","Q5","Q8",p1,p2,p3,p4)
c_p5 =c()
for (i in vars){
  name = paste("p5",i,sep="_")
  c_p5[name] = fisher.test(table(data[,c("p5",i)]))$p
}; round(c_p5,3)

ps = c(c_p1,c_p2,c_p3,c_p4,c_p5)
pvals = data.frame(p = ps,
                   p_adj = p.adjust(ps,method="BH")) # of "holm"
pvals$part = substr(row.names(pvals),1,2)
pvals$Q = substr(row.names(pvals),4,10)
pvals$part = ifelse(pvals$part=="p1","part1",
                    ifelse(pvals$part=="p2","part2",
                           ifelse(pvals$part=="p3","part3",
                                  ifelse(pvals$part=="p4","part4","part5"))))

# 2. Graph
win.graph(width = 14,height = 14)
nr=0 # Counter
# Number of columns in plot
cols = 6
# Total number of rows in plot
c1 = 1 + 5*ceiling(length(incl)/cols) # part1
c2 = 1 + 5*ceiling(length(c(incl,p1))/cols) # part2
c3 = 1 + 5*ceiling(length(c(incl,p1,p2))/cols) # part3
c4 = 1 + 5*ceiling(length(c(incl,p1,p2,p3))/cols) # part4
c5 = 1 + 5*ceiling(length(c(incl,p1,p2,p3,p4))/cols) # part5
rows = sum(c1,c2,c3,c4,c5)
for (part in names(list)){
  nr = nr+1
  # -----------------------------------------------------------------------------
  # Copy dataset:
  t = list[[part]]
  # Get the dropout variable
  pi = names(t)[ncol(t)] # is always the last variable in the dataset
  # sample sizes:
  n0 = table(t[,pi])[1] # Number of dropouts in part
  n1 = table(t[,pi])[2] # Number of completers
  # Create percentages per category:
  plot = list()
  qs = names(t)[names(t)!=pi]
  for (i in qs){
    # Calculate percentages of the response categories for the completers and non-completers
    a = table(t[t[,pi]==0,i])*100/n0
    b = table(t[t[,pi]==1,i])*100/n1
    # Number of catergories
    nl = length(a)
    # Collect in a dataframe
    plot[[i]] = data.frame(levels = rep(names(a),2),
                           percent = c(a,b),
                           drop = c(rep(0,nl),rep(1,nl)))
    
  }
  # ----------------------------------------------------------------------------
  pushViewport(viewport(layout=grid.layout( rows , cols )))
  # a. Title graph
  ti = ggplot(data=data.frame(x=c(1),y=c(1)),aes(x=x,y=y))+
    geom_point(shape=as.character(nr),size=5)+
    theme_void()
  if (nr==1){print(ti,vp=viewport(layout.pos.row=1,layout.pos.col=c(1:cols)))}
  if (nr==2){print(ti,vp=viewport(layout.pos.row=(1+c1),layout.pos.col=c(1:cols)))}
  if (nr==3){print(ti,vp=viewport(layout.pos.row=(1+c1+c2),layout.pos.col=c(1:cols)))}
  if (nr==4){print(ti,vp=viewport(layout.pos.row=(1+c1+c2+c3),layout.pos.col=c(1:cols)))}
  if (nr==5){print(ti,vp=viewport(layout.pos.row=(1+c1+c2+c3+c4),layout.pos.col=c(1:cols)))}
  # b. Graphs per survey item
  for (i in 1:length(plot)){
    # Get the pvalues from the 'pvals' dataset
    item = names(plot)[i]
    a1 = round(pvals$p[pvals$part==part & pvals$Q==item],3) # unadjusted pvalue
    a2 = round(pvals$p_adj[pvals$part==part & pvals$Q==item],3) # unadjusted pvalue
    # The plot
    g = ggplot(data=plot[[i]],aes(x=levels,y=percent,fill=factor(drop)))+
      geom_col(position="dodge")+
      scale_fill_manual(values = c("brown4","antiquewhite2"),guide=FALSE)+
      coord_flip()+
      ggtitle(paste(names(plot)[i]))+
      scale_x_discrete(expand=c(0,0))+scale_y_continuous(expand=c(0,0))+ # Bars start at axis (remakr: should be in fornt of theme for overwriting reasons)
      ylab(paste("p = ",a1,"  |  ","p adj = ",a2,sep=""))+
      theme(# Remove plot background:
        panel.background = element_blank(),
        # Remove axes & its attributes:
        axis.line.y=element_line(), # draw yaxis
        axis.text.x = element_blank(), # remark: coord_flip --> x becomes y
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size=11),
        # Remove facet_grid boxes
        strip.text.x = element_blank(),
        # Centre title:
        plot.title = element_text(hjust = 0.5,vjust=1,color = "Gray15"))
    # Position of the plot
    if (nr==1){
      if (i %in% c(1:cols)){print(g,vp=viewport(layout.pos.row=1+c(1:5),layout.pos.col=i))}
      if (i %in% c((cols+1):(2*cols))){ # when there are more variables to plot then columns specified
        start = max(1+c(1:5))+1
        print(g,vp=viewport(layout.pos.row=c(start:(start+4)),layout.pos.col=(i-cols)))}
      if (i %in% c((2*cols+1):(3*cols))){
        start = max(1+c(1:5))+1+5
        print(g,vp=viewport(layout.pos.row=c(start:(start+4)),layout.pos.col=(i-2*cols)))}
      if (i %in% c((3*cols+1):(4*cols))){
        start = max(1+c(1:5))+1+2*5
        print(g,vp=viewport(layout.pos.row=c(start:(start+4)),layout.pos.col=(i-3*cols)))}
      if (i %in% c((4*cols+1):(5*cols))){
        start = max(1+c(1:5))+1+3*5
        print(g,vp=viewport(layout.pos.row=c(start:(start+4)),layout.pos.col=(i-4*cols)))}
      if (i %in% c((5*cols+1):(6*cols))){
        start = max(1+c(1:5))+1+4*5
        print(g,vp=viewport(layout.pos.row=c(start:(start+4)),layout.pos.col=(i-5*cols)))}
      if (i %in% c((6*cols+1):(7*cols))){
        start = max(1+c(1:5))+1+5*5
        print(g,vp=viewport(layout.pos.row=c(start:(start+4)),layout.pos.col=(i-6*cols)))}
    }
    if (nr==2){
      if (i %in% c(1:cols)){print(g,vp=viewport(layout.pos.row=c1+1+c(1:5),layout.pos.col=i))}
      if (i %in% c((cols+1):(2*cols))){
        start = max(c1+1+c(1:5))+1
        print(g,vp=viewport(layout.pos.row=c(start:(start+4)),layout.pos.col=(i-cols)))}
      if (i %in% c((2*cols+1):(3*cols))){
        start = max(c1+1+c(1:5))+1+5
        print(g,vp=viewport(layout.pos.row=c(start:(start+4)),layout.pos.col=(i-2*cols)))}
      if (i %in% c((3*cols+1):(4*cols))){
        start = max(c1+1+c(1:5))+1+2*5
        print(g,vp=viewport(layout.pos.row=c(start:(start+4)),layout.pos.col=(i-3*cols)))}
      if (i %in% c((4*cols+1):(5*cols))){
        start = max(c1+1+c(1:5))+1+3*5
        print(g,vp=viewport(layout.pos.row=c(start:(start+4)),layout.pos.col=(i-4*cols)))}
      if (i %in% c((5*cols+1):(6*cols))){
        start = max(c1+1+c(1:5))+1+4*5
        print(g,vp=viewport(layout.pos.row=c(start:(start+4)),layout.pos.col=(i-5*cols)))}
      if (i %in% c((6*cols+1):(7*cols))){
        start = max(c1+1+c(1:5))+1+5*5
        print(g,vp=viewport(layout.pos.row=c(start:(start+4)),layout.pos.col=(i-6*cols)))}
    }
    if (nr==3){
      if (i %in% c(1:cols)){print(g,vp=viewport(layout.pos.row=1+c(1:5)+c1+c2,layout.pos.col=i))}
      if (i %in% c((cols+1):(2*cols))){
        start = max(1+c(1:5)+c1+c2)+1
        print(g,vp=viewport(layout.pos.row=c(start:(start+4)),layout.pos.col=(i-cols)))}
      if (i %in% c((2*cols+1):(3*cols))){
        start = max(1+c(1:5)+c1+c2)+1+5
        print(g,vp=viewport(layout.pos.row=c(start:(start+4)),layout.pos.col=(i-2*cols)))}
      if (i %in% c((3*cols+1):(4*cols))){
        start = max(1+c(1:5)+c1+c2)+1+2*5
        print(g,vp=viewport(layout.pos.row=c(start:(start+4)),layout.pos.col=(i-3*cols)))}
      if (i %in% c((4*cols+1):(5*cols))){
        start = max(1+c(1:5)+c1+c2)+1+3*5
        print(g,vp=viewport(layout.pos.row=c(start:(start+4)),layout.pos.col=(i-4*cols)))}
      if (i %in% c((5*cols+1):(6*cols))){
        start = max(1+c(1:5)+c1+c2)+1+4*5
        print(g,vp=viewport(layout.pos.row=c(start:(start+4)),layout.pos.col=(i-5*cols)))}
      if (i %in% c((6*cols+1):(7*cols))){
        start = max(1+c(1:5)+c1+c2)+1+2*5
        print(g,vp=viewport(layout.pos.row=c(start:(start+4)),layout.pos.col=(i-6*cols)))}
    }
    if (nr==4){
      if (i %in% c(1:cols)){print(g,vp=viewport(layout.pos.row=1+c(1:5)+c1+c2+c3,layout.pos.col=i))}
      if (i %in% c((cols+1):(2*cols))){
        start = max(1+c(1:5)+c1+c2++c3)+1
        print(g,vp=viewport(layout.pos.row=c(start:(start+4)),layout.pos.col=(i-cols)))}
      if (i %in% c((2*cols+1):(3*cols))){
        start = max(1+c(1:5)+c1+c2++c3)+1+5
        print(g,vp=viewport(layout.pos.row=c(start:(start+4)),layout.pos.col=(i-2*cols)))}
      if (i %in% c((3*cols+1):(4*cols))){
        start = max(1+c(1:5)+c1+c2++c3)+1+2*5
        print(g,vp=viewport(layout.pos.row=c(start:(start+4)),layout.pos.col=(i-3*cols)))}
      if (i %in% c((4*cols+1):(5*cols))){
        start = max(1+c(1:5)+c1+c2++c3)+1+3*5
        print(g,vp=viewport(layout.pos.row=c(start:(start+4)),layout.pos.col=(i-4*cols)))}
      if (i %in% c((6*cols+1):(7*cols))){
        start = max(1+c(1:5)+c1+c2++c3)+1+4*5
        print(g,vp=viewport(layout.pos.row=c(start:(start+4)),layout.pos.col=(i-5*cols)))}
      if (i %in% c((7*cols+1):(8*cols))){
        start = max(1+c(1:5)+c1+c2++c3)+1+5*5
        print(g,vp=viewport(layout.pos.row=c(start:(start+4)),layout.pos.col=(i-6*cols)))}
    }
    if (nr==5){
      if (i %in% c(1:cols)){print(g,vp=viewport(layout.pos.row=1+c(1:5)+c1+c2+c3+c4,layout.pos.col=i))}
      if (i %in% c((cols+1):(2*cols))){
        start = max(1+c(1:5)+c1+c2+c3+c4)+1
        print(g,vp=viewport(layout.pos.row=c(start:(start+4)),layout.pos.col=(i-cols)))}
      if (i %in% c((2*cols+1):(3*cols))){
        start = max(1+c(1:5)+c1+c2+c3+c4)+1+5
        print(g,vp=viewport(layout.pos.row=c(start:(start+4)),layout.pos.col=(i-2*cols)))}
      if (i %in% c((3*cols+1):(4*cols))){
        start = max(1+c(1:5)+c1+c2+c3+c4)+1+2*5
        print(g,vp=viewport(layout.pos.row=c(start:(start+4)),layout.pos.col=(i-3*cols)))}
      if (i %in% c((4*cols+1):(5*cols))){
        start = max(1+c(1:5)+c1+c2+c3+c4)+1+3*5
        print(g,vp=viewport(layout.pos.row=c(start:(start+4)),layout.pos.col=(i-4*cols)))}
      if (i %in% c((5*cols+1):(6*cols))){
        start = max(1+c(1:5)+c1+c2+c3+c4)+1+4*5
        print(g,vp=viewport(layout.pos.row=c(start:(start+4)),layout.pos.col=(i-5*cols)))}
      if (i %in% c((6*cols+1):(7*cols))){
        start = max(1+c(1:5)+c1+c2+c3+c4)+1+5*5
        print(g,vp=viewport(layout.pos.row=c(start:(start+4)),layout.pos.col=(i-6*cols)))}
    }
  }
}
export::graph2ppt(file="D:/Unief/Master Statistiek/Jaar 2/Thesis/5. Thesis/3. Results/1. Descriptive statistics/Missingness_parts_hist_p.ppt",
                  width=100,height=200,append=TRUE)








