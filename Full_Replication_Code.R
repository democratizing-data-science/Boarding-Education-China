## load or install&load all packages required
packages = c("twang", "texreg", "xtable", "survey")
package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

boarding_china<-read.csv("https://raw.githubusercontent.com/democratizing-data-science/Boarding-Education-China/refs/heads/main/students%20cleaned%20version.csv")

boarding_china$hukou<-NA
boarding_china$hukou[boarding_china$sAgricultural.Hukou==1]<-"Agricultural"
boarding_china$hukou[boarding_china$sNon.agricultural.Hukou==1]<-"Non-Agricultural"
boarding_china$hukou[boarding_china$sResidential.Hukou==1]<-"Residential"

boarding_china$left <- ifelse(boarding_china$hukou=="Agricultural"&(boarding_china$motherlivehouse==0|boarding_china$living_with_father==0),1,0)
table(boarding_china$left)

### Estimate  propensity score boarding_china:
init_name = names(boarding_china)[5]
for (var1 in names(boarding_china)[c(5:92,94:113)]) { #loop
  init_name = paste(init_name, var1, sep=" + ")
}
set.seed(1)

ps.boarding_china <- ps(treatment ~ left + student_cognition + trad_chn + trad_mat + trad_eng + women + age_yrs + han_nation + migration + mother_gov + mother_middle_senior + mother_academia + mother_tech_ordinary + mother_self + mother_peasant + mother_unemployed + mother_other + father_gov + father_middle_senior + father_academia + father_tech_ordinary + father_self + father_peasant + father_unemployed + father_other + sVery.few + sNot.many + sSome + sQuite.a.few + sA.great.number + sNo..we.don.t + sWe.have.a.computer.but.no.access.to.the.Internet + sYes..we.have.both + only_child + height + homework_commitment + living_with_father + living_with_nonrelative + sNo..we.don.t.1 + sWe.have.a.computer.but.no.access.to.the.Internet.1 + sYes..we.have.both.1 + sGovernment.official..staff.of.public.institutions..civil.servant + sManager.or.administrator.of.enterprises.corporations + sScientist.engineer + sTeacher.doctor.lawyer + sDesigner + sArtistic.performer.actor.host + sProfessional.athlete + sTechnical.worker..including.driver. + sOther + sThey.don..t.care + sNot.clear + sNot.confident.at.all + sNot.so.confident + sSomewhat.confident + sVery.confident + ever_suspended_school + sNear.the.bottom + sBelow.the.average + sAbout.the.average + sAbove.the.average + sAround.the.top + go_school + motherlivehouse + hrs_hwrk + ever_repeated + academic_rank + sGovernment.official..staff.of.public.institutions..civil.servant.1 + sManager.or.administrator.of.enterprises.corporations.1 + sScientist.engineer.1 + sTeacher.doctor.lawyer.1 + sDesigner.1 + sArtistic.performer.actor.host.1 + sProfessional.athlete.1 + sTechnical.worker..including.driver..1 + sOther.1 + sI.don..t.care + not_comm_if_time_consuming + grand_live_home + rich + poor + moderate + sIn.rural.area + sIn.small.or.medium.cities + sIn.big.cities..such.as.Beijing.Shanghai.Guangzhou + sAbroad + sThey.don..t.care.1 + sNot.clear.1 + school_transfer + hospitalized + Other_rel + desk + Not_conf + skip_grade + sGraduate.from.junior.high.school + sGo.to.technical.secondary.school.or.technical.school + sGo.to.vocational.high.school + sGo.to.senior.high.school + sGraduate.from.junior.college + sGet.a.bachelor.degree + sGet.a.Master.degree + sGet.a.Doctor.degree + sThey.don..t.care.2 + not_good_health + good_health + moderate_health + sAgricultural.Hukou + sNon.agricultural.Hukou + sResidential.Hukou,
	data = boarding_china, 
	n.trees=6000,#maximum number of iterations that gbm will run.
	interaction.depth=3,#controls the level of interactions allowed in the GBM, with default =3
	shrinkage=0.01,# Shrinkage is commonly used in ridge regression where it literally shrinks regression coefficients to zero and, thus, reduces the impact of potentially unstable regression coefficients. In the context of GBMs, shrinkage is used for reducing, or shrinking, the impact of each additional fitted base-learner. It reduces the size of incremental steps and thus penalizes the importance of each consecutive iteration http://www.ncbi.nlm.nih.gov/pmc/articles/PMC3885826/
	perm.test.iters=100,#of Monte Carlo trials are run to establish the reference distribution of KS statistics for each covariate
	stop.method=c("es.mean"),#specifies a set (or sets) of rules and measures for assessing the balance, or equivalence, established on the pretreatment covariates of the weighted treatment and control groups. The ps function selects the optimal number of GBM iterations to minimize the differences between the treatment and control groups as measured by the rules of the given stop.method object.
	estimand = "ATT",
	verbose=FALSE)

summary(ps.boarding_china)

trellis.device(color = FALSE)
plot(ps.boarding_china, main="Generalized Boosted Regression Models", col=F)
#To get relative influence
tableModel<-summary(ps.boarding_china$gbm.obj, n.trees=ps.boarding_china$desc$es.mean.ATT$n.trees, plot=F)
rownames(tableModel)<-NULL
library(xtable)
colnames(tableModel) <- c("Variable","Rel Influence")
xtable(tableModel,
caption = "Model fitted for PSM and relative influence of each variable",
label = "MOdelRelInfTYDCFILY",
digits = c(0,0, 2),
align=c("l","r","r"))

# # # Graphical assessment
plot(ps.boarding_china, plots=2)#common support
plot(ps.boarding_china, plots=3)#stand diff
plot(ps.boarding_china, plots=4)


boarding_china.balance <- bal.table(ps.boarding_china)
boarding_china.balance

# Table 1
pretty.tab <- boarding_china.balance$es.mean.ATT[,c("tx.mn","ct.mn","p")]
pretty.tab <- cbind(pretty.tab, boarding_china.balance$unw[,c("ct.mn","p")])
lg<-xtable(pretty.tab,
       caption = "Balance of the treatment and comparison groups",
       label = "tab:balance",
       digits = c(0, 2, 2, 4, 2,2),
       align=c("l","r","r","r","r","r"))
print(lg, tabular.environment = "longtable")

#extract PS weights
boarding_china$w <- get.weights(ps.boarding_china, stop.method="es.mean")#original dataset

# Table 2
lm1 <- lm(chn_pct ~ treatment, data=boarding_china) 
lm2 <- lm(mat_pct ~ treatment, data = boarding_china)
lm3 <- lm(eng_pct ~ treatment, data = boarding_china)

psweight.ch <- lm(chn_pct ~ treatment, data = boarding_china, weights = w) 
psweight.ma <- lm(mat_pct ~ treatment, data = boarding_china, weights = w)
psweight.en <- lm(eng_pct ~ treatment, data = boarding_china, weights = w)

psweight.chd <- lm(chn_pct ~ treatment + trad_chn, data = boarding_china, weights = w) 
psweight.mad <- lm(mat_pct ~ treatment + trad_mat, data = boarding_china, weights = w)
psweight.end <- lm(eng_pct ~ treatment + trad_eng, data = boarding_china, weights = w)

table <- texreg(list(lm1, lm2, lm3, psweight.ch, psweight.ma, psweight.en, psweight.chd, psweight.mad, psweight.end), use.packages=TRUE, label="tab:3", caption="Aggregate Effects", scriptsize=FALSE, custom.model.names=c("OLS Chn","OLS Mat","OLS Eng", "PSW Chn", "PSW Mat", "PSW Eng", "PSW D* Chn", "PSW D* Mat", "PSW D* Eng"), float.pos="b", digits = 3)
table

# Table 3

Rpsweight.chd <- lm(chn_pct ~ treatment + trad_chn, data = boarding_china[boarding_china$hukou=="Agricultural",], weights = w) 
Rpsweight.mad <- lm(mat_pct ~ treatment + trad_mat, data = boarding_china[boarding_china$hukou=="Agricultural",], weights = w)
Rpsweight.end <- lm(eng_pct ~ treatment + trad_eng, data = boarding_china[boarding_china$hukou=="Agricultural",], weights = w)

Upsweight.chd <- lm(chn_pct ~ treatment + trad_chn, data = boarding_china[boarding_china$hukou!="Agricultural",], weights = w) 
Upsweight.mad <- lm(mat_pct ~ treatment + trad_mat, data = boarding_china[boarding_china$hukou!="Agricultural",], weights = w)
Upsweight.end <- lm(eng_pct ~ treatment + trad_eng, data = boarding_china[boarding_china$hukou!="Agricultural",], weights = w)

table <- texreg(list(Rpsweight.chd, Rpsweight.mad, Rpsweight.end,Upsweight.chd, Upsweight.mad, Upsweight.end), use.packages=TRUE, label="tab:3", caption="Aggregate Effects", scriptsize=FALSE, custom.model.names=c("PSW D* Rural Chn", "PSW D* Rural Mat", "PSW D* Rural Eng","PSW D* Urban Chn", "PSW D* Urban Mat", "PSW D* Urban Eng"), float.pos="b", digits = 3)
table

#Table 4 
Ppsweight.chd <- lm(chn_pct ~ treatment + trad_chn, data = boarding_china[boarding_china$poor==1,], weights = w) 
Ppsweight.mad <- lm(mat_pct ~ treatment + trad_mat, data = boarding_china[boarding_china$poor==1,], weights = w)
Ppsweight.end <- lm(eng_pct ~ treatment + trad_eng, data = boarding_china[boarding_china$poor==1,], weights = w)

Mpsweight.chd <- lm(chn_pct ~ treatment + trad_chn, data = boarding_china[boarding_china$moderate==1,], weights = w) 
Mpsweight.mad <- lm(mat_pct ~ treatment + trad_mat, data = boarding_china[boarding_china$moderate==1,], weights = w)
Mpsweight.end <- lm(eng_pct ~ treatment + trad_eng, data = boarding_china[boarding_china$moderate==1,], weights = w)

Rpsweight.chd <- lm(chn_pct ~ treatment + trad_chn, data = boarding_china[boarding_china$rich==1,], weights = w) 
Rpsweight.mad <- lm(mat_pct ~ treatment + trad_mat, data = boarding_china[boarding_china$rich==1,], weights = w)
Rpsweight.end <- lm(eng_pct ~ treatment + trad_eng, data = boarding_china[boarding_china$rich==1,], weights = w)

table <- texreg(list(Ppsweight.chd, Ppsweight.mad, Ppsweight.end, Mpsweight.chd, Mpsweight.mad, Mpsweight.end,Rpsweight.chd, Rpsweight.mad, Rpsweight.end), use.packages=TRUE, label="tab:3", caption="Aggregate Effects", scriptsize=FALSE, custom.model.names=c("Low SES Chn", "Low SES Mat", "Low SES Eng","Mid SES Chn", "Mid SES Mat", "Mid SES Eng", "High SES Chn", "High SES Mat", "High SES Eng"), float.pos="b", digits = 3)
table

# Table 5 
Lpsweight.chd <- lm(chn_pct ~ treatment + trad_chn, data = boarding_china[boarding_china$left==1,], weights = w) 
Lpsweight.mad <- lm(mat_pct ~ treatment + trad_mat, data = boarding_china[boarding_china$left==1,], weights = w)
Lpsweight.end <- lm(eng_pct ~ treatment + trad_eng, data = boarding_china[boarding_china$left==1,], weights = w)

NLRpsweight.chd <- lm(chn_pct ~ treatment + trad_chn, data = boarding_china[boarding_china$left==0&boarding_china$hukou=="Agricultural",], weights = w) 
NLRpsweight.mad <- lm(mat_pct ~ treatment + trad_mat, data = boarding_china[boarding_china$left==0&boarding_china$hukou=="Agricultural",], weights = w)
NLRpsweight.end <- lm(eng_pct ~ treatment + trad_eng, data = boarding_china[boarding_china$left==0&boarding_china$hukou=="Agricultural",], weights = w)

table <- texreg(list(Lpsweight.chd, Lpsweight.mad, Lpsweight.end, NLRpsweight.chd, NLRpsweight.mad, NLRpsweight.end), use.packages=TRUE, label="tab:3", caption="Aggregate Effects", scriptsize=FALSE, custom.model.names=c("LB Chn", "LB Mat", "LB Eng", "NoLB R Chn", "NoLB R Mat", "NoLB R Eng"), float.pos="b", digits = 3)
table

# Table 6
LPpsweight.chd <- lm(chn_pct ~ treatment + trad_chn, data = boarding_china[boarding_china$left==1&boarding_china$poor==1,], weights = w) 
LPpsweight.mad <- lm(mat_pct ~ treatment + trad_mat, data = boarding_china[boarding_china$left==1&boarding_china$poor==1,], weights = w)
LPpsweight.end <- lm(eng_pct ~ treatment + trad_eng, data = boarding_china[boarding_china$left==1&boarding_china$poor==1,], weights = w)

NLRPpsweight.chd <- lm(chn_pct ~ treatment + trad_chn, data = boarding_china[boarding_china$left==0&boarding_china$poor==1&boarding_china$hukou=="Agricultural",], weights = w) 
NLRPpsweight.mad <- lm(mat_pct ~ treatment + trad_mat, data = boarding_china[boarding_china$left==0&boarding_china$poor==1&boarding_china$hukou=="Agricultural",], weights = w)
NLRPpsweight.end <- lm(eng_pct ~ treatment + trad_eng, data = boarding_china[boarding_china$left==0&boarding_china$poor==1&boarding_china$hukou=="Agricultural",], weights = w)

NLUPpsweight.chd <- lm(chn_pct ~ treatment + trad_chn, data = boarding_china[boarding_china$left==0&boarding_china$poor==1&boarding_china$hukou!="Agricultural",], weights = w) 
NLUPpsweight.mad <- lm(mat_pct ~ treatment + trad_mat, data = boarding_china[boarding_china$left==0&boarding_china$poor==1&boarding_china$hukou!="Agricultural",], weights = w)
NLUPpsweight.end <- lm(eng_pct ~ treatment + trad_eng, data = boarding_china[boarding_china$left==0&boarding_china$poor==1&boarding_china$hukou!="Agricultural",], weights = w)


table <- texreg(list(
LPpsweight.chd,
LPpsweight.mad,
LPpsweight.end,
NLRPpsweight.chd,
NLRPpsweight.mad,
NLRPpsweight.end,
NLUPpsweight.chd,
NLUPpsweight.mad,
NLUPpsweight.end), use.packages=TRUE, label="tab:3", caption="Aggregate Effects", scriptsize=FALSE, custom.model.names=c("LB L Chn", "LB L Mat", "LB L Eng", "NoLB RL Chn", "NoLB RL Mat", "NoLB RL Eng", "NoLB UL Chn", "NoLB UL Mat", "NoLB UL Eng"), float.pos="b", digits = 3)
table

# Table 7 

boarding_china$t_e <- ifelse(boarding_china$hukou!="Agricultural" & boarding_china$treatment==0, "Commuter_Urban", ifelse(boarding_china$hukou=="Agricultural" & boarding_china$treatment==0, "Commuter_Rural", NA))

Upsweighte.end <- lm(eng_pct ~ t_e, data = boarding_china[!is.na(boarding_china$t_e),], weights = w)
Upsweightc.end <- lm(eng_pct ~ t_e + trad_eng, data = boarding_china[!is.na(boarding_china$t_e),], weights = w)

texreg(list(Upsweighte.end, Upsweightc.end), use.packages=TRUE, label="tab:3", caption="Testing for Rurality Impact on English", scriptsize=FALSE, custom.model.names=c("English Proficiency Empty","English Proficiency Control"), float.pos="b")

### These Oster's procedures require STATA
ssc install psacalc 
insheet using "G:\My Drive\Phudcfily\Chinese context\Paper and TWANG\boarding_weightstwang.csv", clear

pstest age age_sq educ educ_sq married nodegree black hisp re74 re75 re74_sq re75_sq u74 u75 u74_hisp u74_black, t(treatment) both mweight(w) scatter

regress chn_pct treatment trad_chn  
display 1.3 * 0.2977
psacalc delta treatment, rmax(.38701)
*delta        |      -4.37120

*Table 2 aggregate
regress chn_pct treatment trad_chn [pweight=w]  
display 1.3 * 0.3452
psacalc delta treatment, rmax(.44876)
*delta        |      84.54830

regress mat_pct treatment trad_mat [pweight=w]  
display 1.3 * 0.4679
psacalc delta treatment, rmax(.60827)
*delta        |      1502.275

regress eng_pct treatment trad_eng [pweight=w]  
display 1.3 * 0.4097
psacalc delta treatment, rmax(.53261)
*delta        |      1502.275

*Table 3 rural
regress chn_pct treatment trad_chn [pweight=w]  if hukou == "Agricultural"
display 1.3 * 0.3460
psacalc delta treatment, rmax(.4498)
*delta        |      411.66

regress mat_pct treatment trad_mat [pweight=w]  if hukou == "Agricultural"
display 1.3 * 0.4681
psacalc delta treatment, rmax(.60853)
*delta        |      32.35

regress eng_pct treatment trad_eng [pweight=w]  if hukou == "Agricultural"
display 1.3 * 0.4127
psacalc delta treatment, rmax(.53651)
*delta        |      -19.473

regress chn_pct treatment trad_chn [pweight=w]  if hukou != "Agricultural"
display 1.3 * 0.3498
psacalc delta treatment, rmax(.45474)
*delta        |      6.065

regress mat_pct treatment trad_mat [pweight=w]  if hukou != "Agricultural"
display 1.3 * 0.4722
psacalc delta treatment, rmax(.61386)
*delta        |      3.375

regress eng_pct treatment trad_eng [pweight=w]  if hukou != "Agricultural"
display 1.3 * 0.4092
psacalc delta treatment, rmax(.53196)
*delta        |      24.84

*Table 4 SES
regress chn_pct treatment trad_chn [pweight=w]  if poor == 1
display 1.3 * 0.4035
psacalc delta treatment, rmax(.52455)
*delta        |      9.17

regress mat_pct treatment trad_mat [pweight=w]  if poor == 1
display 1.3 * 0.399
psacalc delta treatment, rmax(.60034)
*delta        |      32.35

regress eng_pct treatment trad_eng [pweight=w]  if poor == 1
display 1.3 * 0.3987
psacalc delta treatment, rmax(.51831)
*delta        |      1.804

regress chn_pct treatment trad_chn [pweight=w]  if moderate == 1
display 1.3 * 0.3337
psacalc delta treatment, rmax(.43381)
*delta        |      -156.51369

regress mat_pct treatment trad_mat [pweight=w]  if moderate == 1
display 1.3 * 0.4672
psacalc delta treatment, rmax(.60736)
*delta        |      32.35

regress eng_pct treatment trad_eng [pweight=w]  if moderate == 1
display 1.3 * 0.4108
psacalc delta treatment, rmax(.53404)
*delta        |      31.783

regress chn_pct treatment trad_chn [pweight=w]  if rich == 1
display 1.3 * 0.2873
psacalc delta treatment, rmax(.37349)
*delta        |      -6.257

regress mat_pct treatment trad_mat [pweight=w]  if rich == 1
display 1.3 * 0.4850
psacalc delta treatment, rmax(.6305)
*delta        |      -0.664

regress eng_pct treatment trad_eng [pweight=w]  if rich == 1
display 1.3 * 0.4246
psacalc delta treatment, rmax(.55198)
*delta        |      -9.72645

*Table 5 LB SES PHUDCFILY
regress chn_pct treatment trad_chn [pweight=w]  if left == 1
display 1.3 * 0.2689
psacalc delta treatment, rmax(.349)
*delta        |      18.621

regress mat_pct treatment trad_mat [pweight=w]  if left == 1
display 1.3 * 0.4545
psacalc delta treatment, rmax(.59085)
*delta        |      32.35

regress eng_pct treatment trad_eng [pweight=w]  if left == 1
display 1.3 * 0.3685
psacalc delta treatment, rmax(.47905)
*delta        |      1.804

regress chn_pct treatment trad_chn [pweight=w]  if left == 0 & hukou == "Agricultural"
display 1.3 * 0.3932
psacalc delta treatment, rmax(.51116)
*delta        |      -49.925

regress mat_pct treatment trad_mat [pweight=w]  if left == 0 & hukou == "Agricultural"
display 1.3 * 0.4745
psacalc delta treatment, rmax(.61685)
*delta        |      25.922

regress eng_pct treatment trad_eng [pweight=w]  if left == 0 & hukou == "Agricultural"
display 1.3 * 0.441
psacalc delta treatment, rmax(.5733)
*delta        |      1.804

*Table 6 LB among low SES PHUDCFILY
regress chn_pct treatment trad_chn [pweight=w]  if left == 1 & poor == 1
display 1.3 * 0.2378
psacalc delta treatment, rmax(.30914)
*delta        |      4.65

regress mat_pct treatment trad_mat [pweight=w]  if left == 1 & poor == 1
display 1.3 * 0.4169
psacalc delta treatment, rmax(.54197)
*delta        |      -248.645

regress eng_pct treatment trad_eng [pweight=w]  if left == 1 & poor == 1
display 1.3 * 0.350
psacalc delta treatment, rmax(.455)
*delta        |      1.804

regress chn_pct treatment trad_chn [pweight=w]  if left == 0 & hukou == "Agricultural" & poor == 1
display 1.3 * 0.471
psacalc delta treatment, rmax(.6123)
*delta        |      10.265

regress mat_pct treatment trad_mat [pweight=w]  if left == 0 & hukou == "Agricultural" & poor == 1
display 1.3 * 0.4824
psacalc delta treatment, rmax(.627)
*delta        |      7.594

regress eng_pct treatment trad_eng [pweight=w]  if left == 0 & hukou == "Agricultural" & poor == 1
display 1.3 * 0.428
psacalc delta treatment, rmax(.5564)
*delta        |       2.341

regress chn_pct treatment trad_chn [pweight=w]  if left == 0 & hukou != "Agricultural" & poor == 1
display 1.3 * 0.5643
psacalc delta treatment, rmax(.73359)
*delta        |      -54.28239

regress mat_pct treatment trad_mat [pweight=w]  if left == 0 & hukou != "Agricultural" & poor == 1
display 1.3 * 0.5161
psacalc delta treatment, rmax(.67093)
*delta        |       17.00359

regress eng_pct treatment trad_eng [pweight=w]  if left == 0 & hukou != "Agricultural" & poor == 1
display 1.3 * 0.448
psacalc delta treatment, rmax(.582)
*delta        |       0.96798
