install.packages("ggplot2")
install.packages("data.table")
install.packages("openxlsx")
install.packages("readxl")
install.packages("dplyr")
install.packages("tidyverse")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("tidyr")
install.packages("ggpubr")
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("tibbletime")
install.packages("viridis")
install.packages("corrplot")
install.packages("PerformanceAnalytics")
install.packages("RColorBrewer")
install.packages("broom")           
install.packages("palmerpenguins")
install.packages("sandwich")
install.packages("lmtest")
install.packages("wooldridge")
install.packages("nullabor")
install.packages("ggthemes")
install.packages("lme4")
install.packages("jtools")
install.packages("psych")
install.packages("ACSWR")
install.packages("Matrix")
install.packages("stargazer")
install.packages("ggExtra")
install.packages("ggridges")
install.packages("ggplot2")
install.packages("viridis")
install.packages("hrbrthemes")
install.packages("ggridges")
install.packages("GGally")
install.packages("VGAM")
install.packages("ISLR")
install.packages("ggcorrplot")
install.packages("stats4")
install.packages("splines")
install.packages("htobit")
install.packages("crch")
install.packages("car")
install.packages("glmx")
install.packages("truncreg")
install.packages("boot")
install.packages("foreign")
install.packages("olsrr")
install.packages("gvlma")
install.packages("caret")
install.packages("AICcmodavg")
install.packages("ggstream")
install.packages("scales")
install.packages("broom.helpers")
install.packages("mapproj")
install.packages("GGally")
install.packages("broom.helpers")
install.packages("RSQLite")


library(broom.helpers)
library(ggplot2)
library(data.table)
library(openxlsx)
library(readxl)
library(dplyr)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(tidyr)
library(ggpubr)
library(tidyverse)
library(ggplot2)
library(tibbletime)
library(viridis)
library(corrplot)
library(PerformanceAnalytics)
library(RColorBrewer)
library(broom)           
library(palmerpenguins)
library(sandwich)
library(lmtest)
library(wooldridge)
library(nullabor)
library(ggthemes)
library(lme4)
library(jtools)
library(psych)
library(ACSWR)
library(Matrix)
library(stargazer)
library(ggExtra)
library(ggridges)
library(ggplot2)
library(viridis)
library(hrbrthemes)
library(ggridges)
library(GGally)
library(VGAM)
library(ISLR)
library(ggcorrplot)
library(stats4)
library(splines)
library(htobit)
library(crch)
library(car)
library(glmx)
library(truncreg)
library(boot)
library(foreign)
library(gvlma)
library(olsrr)
library(caret)
library(AICcmodavg)
library(ggstream)
library(scales)
library(broom)
library(GGally)
library(RSQLite)

dev.off()
#Set up the working directory#
setwd("D://FIA_Data")

#To stop R from using scientific notation+
options(scipen=999)
options(digits=15)


##======================================================================================#
#Connecting to the database
#Specify driver connection
setwd("D://FIA_Data")
sqlite.driver <- dbDriver("SQLite")

#Read the database file. The state code at the end will change for different states (ie, database for Florida would be FIADB_GA.db)
FIA_GA <- "FIADB_GA.db"
FIA_LA <- "FIADB_LA.db"
FIA_FL <- "FIADB_FL.db"
FIA_MS <- "FIADB_MS.db"
FIA_AL <- "FIADB_AL.db"

#Create connection between R and the database
GA_db <- dbConnect(sqlite.driver,
                   dbname = FIA_GA)
LA_db <- dbConnect(sqlite.driver,
                   dbname = FIA_LA)
FL_db <- dbConnect(sqlite.driver,
                   dbname = FIA_FL)
MS_db <- dbConnect(sqlite.driver,
                   dbname = FIA_MS)
AL_db <- dbConnect(sqlite.driver,
                   dbname = FIA_AL)


#Extract and view the plot table from the FIA Database 
plot_table_ga <- dbReadTable(GA_db,"PLOT")
View(plot_table_ga)
plot_table_la <- dbReadTable(LA_db,"PLOT")
View(plot_table_la)
plot_table_fl <- dbReadTable(FL_db,"PLOT")
View(plot_table_fl)
plot_table_ms <- dbReadTable(MS_db,"PLOT")
View(plot_table_ms)
plot_table_al <- dbReadTable(AL_db,"PLOT")
View(plot_table_al)
#Extract and view the condition table from the FIA Database 
condition_table_ga <- dbReadTable(GA_db,"COND")
View(condition_table_ga)
condition_table_la <- dbReadTable(LA_db,"COND")
View(condition_table_la)
condition_table_fl <- dbReadTable(FL_db,"COND")
View(condition_table_fl)
condition_table_ms <- dbReadTable(MS_db,"COND")
View(condition_table_ms)
condition_table_al <- dbReadTable(AL_db,"COND")
View(condition_table_al)
#Extract and view the tree table from the FIA Database 
tree_table_al <- dbReadTable(AL_db,"TREE")
View(plot_table_al)
colnames(tree_data_al)
tree_table_la <- dbReadTable(LA_db,"TREE")
View(tree_table_la)
tree_table_ga <- dbReadTable(GA_db,"TREE")
View(tree_table_ga)
tree_table_fl <- dbReadTable(FL_db,"TREE")
View(tree_table_fl)
tree_table_ms <- dbReadTable(MS_db,"TREE")
View(tree_table_ms)
#Extract and view the population table from the FIA Database 
population_table_al <- dbReadTable(AL_db,"POP_STRATUM")
View(population_table_al)
population_table_la <- dbReadTable(LA_db,"POP_STRATUM")
View(population_table_la)
population_table_ga <- dbReadTable(GA_db,"POP_STRATUM")
View(population_table_ga)
population_table_fl <- dbReadTable(FL_db,"POP_STRATUM")
View(population_table_fl)
population_table_ms <- dbReadTable(MS_db,"POP_STRATUM")
View(population_table_ms)  
#Extract and view the population EVALUATION table from the FIA Database 
pop_eval_table_al <- dbReadTable(AL_db,"POP_EVAL")
View(pop_eval_table_al)
pop_eval_table_la <- dbReadTable(LA_db,"POP_EVAL")
View(pop_eval_table_la)
pop_eval_table_ga <- dbReadTable(GA_db,"POP_EVAL")
View(pop_eval_table_ga)
pop_eval_table_fl <- dbReadTable(FL_db,"POP_EVAL")
View(pop_eval_table_fl)
pop_eval_table_ms <- dbReadTable(MS_db,"POP_EVAL")
View(pop_eval_table_ms) 
#Extract and view the Population Plot Stratum Assignment table from the FIA Database 
POP_PLOT_STRATUM_ASSGN_table_al <- dbReadTable(AL_db,"POP_PLOT_STRATUM_ASSGN")
View(POP_PLOT_STRATUM_ASSGN_table_al)
POP_PLOT_STRATUM_ASSGN_table_la <- dbReadTable(LA_db,"POP_PLOT_STRATUM_ASSGN")
View(POP_PLOT_STRATUM_ASSGN_table_la)
POP_PLOT_STRATUM_ASSGN_table_ga <- dbReadTable(GA_db,"POP_PLOT_STRATUM_ASSGN")
View(POP_PLOT_STRATUM_ASSGN_table_ga)
POP_PLOT_STRATUM_ASSGN_table_fl <- dbReadTable(FL_db,"POP_PLOT_STRATUM_ASSGN")
View(POP_PLOT_STRATUM_ASSGN_table_fl)
POP_PLOT_STRATUM_ASSGN_table_ms <- dbReadTable(MS_db,"POP_PLOT_STRATUM_ASSGN")
View(POP_PLOT_STRATUM_ASSGN_table_ms)
#Extract and view the Reference Species table from the FIA Database 
REF_SPECIES_table_al <- dbReadTable(AL_db,"REF_SPECIES")
View(REF_SPECIES_table_al)
REF_SPECIES_table_la <- dbReadTable(LA_db,"REF_SPECIES")
View(REF_SPECIES_table_la)
REF_SPECIES_table_ga <- dbReadTable(GA_db,"REF_SPECIES")
View(REF_SPECIES_table_ga)
REF_SPECIES_table_fl <- dbReadTable(FL_db,"REF_SPECIES")
View(REF_SPECIES_table_fl)
REF_SPECIES_table_ms <- dbReadTable(MS_db,"REF_SPECIES")
View(REF_SPECIES_table_ms)

#Extract 
pop_estn_unit_ga <- dbReadTable(GA_db, "POP_ESTN_UNIT")
View(pop_estn_unit_ga)
pop_estn_unit_fl <- dbReadTable(FL_db, "POP_ESTN_UNIT")
pop_estn_unit_la <- dbReadTable(LA_db, "POP_ESTN_UNIT")
pop_estn_unit_al <- dbReadTable(AL_db, "POP_ESTN_UNIT")
pop_estn_unit_ms <- dbReadTable(MS_db, "POP_ESTN_UNIT")

pop_eval_grp_ga <- dbReadTable(GA_db, "POP_EVAL_GRP")
pop_eval_grp_fl <- dbReadTable(FL_db, "POP_EVAL_GRP")
pop_eval_grp_la <- dbReadTable(LA_db, "POP_EVAL_GRP")
pop_eval_grp_al <- dbReadTable(AL_db, "POP_EVAL_GRP")
pop_eval_grp_ms <- dbReadTable(MS_db, "POP_EVAL_GRP")

#Export the tree tables in the local drive 
write.csv(plot_table_ga, "D:\\FIA_Data\\plot_ga.csv", row.names = FALSE)
write.csv(plot_table_al, "D:\\FIA_Data\\plot_al.csv", row.names = FALSE)
write.csv(plot_table_fl, "D:\\FIA_Data\\plot_fl.csv", row.names = FALSE)
write.csv(plot_table_la, "D:\\FIA_Data\\plot_la.csv", row.names = FALSE)
write.csv(plot_table_ms, "D:\\FIA_Data\\plot_ms.csv", row.names = FALSE)

#Export the tree tables in the local drive 
write.csv(tree_table_ga, "D:\\FIA_Data\\tree_ga.csv", row.names = FALSE)
write.csv(tree_table_al, "D:\\FIA_Data\\tree_al.csv", row.names = FALSE)
write.csv(tree_table_fl, "D:\\FIA_Data\\tree_fl.csv", row.names = FALSE)
write.csv(tree_table_la, "D:\\FIA_Data\\tree_la.csv", row.names = FALSE)
write.csv(tree_table_ms, "D:\\FIA_Data\\tree_ms.csv", row.names = FALSE)
#Export the population tables in the local drive 
write.csv(population_table_ga, "D:\\FIA_Data\\population_ga.csv", row.names = FALSE)
write.csv(population_table_al, "D:\\FIA_Data\\population_al.csv", row.names = FALSE)
write.csv(population_table_fl, "D:\\FIA_Data\\population_fl.csv", row.names = FALSE)
write.csv(population_table_la, "D:\\FIA_Data\\population_la.csv", row.names = FALSE)
write.csv(population_table_ms, "D:\\FIA_Data\\population_ms.csv", row.names = FALSE)
#Export the population evaluation tables in the local drive 
write.csv(pop_eval_table_ga, "D:\\FIA_Data\\pop_eval_ga.csv", row.names = FALSE)
write.csv(pop_eval_table_al, "D:\\FIA_Data\\pop_eval_al.csv", row.names = FALSE)
write.csv(pop_eval_table_fl, "D:\\FIA_Data\\pop_eval_fl.csv", row.names = FALSE)
write.csv(pop_eval_table_la, "D:\\FIA_Data\\pop_eval_la.csv", row.names = FALSE)
write.csv(pop_eval_table_ms, "D:\\FIA_Data\\pop_eval_ms.csv", row.names = FALSE)
#Export the Population Plot Stratum Assignment tables in the local drive 
write.csv(POP_PLOT_STRATUM_ASSGN_table_ga, "D:\\FIA_Data\\POP_PLOT_STRATUM_ASSGN_ga.csv", row.names = FALSE)
write.csv(POP_PLOT_STRATUM_ASSGN_table_al, "D:\\FIA_Data\\POP_PLOT_STRATUM_ASSGN_al.csv", row.names = FALSE)
write.csv(POP_PLOT_STRATUM_ASSGN_table_fl, "D:\\FIA_Data\\POP_PLOT_STRATUM_ASSGN_fl.csv", row.names = FALSE)
write.csv(POP_PLOT_STRATUM_ASSGN_table_la, "D:\\FIA_Data\\POP_PLOT_STRATUM_ASSGN_la.csv", row.names = FALSE)
write.csv(POP_PLOT_STRATUM_ASSGN_table_ms, "D:\\FIA_Data\\POP_PLOT_STRATUM_ASSGN_ms.csv", row.names = FALSE)
#Export the Reference Species tables in the local drive 
write.csv(REF_SPECIES_table_ga, "D:\\FIA_Data\\REF_SPECIES_ga.csv", row.names = FALSE)
write.csv(REF_SPECIES_table_al, "D:\\FIA_Data\\REF_SPECIES_al.csv", row.names = FALSE)
write.csv(REF_SPECIES_table_fl, "D:\\FIA_Data\\REF_SPECIES_fl.csv", row.names = FALSE)
write.csv(REF_SPECIES_table_la, "D:\\FIA_Data\\REF_SPECIES_la.csv", row.names = FALSE)
write.csv(REF_SPECIES_table_ms, "D:\\FIA_Data\\REF_SPECIES_ms.csv", row.names = FALSE)
#Export the population tables in the local drive 
write.csv(population_table_ga, "D:\\FIA_Data\\pop_stratum_ga.csv", row.names = FALSE)
write.csv(population_table_al, "D:\\FIA_Data\\pop_stratum_al.csv", row.names = FALSE)
write.csv(population_table_fl, "D:\\FIA_Data\\pop_stratum_fl.csv", row.names = FALSE)
write.csv(population_table_la, "D:\\FIA_Data\\pop_stratum_la.csv", row.names = FALSE)
write.csv(population_table_ms, "D:\\FIA_Data\\pop_stratum_ms.csv", row.names = FALSE)
#Export population estimation unit in the local drive
write.csv(pop_estn_unit_ga, "D:\\FIA_Data\\pop_est_unit_ga.csv", row.names = FALSE)
write.csv(pop_estn_unit_la, "D:\\FIA_Data\\pop_est_unit_la.csv", row.names = FALSE)
write.csv(pop_estn_unit_fl, "D:\\FIA_Data\\pop_est_unit_fl.csv", row.names = FALSE)
write.csv(pop_estn_unit_al, "D:\\FIA_Data\\pop_est_unit_al.csv", row.names = FALSE)
write.csv(pop_estn_unit_ms, "D:\\FIA_Data\\pop_est_unit_ms.csv", row.names = FALSE)

write.csv(pop_eval_grp_ga, "D:\\FIA_Data\\pop_eval_grp_ga.CSV", row.names = FALSE)
write.csv(pop_eval_grp_la, "D:\\FIA_Data\\pop_eval_grp_la.CSV", row.names = FALSE)
write.csv(pop_eval_grp_al, "D:\\FIA_Data\\pop_eval_grp_al.CSV", row.names = FALSE)
write.csv(pop_eval_grp_fl, "D:\\FIA_Data\\pop_eval_grp_fl.CSV", row.names = FALSE)
write.csv(pop_eval_grp_ms, "D:\\FIA_Data\\pop_eval_grp_ms.CSV", row.names = FALSE)

#==============================0===================================================================##
# Merging PLOT table to COND table with the variable PLT_CN in COND table 
plot_condition_ga <-merge(x=plot_table_ga,y=condition_table_ga, by.x="CN", by.y="PLT_CN")
plot_condition_la <-merge(x=plot_table_la,y=condition_table_la, by.x="CN", by.y="PLT_CN")
plot_condition_fl <-merge(x=plot_table_fl,y=condition_table_fl, by.x="CN", by.y="PLT_CN")
plot_condition_ms <-merge(x=plot_table_ms,y=condition_table_ms, by.x="CN", by.y="PLT_CN")
plot_condition_al <-merge(x=plot_table_al,y=condition_table_al, by.x="CN", by.y="PLT_CN")
View(plot_condition_ga)
View(plot_condition_la)
View(plot_condition_fl)
View(plot_condition_ms)
View(plot_condition_al)

#View the column names in the console 
colnames(plot_condition_ga)
colnames(plot_condition_la)
colnames(plot_condition_fl)
colnames(plot_condition_ms)
colnames(plot_condition_al)

#Export CSV files to the drive 
write.csv(plot_condition_ga, "D:\\FIA_Data\\plot_condition_ga.csv", row.names = FALSE)
write.csv(plot_condition_la, "D:\\FIA_Data\\plot_condition_la.csv", row.names = FALSE)
write.csv(plot_condition_fl, "D:\\FIA_Data\\plot_condition_fl.csv", row.names = FALSE)
write.csv(plot_condition_ms, "D:\\FIA_Data\\plot_condition_ms.csv", row.names = FALSE)
write.csv(plot_condition_al, "D:\\FIA_Data\\plot_condition_al.csv", row.names = FALSE)

##============================================================================##
# Join tree table to condition table
plot_tree_ga <-merge(x=plot_table_ga,y=tree_table_ga, by.x="CN", by.y="PLT_CN")
plot_tree_la <-merge(x=plot_table_la,y=tree_table_la, by.x="CN", by.y="PLT_CN")
plot_tree_fl <-merge(x=plot_table_fl,y=tree_table_fl, by.x="CN", by.y="PLT_CN")
plot_tree_ms <-merge(x=plot_table_ms,y=tree_table_ms, by.x="CN", by.y="PLT_CN")
plot_tree_al <-merge(x=plot_table_al,y=tree_table_al, by.x="CN", by.y="PLT_CN")
View(plot_tree_ga)
colnames(plot_tree_ga)
subset_plot_tree_ga <- select(plot_tree_ga, CN, INVYR.x, PLOT.x, STATECD.x, LAT, LON, CONDID, TREE, SUBP, SPCD, SPGRPCD, DIA, ACTUALHT, TPA_UNADJ, TPAMORT_UNADJ, TPAREMV_UNADJ, TPAGROW_UNADJ, DRYBIO_BOLE, DRYBIO_TOP, DRYBIO_STUMP, DRYBIO_SAPLING, DRYBIO_WDLD_SPP, DRYBIO_BG, CARBON_AG, CARBON_BG)
View(subset_plot_tree_ga)
View(plot_tree_la)
View(plot_tree_fl)
View(plot_tree_ms)
View(plot_tree_al)


#============================================================================================================================#
#Access the database from the local disk 
perdido_data_2001_2007 <- read_excel("Perdido_Carbon_Analysis_v2.xlsx", sheet = 'Perdido_Carbon_Analysis_P1') #set the location 
colnames(perdido_data_2001_2007) #to see the column names in the dataset 

perdido_data_2001_2007 <- read_excel("Perdido_Carbon_Analysis_v2.xlsx", sheet = 'PLOT_TimeSeries') #set the location 
colnames(perdido_data_2001_2007) #to see the column names in the dataset 

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
#Run a multiple linear regression model for 2001-2007 period
perdido_lm_2001_2007_WF <- lm(log(CARBON_AG_WT_Foliage) ~ as.character(INVYR)+AFFETED_STATUS+HT+STDSZ_NAME+FLD_AGE+ELEV+PHYSCL_NAME+STAND_TYPE, data = perdido_data_2001_2007)

summary(perdido_lm_2001_2007_WF)
ggcoef_model(perdido_lm_2001_2007_WF)
par(mfrow=c(2,2)) # init 4 charts in 1 panel
plot(perdido_lm_2001_2007_WF)
coeftest(perdido_lm_2001_2007_WF, confint = TRUE, ci.width = .3, vcov = vcovHC(perdido_lm_2001_2007_WF, type = "HC0"))
ggcoef_model(perdido_lm_2001_2007_WF)

perdido_coef_WF <- coeftest(perdido_lm_2001_2007_WF, confint = TRUE, ci.width = .3, vcov = vcovHC(perdido_lm_2001_2007_WF, type = "HC0"))
perdido_coef_WF
BIC(perdido_coef_WF)
ggcoef_model(perdido_coef_WF, facet_row = NULL)+  xlab("estimate")


#Model for ANOVA test (with foliage)
perdido_lm_2001_2007_WF_m2 <- lm(log(CARBON_AG_WT_Foliage) ~ as.character(INVYR)+STDSZ_NAME+AFFETED_STATUS+FLD_AGE++ELEV+PHYSCL_NAME+STAND_TYPE, data = perdido_data_2001_2007)
summary(perdido_lm_2001_2007_WF_m2)
coeftest(perdido_lm_2001_2007_WF_m2, confint = TRUE, ci.width = .3, vcov = vcovHC(perdido_lm_2001_2007_WF, type = "HC0"))
perdido_lm_2001_2007_WF_int <- lm(log(CARBON_AG_WT_Foliage) ~ as.character(INVYR)+STDSZ_NAME+AFFETED_STATUS+HT*FLD_AGE++ELEV+PHYSCL_NAME+STAND_TYPE, data = perdido_data_2001_2007)
summary(perdido_lm_2001_2007_WF_int)
coeftest(perdido_lm_2001_2007_WF_int, confint = TRUE, ci.width = .3, vcov = vcovHC(perdido_lm_2001_2007_WF, type = "HC0"))
ggcoef_model(perdido_lm_2001_2007_WF_int, facet_row = NULL)+  xlab("estimate for trees and foliage")
perdido_lm_2001_2007_WF_blc <- lm(log(CARBON_AG_WT_Foliage) ~ as.character(INVYR)+STDSZ_NAME+AFFETED_STATUS+HT+FLD_AGE++ELEV+PHYSCL_NAME+STAND_TYPE+BASAL_AREA, data = perdido_data_2001_2007)
summary(perdido_lm_2001_2007_WF_blc)
coeftest(perdido_lm_2001_2007_WF_blc, confint = TRUE, ci.width = .3, vcov = vcovHC(perdido_lm_2001_2007_WF, type = "HC0"))


model.set <- list("Model 1" = perdido_lm_2001_2007_WF, "Model 2" = perdido_lm_2001_2007_WF_m2, "Model 3" = perdido_lm_2001_2007_WF_int, "Model 4" = perdido_lm_2001_2007_WF_blc)
ggcoef_compare(model.set, type = "faceted")+ xlab("estimate")
model.names <- c("Model 1", "Model 2", "Model 3", "Model 4")
aictab(model.set, modnames = model.names)
bictab(model.set, modnames = model.names)

lmtest::bptest(perdido_lm_2001_2007_WF)  # Breusch-Pagan test
perdido_box <- caret::BoxCoxTrans(perdido_data_2001_2007$CARBON_AG_WT_Foliage)
print(perdido_box)

perdido_data_2001_2007 <- cbind(perdido_data_2001_2007, box_new = predict(perdido_box,perdido_data_2001_2007$CARBON_AG_WT_Foliage))
head(perdido_data_2001_2007)
perdido_lm_2001_2007_WF_het <- lm(log(box_new) ~ as.character(INVYR)+STDSZ_NAME+AFFETED_STATUS+HT*FLD_AGE++ELEV+PHYSCL_NAME+STAND_TYPE, data = perdido_data_2001_2007)
bptest(perdido_lm_2001_2007_WF_het)
par(mfrow=c(2,2)) # init 4 charts in 1 panel
plot(perdido_lm_2001_2007_WF_het, pch = 5, cex  = 0.1)

# Look at relationship between fitted values and residuals with foliage and trees
ggplot(perdido_lm_2001_2007_WF, aes(x = .fitted, y = .resid)) + 
  geom_point(color = "black", size = 2) +
  geom_smooth(method = "lm", color = "red")+
  xlab("Fitted Values")+
  ylab("Residuals")+
  scale_x_continuous(labels = number_format(accuracy = 0.1))

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
#Run a multiple linear regression model for 2001-2007 period for without foliage only 
perdido_lm_2001_2007_WOF <- lm(log(CARBON_AG_WO_Foliage) ~ as.character(INVYR)+AFFETED_STATUS+HT+STDSZ_NAME+FLD_AGE+ELEV+PHYSCL_NAME+STAND_TYPE, data = perdido_data_2001_2007)
summary(perdido_lm_2001_2007_WOF)
ggcoef(perdido_lm_2001_2007_WOF, color = "red", size =4 , shape = 20)
par(mfrow=c(2,2)) # init 4 charts in 1 panel
plot(perdido_lm_2001_2007_WOF)

# Robust standard errors t test with lm() function 
perdido_coef_WOF <- coeftest(perdido_lm_2001_2007_WOF, confint = TRUE, ci.width = .3, vcov = vcovHC(perdido_lm_2001_2007_WOF, type = "HC0"))
perdido_coef_WOF
par(mfrow=c(2,2)) # init 4 charts in 1 panel
plot(perdido_coef_WOF)
ggcoef_model(perdido_coef_WOF, facet_row = NULL)+  xlab("estimate")
ggcoef(perdido_coef_WOF, color = "purple", vline_color = "red", vline_linetype =  "solid", size =3 , shape = 18)

#Model for ANOVA test (Without Foliage)
perdido_lm_2001_2007_WOF_m2 <- lm(log(CARBON_AG_WO_Foliage) ~ as.character(INVYR)+STDSZ_NAME+AFFETED_STATUS+FLD_AGE++ELEV+PHYSCL_NAME+STAND_TYPE, data = perdido_data_2001_2007)
summary(perdido_lm_2001_2007_WOF_m2)
coeftest(perdido_lm_2001_2007_WOF_m2, confint = TRUE, ci.width = .3, vcov = vcovHC(perdido_lm_2001_2007_WOF, type = "HC0"))
perdido_lm_2001_2007_WOF_int <- lm(log(CARBON_AG_WO_Foliage) ~ as.character(INVYR)+STDSZ_NAME+AFFETED_STATUS+HT*FLD_AGE++ELEV+PHYSCL_NAME+STAND_TYPE, data = perdido_data_2001_2007)
summary(perdido_lm_2001_2007_WOF_int)
coeftest(perdido_lm_2001_2007_WOF_int, confint = TRUE, ci.width = .3, vcov = vcovHC(perdido_lm_2001_2007_WOF, type = "HC0"))
ggcoef_model(perdido_lm_2001_2007_WOF_int, facet_row = NULL)+  xlab("estimate for foliage only")
perdido_lm_2001_2007_WOF_blc <- lm(log(CARBON_AG_WO_Foliage) ~ as.character(INVYR)+STDSZ_NAME+AFFETED_STATUS+HT+FLD_AGE++ELEV+PHYSCL_NAME+STAND_TYPE+BASAL_AREA, data = perdido_data_2001_2007)
summary(perdido_lm_2001_2007_WOF_blc)
coeftest(perdido_lm_2001_2007_WOF_blc, confint = TRUE, ci.width = .3, vcov = vcovHC(perdido_lm_2001_2007_WOF, type = "HC0"))

model.set <- list("Model 1" = perdido_lm_2001_2007_WOF, "Model 2" = perdido_lm_2001_2007_WOF_m2, "Model 3" = perdido_lm_2001_2007_WOF_int, "Model 4" = perdido_lm_2001_2007_WOF_blc)
ggcoef_compare(model.set, type = "faceted")+ xlab("estimate")
model.names <- c("Model 1", "Model 2", "Model 3", "Model 4")
aictab(model.set, modnames = model.names)
bictab(model.set, modnames = model.names)

lmtest::bptest(perdido_lm_2001_2007_WOF)  # Breusch-Pagan test
perdido_box <- caret::BoxCoxTrans(perdido_data_2001_2007$CARBON_AG_WO_Foliage)
print(perdido_box)

perdido_data_2001_2007 <- cbind(perdido_data_2001_2007, box_new = predict(perdido_box,perdido_data_2001_2007$CARBON_AG_WO_Foliage))
head(perdido_data_2001_2007)
perdido_lm_2001_2007_WOF_het <- lm(log(box_new) ~ as.character(INVYR)+STDSZ_NAME+AFFETED_STATUS+HT*FLD_AGE++ELEV+PHYSCL_NAME+STAND_TYPE, data = perdido_data_2001_2007)
bptest(perdido_lm_2001_2007_WOF_het)
par(mfrow=c(2,2)) # init 4 charts in 1 panel
plot(perdido_lm_2001_2007_WOF_het, pch = 5, cex  = 0.1)

# Look at relationship between fitted values and residuals without foliage and trees
ggplot(perdido_lm_2001_2007_WOF, aes(x = .fitted, y = .resid)) + 
  geom_point(color = "black", size = 2) +
  geom_smooth(method = "lm", color = "red")+
  xlab("Fitted Values")+
  ylab("Residuals")+
  scale_x_continuous(labels = number_format(accuracy = 0.1))

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
#Run a multiple linear regression model for 2001-2007 period for foliage only 
perdido_lm_2001_2007_Fol <- lm(log(Foliage) ~ as.character(INVYR)+COUNTY+HT+STDSZ_NAME+AFFETED_STATUS+FLD_AGE++ELEV+PHYSCL_Name+TREE_CLASS, data = perdido_data_2001_2007)
summary(perdido_lm_2001_2007_Fol)
ggcoef(perdido_lm_2001_2007_Fol, color = "red", size =4 , shape = 20)
par(mfrow=c(2,2)) # init 4 charts in 1 panel
plot(perdido_lm_2001_2007_Fol)

# Robust standard errors t test with lm() function 
perdido_coef_Fol <- coeftest(perdido_lm_2001_2007_Fol, confint = TRUE, ci.width = .3, vcov = vcovHC(perdido_lm_2001_2007_Fol, type = "HC0"))
perdido_coef_Fol
par(mfrow=c(2,2)) # init 4 charts in 1 panel
plot(perdido_coef_Fol)
ggcoef_model(perdido_coef_Fol, facet_row = NULL)+  xlab("estimate")
ggcoef(perdido_coef_Fol, color = "blue", vline_color = "red", vline_linetype =  "solid", size =3 , shape = 18)

#Model for ANOVA test (Foliage)
perdido_lm_2001_2007_Fol_m2 <- lm(log(Foliage) ~ as.character(INVYR)+COUNTY+STDSZ_NAME+AFFETED_STATUS+FLD_AGE++ELEV+PHYSCL_Name+TREE_CLASS, data = perdido_data_2001_2007)
summary(perdido_lm_2001_2007_Fol_m2)
coeftest(perdido_lm_2001_2007_Fol_m2, confint = TRUE, ci.width = .3, vcov = vcovHC(perdido_lm_2001_2007_Fol, type = "HC0"))
perdido_lm_2001_2007_Fol_int <- lm(log(Foliage) ~ as.character(INVYR)+COUNTY+STDSZ_NAME+AFFETED_STATUS+HT*FLD_AGE++ELEV+PHYSCL_Name+TREE_CLASS, data = perdido_data_2001_2007)
summary(perdido_lm_2001_2007_Fol_int)
coeftest(perdido_lm_2001_2007_Fol_int, confint = TRUE, ci.width = .3, vcov = vcovHC(perdido_lm_2001_2007_Fol, type = "HC0"))
perdido_lm_2001_2007_Fol_blc <- lm(log(Foliage) ~ as.character(INVYR)+COUNTY+STDSZ_NAME+AFFETED_STATUS+HT+FLD_AGE++ELEV+PHYSCL_Name+TREE_CLASS+BASAL_AREA, data = perdido_data_2001_2007)
summary(perdido_lm_2001_2007_Fol_blc)
coeftest(perdido_lm_2001_2007_Fol_blc, confint = TRUE, ci.width = .3, vcov = vcovHC(perdido_lm_2001_2007_Fol, type = "HC0"))

model.set <- list("Full" = perdido_lm_2001_2007_Fol, "Drop" = perdido_lm_2001_2007_Fol_m2, "Interaction" = perdido_lm_2001_2007_Fol_int, "Block" = perdido_lm_2001_2007_Fol_blc)
ggcoef_compare(model.set, type = "faceted")+ xlab("estimate")
model.names <- c("Original", "Dropping", "Interaction", "Blocking")
aictab(model.set, modnames = model.names)
bictab(model.set, modnames = model.names)

lmtest::bptest(perdido_lm_2001_2007_Fol)  # Breusch-Pagan test
perdido_box <- caret::BoxCoxTrans(perdido_data_2001_2007$CARBON_AG_WO_Foliage)
print(perdido_box)

perdido_data_2001_2007 <- cbind(perdido_data_2001_2007, box_new = predict(perdido_box,perdido_data_2001_2007$CARBON_AG_WO_Foliage))
head(perdido_data_2001_2007)
perdido_lm_2001_2007_Fol_het <- lm(log(box_new) ~ as.character(INVYR)+COUNTY+HT+STDSZ_NAME+AFFETED_STATUS+FLD_AGE++ELEV+PHYSCL_Name+TREE_CLASS, data = perdido_data_2001_2007)
bptest(perdido_lm_2001_2007_Fol_het)
par(mfrow=c(2,2)) # init 4 charts in 1 panel
plot(perdido_lm_2001_2007_Fol_het)

# Look at relationship between fitted values and residuals with foliage 
ggplot(perdido_lm_2001_2007_Fol, aes(x = .fitted, y = .resid)) + 
  geom_point(color = "black", size = 2) +
  geom_smooth(method = "lm", color = "red")+
  xlab("Fitted Values")+
  ylab("Residuals")+
  scale_x_continuous(labels = number_format(accuracy = 0.1))

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#

#Plot
ggplot(data = perdido_data_2001_2007, aes(x=LON,y=LAT,color = CARBON_AG_WT_Foliage)) + geom_point() +
  scale_color_gradient2(high = "red", low = "blue", mid = "white")

perdido_data_2001_2007 <- read.xlsx("Perdido_Carbon_Analysis_v2.xlsx", sheet = 'Perdido_Carbon_Analysis_PLOT') #set the location 
colnames(perdido_data_2001_2007) #to see the column names in the dataset 

#Changes of carbon after the hurricane with foliage 
ggplot(perdido_data_2001_2007, aes(y = CARBON_AG_WT_Foliage, x = Before_After)) +
  geom_boxplot(aes(fill=STDSZ_NAME))+
  xlab("Stand size condition before and after hurricane")+
  ylab("Aboveground Carbon with foliage (tons)")+
  facet_wrap(~TREE_CLASS)+
  theme(legend.position="bottom", axis.text.y = element_text(size = 10), axis.text.x = element_text(size = 10))

#Changes of carbon after the hurricane in foliage 
ggplot(perdido_data_2001_2007, aes(y = Foliage, x = Before_After)) +
  geom_boxplot(aes(fill=STDSZ_NAME))+
  xlab("Stand size condition before and after hurricane")+
  ylab("Aboveground Carbon in foliage (tons)")+
  facet_wrap(~TREE_CLASS)+
  theme(legend.position="bottom", axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12))

#Changes of carbon after the hurricane with foliage considering tree height 
ggplot(perdido_data_2001_2007, aes(y = CARBON_AG_WT_Foliage, x = Height, color = TREE_CLASS)) +
  geom_point(size=2)+
  geom_smooth(method = "lm", color = "blue")+
  xlab("Height (m)")+
  ylab("Aboveground Carbon with foliage (tons)")+
  facet_wrap(~Before_After)+
  theme(legend.position="bottom", axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12))

ggplot(perdido_data_2001_2007, aes(y = Foliage, x = Height, color = TREE_CLASS)) +
  geom_point(size=2)+
  geom_smooth(method = "lm", color = "blue")+
  xlab("Height (m)")+
  ylab("Aboveground Carbon in foliage (tons)")+
  facet_wrap(~Before_After)+
  theme(legend.position="bottom", axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12))

#Changes of carbon after the hurricane with foliage considering physicgraophic zone 
ggplot(perdido_data_2001_2007, aes(fill=PHYSCL_Name, y=CARBON_AG_WT_Foliage, x=Before_After)) + 
  geom_bar(position="fill", stat="identity") +
  scale_fill_viridis(discrete = T) +
  xlab("Physiographic condition before and after hurricane")+
  ylab("% of Aboveground Carbon with foliage (tons)")+
  facet_wrap(~TREE_CLASS)+
  theme(legend.position="right", axis.title.x = element_blank(),
        axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 10))

ggplot(perdido_data_2001_2007, aes(fill=PHYSCL_Name, y=Foliage, x=Before_After)) + 
  geom_bar(position="fill", stat="identity") +
  scale_fill_viridis(discrete = T) +
  xlab("Physiographic condition before and after hurricane")+
  ylab("% of Aboveground Carbon with foliage (tons)")+
  facet_wrap(~TREE_CLASS)+
  theme(legend.position="right", axis.title.x = element_blank(),
        axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 10))


#Changes of carbon after the hurricane with foliage considering field age
ggplot(perdido_data_2001_2007, aes(y = CARBON_AG_WT_Foliage, x = FLD_AGE, color = TREE_CLASS)) +
  geom_point(size=2)+
  geom_smooth(method = "lm", color = "blue")+
  xlab("Stand Age (year)")+
  ylab("Aboveground Carbon with foliage (tons)")+
  facet_wrap(~Before_After)+
  theme(legend.position="bottom", axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12))

ggplot(perdido_data_2001_2007, aes(y = Foliage, x = FLD_AGE, color = TREE_CLASS)) +
  geom_point(size=2)+
  geom_smooth(method = "lm", color = "blue")+
  xlab("Stand Age (year)")+
  ylab("Aboveground Carbon in foliage (tons)")+
  facet_wrap(~Before_After)+
  theme(legend.position="bottom", axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12))

#Changes of carbon after the hurricane with foliage considering elevation 
ggplot(perdido_data_2001_2007, aes(y = CARBON_AG_WT_Foliage, x = Elevation, color = TREE_CLASS)) +
  geom_point(size=2)+
  geom_smooth(method = "lm", color = "blue")+
  xlab("Elevation (m)")+
  ylab("Aboveground Carbon with foliage (tons)")+
  facet_wrap(~Before_After)+
  theme(legend.position="bottom", axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12))

ggplot(perdido_data_2001_2007, aes(y = Foliage, x = Elevation, color = TREE_CLASS)) +
  geom_point(size=2)+
  geom_smooth(method = "lm", color = "blue")+
  xlab("Elevation (m)")+
  ylab("Aboveground Carbon in foliage (tons)")+
  facet_wrap(~Before_After)+
  theme(legend.position="bottom", axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12))


# A few constants
temperatureColor <- "#69b3a2"
priceColor <- rgb(0.2, 0.6, 0.9, 1)

ggplot(perdido_data_2001_2007, aes(x=CARBON_AG_WT_Foliage)) +
  geom_point( aes(y=FLD_AGE), stat="identity", size=2, color= "blue") +
  geom_line( aes(y=Height), size=1, color="red") + 
  xlab("Aboveground Carbon with foliage (tons)")+
  facet_wrap(~Before_After)+
  scale_y_continuous(
    
    # Features of the first axis
    name = "Tree Height (m)",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(trans=~.*1, name="Stand age (year)")
  ) 

ggplot(perdido_data_2001_2007, aes(x=Foliage)) +
  geom_point( aes(y=FLD_AGE), stat="identity", size=2, color= "blue") +
  geom_line( aes(y=Height/3), size=1, color="red") + 
  xlab("Aboveground Carbon in foliage (tons)")+
  facet_wrap(~Before_After)+
  scale_y_continuous(limits = c(0,60),
    
    # Features of the first axis
    name = "Tree Height (m)",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(trans=~.*1, name="Stand age (year)")
  ) 

