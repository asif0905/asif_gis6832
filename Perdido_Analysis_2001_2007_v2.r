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
library(broom.helpers)
dev.off()
#Set up the working directory#
setwd("D://FIA_Data")

#============================================================================================================================#
#Access the database from the local disk 
perdido_data_2001_2007 <- read.xlsx("Perdido_Carbon_Analysis_v2.xlsx", sheet = 'Perdido_Carbon_Analysis_01_07_P') #set the location 
colnames(perdido_data_2001_2007) #to see the column names in the dataset 

perdido_data_2001_2007 <- read.xlsx("Perdido_Carbon_Analysis_v2.xlsx", sheet = 'PLOT_TimeSeries') #set the location 
colnames(perdido_data_2001_2007) #to see the column names in the dataset 

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
#Run a multiple linear regression model for 2001-2007 period
perdido_lm_2001_2007_WF <- lm(log(CARBON_AG_WT_Foliage) ~ as.character(INVYR)+COUNTY+HT+STDSZ_NAME+AFFETED_STATUS+FLD_AGE+ELEV+PHYSCL_NAME+TREE_CLASS, data = perdido_data_2001_2007)
summary(perdido_lm_2001_2007_WF)
ggcoef_model(perdido_lm_2001_2007_WF, facet_row = NULL)
par(mfrow=c(2,2)) # init 4 charts in 1 panel
plot(perdido_lm_2001_2007_WF)
coeftest(perdido_lm_2001_2007_WF, confint = TRUE, ci.width = .3, vcov = vcovHC(perdido_lm_2001_2007_WF, type = "HC0"))

perdido_coef_WF <- coeftest(perdido_lm_2001_2007_WF, confint = TRUE, ci.width = .3, vcov = vcovHC(perdido_lm_2001_2007_WF, type = "HC0"))
perdido_coef_WF
BIC(perdido_coef_WF)
ggcoef_model(perdido_coef_WF, facet_row = NULL)+  xlab("estimate")
ggcoef(perdido_coef_WF, color = "blue", vline_color = "red", vline_linetype =  "solid", size =3 , shape = 18)

#Model for ANOVA test (with foliage)
perdido_lm_2001_2007_WF_m2 <- lm(log(CARBON_AG_WT_Foliage) ~ as.character(INVYR)+COUNTY+STDSZ_NAME+AFFETED_STATUS+FLD_AGE++ELEV+PHYSCL_Name+TREE_CLASS, data = perdido_data_2001_2007)
summary(perdido_lm_2001_2007_WF_m2)
coeftest(perdido_lm_2001_2007_WF_m2, confint = TRUE, ci.width = .3, vcov = vcovHC(perdido_lm_2001_2007_WF, type = "HC0"))
perdido_lm_2001_2007_WF_int <- lm(log(CARBON_AG_WT_Foliage) ~ as.character(INVYR)+COUNTY+STDSZ_NAME+AFFETED_STATUS+HT*FLD_AGE++ELEV+PHYSCL_Name+TREE_CLASS, data = perdido_data_2001_2007)
summary(perdido_lm_2001_2007_WF_int)
coeftest(perdido_lm_2001_2007_WF_int, confint = TRUE, ci.width = .3, vcov = vcovHC(perdido_lm_2001_2007_WF, type = "HC0"))
perdido_lm_2001_2007_WF_blc <- lm(log(CARBON_AG_WT_Foliage) ~ as.character(INVYR)+COUNTY+STDSZ_NAME+AFFETED_STATUS+HT+FLD_AGE++ELEV+PHYSCL_Name+TREE_CLASS+BASAL_AREA, data = perdido_data_2001_2007)
summary(perdido_lm_2001_2007_WF_blc)
coeftest(perdido_lm_2001_2007_WF_blc, confint = TRUE, ci.width = .3, vcov = vcovHC(perdido_lm_2001_2007_WF, type = "HC0"))


model.set <- list("Full" = perdido_lm_2001_2007_WF, "Drop" = perdido_lm_2001_2007_WF_m2, "Interaction" = perdido_lm_2001_2007_WF_int, "Block" = perdido_lm_2001_2007_WF_blc)
ggcoef_compare(model.set, type = "faceted")+ xlab("estimate")
model.names <- c("Original", "Dropping", "Interaction", "Blocking")
aictab(model.set, modnames = model.names)
bictab(model.set, modnames = model.names)

lmtest::bptest(perdido_lm_2001_2007_WF)  # Breusch-Pagan test
perdido_box <- caret::BoxCoxTrans(perdido_data_2001_2007$CARBON_AG_WT_Foliage)
print(perdido_box)

perdido_data_2001_2007 <- cbind(perdido_data_2001_2007, box_new = predict(perdido_box,perdido_data_2001_2007$CARBON_AG_WT_Foliage))
head(perdido_data_2001_2007)
perdido_lm_2001_2007_WF_het <- lm(log(box_new) ~ as.character(INVYR)+COUNTY+HT+STDSZ_NAME+AFFETED_STATUS+FLD_AGE++ELEV+PHYSCL_Name+TREE_CLASS, data = perdido_data_2001_2007)
bptest(perdido_lm_2001_2007_WF_het)
par(mfrow=c(2,2)) # init 4 charts in 1 panel
plot(perdido_lm_2001_2007_WF_het)

# Look at relationship between fitted values and residuals with foliage and trees
ggplot(perdido_lm_2001_2007_WF, aes(x = .fitted, y = .resid)) + 
  geom_point(color = "black", size = 2) +
  geom_smooth(method = "lm", color = "red")+
  xlab("Fitted Values")+
  ylab("Residuals")+
  scale_x_continuous(labels = number_format(accuracy = 0.1))

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
#Run a multiple linear regression model for 2001-2007 period for without foliage only 
perdido_lm_2001_2007_WOF <- lm(log(CARBON_AG_WO_Foliage) ~ as.character(INVYR)+COUNTY+HT+STDSZ_NAME+AFFETED_STATUS+FLD_AGE++ELEV+PHYSCL_Name+TREE_CLASS, data = perdido_data_2001_2007)
summary(perdido_lm_2001_2007_WOF)
ggcoef(perdido_lm_2001_2007_WOF, color = "red", size =4 , shape = 20)
par(mfrow=c(2,2)) # init 4 charts in 1 panel
plot(perdido_lm_2001_2007_WOF)

# Robust standard errors t test with lm() function 
perdido_coef_WOF <- coeftest(perdido_lm_2001_2007_WOF, confint = TRUE, ci.width = .3, vcov = vcovHC(perdido_lm_2001_2007_WOF, type = "HC0"))
perdido_coef_WOF
par(mfrow=c(2,2)) # init 4 charts in 1 panel
plot(perdido_coef_Fol)
ggcoef_model(perdido_coef_WOF, facet_row = NULL)+  xlab("estimate")
ggcoef(perdido_coef_WOF, color = "purple", vline_color = "red", vline_linetype =  "solid", size =3 , shape = 18)

#Model for ANOVA test (Without Foliage)
perdido_lm_2001_2007_WOF_m2 <- lm(log(CARBON_AG_WO_Foliage) ~ as.character(INVYR)+COUNTY+STDSZ_NAME+AFFETED_STATUS+FLD_AGE++ELEV+PHYSCL_Name+TREE_CLASS, data = perdido_data_2001_2007)
summary(perdido_lm_2001_2007_WOF_m2)
coeftest(perdido_lm_2001_2007_WOF_m2, confint = TRUE, ci.width = .3, vcov = vcovHC(perdido_lm_2001_2007_WOF, type = "HC0"))
perdido_lm_2001_2007_WOF_int <- lm(log(CARBON_AG_WO_Foliage) ~ as.character(INVYR)+COUNTY+STDSZ_NAME+AFFETED_STATUS+HT*FLD_AGE++ELEV+PHYSCL_Name+TREE_CLASS, data = perdido_data_2001_2007)
summary(perdido_lm_2001_2007_WOF_int)
coeftest(perdido_lm_2001_2007_WOF_int, confint = TRUE, ci.width = .3, vcov = vcovHC(perdido_lm_2001_2007_WOF, type = "HC0"))
perdido_lm_2001_2007_WOF_blc <- lm(log(CARBON_AG_WO_Foliage) ~ as.character(INVYR)+COUNTY+STDSZ_NAME+AFFETED_STATUS+HT+FLD_AGE++ELEV+PHYSCL_Name+TREE_CLASS+BASAL_AREA, data = perdido_data_2001_2007)
summary(perdido_lm_2001_2007_WOF_blc)
coeftest(perdido_lm_2001_2007_WOF_blc, confint = TRUE, ci.width = .3, vcov = vcovHC(perdido_lm_2001_2007_WOF, type = "HC0"))

model.set <- list(perdido_lm_2001_2007_WOF, perdido_lm_2001_2007_WOF_m2, perdido_lm_2001_2007_WOF_int, perdido_lm_2001_2007_WOF_blc)
model.names <- c("Original", "Dropping", "Interaction", "Blocking")
aictab(model.set, modnames = model.names)
bictab(model.set, modnames = model.names)

lmtest::bptest(perdido_lm_2001_2007_WOF)  # Breusch-Pagan test
perdido_box <- caret::BoxCoxTrans(perdido_data_2001_2007$CARBON_AG_WO_Foliage)
print(perdido_box)

perdido_data_2001_2007 <- cbind(perdido_data_2001_2007, box_new = predict(perdido_box,perdido_data_2001_2007$CARBON_AG_WO_Foliage))
head(perdido_data_2001_2007)
perdido_lm_2001_2007_WOF_het <- lm(log(box_new) ~ as.character(INVYR)+COUNTY+HT+STDSZ_NAME+AFFETED_STATUS+FLD_AGE++ELEV+PHYSCL_Name+TREE_CLASS, data = perdido_data_2001_2007)
bptest(perdido_lm_2001_2007_WOF_het)
par(mfrow=c(2,2)) # init 4 charts in 1 panel
plot(perdido_lm_2001_2007_WOF_het)

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

