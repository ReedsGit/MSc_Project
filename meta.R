######################## meta analysis #######################

library(tidyverse)
library(meta)
library(dmetar)
library(readxl)

meta_data <- read_excel("meta_data.xlsx")
head(meta_data)

#pool memory effect sizes 
m.gen.mem <- metagen(TE=memory,
                 seTE=memory_SE,
                 studlab=First_Author,
                 data=meta_data[1:3,],
                 sm="SMD",
                 comb.fixed=FALSE,
                 comb.random=TRUE,
                 method.tau="REML",
                 hakn=F,        #knapp hartung adjustment
                 prediction=TRUE,
                 title="Meta analysis - Memory")
summary(m.gen.mem)

#pool executive function effect sizes
m.gen.exec <- metagen(TE=executive_func,
                      seTE=executive_func_SE,
                      studlab=First_Author,
                      data=meta_data[1:3,],
                      sm="SMD",
                      comb.fixed=F,
                      comb.random=TRUE,
                      method.tau="REML",
                      hakn=F,       #knapp hartung adjustment
                      prediction=TRUE,
                      title="Meta analysis - Executive Function")
summary(m.gen.exec)

#pool PPCS effect sizes 
m.gen.ppcs <- metagen(TE=ppcs,
                     seTE=ppcs_SE,
                     studlab=First_Author,
                     data=meta_data,
                     sm="SMD",
                     comb.fixed=F,
                     comb.random=TRUE,
                     method.tau="REML",
                     hakn=TRUE,        #knapp hartung adjustment
                     prediction=TRUE,
                     title="Meta analysis - PPCS")
summary(m.gen.ppcs)

######################## forest plots #######################

forest.meta(m.gen.mem,
            sortvar=TE,
            print.tau2=FALSE,
            print.I2.ci = TRUE,
            comb.fixed=FALSE,
            label.right = "Favours treatment",
            xlim=c(-2,3),
            col.diamond = "blue",
            col.square = "light blue",
            col.predict = "blue",
            leftlabs = c("Author", "d", "SE"))


forest.meta(m.gen.exec,
            sortvar=TE,
            print.tau2=FALSE,
            print.I2.ci = TRUE,
            comb.fixed=FALSE,
            label.right = "Favours treatment",
            xlim=c(-2,3),
            col.diamond = "blue",
            col.square = "light blue",
            col.predict = "blue",
            leftlabs = c("Author", "d", "SE"))



forest.meta(m.gen.ppcs,
            sortvar=TE,
            print.tau2=FALSE,
            print.I2 = TRUE,
            comb.fixed=FALSE,
            label.right = "Favours treatment",
            xlim=c(-3,5),
            col.diamond = "blue",
            col.square = "light blue",
            col.predict = "blue",
            leftlabs = c("Author", "d", "SE"))


#Forest plots with RevMan5 layout
forest.meta(m.gen.mem, layout="RevMan5")  
forest.meta(m.gen.exec, layout="RevMan5")
forest.meta(m.gen.ppcs, layout="RevMan5")

########################## Funnel Plots ##################################
col.contour=c("indianred", "orange", "turquoise")

funnel.meta(m.gen.mem,
            xlim=c(0,1.5),
            contour=c(0.9, 0.95, 0.99),
            col.contour=col.contour,
            studlab=F)
legend(x=1.2, y=0.02,
       legend= c("p<0.1", "p<0.05", "p<0.01"),
       fill=col.contour)
title("Funnel Plot (Memory effect)")

funnel.meta(m.gen.exec,
            xlim=c(0,1.5),
            contour=c(0.9, 0.95, 0.99),
            col.contour=col.contour,
            studlab=F)
legend(x=1.2, y=0.02,
       legend= c("p<0.1", "p<0.05", "p<0.01"),
       fill=col.contour)
title("Funnel Plot (Executive function effect)")

funnel.meta(m.gen.ppcs,
            xlim=c(-1,3),
            contour=c(0.9, 0.95, 0.99),
            col.contour=col.contour,
            studlab=F)
legend(x=2, y=0.02,
       legend= c("p<0.1", "p<0.05", "p<0.01"),
       fill=col.contour)
title("Funnel Plot (PPCS effect)")

######################### Risk of bias plots #################################
library(robvis)
str(data_rob2)
bias <- read_excel("Risk_of_Bias.xlsx")
rob_summary(bias, "ROB2")

rob_traffic_light(bias, "ROB2")

########## example plots ###########
bias_example <- read_excel("Risk_of_Bias_example.xlsx")
rob_summary(bias_example, "ROB2")

rob_traffic_light(bias_example, "ROB2")

#################################################################################
###########     Sensitivity Studies.      #################################

######### Boussi-Gross measured from treat HBOT and control pre-HBOT ###########
meta_dataV <- read_excel("meta_data_VAS.xlsx")  

#pool PPCS effect sizes 
m.gen.ppcsV <- metagen(TE=ppcs,
                      seTE=ppcs_SE,
                      studlab=First_Author,
                      data=meta_dataV,
                      sm="SMD",
                      comb.fixed=F,
                      comb.random=TRUE,
                      method.tau="REML",
                      hakn=TRUE,        #knapp hartung adjustment
                      prediction=TRUE,
                      title="Meta analysis - PPCS")
summary(m.gen.ppcsV)

forest.meta(m.gen.ppcsV,
            sortvar=TE,
            print.tau2=FALSE,
            print.I2 = TRUE,
            comb.fixed=FALSE,
            label.right = "Favours treatment",
            xlim=c(-3,5),
            col.diamond = "blue",
            col.square = "light blue",
            col.predict = "blue",
            leftlabs = c("Author", "d", "SE"))

################ Data with no Boussi.  ####################
meta_dataNB <- read_excel("meta_data_NOBoussi.xlsx")  

#pool PPCS effect sizes 
m.gen.ppcsNB <- metagen(TE=ppcs,
                       seTE=ppcs_SE,
                       studlab=First_Author,
                       data=meta_dataNB,
                       sm="SMD",
                       comb.fixed=F,
                       comb.random=TRUE,
                       method.tau="REML",
                       hakn=TRUE,        #knapp hartung adjustment
                       prediction=TRUE,
                       title="Meta analysis - PPCS")
summary(m.gen.ppcsNB)

forest.meta(m.gen.ppcsNB,
            sortvar=TE,
            print.tau2=FALSE,
            print.I2 = TRUE,
            comb.fixed=FALSE,
            label.right = "Favours treatment",
            xlim=c(-3,5),
            col.diamond = "blue",
            col.square = "light blue",
            col.predict = "blue",
            leftlabs = c("Author", "d", "SE"))

######################## New B-G calc and NO Harch ################################
meta_dataNH <- read_excel("meta_data_VAS_NoHar.xlsx") 

#pool PPCS effect sizes 
m.gen.ppcsNH <- metagen(TE=ppcs,
                        seTE=ppcs_SE,
                        studlab=First_Author,
                        data=meta_dataNH,
                        sm="SMD",
                        comb.fixed=F,
                        comb.random=TRUE,
                        method.tau="REML",
                        hakn=TRUE,        #knapp hartung adjustment
                        prediction=TRUE,
                        title="Meta analysis - PPCS")
summary(m.gen.ppcsNH)

forest.meta(m.gen.ppcsNH,
            sortvar=TE,
            print.tau2=FALSE,
            print.I2 = TRUE,
            comb.fixed=FALSE,
            label.right = "Favours treatment",
            xlim=c(-1,1.5),
            col.diamond = "blue",
            col.square = "light blue",
            col.predict = "blue",
            leftlabs = c("Author", "d", "SE"))

################# No Harch or Hadanny ################
meta_dataNHH <- read_excel("meta_data_VAS_NoHarNoHad.xlsx") 

#pool PPCS effect sizes 
m.gen.ppcsNHH <- metagen(TE=ppcs,
                        seTE=ppcs_SE,
                        studlab=First_Author,
                        data=meta_dataNHH,
                        sm="SMD",
                        comb.fixed=F,
                        comb.random=TRUE,
                        method.tau="REML",
                        hakn=TRUE,        #knapp hartung adjustment
                        prediction=TRUE,
                        title="Meta analysis - PPCS")
summary(m.gen.ppcsNHH)

forest.meta(m.gen.ppcsNHH,
            sortvar=TE,
            print.tau2=FALSE,
            print.I2 = TRUE,
            comb.fixed=FALSE,
            label.right = "Favours treatment",
            xlim=c(-1,1.5),
            col.diamond = "blue",
            col.square = "light blue",
            col.predict = "blue",
            leftlabs = c("Author", "d", "SE"))

################# No Hadanny ################
meta_dataNHad <- read_excel("meta_data_VAS_NoHad.xlsx") 

#pool PPCS effect sizes 
m.gen.ppcsNHad <- metagen(TE=ppcs,
                         seTE=ppcs_SE,
                         studlab=First_Author,
                         data=meta_dataNHad,
                         sm="SMD",
                         comb.fixed=F,
                         comb.random=TRUE,
                         method.tau="REML",
                         hakn=TRUE,        #knapp hartung adjustment
                         prediction=TRUE,
                         title="Meta analysis - PPCS")
summary(m.gen.ppcsNHad)

forest.meta(m.gen.ppcsNHad,
            sortvar=TE,
            print.tau2=FALSE,
            print.I2 = TRUE,
            comb.fixed=FALSE,
            label.right = "Favours treatment",
            xlim=c(-1,4),
            col.diamond = "blue",
            col.square = "light blue",
            col.predict = "blue",
            leftlabs = c("Author", "d", "SE"))

################## NNTs. #########################
NNT(d=0.64)

################## Influence diagnostics. #######################
#Look for any outliers
find.outliers(m.gen.ppcs)

# Influence diagnostics
m.gen.ppcs.inf <- InfluenceAnalysis(m.gen.ppcs, random=TRUE, text.scale = 2)
plot(m.gen.ppcs.inf)

plot(m.gen.ppcs.inf, "baujat")
plot(m.gen.ppcs.inf, "influence")
plot(m.gen.ppcs.inf, "es")
plot(m.gen.ppcs.inf, "i2")
