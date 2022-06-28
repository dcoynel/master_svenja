# Predict FC from SC

############################ Read in data ###################################################

setwd("C:/Users/svenj/Documents/Uni Basel/Master/Master Thesis/SC-FC")
library(data.table)
library(tidyverse)
library(dplyr)

# Read name of the ROIs
ROI_names<- read.table("1449_parcel_L2008-scale250_cog.txt")

n=6 # number of subjects

######### This is the TRAIN set

# Read SC Matrix
sc_mat<- read.csv("1449_prob_pft_ants_25npv_5-200_COMPRESS_0.2__freesurfer6.0.csv", h=T, sep = ',', row.names = 1)

# Read functional Residuals
f_residuals <- read.table("sub-1449_ROIv_scale250_data-residuals.txt")

# Read SC Matrix for more subjects
sc_mat_a<- read.csv("1449_prob_pft_ants_25npv_5-200_COMPRESS_0.2__freesurfer6.0.csv", h=T, sep = ',', row.names = 1)
sc_mat_b<- read.csv("1449_prob_pft_ants_25npv_5-200_COMPRESS_0.2__freesurfer6.0.csv", h=T, sep = ',', row.names = 1)
sc_mat_c<- read.csv("1449_prob_pft_ants_25npv_5-200_COMPRESS_0.2__freesurfer6.0.csv", h=T, sep = ',', row.names = 1)
sc_mat_d<- read.csv("1449_prob_pft_ants_25npv_5-200_COMPRESS_0.2__freesurfer6.0.csv", h=T, sep = ',', row.names = 1)
sc_mat_e<- read.csv("1449_prob_pft_ants_25npv_5-200_COMPRESS_0.2__freesurfer6.0.csv", h=T, sep = ',', row.names = 1)

# read functional residuals for more subjects
f_residuals_a <- read.table("sub-1449_ROIv_scale250_data-residuals.txt")
f_residuals_b <- read.table("sub-1449_ROIv_scale250_data-residuals.txt")
f_residuals_c <- read.table("sub-1449_ROIv_scale250_data-residuals.txt")
f_residuals_d <- read.table("sub-1449_ROIv_scale250_data-residuals.txt")
f_residuals_e <- read.table("sub-1449_ROIv_scale250_data-residuals.txt")

################################################ Prepare data ################################################################

# Functional

# Remove last column in f_residuals. Therefore create list and them remove for all subjects
f.list <- list(f_residuals, f_residuals_a, f_residuals_b, f_residuals_c, f_residuals_d, f_residuals_e)
res <- lapply(f.list, function(x) {subset(f_residuals, select= -V463)})

# Now I have all the functional residuals dataframes (6 at the moment) in a list of dataframes calles res

# create connectivity matrix for all dataframes in res
res_fc <- lapply(res, cor)

# transform fc back into one concacenated dataframe (by rows)
con_fc <- as.data.frame(do.call(rbind, res_fc))

# change header of con_fc
colnames(con_fc) <- ROI_names$V2

# add subject column to dataframe
sub<- rep(1:n, each = 462) # n = number of subjects
con_fc$subj <- sub
# put last subj column as first
con_fc <- con_fc[,c(ncol(con_fc),1:(ncol(con_fc)-1))]

#########################################################################

# structural

# # change header of sc_mat
sc.list <- list(sc_mat, sc_mat_a, sc_mat_b, sc_mat_c, sc_mat_d, sc_mat_e)
sc <- lapply(sc.list, function(x) {setnames(x = sc_mat,old = colnames(sc_mat),new = ROI_names$V2)})

# transform list back to one concacenated dataframe (by rows)
con_sc <- as.data.frame(do.call(rbind, sc))

# add subject column to dataframe
sub<- rep(1:n, each = 462)          ##### n=number of subjects
con_sc$subj <- sub
# put last subj column as first
con_sc <- con_sc[,c(ncol(con_sc),1:(ncol(con_sc)-1))]

########################################################################################################################

# read in test data # sc
sc_test<- read.csv("1449_prob_pft_ants_25npv_5-200_COMPRESS_0.2__freesurfer6.0.csv", h=T, sep = ',', row.names = 1)
# change header
setnames(x = sc_test,old = colnames(sc_test),new = ROI_names$V2)

# fc
f_residuals_test <- read.table("sub-1449_ROIv_scale250_data-residuals.txt")
fc_residuals = subset(f_residuals, select = -V463 )

# correlation matrix
fc_test<- cor(fc_residuals)

# 
L.caudalmiddlefrontal_1_sc_test <- sc_test %>% select(L.caudalmiddlefrontal_1)

######################## Model ###################################

# connectivity from ONE seed REGION to every other parcel is calculated (for each individual)
# here for all 6 invididuals
# here choose : L.caudalmiddlefrontal_1

# selct L.caudalmiddlefrontal_1

L.caudalmiddlefrontal_1_fc_vec <- con_fc %>% select(L.caudalmiddlefrontal_1, subj)
L.caudalmiddlefrontal_1_sc_vec <- con_sc %>% select(L.caudalmiddlefrontal_1, subj)
###########################################################################################################
# now I have 2 vectors. FC and SC for region L.caudalmiddlefrontal_1'

# Linear regression model 
# Predicitve coefficients f(x) ; Length 462; Contribution of each target parcel's connectivity in predicting functional connectivity

# linear model
fit <- lm(formula = L.caudalmiddlefrontal_1_fc_vec$L.caudalmiddlefrontal_1 ~ L.caudalmiddlefrontal_1_sc_vec$L.caudalmiddlefrontal_1)
summary(fit)
pred_fc <- predict(fit, sc_test %>% select(L.caudalmiddlefrontal_1))
###### FEHLER 'newdata' hat 462 Zeilen , aber die gefundenen Variablen haben 2772 Zeilen 

# Try to build model on n-1
fit <- lm(formula = L.caudalmiddlefrontal_1_fc_vec$L.caudalmiddlefrontal_1 ~ L.caudalmiddlefrontal_1_sc_vec$L.caudalmiddlefrontal_1,
          data= subset(L.caudalmiddlefrontal_1_sc_vec, L.caudalmiddlefrontal_1_sc_vec$subj!=6 
                       & L.caudalmiddlefrontal_1_fc_vec$subj!=6))

# predict fc for subject 6
pred_fc <-    predict(fit, subset(L.caudalmiddlefrontal_1_sc_vec, L.caudalmiddlefrontal_1_sc_vec$subj==6 ))
# ERROR : 'newdata' hat 462 Zeilen , aber die gefundenen Variablen haben 2772 Zeilen 

# predicted neural response for test subject
pred.fc <- c(pred.fc, fit$coefficients[2] * L.caudalmiddlefrontal_1_sc_test$L.caudalmiddlefrontal_1 + fit$coefficients[1])
View(pred.fc)

# repeat this for every subject

# compare observed and predicted responses