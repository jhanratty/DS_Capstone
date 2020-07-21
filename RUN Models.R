##===============================================================
# MODEL INTITATION AND EXECUTION SCRIPT (RUN THIS FIRST)
# This script 
#    - Sets up the test environmen from scratch (libraies, etc)
#    - loads the edx data set
#    - Runs Model_Scripts R script to load the model code 
#    - Partitions the data set into test_set_raw and train_set_raw
#    - Creates train and test test sets
#    - Runs the mode against the test sets
#    - Graphs the results
# This script is meant to build run from a clean R environment
# ENJOY
##===============================================================
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(dplyr))     install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(stringr))   install.packages("stringr", repos = "http://cran.us.r-project.org")
if(!require(caret))     install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(ggplot2))   install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(scales))    install.packages("scales", repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")
if(!require(magrittr))  install.packages("magrittr", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(dplyr)
library(stringr)
library(caret)
library(lubridate)
library(ggplot2)
library(scales)
library(gridExtra)
library(magrittr)

options(digits=6)

# GET MODEL FUNTIONS FOR ANALYSIS
if(!file.exists("Model_Functions_Scripts.R")) {
  print("Working Directory not set correctly.  Cannot find Model_Functions_Scripts.R")
  print(getwd())
  print(list.files())
}
source("Model_Functions_Scripts.R")

# GET DATA SETS FOR TESTING AND ANALYSIS
if(file.exists("_edx_dataset") & file.exists("_val_dataset")) {
  print("EDX USING _edx_dataset AND _val_datase IN WORKING DIRECTORY")
  edx <- readRDS("_edx_dataset")
  validation <- readRDS("_val_dataset")
} else {
  print("DOWNLOADING EDX DATASET ")
  edx <- getEdx()
}
##===============================================================
# CREATE TEST AND TRAIN DATA SETS FOR ANALYSIS
##===============================================================
set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.1, list = FALSE)
train_set_raw <- edx[-test_index,]
test_set_raw <- edx[test_index,]
train_set <- edx[-test_index,]
test_set <- edx[test_index,]

##===============================================================
##  TEST SET ANALYSIS SCRIPTS
##===============================================================
# THIS FUNTION RUNS THE MODE WITH THE SPECIFIED SDATA SETS
# 
Run_Model <- function( TRAIN_DAT = train_set,  # the training data set
                       TEST_DAT = test_set,    # the test data set
                       TITLE = "RSME TEST") # string that prints with out put to identify the results
  {  
  options(digits=5)
  in_y <- PackPipe(data.frame(pred = TRAIN_DAT$rating, residual = TRAIN_DAT$rating), 
                 data.frame(pred = TEST_DAT$rating, residual = TEST_DAT$rating))
  pipe_outr <- Ratings_Model(in_y, TRAIN_DAT, TEST_DAT)
  test_yr <- UnpackPipe(pipe_outr, 2)
  pipe_outgro <- Ratings_Model(in_y, TRAIN_DAT, TEST_DAT) %>% Genre_Model(TRAIN_DAT, TEST_DAT)  #CALLS ANALYSIS FUNCTIONS PR2_U_M_v1 & GENRE_FACTOR_ave_v1()
  test_ygro <- UnpackPipe(pipe_outgro, 2)
  # address the handful of values <0 or >5
  test_out <- fast_oob(test_ygro$pred + test_yr$pred) 
  # Bucketize Results
  test_tt <- round(2*test_out)/2
  print(TITLE)
  print(paste("RMSE: ",RMSE(test_tt, TEST_DAT$rating)))
  test_tt
}

##===============================================================
# THIS FUNTION GRAPHS THE RESULST OF Run_Model
# 
Graph_Model <- function(DATA1,                    # Output of model (Predictions)
                        DATA2 = test_set$rating,  # Reference set (Actual Ratings)
                        GTITLE = "Rating Model Performance",
                        SUBTITLE1 = "", 
                        SUBTITLE2 = "" ) {
text_size <- 12
options(digits=6)
hcount <- max(sum(DATA1 == 4), max(DATA1 == 3.5), max(DATA2 == 4), max(DATA2==3.5))
rmse_label = paste("RMSE = ", round(RMSE(DATA1, DATA2), 4))
data.frame(Model = DATA1, Actual = DATA2) %>% 
  gather(Data_Source, Rating, Model:Actual)%>% 
  ggplot(aes(x=Rating, fill=Data_Source)) + 
  geom_bar(position = position_dodge(width=0.3)) +
  scale_y_continuous(label = unit_format(unit = "k", scale = 1e-3)) +
  ggtitle(GTITLE) +
  annotate("text", label = SUBTITLE1,  x = 0.5, y = round(.8*hcount), fontface = 1, size = 4, hjust = 0) +
  annotate("text", label = SUBTITLE2,  x = 0.5, y = round(.7*hcount), fontface = 1, size = 4, hjust = 0) +
  annotate("text", label = rmse_label, x = 0.5, y = round(.5*hcount), fontface = 2, size = 4, hjust = 0) +
  xlab("Rating") +
  ylab("Review Count") +
  labs(fill = "Results") +
  theme(text = element_text(size = text_size + 2)) +        # all text
  theme(plot.title = element_text(size = text_size + 4)) +
  theme(axis.title = element_text(size = text_size + 2)) +
  theme(axis.text = element_text(size = text_size)) +
  theme(legend.text = element_text(size = text_size)) +
  theme(legend.title = element_text(size = text_size + 2)) 
}

##===============================================================
## PRE-PROCESS DATA SETS FOR TESTING
## Fixes dates, extracts release date, creates some fields
##===============================================================
test_set <- Pre_process(test_set_raw)
train_set <- Pre_process(train_set_raw)
edx_test_set <-    Pre_process(test_set_raw)
edx_train_set <-   Pre_process(train_set_raw)
edx_test_gt02 <-   edx_test_set %>% filter(reviewed > 2002)
edx_train_gt02 <- edx_train_set %>% filter(reviewed > 2002)
val_test_set <-    Pre_process(validation)

##===============================================================
## RUN MODEL AGAINST DATA SETES AND GRAPH RESULTS
##===============================================================
gr_edx <- Graph_Model(dat_edx, 
                         edx_test_set$rating,
                         "Rating Model Performance",
                         "Train:     EDX Train Set",
                         "Reference: EDX Test Set")

dat_val <-Run_Model(edx_train_set, val_test_set, "EDX Train (all) & Validation (all) Sets")
gr_val <- Graph_Model(dat_val, 
                         val_test_set$rating,
                         "Rating Model Performance",
                         "Train:     EDX Train Set",
                         "Reference: Validation Set ")

dat_gt02edx <-Run_Model(edx_train_gt02, edx_test_gt02, "EDX Train (>2002)  & Test Sets (>2002) ")
gr_gt02edx <- Graph_Model(dat_gt02edx, 
                             edx_test_gt02$rating,
                             "Rating Model Performance",
                             "Train:     EDX Post-2002 ",
                             "Reference: EDX Post-2002 ")

# dat_gt02val <-Run_Model(edx_train_gt02, val_test_set, "EDX Train (>2002)  & Validation (All) Sets")
# gr_gt02val <- Graph_Model(dat_gt02val, 
#                             val_test_set$rating,
#                             "Rating Model Performance",
#                             "Train:     EDX (post-2002)",
#                             "Reference: Validation Set  ")

gr_edx
gr_val
gr_gt02edx
# gr_gt02val
