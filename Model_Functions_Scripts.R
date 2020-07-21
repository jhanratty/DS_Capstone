library(tidyverse)
library(dplyr)
library(stringr)
library(caret)
library(lubridate)
library(ggplot2)
library(scales)
library(gridExtra)
library(magrittr)

######################################
# PREPROCESS TRAIN and TEST DATASETS
# Pre_process(dat) 
#   INPUT
#       dat - dataframe in edx format
#   OUTPUT
#       dataframe of processed dataframe
#   FUNCTIONS
#      Extract movie release date, 
#      convert timestamp for lubridate, 
#      add 'age' (how old movie was at review date)


Fix_date <- function(dat) {
  dat %>% 
    mutate(
      timestamp = as_datetime(timestamp), 
      reviewed = year(timestamp), 
      released = as.numeric(str_match(title, "\\(([12][09][0-9][0-9])\\)")[,2]),
      age = ifelse(released <= reviewed, 1, trunc((released - reviewed)/5) + 1)
    )
}

# Add subtotals to data sets
# u_tot_rev = total reviews by user in data_set processed
# m_tot_rev = total reviews of movie in data_set processed
Add_subtotals <- function(dat) {
  u1 <- dat %>% group_by(userId) %>%
    summarize(unum = n(), uave = mean(rating), umed = median(rating))
  m1 <- dat %>%  group_by(movieId) %>%
    summarize(mnum = n(), mave = mean(rating), mmed = median(rating))
  dat %>% left_join(u1, by="userId") %>% left_join(m1, by="movieId")
}

Pre_process <- function(dat) {
  if(is.null(dat$age)) dat %>% Fix_date() %>% Add_subtotals()
}
##### END PREPOCSS #######

##### Output Packing & Unpacking
# Multiplexes output of train and test to be combined for piping to the next prediction phase
# In particular, the predicion and residue valuse for both the test and train sets
# The train and test result dataframes are packed and unpacked in the next phase
# INPUT 
#    fr1 and fr2 are dataframes
# OUTPUT
#    combined dataframe
PackPipe <- function(fr1, fr2) {
  nc <- ifelse(ncol(fr1) < ncol(fr2), ncol(fr1), ncol(fr2))
  ro <- rbind(fr1[1:nc], rep(99999, times=nc))
  rbind(ro, fr2[1:nc])
}
# INPUT 
#    fr1 is the combined dataframe for PackOutput() above
#    f - if f = 1 fr1 extracted, else fr2 is extracted
# OUTPUT
#   dataframe - first or second packed dataframes

UnpackPipe <- function(fr, f = 1 ) {
  nc <- which(fr[1] == 99999)
  #  if(nc == 0) print("UnpackInput: wrong input format")
  e <- nrow(fr)
  ifelse(f == 1, c1 <- (1:(nc-1)), c1 <-((nc+1):e))
  slice(fr, c1)
}

############# END Output Packing & Unpacking ###




#############################
### PREDICTION FUNCTIONS ####
#############################

###### PR1_MoAve Movie Average Ratitn Only
### INPUT
##   in_y - multiplexed (PackPipe()) dataframe pred & residual from previous stage (can pipe)
##   train_dat - train data set (default = train_set)
##   test_dat - test data set for prediction (default = test_set)
## OUTPUT multiplexed data.frame paccked with PackPipe()
##  Channel 1 (train)
##   $pred - prediction
##   $resid = residual after subtracting from rating
##  Channel 2 (test)
##   $pred - prediction
##   $resid = residual after subtracting from rating
## 
PR1_MovAve_v1 <- function(
  in_y = data.frame(pred = train_set$rating), 
  train_dat = train_set, 
  test_dat = test_set)  {
  train_in <- UnpackPipe(in_y, 1)
  test_in <- UnpackPipe(in_y, 2)
  MOVIES <- train_dat %>% group_by(movieId) %>% summarize(m_ave = mean(rating))
  #Find movies not in training set and add them to movie set
  NEW_MOVIES <- anti_join(test_dat, train_dat, by="movieId") %>% group_by(movieId) %>% summarize(m_ave = mean(rating))
  MOVIES <- bind_rows(NEW_MOVIES, MOVIES)
  test_out <-test_dat %>% 
    left_join(MOVIES, by = "movieId") %>% 
    mutate(pred = m_ave, residual = test_in$pred - pred) %>%
    select(pred, residual)
  train_out <- train_dat %>% 
    left_join(MOVIES, by = "movieId") %>% 
    mutate(pred = m_ave, residual = train_in$pred - pred) %>%
    select(pred, residual)
  PackPipe(train_out, test_out)
}


# ======================================================================
# Ratings_Model() Movie Average Rating Adusted By User Rating Bias
# ======================================================================
## INPUT
##   in_y - multiplexed (PackPipe()) dataframe pred & residual from previous stage (can pipe)
##   train_dat - train data set (default = train_set)
##   test_dat - test data set for prediction (default = test_set)
## OUTPUT multiplexed data.frame paccked with PackPipe()
##  Channel 1 (train)
##   $pred - prediction
##   $resid = residual after subtracting from rating
##  Channel 2 (test)
##   $pred - prediction
##   $resid = residual after subtracting from rating
## 
Ratings_Model <- function(in_y, 
                       train_dat = train_set, 
                       test_dat = test_set) {
  
  train_in <- UnpackPipe(in_y, 1)
  test_in <- UnpackPipe(in_y, 2)
  USERS <- train_dat %>% group_by(userId) %>% summarize(u_num = n(), u_ave = mean(rating))
  # USER_ave is the average of all users' average rating
  USER_ave <- mean(USERS$u_ave)
  
  # b_u is the user bias which is how a particular user's average ratins differs 
  #  from the average for all users
  USERS <- USERS %>% mutate(b_u = u_ave - USER_ave)
  MOVIES <- train_dat %>% group_by(movieId) %>% summarize(m_num = n(), m_ave = mean(rating))
  #Find movies not in training set and add them to training set
  NEW_MOVIES <- anti_join(test_dat, train_dat, by="movieId") %>% group_by(movieId) %>% summarize(m_num = n(), m_ave = mean(rating))
  MOVIES <- bind_rows(NEW_MOVIES, MOVIES)
  
  # The prediction (pred) is the {movie ave rating} - {the user's rating bias}  
  test_out1 <- test_dat %>%
    left_join(USERS, by = "userId") %>% 
    left_join(MOVIES, by = "movieId") %>% 
    mutate(pred = m_ave + b_u, residual = rating - (m_ave + b_u)) %>%
    select(pred, residual)
  # residue feed the train logic of the next stage of analysis
  #  residue = {train_set$rating} - {the prediction based ib train_set}
  train_out <- train_dat %>%
    left_join(USERS, by = "userId") %>% 
    left_join(MOVIES, by = "movieId") %>% 
    mutate(pred = m_ave + b_u, residual = rating - (m_ave + b_u)) %>%
    #    filter(!is.na(pred)) %>%
    select(pred, residual)
  # replace NA values with average (this is a small number but causes RMSE to fail)
  pred_ave <-     mean(train_out$pred, na.rm = TRUE)
  residual_ave <- mean(train_out$residual, na.rm = TRUE)
  train_out$pred <-   ifelse(is.na(train_out$pred),   pred_ave, train_out$pred)
  train_out$residual <- ifelse(is.na(train_out$residual), residual_ave, train_out$residual)
  pred_ave <- mean(test_out1$pred, na.rm = TRUE)
  residual_ave <- mean(test_out1$residual, na.rm = TRUE)
  test_out1$pred <-   ifelse(is.na(test_out1$pred),   pred_ave, test_out1$pred)
  test_out1$residual <- ifelse(is.na(test_out1$residual), residual_ave, test_out1$residual)
  
  PackPipe(train_out, test_out1)
}

################# END Ratings_Model ##########


# ======================================================================
# Genre_Model() - User Preferences by Genre
# ======================================================================## INPUT
## INTPUT
##   train_dat - training data (usually test_set)
##   train_y - pred or residual from previous step
##   test_dat - test set data to drive results
## OUTPUT
##   data.frame with 
##   $pred - prediction
##   $resid = residual after subtracting from rating

Genre_Model <- function(in_y, 
                        train_dat = train_set, 
                        test_dat = test_set) {
#  test_dat <- test_set     # quick fix cludge
#  train_dat <- train_set
  
  train_in <- UnpackPipe(in_y, 1)
  test_in <- UnpackPipe(in_y, 2)
  train_dat1 <- cbind(train_dat, train_y = train_in$residual)
  
  # TRAIN DATA expand Genre to one colume per genre
  TRAIN_GENRE <- train_dat1 %>% select(movieId, userId, genres, train_y) %>%
    mutate(  # expand Genre list one column each, TRUE = genre of film, GXa = rating
      G1 = ifelse(str_detect(genres, "Action"),   TRUE, FALSE), G1a  = G1*train_y,
      G2 = ifelse(str_detect(genres,"Adventure"), TRUE, FALSE), G2a  = G2*train_y,
      G3 = ifelse(str_detect(genres,"Animation"), TRUE, FALSE), G3a  = G3*train_y,
      G4 = ifelse(str_detect(genres,"Children"),  TRUE, FALSE), G4a  = G4*train_y,
      G5 = ifelse(str_detect(genres, "Comedy"),   TRUE, FALSE), G5a  = G5*train_y,
      G6 = ifelse(str_detect(genres, "Crime"),    TRUE, FALSE), G6a  = G6*train_y,
      G7 = ifelse(str_detect(genres, "Drama"),    TRUE, FALSE), G7a  = G7*train_y,
      G8 = ifelse(str_detect(genres,"Fantasy"),   TRUE, FALSE), G8a  = G8*train_y,
      G9 = ifelse(str_detect(genres,"Mystery"),   TRUE, FALSE), G9a  = G9*train_y,
      G10 = ifelse(str_detect(genres,"Musical"),  TRUE, FALSE), G10a = G10*train_y,
      G11 = ifelse(str_detect(genres,"Romance"),  TRUE, FALSE), G11a = G11*train_y,
      G12 = ifelse(str_detect(genres,"Sci"),      TRUE, FALSE), G12a = G12*train_y,
      G13 = ifelse(str_detect(genres,"Thriller"), TRUE, FALSE), G13a = G13*train_y,
      G14 = ifelse(str_detect(genres,"War"),      TRUE, FALSE), G14a = G14*train_y,
      G15 = ifelse(str_detect(genres,"Docu"),     TRUE, FALSE), G15a = G15*train_y,
      G16 = ifelse(str_detect(genres,"Horr"),     TRUE, FALSE), G16a = G16*train_y,
      G17 = ifelse(str_detect(genres,"West"),     TRUE, FALSE), G17a = G17*train_y
    )
  
  # TRAIN DATA
  # use USER'S average rating if none exists for genera
  USER_GENRE_weighs <- TRAIN_GENRE %>% group_by(userId) %>%
    summarize(  #if review has genre keep average Rating, else user def_wt
      ave_rating = mean(train_y),
      UG1 = ifelse(sum(G1)   > 0, sum(G1a)  / sum(G1),  ave_rating),
      UG2 = ifelse(sum(G2)   > 0, sum(G2a)  / sum(G2),  ave_rating),
      UG3 = ifelse(sum(G3)   > 0, sum(G3a)  / sum(G3),  ave_rating),
      UG4 = ifelse(sum(G4)   > 0, sum(G4a)  / sum(G4),  ave_rating),
      UG5 = ifelse(sum(G5)   > 0, sum(G5a)  / sum(G5),  ave_rating),
      UG6 = ifelse(sum(G6)   > 0, sum(G6a)  / sum(G6),  ave_rating),
      UG7 = ifelse(sum(G7)   > 0, sum(G7a)  / sum(G7),  ave_rating),
      UG8 = ifelse(sum(G8)   > 0, sum(G8a)  / sum(G8),  ave_rating),
      UG9 = ifelse(sum(G9)   > 0, sum(G9a)  / sum(G9),  ave_rating),
      UG10 = ifelse(sum(G10) > 0, sum(G10a) / sum(G10), ave_rating),
      UG11 = ifelse(sum(G11) > 0, sum(G11a) / sum(G11), ave_rating),
      UG12 = ifelse(sum(G12) > 0, sum(G12a) / sum(G12), ave_rating),
      UG13 = ifelse(sum(G13) > 0, sum(G13a) / sum(G13), ave_rating),
      UG14 = ifelse(sum(G14) > 0, sum(G14a) / sum(G14), ave_rating),
      UG15 = ifelse(sum(G15) > 0, sum(G15a) / sum(G15), ave_rating),
      UG16 = ifelse(sum(G16) > 0, sum(G16a) / sum(G16), ave_rating),
      UG17 = ifelse(sum(G17) > 0, sum(G17a) / sum(G17), ave_rating))
  
  TRAIN_GENRE <- TRAIN_GENRE %>% 
    left_join(USER_GENRE_weighs) %>% 
    mutate(
      denom = (G1+G2+G3+G4+G5+G6+G7+G8+G9+G10+G11+G12+G13+G14+G15+G16+G17),
      pred = ifelse(denom == 0, ave_rating, (G1*UG1+G2*UG2+G3*UG3+G4*UG4+G5*UG5+G6*UG6+G7*UG7+G8*UG8+G9*UG9+G10*UG10+G11*UG11+G12*UG12+G13*UG13+G14*UG14+G15*UG15+G16*UG16+G17*UG17) / denom)
    ) %>% select(pred, rating = train_y)
  # replace NA values with average (this is a small number but causes RMSE to fail)
  
  TEST_GENRE <- test_dat %>% select(movieId, userId, genres, rating) %>%
    left_join(USER_GENRE_weighs) %>%  
    mutate(  # expand Genre list one column each, TRUE = genre of film, GXa = rating
      G1 = ifelse(str_detect(genres, "Action"),   TRUE, FALSE),
      G2 = ifelse(str_detect(genres,"Adventure"), TRUE, FALSE),
      G3 = ifelse(str_detect(genres,"Animation"), TRUE, FALSE),
      G4 = ifelse(str_detect(genres,"Children"),  TRUE, FALSE),
      G5 = ifelse(str_detect(genres, "Comedy"),   TRUE, FALSE),
      G6 = ifelse(str_detect(genres, "Crime"),    TRUE, FALSE),
      G7 = ifelse(str_detect(genres, "Drama"),    TRUE, FALSE),
      G8 = ifelse(str_detect(genres,"Fantasy"),   TRUE, FALSE),
      G9 = ifelse(str_detect(genres,"Mystery"),   TRUE, FALSE),
      G10 = ifelse(str_detect(genres,"Musical"),  TRUE, FALSE),
      G11 = ifelse(str_detect(genres,"Romance"),  TRUE, FALSE),
      G12 = ifelse(str_detect(genres,"Sci"),    TRUE, FALSE),
      G13 = ifelse(str_detect(genres,"Thriller"), TRUE, FALSE),
      G14 = ifelse(str_detect(genres,"War"),      TRUE, FALSE),
      G15 = ifelse(str_detect(genres,"Docu"),      TRUE, FALSE),
      G16 = ifelse(str_detect(genres,"Horr"),      TRUE, FALSE),
      G17 = ifelse(str_detect(genres,"West"),      TRUE, FALSE),
      denom = (G1+G2+G3+G4+G5+G6+G7+G8+G9+G10+G11+G12+G13+G14+G15+G16+G17),
      pred = ifelse(denom == 0, ave_rating, (G1*UG1+G2*UG2+G3*UG3+G4*UG4+G5*UG5+G6*UG6+G7*UG7+G8*UG8+G9*UG9+G10*UG10+G11*UG11+G12*UG12+G13*UG13+G14*UG14+G15*UG15+G16*UG16+G17*UG17) / denom)
    ) %>% select(pred, rating)
  
  # replace NA values with average (this is a small number but causes RMSE to fail)
  pred_ave <- mean(TEST_GENRE$pred, na.rm = TRUE)
  rating_ave <- mean(TEST_GENRE$rating, na.rm = TRUE)
  TEST_GENRE$pred <-   ifelse(is.na(TEST_GENRE$pred),   pred_ave, TEST_GENRE$pred)
  TEST_GENRE$rating <- ifelse(is.na(TEST_GENRE$rating), rating_ave, TEST_GENRE$rating)
  pred_ave <- mean(TRAIN_GENRE$pred, na.rm = TRUE)
  rating_ave <- mean(TRAIN_GENRE$rating, na.rm = TRUE)
  TRAIN_GENRE$pred <-   ifelse(is.na(TRAIN_GENRE$pred),   pred_ave, TRAIN_GENRE$pred)
  TRAIN_GENRE$rating <- ifelse(is.na(TRAIN_GENRE$rating), rating_ave, TRAIN_GENRE$rating)
  
  PackPipe(TRAIN_GENRE, TEST_GENRE)
}

#### Quick and dirty fix for Fix_OOB below  ###
#  fix another day
fast_oob <- function(dat){
  dat <- ifelse(dat < 0.5, 0.5, dat)
  dat <- ifelse(dat > 5, 4.5, dat)
}

########### FIX OUT-OF-BOUNDS VALUES  ######
## FIX: X < 0.5, X > 5  ####
Fix_OOB <- function(in_y) {
  
  train_in <- UnpackPipe(in_y, 1)
  test_in <- UnpackPipe(in_y, 2)
  
  train_in$pred <- ifelse(train_in$pred < 0.5, 0.5, train_in$pred)
  train_in$pred <- ifelse(train_in$pred > 5 & train_in$pred != 99999, 4.5, train_in$pred)
  test_in$pred <- ifelse(test_in$pred < 0.5, 0.5, test_in$pred)
  test_in$pred <- ifelse(test_in$pred > 5 & test_in$pred != 99999, 4.5, test_in$pred)
  PackPipe(train_in, test_in)
}


#############
#REGULARIZE RATINGS
## Average of 
## INPUT
##   train_dat - training data (usually test_set)
##   train_y - pred or residual from previous step
##   test_dat - test set data to drive results
## OUTPUT
##   data.frame with 
##   $pred - prediction
##   $resid = residual after subtracting from rating

REGULARIZE_v1 <- function(train_dat, train_y, test_dat, lambda = 3) {
  train_dat1 <- data.frame(movieId = train_dat$movieId, userId = train_dat$userId, rating = train_dat$rating, train_y = train_y)
  mu <- mean(train_y)
  movie_reg_avgs <- train_dat1 %>% 
    group_by(movieId) %>% 
    summarize(b_i = sum(train_y - mu)/(n()+lambda), n_i = n(), pred= mu + b_i)
  train_dat1 %>% left_join(movie_reg_avgs, by = "movieId") %>% select(pred, resid = b_i, n_i, r = rating)
}

getEdx <- function() {
  ### Provided Code to Create edx Data Set
  # Modidfied the script provided because the line below changed MovieID into NA.
  # movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
  #                                            title = as.character(title),
  #                                            genres = as.character(genres))
  # Changed to:
  #   movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
  #                                              title = as.character(title),
  #                                              genres = as.character(genres))
  # Seems to have fixed the problem 
  # 
  # 
  # ```{r initial data set}
  ################################
  # Create edx set, validation set
  ################################
  
  # Note: this process could take a couple of minutes
  
  if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
  if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
  if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
  
  # MovieLens 10M dataset:
  # https://grouplens.org/datasets/movielens/10m/
  # http://files.grouplens.org/datasets/movielens/ml-10m.zip
  
  dl <- tempfile()
  download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)
  
  ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                   col.names = c("userId", "movieId", "rating", "timestamp"))
  
  movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
  colnames(movies) <- c("movieId", "title", "genres")
  movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                             title = as.character(title),
                                             genres = as.character(genres))
  
  movielens <- left_join(ratings, movies, by = "movieId")
  
  # Validation set will be 10% of MovieLens data
  set.seed(1, sample.kind="Rounding")
  # if using R 3.5 or earlier, use `set.seed(1)` instead
  test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
  edx <- movielens[-test_index,]
  temp <- movielens[test_index,]
  
  # Make sure userId and movieId in validation set are also in edx set
  validation <- temp %>% 
    semi_join(edx, by = "movieId") %>%
    semi_join(edx, by = "userId")
  
  # Add rows removed from validation set back into edx set
  removed <- anti_join(temp, validation)
  edx <- rbind(edx, removed)
  
  rm(dl, ratings, movies, test_index, temp, removed)
  
  saveRDS(edx, "_edx_dataset")
  saveRDS(validation, "_val_dataset")
}




