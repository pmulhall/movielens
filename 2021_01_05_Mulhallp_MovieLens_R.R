##########################################################
# Create edx set, validation set (final hold-out test set)
# Code provided by course, I take no credit for this section.
##########################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(scales)) install.packages("scales", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)
library(scales)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines("movies.dat"), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

# if using R 4.0 or later:
# Added code to split year data out from movie title
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres),
                                           year = as.numeric(substr(as.character(title),nchar(as.character(title))-4,nchar(as.character(title))-1)))


movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
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

rm(dl, ratings, movies, test_index, temp, movielens, removed)

##### end edx section #######

#Begin RMSE Model

# Root Mean Square Error Function
RMSE <- function(actual_rating, predicted_rating)
{
  sqrt(mean((actual_rating - predicted_rating)^2))
}

adj_factors <- seq(0, 10, 0.5) #test for lambda value
rmses <- sapply(adj_factors, function(l){
  
  mts <- mean(edx$rating) # mean rating of training set

  me <- edx %>% 
    group_by(movieId) %>% #adjust by movie rating
    summarize(me = sum(rating - mts)/(n()+l), .groups = 'drop') # penalize low number of ratings
  
  # adjust mean by user ratings and movie ratings and penalize low number of ratings
  am <- edx %>% 
    left_join(me, by="movieId") %>%
    group_by(userId) %>% #adjust by user rating
    summarize(am = sum(rating - me - mts)/(n()+l), .groups = 'drop')  
  
  # calculate predicated ratings based on movie and user effects
  predicted_ratings <- 
    validation %>% 
    left_join(me, by = "movieId") %>%
    left_join(am, by = "userId") %>%
    mutate(pred = mts + me + am) %>% # combine all three adjustments to make a prediction
    .$pred
  
  return(RMSE(predicted_ratings, validation$rating))
})
plot(adj_factors, rmses)
adj_factor <- adj_factors[which.min(rmses)]
paste('RMSE:',min(rmses))
