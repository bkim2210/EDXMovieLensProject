
library(tidyverse)
library(caret)
library(data.table)


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

set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.2, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]



validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")


removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)
rm(dl, ratings, movies, test_index, temp, movielens, removed)


head(edx)
str(edx)


#counts the number of rating of each given rating
edx %>% group_by(rating) %>% summarize(count=n())%>% top_n(5)



#number of rating per movies
edx %>% count(movieId) %>% ggplot(aes(n))+ geom_histogram(color= "blue",fill="red") + scale_x_log10() + ggtitle(" Rating Per Movies")




edx %>% count(userId) %>%  ggplot(aes(n)) + geom_histogram(color= "blue",fill="red") + scale_x_log10() + ggtitle("Rating Per User") 




p<-edx %>% separate_rows(genres, sep = "\\|") %>%
  group_by(genres) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) %>% ggplot(aes(genres,count)) + 
  geom_bar(aes(fill =genres),stat = "identity")+ 
  labs(title = " Number of Rating for Each Genre")+
  theme(axis.text.x  = element_text(angle= 90, vjust = 50 ))+
  theme_light()
p<-coord_flip()



edx %>% separate_rows(genres, sep = "\\|") %>%
  group_by(genres) %>%
  summarize(count = n()) %>%
  arrange(desc(count))



set.seed(1)
test_index <- createDataPartition(y=edx$rating,times = 1,p=0.2,list=FALSE)
train <- edx[-test_index,]
test <- edx[test_index,]



RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2, na.rm = TRUE))
}



model_1 <- mean(train$rating)
model_1



RMSE1 <- RMSE(test$rating,model_1)
RMSE1


model_2 <- mean(train$rating)
avgsmovie <- train %>% group_by(movieId) %>% summarize(b_i= mean(rating-model_2))



rating_predicted <- model_2 + test %>% left_join(avgsmovie, by ='movieId') %>% pull(b_i)
RMSE2_Model_2 <- RMSE(rating_predicted,test$rating)
RMSE2_Model_2 


user_avgs <- train %>% 
  left_join(avgsmovie, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - RMSE2_Model_2 - b_i))


predicted_ratings <- test %>% 
  left_join(avgsmovie, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred =  RMSE2_Model_2 + b_i + b_u) %>%
  pull(pred)



model_3_rmse <- RMSE(predicted_ratings, test$rating)
model_3_rmse



valid_pred_rating <- validation %>%
  left_join(avgsmovie, by = "movieId" ) %>% 
  left_join(user_avgs , by = "userId") %>%
  mutate(pred = RMSE2_Model_2 + b_i + b_u ) %>%
  pull(pred)

model_3_valid <- RMSE(validation$rating, valid_pred_rating)
model_3_valid




RMSE_table<-data.frame(RMSE1,RMSE2_Model_2,model_3_rmse,model_3_valid)
RMSE_table
