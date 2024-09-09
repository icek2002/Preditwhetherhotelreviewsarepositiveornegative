setwd()
library(ggplot2)
library(rpart)
library(readr)
library(gplots)
library(dplyr)
library(stringr)
library(tm)
library(wordcloud)
library(leaflet)
library(purrr)
library(tidyverse)

df <- read_csv("Hotel_Reviews.csv")

head(df)

summary(df)

# Calculate the number of NA values in each column
na_counts <- colSums(is.na(df))

# Remove rows with any NA values
df <- df[complete.cases(df), ]
# Print the result
print(na_counts)

# Calculate the number of duplicated rows
duplicate_count <- sum(duplicated(df))
# Print the result
print(duplicate_count)


# Drop duplicates
df <- df %>% distinct()

# Check for duplicates to confirm they have been dropped
duplicates <- df[duplicated(df), ]
print(duplicates)

# Print info
str(df)


# Remove leading and trailing spaces
df$Reviewer_Nationality <- str_trim(df$Reviewer_Nationality)

# Replace specific nationalities
df$Reviewer_Nationality <- df$Reviewer_Nationality %>%
  str_replace_all('United Kingdom', 'UK') %>%
  str_replace_all('United Arab Emirates', 'UAE') %>%
  str_replace_all('United States of America', 'USA') %>%
  str_replace_all('Saudi Arabia', 'Saudi') %>%
  str_replace_all('Netherlands', 'Nether L') %>%
  str_replace_all('Switzerland', 'Switzer L')


# Calculate the top 10 hotels by frequency
top_hotels <- df %>%
  count(Hotel_Name) %>%
  arrange(desc(n)) %>%
  top_n(10, n)

# Create the plot
ggplot(top_hotels, aes(x = reorder(Hotel_Name, -n), y = n)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Top 10 Hotels in the Review", x = "Hotel", y = "Frequency") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# Count the number of reviews per country
country_counts <- df %>%
  count(Reviewer_Nationality) %>%
  arrange(desc(n)) %>%
  top_n(10, n)

# Create the plot
ggplot(country_counts, aes(x = reorder(Reviewer_Nationality, -n), y = n)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Top 10 Countries with Most Participation in the Review",
       x = "Country",
       y = "Number of Reviews") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# Create the review text by concatenating Negative_Review and Positive_Review
df <- df %>%
  mutate(review = paste(Negative_Review, Positive_Review, sep = " "))

# Create the sentiment label based on Reviewer_Score
df <- df %>%
  mutate(sentiment = ifelse(Reviewer_Score < 5, "Negative", "Positive"))





# Create the plot
ggplot(df, aes(x = sentiment, fill = sentiment)) +
  geom_bar() +
  scale_fill_manual(values = c("Negative" = "blue", "Positive" = "orange")) +
  labs(title = "Sentiment Outcome Distribution",
       x = "Outcome",
       y = "Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0),
        panel.grid.major.y = element_line(size = 0.5, linetype = 'dashed', colour = "grey"),
        panel.grid.minor.y = element_line(size = 0.25, linetype = 'dashed', colour = "grey"))





# Filter the positive reviews with Reviewer_Score >= 6
positive_reviews <- df %>% 
  filter(Reviewer_Score >= 5) %>% 
  pull(Positive_Review) %>%
  paste(collapse = " ")

# Filter the positive reviews with Reviewer_Score <= 5
negative_reviews <- df %>% 
  filter(Reviewer_Score < 5) %>% 
  pull(Negative_Review) %>%
  paste(collapse = " ")

# Create a corpus
Pcorpus <- Corpus(VectorSource(positive_reviews))
Ncorpus <- Corpus(VectorSource(negative_reviews))

# Clean the text data
Pcorpus <- Pcorpus %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>%
  tm_map(removeWords, stopwords("en")) %>%
  tm_map(stripWhitespace)

Ncorpus <- Ncorpus %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>%
  tm_map(removeWords, stopwords("en")) %>%
  tm_map(stripWhitespace)

# Convert corpus to a term-document matrix
Ptdm <- TermDocumentMatrix(Pcorpus)
Ntdm <- TermDocumentMatrix(Ncorpus)

# Convert term-document matrix to a matrix
Ptdm_matrix <- as.matrix(Ptdm)
Ntdm_matrix <- as.matrix(Ntdm)

# Get word frequencies
Pword_freqs <- sort(rowSums(Ptdm_matrix), decreasing = TRUE)
Nword_freqs <- sort(rowSums(Ntdm_matrix), decreasing = TRUE)

# Create a data frame with words and their frequencies
Pdf_word_freq <- data.frame(word = names(Pword_freqs), freq = Pword_freqs)
Ndf_word_freq <- data.frame(word = names(Nword_freqs), freq = Nword_freqs)

# Generate the word cloud
set.seed(1234) # For reproducibility
wordcloud(words = Pdf_word_freq$word, freq = Pdf_word_freq$freq, min.freq = 1,
          max.words = 200, random.order = FALSE, rot.per = 0.35, 
          colors = brewer.pal(8, "Dark2"))

# Add a title (if you want to add titles, you might use other packages like grid)
title(main = "Word Cloud of Positive Reviews")

wordcloud(words = Ndf_word_freq$word, freq = Ndf_word_freq$freq, min.freq = 1,
          max.words = 200, random.order = FALSE, rot.per = 0.35, 
          colors = brewer.pal(8, "Dark2"))

title(main = "Word Cloud of Negative Reviews")


# Create the new data frame
features_df <- df %>%
  select(Latitude = lat, Longitude = lng, Review_Total_Negative_Word_Counts, Review_Total_Positive_Word_Counts,
         Reviewer_Score, sentiment)


# Create the new data frame
geo_df <- df %>%
  select(Latitude = lat, Longitude = lng, Positive_Review, Hotel_Name)

# Check for NaNs (NAs in R) in each column
na_counts <- colSums(is.na(geo_df))
Feature_na_counts <- colSums(is.na(features_df))
cat("Number of NAs in each column:\n")
print(Feature_na_counts)


# Remove rows with NaNs
df_cleaned <- geo_df %>% drop_na()
features_df <- features_df %>% drop_na()



# Group by Hotel_Name and calculate mean latitude and longitude, combine positive reviews
grouped <- df_cleaned %>%
  group_by(Hotel_Name) %>%
  summarise(
    Latitude = mean(Latitude, na.rm = TRUE),
    Longitude = mean(Longitude, na.rm = TRUE),
    Positive_Review = paste(Positive_Review, collapse = " | ")
  ) %>%
  ungroup()

# Calculate the center of the map
map_center <- c(mean(grouped$Latitude, na.rm = TRUE), mean(grouped$Longitude, na.rm = TRUE))

# Initialize a map centered around the average location
map <- leaflet() %>%
  addTiles() %>%
  setView(lng = map_center[2], lat = map_center[1], zoom = 5)

# Add points to the map
map <- map %>%
  addMarkers(data = grouped,
             ~Longitude, ~Latitude,
             popup = ~paste0("<strong>", Hotel_Name, ":</strong><br>", Positive_Review))

# Display the map
map


library(tree)
library(caret)
library(randomForest)
library(rpart.plot)  # for plotting decision trees
library(reshape2)
library(RColorBrewer)

# Prepare the data
set.seed(123)

# Convert sentiment values to 1 and 0
features_df$sentiment <- str_trim(features_df$sentiment)
features_df$sentiment <- ifelse(features_df$sentiment == "Positive", 1, 0)
features_df$sentiment <- as.factor(features_df$sentiment)

# Ensure columns are numeric
features_df$Latitude <- as.numeric(as.character(features_df$Latitude))
features_df$Longitude <- as.numeric(as.character(features_df$Longitude))
features_df$Review_Total_Negative_Word_Counts <- as.numeric(as.character(features_df$Review_Total_Negative_Word_Counts))
features_df$Review_Total_Positive_Word_Counts <- as.numeric(as.character(features_df$Review_Total_Positive_Word_Counts))
features_df$Reviewer_Score <- as.numeric(as.character(features_df$Reviewer_Score))
features_df$sentiment <- as.numeric(as.character(features_df$sentiment))

# Renaming columns
features_df <- features_df %>%
  rename(
    Negative_Word = Review_Total_Negative_Word_Counts,
    Postive_Word = Review_Total_Positive_Word_Counts
  )

# Calculate the correlation matrix
cor_matrix <- cor(features_df)

# Melt the correlation matrix for ggplot
melted_cor_matrix <- melt(cor_matrix)

# Plot the heatmap
ggplot(data = melted_cor_matrix, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(value, 2)), color = "black", size = 4) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), space = "Lab", 
                       name="Correlation") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1)) +
  coord_fixed()

train_indices <- sample(1:nrow(features_df), size = 0.7 * nrow(features_df))
features_df.train <- features_df[train_indices, ]
features_df.test <- features_df[-train_indices, ]
sentiment.test <- features_df$sentiment[-train_indices]

# Ensure 'sentiment' in train and test data have the same levels
features_df.train$sentiment <- factor(features_df.train$sentiment, 
                                      levels = unique(features_df$sentiment))
sentiment.test<- factor(sentiment.test, levels = levels(features_df.train$sentiment))

# Decision Tree with hyperparameter tuning and cross-validation
set.seed(301)
control <- trainControl(method = "cv", number = 10)
tuneGrid <- expand.grid(.cp = seq(0.01, 0.1, by = 0.01))

tree_model <- train(sentiment ~ ., data = features_df.train, method = "rpart",
                    trControl = control, tuneGrid = tuneGrid)
print(tree_model)

# Plot the best decision tree model
best_tree <- tree_model$finalModel
rpart.plot(best_tree)

# Make predictions and calculate test error
tree.pred <- predict(tree_model, newdata = features_df.test)
tab <- table(tree.pred, sentiment.test)
tab
tree_error <- (tab[1,2] + tab[2,1]) / sum(tab) # test error
tree_error


#convert predictions to factor to ensure compatibility with test data
tree.pred.fac <- factor(tree.pred, levels = levels(sentiment.test))

#confusion matrix and classification report
Dconfusion <- confusionMatrix(tree.pred.fac, sentiment.test)

#print confusion matrix and classification report
print(Dconfusion)

# Create a scatter plot of actual vs predicted values
plot_data <- data.frame(Actual = sentiment.test, Predicted = tree.pred)
ggplot(plot_data, aes(x = Actual, y = Predicted)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Actual vs Predicted Sentiment",
       x = "Actual Sentiment",
       y = "Predicted Sentiment") +
  theme_minimal()


# Random Forest with hyperparameter tuning and cross-validation
set.seed(201)
rf_control <- trainControl(method = "cv", number = 10)
rf_grid <- expand.grid(.mtry = c(2, 4, 6, 8))

rf_model <- train(sentiment ~ ., data = features_df.train, method = "rf",
                  trControl = rf_control, tuneGrid = rf_grid, ntree = 500, importance = TRUE)
print(rf_model)

# Plot the random forest model's error rates and variable importance
plot(rf_model$finalModel)
varImpPlot(rf_model$finalModel)

# Make predictions and calculate test error
rf.pred <- predict(rf_model, newdata = features_df.test)
tab <- table(rf.pred, sentiment.test)
tab
rf_error <- (tab[1,2] + tab[2,1]) / sum(tab) # test error
rf_error

#convert predictions to factor to ensure compatibility with test data
rf.pred.fac <- factor(rf.pred, levels = levels(sentiment.test))

#confusion matrix and classification report
RFconfusion <- confusionMatrix(rf.pred.fac, sentiment.test)

#print confusion matrix and classification report
print(RFconfusion)

# Create a scatter plot of actual vs predicted values
plot_data <- data.frame(Actual = sentiment.test, Predicted = rf.pred)
ggplot(plot_data, aes(x = Actual, y = Predicted)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Actual vs Predicted Sentiment",
       x = "Actual Sentiment",
       y = "Predicted Sentiment") +
  theme_minimal()


# Evaluation (using accuracy as an example metric)
rf_accuracy <- sum(rf.pred == features_df.test$sentiment) / nrow(features_df.test)
dt_accuracy <- sum(tree.pred == features_df.test$sentiment) / nrow(features_df.test)

# Random Forest with hyperparameter tuning and cross-validation

Greater5 <- features_df[features_df$Reviewer_Score >= 5, ]
Less5 <- features_df[features_df$Reviewer_Score < 5, ]

# Select the first 150000 rows from Greater5
New5 <- head(Greater5, 48903)
Newless <- head(Less5, 2128)

# Combine New5 and Less5 into a single data frame
svm_features_df <- rbind(New5, Newless)

svmtrain_indices <- sample(1:nrow(svm_features_df), size = 0.7 * nrow(svm_features_df))
svmfeatures_df.train <- svm_features_df[svmtrain_indices, ]
svmfeatures_df.test <- svm_features_df[-svmtrain_indices, ]
svmsentiment.test <- svm_features_df$sentiment[-svmtrain_indices]

# Ensure 'sentiment' in train and test data have the same levels
svmfeatures_df.train$sentiment <- factor(svmfeatures_df.train$sentiment, levels = c(0, 1))
svmfeatures_df.test$sentiment <- factor(svmfeatures_df.test$sentiment, levels = c(0, 1))

# SVM with hyperparameter tuning and cross-validation
set.seed(101)
sv_control <- trainControl(method = "cv", number = 10)
sv_tuneGrid <- expand.grid(C = 2^(-1:2), sigma = 2^(-1:2))

svm_model <- train(sentiment ~ ., data = svmfeatures_df.train, method = "svmRadial",
                   trControl = sv_control, tuneGrid = sv_tuneGrid)

print(svm_model)

# Plot the results of hyperparameter tuning
plot(svm_model)

# Make predictions and calculate test error
svm.pred <- predict(svm_model, newdata = features_df.test)
tab <- table(svm.pred, sentiment.test)
tab
svm_error <- (tab[1,2] + tab[2,1]) / sum(tab) # test error
svm_error

#convert predictions to factor to ensure compatibility with test data
svm.pred.fac <- factor(svm.pred, levels = levels(sentiment.test))

#confusion matrix and classification report
SVMconfusion <- confusionMatrix(svm.pred.fac, sentiment.test)

#print confusion matrix and classification report
print(SVMconfusion)


# Summary of Performance Metrics
performance_metrics <- data.frame(
  Model = c("Decision Tree", "Random Forest", "SVM"),
  Accuracy = c(dt_accuracy, rf_accuracy, svm_accuracy),
  Error = c(tree_error, rf_error, svm_error)
)

print(performance_metrics)











