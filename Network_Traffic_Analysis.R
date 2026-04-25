#PFDA Amalan Kesari Gupta TP084948


#------------------------------------------------------------
#loading libraries
#------------------------------------------------------------
install.packages("rpart")
install.packages("rpart.plot")
install.packages("e1071")

library(e1071)
library(dplyr)
library(ggplot2)
library(rpart)
library(rpart.plot)

#------------------------------------------------------------
#data import
#------------------------------------------------------------
file_path_data <- "C:\\Users\\gupta\\OneDrive\\Desktop\\YEAR 2 SEM 1\\Data Analysis\\PFDA assignment\\UNSW-NB15_uncleaned.csv"
file_path_features <- "C:\\Users\\gupta\\OneDrive\\Desktop\\YEAR 2 SEM 1\\Data Analysis\\PFDA assignment\\6. NUSW-NB15_features (data description).csv"

df <- read.csv(file_path_data, stringsAsFactors = FALSE)
features_descr <- read.csv(file_path_features, stringsAsFactors = FALSE)

head(df)
str(df)

#------------------------------------------------------------
#variable inspection
#------------------------------------------------------------
colnames(df)

#------------------------------------------------------------
#data cleaning
#------------------------------------------------------------
df <- df %>%
  mutate(
    sbytes = as.numeric(gsub("[^0-9\\.\\-]", "", as.character(sbytes))),
    dbytes = as.numeric(gsub("[^0-9\\.\\-]", "", as.character(dbytes)))
  )

median_s <- median(df$sbytes, na.rm = TRUE)
median_d <- median(df$dbytes, na.rm = TRUE)

df <- df %>%
  mutate(
    sbytes = ifelse(is.na(sbytes), median_s, sbytes),
    dbytes = ifelse(is.na(dbytes), median_d, dbytes)
  )

df <- df %>%
  mutate(
    s_d_ratio = sbytes / (dbytes + 1),
    log_sbytes = log10(sbytes + 1),
    log_dbytes = log10(dbytes + 1),
    log_ratio = log10(s_d_ratio + 1)
  )

#LABEL CLEANING BLOCK
df <- df %>%
  mutate(
    label_raw = as.character(label),
    label_clean_num = gsub("[^0-9]", "", label_raw),        
    label2 = ifelse(label_clean_num == "0", "Normal", "Attack"),
    label2 = factor(label2, levels = c("Normal","Attack"))
  )

#------------------------------------------------------------
#data validation 
#------------------------------------------------------------
# 1. Validation of cleaned variable types
head(df[, c("sbytes","dbytes","s_d_ratio","log_sbytes","log_dbytes","log_ratio")])
str(df[, c("sbytes","dbytes","s_d_ratio","log_sbytes","log_dbytes","log_ratio")])

# 2. Missing value verification
colSums(is.na(df[, c("sbytes","dbytes","s_d_ratio","log_sbytes","log_dbytes","log_ratio")]))

# 3. Summary statistics validation
summary(df[, c("sbytes","dbytes","s_d_ratio","log_sbytes","log_dbytes","log_ratio")])

# 4. Validation of cleaned label categories
table(df$label2)

#------------------------------------------------------------
#descriptive stats
#------------------------------------------------------------
summary(df[, c("sbytes","dbytes","s_d_ratio")])

df %>% group_by(label2) %>%
  summarise(
    sbytes_mean = mean(sbytes),
    dbytes_mean = mean(dbytes),
    ratio_mean  = mean(s_d_ratio)
  )

#------------------------------------------------------------
#histograms
#------------------------------------------------------------
ggplot(df, aes(log_sbytes)) +
  geom_histogram(bins = 50) + theme_minimal() + labs(title = "Histogram: log10(sbytes+1)")
ggplot(df, aes(log_dbytes)) + 
  geom_histogram(bins = 50) + theme_minimal() + labs(title = "Histogram: log10(dbytes+1)")


#------------------------------------------------------------
# pie chart (label distribution)
label_counts <- table(df$label2)
pie(label_counts,
    main = "Distribution of Normal vs Attack Traffic",
    col = c("skyblue", "tomato"))

# bar chart (mean sbytes and dbytes by label)
mean_bytes <- df %>%
  group_by(label2) %>%
  summarise(
    mean_sbytes = mean(sbytes, na.rm = TRUE),
    mean_dbytes = mean(dbytes, na.rm = TRUE)
  )

ggplot(mean_bytes, aes(x = label2)) +
  geom_col(aes(y = mean_sbytes, fill = "sbytes"), position = position_dodge(width = 0.9)) +
  geom_col(aes(y = mean_dbytes, fill = "dbytes"), position = position_dodge(width = 0.9)) +
  labs(title = "Mean sbytes and dbytes by Label",
       x = "Label",
       y = "Mean Value",
       fill = "Variable") +
  theme_minimal()

# line chart (average sbytes across dataset chunks)
df$index <- 1:nrow(df)

line_data <- df %>%
  mutate(chunk = index %/% 1000) %>%      # group every 1000 rows
  group_by(chunk) %>%
  summarise(avg_sbytes = mean(sbytes, na.rm = TRUE))

ggplot(line_data, aes(x = chunk, y = avg_sbytes)) +
  geom_line(color = "blue") +
  labs(title = "Trend of Average sbytes Across Dataset",
       x = "Chunk of 1000 Observations",
       y = "Average sbytes") +
  theme_minimal()

#------------------------------------------------------------
#boxplots
#------------------------------------------------------------
ggplot(df, aes(label2, log_sbytes)) + geom_boxplot() + theme_minimal()
ggplot(df, aes(label2, log_dbytes)) + geom_boxplot() + theme_minimal()

#------------------------------------------------------------
#scatterplot
#------------------------------------------------------------
ggplot(df, aes(log_dbytes, log_sbytes, color = label2)) +
  geom_point(alpha = 0.4, size = 1) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal()

#------------------------------------------------------------
#density plot (ratio)
#------------------------------------------------------------
ggplot(df, aes(log_ratio, fill = label2)) +
  geom_density(alpha = 0.4) +
  theme_minimal()

#------------------------------------------------------------
#correlation
#------------------------------------------------------------
cor(df$log_sbytes, df$log_dbytes)

df %>%
  group_by(label2) %>%
  summarise(corr = cor(log_sbytes, log_dbytes))

#------------------------------------------------------------
#simple regression
#------------------------------------------------------------
lm_fit <- lm(log_sbytes ~ log_dbytes, data = df)
summary(lm_fit)

#------------------------------------------------------------
#t-test
#------------------------------------------------------------
t.test(log_ratio ~ label2, data = df)

#------------------------------------------------------------
#train-test split
#------------------------------------------------------------
set.seed(123)
idx <- sample(1:nrow(df), size = 0.8*nrow(df))
train <- df[idx, ]
test  <- df[-idx, ]

df <- df %>% mutate(label_bin = ifelse(label2 == "Attack", 1, 0))
train <- train %>% mutate(label_bin = ifelse(label2 == "Attack", 1, 0))
test  <- test %>% mutate(label_bin = ifelse(label2 == "Attack", 1, 0))

#------------------------------------------------------------
#logistic model
#------------------------------------------------------------
glm_fit <- glm(label_bin ~ log_sbytes + log_dbytes + log_ratio, data = train, family = binomial)
summary(glm_fit)

#------------------------------------------------------------
#prediction
#------------------------------------------------------------
test$pred_prob  <- predict(glm_fit, newdata = test, type = "response")
test$pred_label <- ifelse(test$pred_prob >= 0.5, 1, 0)

#------------------------------------------------------------
#confusion matrix
#------------------------------------------------------------
table(Predicted = test$pred_label, Actual = test$label_bin)

#------------------------------------------------------------
#decision tree
#------------------------------------------------------------
tree_model <- rpart(label_bin ~ log_sbytes + log_dbytes + log_ratio,
                    data = train,
                    method = "class")
summary(tree_model)
#------------------------------------------------------------
#decision tree plot
#------------------------------------------------------------
plot(tree_model)
text(tree_model, use.n = TRUE)
rpart.plot(tree_model)
#------------------------------------------------------------
#decision tree prediction
#------------------------------------------------------------
tree_pred <- predict(tree_model, newdata = test, type = "class")
#------------------------------------------------------------
#decision tree confusion matrix
#------------------------------------------------------------
table(Predicted = tree_pred, Actual = test$label_bin)
#------------------------------------------------------------
# Naive Bayes Model
#------------------------------------------------------------
# Training Naive Bayes using the log-transformed features
nb_model <- naiveBayes(label2 ~ log_sbytes + log_dbytes + log_ratio, data = train)
# model summary
nb_model
# Predict on test set
nb_pred_class <- predict(nb_model, newdata = test, type = "class")
# Confusion matrix
table(Predicted = nb_pred_class, Actual = test$label2)
# Accuracy
mean(nb_pred_class == test$label2)


