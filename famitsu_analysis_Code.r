library(rio)
library(readxl)
library(reshape2)
library(dplyr)
library(tidyr)
library(openxlsx)
library(PanelCount)
library(lme4)
library(zoo)
library(MuMIn)
library(lattice)

############################### Clean and Reform Data############################### 
# Read data from excel into dataframes. 
# Excel data is already organized as timeseries(easy to reshape in scraping code - pandas python)
file_name <- 'data/data_publisher_panel.xlsx'
df_revenue <- read_excel(file_name, sheet="Revenue(Mil)");
df_game_count <- read_excel(file_name, sheet="Count")
df_genre_count <- read_excel(file_name, sheet="GenreCount")
df_avg_price <- read_excel(file_name, sheet="AvgPrice")
df_std_price <- read_excel(file_name, sheet="StdPrice")

# Combine data using merge and reduce
df_list <- list(df_revenue, df_game_count, df_genre_count, df_avg_price, df_std_price)
df <- Reduce(function(x, y) merge(x, y, by = c("publisher", "year", "week_no"), all = TRUE), df_list)
head(df)

# Reshape to long format with metric as column
df_reshaped <- melt(df, id.vars = c("publisher", "year", "week_no"), 
                    variable.name = "metric", value.name = "value")
head(df_reshaped)

# For the purpose of analysis, choosing df as dataframe with column in headers
# Combine year and week int single column
df$time <- paste(df$year, sprintf("%02d", df$week_no), sep = "-")
head(df)
write.xlsx(df, file = "data/long_output.xlsx", rowNames = FALSE)

############################### Add lagged variables based on existing columns ############################### 
# Determine whether publisher made it to ranking in week
df$ranked_tf <- ifelse(df$game_count > 0, 'True', 'False')
df$ranked <- ifelse(df$game_count > 0, 1, 0)

# Number of times publisher is in ranking past week, month, year
df <- df %>% 
  group_by(publisher) %>% 
  mutate(ranked_last_week = lag(ranked, n = 1, default = 0),
         ranked_last_month = rollapplyr(ranked, width = 5, FUN = sum, fill = 0, align = "right"),
         ranked_last_year = rollapplyr(ranked, width = 53, FUN = sum, fill = 0, align = "right")) %>% 
  ungroup()
# Exclude current row from rolling sum
df$ranked_last_month = df$ranked_last_month - df$ranked
df$ranked_last_year = df$ranked_last_year - df$ranked
df[df$ranked_last_month < 0, "ranked_last_month"] = 0
df[df$ranked_last_year < 0, "ranked_last_year"] = 0

# Number of games released by publisher with in past week, month, year
df <- df %>% 
  group_by(publisher) %>% 
  mutate(game_count_last_week = lag(game_count, n = 1, default = 0),
         game_count_last_month = rollapplyr(game_count, width = 5, FUN = sum, fill = 0, align = "right"),
         game_count_last_year = rollapplyr(game_count, width = 53, FUN = sum, fill = 0, align = "right")) %>% 
  ungroup()
# Exclude current row from rolling sum
df$game_count_last_month = df$game_count_last_month - df$game_count
df$game_count_last_year = df$game_count_last_year - df$game_count
df[df$game_count_last_month < 0, "game_count_last_month"] = 0
df[df$game_count_last_year < 0, "game_count_last_year"] = 0

# Number of distinct genres for publisher with in past week, month, year
df <- df %>% 
  group_by(publisher) %>% 
  mutate(genre_count_last_week = lag(genre_count, n = 1, default = 0),
         genre_count_last_month = rollapplyr(genre_count, width = 5, FUN = sum, fill = 0, align = "right"),
         genre_count_last_year = rollapplyr(genre_count, width = 53, FUN = sum, fill = 0, align = "right")) %>% 
  ungroup()
# Exclude current row from rolling sum
df$genre_count_last_month = df$genre_count_last_month - df$genre_count
df$genre_count_last_year = df$genre_count_last_year - df$genre_count
df[df$genre_count_last_month < 0, "genre_count_last_month"] = 0
df[df$genre_count_last_year < 0, "genre_count_last_year"] = 0

# Avg revenue for publisher with in past week, month, year
df <- df %>% 
  group_by(publisher) %>% 
  mutate(revenue_last_week = lag(revenue, n = 1, default = 0),
         revenue_last_month = rollapplyr(revenue, width = 5, FUN = sum, fill = 0, align = "right"),
         revenue_last_year = rollapplyr(revenue, width = 53, FUN = sum, fill = 0, align = "right")) %>% 
  ungroup()
# Exclude current row for rolling mean of revenue in last month, year
df$revenue_last_month = (df$revenue_last_month - df$revenue) / 4
df$revenue_last_year = (df$revenue_last_year - df$revenue) / 52
df[df$revenue_last_month < 0, "revenue_last_month"] = 0
df[df$revenue_last_year < 0, "revenue_last_year"] = 0

# Avg price of all games for publisher with in past week, month, year
df <- df %>% 
  group_by(publisher) %>% 
  mutate(avg_price_last_week = lag(avg_price, n = 1, default = 0),
         avg_price_last_month = rollapplyr(avg_price, width = 5, FUN = sum, fill = 0, align = "right"),
         avg_price_last_year = rollapplyr(avg_price, width = 53, FUN = sum, fill = 0, align = "right")) %>% 
  ungroup()
# Exclude current row for rolling mean of avg_price in last month, year
df$avg_price_last_month = (df$avg_price_last_month - df$avg_price) / 4
df$avg_price_last_year = (df$avg_price_last_year - df$avg_price) / 52
df[df$avg_price_last_month < 0, "avg_price_last_month"] = 0
df[df$avg_price_last_year < 0, "avg_price_last_year"] = 0
df = replace(df, is.na(df), 0)

############################### Ranking mixed effects model ############################### 
# Predict whether a publisher can make it to ranking using random effects logistic regression
# glmer (logistic regression + random effects (group correlation))

# Plot relation between ranked vs ranked_last_week, ranked_last_month, ranked_last_year
bwplot(ranked_tf ~ ranked_last_week | publisher, data=df)
bwplot(ranked_tf ~ ranked_last_month | publisher, data=df)
bwplot(ranked_tf ~ ranked_last_year | publisher, data=df)

model <- glmer(ranked ~ ranked_last_week + ranked_last_month + ranked_last_year + (1 | publisher),
               data = df, family=binomial())
summary(model)

print(fixef(model)) # Fixed effects from the model
print(ranef(model)) # Random effects from the model
random_effects = ranef(model)$publisher
random_effects_df <- data.frame(Publisher = rownames(random_effects), Intercept = random_effects[, 1])
bwplot(Intercept ~ Publisher, data = random_effects_df, xlab = "Publisher", ylab = "Random Effects")


# Get predicted probabilities from the model and residuals
df$predicted_prob <- predict(model, type="response")
df$prob_residual <- df$ranked - df$predicted_prob


############################### Game count random effects model ############################### 
# Subset of dataframe with publishers that are ranked in an given week
df_ranked <- df[df$ranked == 1, ]

# Random effects model on publisher game count
df_ranked <- na.omit(df_ranked)
# Scale price to avoid scaling issues on models
df_ranked["avg_price_last_week"] <- df_ranked["avg_price_last_week"] / 1000
df_ranked["avg_price_last_month"] <- df_ranked["avg_price_last_month"] / 1000
df_ranked["avg_price_last_year"] <- df_ranked["avg_price_last_year"] / 1000
game_count_model = lmer(game_count ~ game_count_last_week + game_count_last_month  + game_count_last_year +
                          genre_count_last_week + genre_count_last_month + genre_count_last_year +
                          avg_price_last_week + avg_price_last_month  + avg_price_last_year +
                          prob_residual + (1 | publisher), data=df_ranked, na.action = "na.fail")
summary(game_count_model)
# t-value of prob_residual is significant in game_count_model indicating that there is self selection i.e second stage model
# is substantially effected by residuals of first stage model

# Use dredge to get best model based on AIC criteria
game_count_model_comparison <- dredge(game_count_model)
print(game_count_model_comparison[which.min(game_count_model_comparison$AICc), ] )
# Best model based on dredge
best_game_count_model = lmer(game_count ~ game_count_last_week + game_count_last_month + game_count_last_year + 
                               genre_count_last_week + 
                               avg_price_last_week + avg_price_last_year +
                               prob_residual + (1 | publisher), data=df_ranked)
summary(best_game_count_model)
print(AIC(best_game_count_model))


############################### Sales/Revenue random effects model ############################### 
# Subset of dataframe with publishers that are ranked in an given week
df_ranked <- df[df$ranked == 1, ]

# Random effects model on publisher revenue
df_ranked <- na.omit(df_ranked)
# Scale price to avoid scaling issues on models
df_ranked["revenue_last_week"] <- df_ranked["revenue_last_week"] / 10
df_ranked["revenue_last_month"] <- df_ranked["revenue_last_month"] / 10
df_ranked["revenue_last_year"] <- df_ranked["revenue_last_year"] / 10
df_ranked["avg_price_last_week"] <- df_ranked["avg_price_last_week"] / 1000
df_ranked["avg_price_last_month"] <- df_ranked["avg_price_last_month"] / 1000
df_ranked["avg_price_last_year"] <- df_ranked["avg_price_last_year"] / 1000
revenue_model = lmer(revenue ~ revenue_last_week + revenue_last_month  + revenue_last_year +
                       genre_count_last_week + genre_count_last_month + genre_count_last_year +
                       avg_price_last_week + avg_price_last_month  + avg_price_last_year + 
                       prob_residual + (1 | publisher), data=df_ranked, na.action = "na.fail")
summary(revenue_model)

# Use dredge to get best model based on AIC criteria
revenue_model_comparison <- dredge(revenue_model)
print(revenue_model_comparison[which.min(revenue_model_comparison$AICc), ] )

# Best model based on dredge
best_revenue_model = lmer(revenue ~ revenue_last_week + revenue_last_month + 
                               genre_count_last_week + genre_count_last_month  + genre_count_last_year + 
                               avg_price_last_week + avg_price_last_month + avg_price_last_year + 
                               prob_residual + (1 | publisher), data=df)
print(AIC(best_revenue_model))


