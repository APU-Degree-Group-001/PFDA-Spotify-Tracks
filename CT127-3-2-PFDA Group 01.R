
###############################################################
# (A) Import CSV
###############################################################

getwd() # check current working directory

# For Shea Wen
setwd("E:/Academics/APU/APD2F2606CS(AI)/Semester 1/Programming for Data Analysis/Assignment/CT127-3-2-PFDA Group 01/dataset")
# For Paureen
setwd("D:/APU/Degree/APD2F2606CS(AI)/SEM 1/Programming for Data Analysis/Assignment/dataset")

# install packages (one-time job)
# library gone when power off, so need to load library at least one time in the script
# install.packages("data.table") #fread() - fast csv import
# install.packages("dplyr")
# install.packages("hexbin")
# install.packages("corrplot")

library(dplyr)

#fread (data.table package) - faster, auto-detects separator/header
#useful for very large files. Same result as read.csv.
library(data.table)
spotify_tracks = fread("2. spotify_tracks_data.csv")



###############################################################
# (B) Data Exploration
###############################################################
head(spotify_tracks)
tail(spotify_tracks)
str(spotify_tracks)
names(spotify_tracks)
# summary(spotify_tracks$Price)
summary(spotify_tracks) # get summary for entire dataset
View(spotify_tracks)



###############################################################
# (C) Data Cleaning
###############################################################
# (i) Check for duplicate records
numDuplicates = sum(duplicated(spotify_tracks)) # 0
spotify_tracks_unique = spotify_tracks

duplicate_tracks <- spotify_tracks_unique %>%
  group_by(track_id) %>%                         # group same (artists, album_Name, track_name)
  summarise(
    frequency = n(),                             # num of rows in that grp
    
    artist_count = n_distinct(artists),          # num of unique values
    album_count = n_distinct(album_name),
    track_count = n_distinct(track_name),
    genre_count = n_distinct(track_genre),
    
    popularity_count = n_distinct(popularity),
    duration_count = n_distinct(duration_ms),
    explicit_count = n_distinct(explicit),
    danceability_count = n_distinct(danceability),
    energy_count = n_distinct(energy),
    key_count = n_distinct(key),
    loudness_count = n_distinct(loudness),
    mode_count = n_distinct(mode),
    speechiness_count = n_distinct(speechiness),
    acousticness_count = n_distinct(acousticness),
    instrumentalness_count = n_distinct(instrumentalness),
    liveness_count = n_distinct(liveness),
    valence_count = n_distinct(valence),
    tempo_count = n_distinct(tempo),
    time_signature_count = n_distinct(time_signature),
    
    .groups = "drop" # after summarizing, remove grping (becuz not necessary anymore)
  ) %>%
  filter(frequency > 1)

View(duplicate_tracks)

# Test to check whether all values are identical
spotify_tracks %>%
  filter(track_id == "00JZ83w0Qm09f4PwWj06sM") # found out that V1 is different

# Decision: Remove V1 column (index in csv) and check for duplicates again
spotify_tracks_unique$V1 = NULL
numDuplicates = sum(duplicated(spotify_tracks_unique))
numDuplicates # 396
spotify_tracks_unique <- spotify_tracks_unique %>% distinct()

# (ii) Check for null values
# (iii) Calculate percentage of null values (To decide whether to remove or impute)
colSums(is.na(spotify_tracks_unique))

# - put in dataframe > better view of No. of N/A & (%)
missing_values_summary = data.frame(
  Column = names(spotify_tracks_unique),
  Missing_Values_Count = colSums(is.na(spotify_tracks_unique)),
  Missing_Percentage = round(colSums(is.na(spotify_tracks_unique)) / nrow(spotify_tracks_unique) * 100, 2)
)

View(missing_values_summary)


# (iv) Calculate no. of rows with 1 or more null values (not specific to which column)
rows_with_missing_values = sum(!complete.cases(spotify_tracks_unique))
rows_with_missing_values
rows_missing_percentage = (rows_with_missing_values / nrow(spotify_tracks_unique)) * 100
rows_missing_percentage
# 2.567691% rows with missing values


# (v) Data Imputation - Missing Value
# Impute with median
spotify_tracks_unique$popularity[is.na(spotify_tracks_unique$popularity)] <- median(spotify_tracks_unique$popularity, na.rm = TRUE)
spotify_tracks_unique$duration_ms[is.na(spotify_tracks_unique$duration_ms)] <- median(spotify_tracks_unique$duration_ms, na.rm = TRUE)
spotify_tracks_unique$danceability[is.na(spotify_tracks_unique$danceability)] <- median(spotify_tracks_unique$danceability, na.rm = TRUE)
spotify_tracks_unique$energy[is.na(spotify_tracks_unique$energy)] <- median(spotify_tracks_unique$energy, na.rm = TRUE)
spotify_tracks_unique$loudness[is.na(spotify_tracks_unique$loudness)] <- median(spotify_tracks_unique$loudness, na.rm = TRUE)
spotify_tracks_unique$speechiness[is.na(spotify_tracks_unique$speechiness)] <- median(spotify_tracks_unique$speechiness, na.rm = TRUE)
spotify_tracks_unique$acousticness[is.na(spotify_tracks_unique$acousticness)] <- median(spotify_tracks_unique$acousticness, na.rm = TRUE)
spotify_tracks_unique$instrumentalness[is.na(spotify_tracks_unique$instrumentalness)] <- median(spotify_tracks_unique$instrumentalness, na.rm = TRUE)
spotify_tracks_unique$liveness[is.na(spotify_tracks_unique$liveness)] <- median(spotify_tracks_unique$liveness, na.rm = TRUE)
spotify_tracks_unique$valence[is.na(spotify_tracks_unique$valence)] <- median(spotify_tracks_unique$valence, na.rm = TRUE)
spotify_tracks_unique$tempo[is.na(spotify_tracks_unique$tempo)] <- median(spotify_tracks_unique$tempo, na.rm = TRUE)

# Impute with mode
count_mode_0 <- sum(spotify_tracks_unique$mode == 0, na.rm = TRUE)
count_mode_1 <- sum(spotify_tracks_unique$mode == 1, na.rm = TRUE)
count_mode_0 # 41070
count_mode_1 # 72314
spotify_tracks_unique$mode[is.na(spotify_tracks_unique$mode)] <- 1

count_false <- sum(spotify_tracks_unique$explicit == FALSE, na.rm = TRUE)
count_true <- sum(spotify_tracks_unique$explicit == TRUE, na.rm = TRUE)
count_false # 103698
count_true # 9704
spotify_tracks_unique$explicit[is.na(spotify_tracks_unique$explicit)] <- FALSE

table(spotify_tracks_unique$time_signature) # 4 - most (101328)
table(spotify_tracks_unique$key) # 7 - most (13187)

spotify_tracks_unique$time_signature[is.na(spotify_tracks_unique$time_signature)] <- 4
spotify_tracks_unique$key[is.na(spotify_tracks_unique$key)] <- 7
colSums(is.na(spotify_tracks_unique))

###############################################################
# (D) Data Validation
###############################################################
# (i) Check for Invalid Data
validation_summary_before = data.frame(
  Column = c("popularity", "danceability", "energy", "valence", "speechiness",
             "acousticness", "instrumentalness", "liveness", "loudness",
             "tempo", "duration_ms", "mode", "key", "time_signature"),
  Invalid_Count = c(
    sum(spotify_tracks_unique$popularity < 0 | spotify_tracks_unique$popularity > 100, na.rm = TRUE),
    sum(spotify_tracks_unique$danceability < 0 | spotify_tracks_unique$danceability > 1, na.rm = TRUE),
    sum(spotify_tracks_unique$energy < 0 | spotify_tracks_unique$energy > 1, na.rm = TRUE),
    sum(spotify_tracks_unique$valence < 0 | spotify_tracks_unique$valence > 1, na.rm = TRUE),
    sum(spotify_tracks_unique$speechiness < 0 | spotify_tracks_unique$speechiness > 1, na.rm = TRUE),
    sum(spotify_tracks_unique$acousticness < 0 | spotify_tracks_unique$acousticness > 1, na.rm = TRUE),
    sum(spotify_tracks_unique$instrumentalness < 0 | spotify_tracks_unique$instrumentalness > 1, na.rm = TRUE),
    sum(spotify_tracks_unique$liveness < 0 | spotify_tracks_unique$liveness > 1, na.rm = TRUE),
    sum(spotify_tracks_unique$loudness < -60 | spotify_tracks_unique$loudness > 0, na.rm = TRUE),
    sum(spotify_tracks_unique$tempo <= 0, na.rm = TRUE),
    sum(spotify_tracks_unique$duration_ms <= 0, na.rm = TRUE),
    sum(spotify_tracks_unique$mode != 0 & spotify_tracks_unique$mode != 1, na.rm = TRUE),
    sum(spotify_tracks_unique$key < 0 | spotify_tracks_unique$key > 11, na.rm = TRUE),
    sum(spotify_tracks_unique$time_signature < 3 | spotify_tracks_unique$time_signature > 7, na.rm = TRUE)
  )
)

validation_summary_before$Invalid_Percentage = round(
  validation_summary_before$Invalid_Count / nrow(spotify_tracks_unique) * 100, 2
)

View(validation_summary_before)


# (ii) Imputation for invalid values
# Impute with median
spotify_tracks_unique$popularity[spotify_tracks_unique$popularity < 0 | spotify_tracks_unique$popularity > 100] <- median(spotify_tracks_unique$popularity, na.rm = TRUE)
spotify_tracks_unique$danceability[spotify_tracks_unique$danceability < 0 | spotify_tracks_unique$danceability > 1] <- median(spotify_tracks_unique$danceability, na.rm = TRUE)
spotify_tracks_unique$energy[spotify_tracks_unique$energy < 0 | spotify_tracks_unique$energy > 1] <- median(spotify_tracks_unique$energy, na.rm = TRUE)
spotify_tracks_unique$valence[spotify_tracks_unique$valence < 0 | spotify_tracks_unique$valence > 1] <- median(spotify_tracks_unique$valence, na.rm = TRUE)
spotify_tracks_unique$speechiness[spotify_tracks_unique$speechiness < 0 | spotify_tracks_unique$speechiness > 1] <- median(spotify_tracks_unique$speechiness, na.rm = TRUE)
spotify_tracks_unique$acousticness[spotify_tracks_unique$acousticness < 0 | spotify_tracks_unique$acousticness > 1] <- median(spotify_tracks_unique$acousticness, na.rm = TRUE)
spotify_tracks_unique$instrumentalness[spotify_tracks_unique$instrumentalness < 0 | spotify_tracks_unique$instrumentalness > 1] <- median(spotify_tracks_unique$instrumentalness, na.rm = TRUE)
spotify_tracks_unique$liveness[spotify_tracks_unique$liveness < 0 | spotify_tracks_unique$liveness > 1] <- median(spotify_tracks_unique$liveness, na.rm = TRUE)
spotify_tracks_unique$loudness[spotify_tracks_unique$loudness < -60 | spotify_tracks_unique$loudness > 0] <- median(spotify_tracks_unique$loudness, na.rm = TRUE)
spotify_tracks_unique$tempo[spotify_tracks_unique$tempo <= 0] <- median(spotify_tracks_unique$tempo, na.rm = TRUE)
spotify_tracks_unique$duration_ms[spotify_tracks_unique$duration_ms <= 0] <- median(spotify_tracks_unique$duration_ms, na.rm = TRUE)

# Impute with mode
count_mode_0 <- sum(spotify_tracks_unique$mode == 0, na.rm = TRUE)
count_mode_1 <- sum(spotify_tracks_unique$mode == 1, na.rm = TRUE)
count_mode_0
count_mode_1 # mode = 1 is more common
spotify_tracks_unique$mode[spotify_tracks_unique$mode != 0 & spotify_tracks_unique$mode != 1] <- 1

spotify_tracks_unique$key[spotify_tracks_unique$key < 0 | spotify_tracks_unique$key > 11] <- 7          # 7 - most
spotify_tracks_unique$time_signature[spotify_tracks_unique$time_signature < 3 | spotify_tracks_unique$time_signature > 7] <- 4   # 4 - most


# Re-check for invalid values after imputation
validation_summary_after = data.frame(
  Column = c("popularity", "danceability", "energy", "valence", "speechiness",
             "acousticness", "instrumentalness", "liveness", "loudness",
             "tempo", "duration_ms", "mode", "key", "time_signature"),
  Invalid_Count = c(
    sum(spotify_tracks_unique$popularity < 0 | spotify_tracks_unique$popularity > 100, na.rm = TRUE),
    sum(spotify_tracks_unique$danceability < 0 | spotify_tracks_unique$danceability > 1, na.rm = TRUE),
    sum(spotify_tracks_unique$energy < 0 | spotify_tracks_unique$energy > 1, na.rm = TRUE),
    sum(spotify_tracks_unique$valence < 0 | spotify_tracks_unique$valence > 1, na.rm = TRUE),
    sum(spotify_tracks_unique$speechiness < 0 | spotify_tracks_unique$speechiness > 1, na.rm = TRUE),
    sum(spotify_tracks_unique$acousticness < 0 | spotify_tracks_unique$acousticness > 1, na.rm = TRUE),
    sum(spotify_tracks_unique$instrumentalness < 0 | spotify_tracks_unique$instrumentalness > 1, na.rm = TRUE),
    sum(spotify_tracks_unique$liveness < 0 | spotify_tracks_unique$liveness > 1, na.rm = TRUE),
    sum(spotify_tracks_unique$loudness < -60 | spotify_tracks_unique$loudness > 0, na.rm = TRUE),
    sum(spotify_tracks_unique$tempo <= 0, na.rm = TRUE),
    sum(spotify_tracks_unique$duration_ms <= 0, na.rm = TRUE),
    sum(spotify_tracks_unique$mode != 0 & spotify_tracks_unique$mode != 1, na.rm = TRUE),
    sum(spotify_tracks_unique$key < 0 | spotify_tracks_unique$key > 11, na.rm = TRUE),
    sum(spotify_tracks_unique$time_signature < 3 | spotify_tracks_unique$time_signature > 7, na.rm = TRUE)
  )
)

View(validation_summary_after)


###############################################################
# (E) Data Transformation
###############################################################
# (i) Convert duration from milliseconds to minutes
spotify_tracks_unique <- spotify_tracks_unique %>%
  mutate(duration_min = duration_ms / 60000)

# (ii) Categorize duration into groups (for Objective 2)
spotify_tracks_unique <- spotify_tracks_unique %>%
  mutate(duration_category = case_when(
    duration_min < 2.5 ~ "Short",
    duration_min >= 2.5 & duration_min < 4.5 ~ "Medium",
    duration_min >= 4.5 ~ "Long"
  ))

# (iii) Categorize tempo into groups (for Objective 2)
spotify_tracks_unique <- spotify_tracks_unique %>%
  mutate(tempo_category = case_when(
    tempo < 90 ~ "Slow",
    tempo >= 90 & tempo < 120 ~ "Moderate",
    tempo >= 120 ~ "Fast"
  ))

# For checking
table(spotify_tracks_unique$duration_category)
table(spotify_tracks_unique$tempo_category)


###############################################################
# (F) Data Exploration
###############################################################
library(ggplot2)
library(dplyr)

# (i) General exploration - structure & summary stats
str(spotify_tracks_unique)
summary(spotify_tracks_unique)

spotify_tracks_unique %>%
  summarise(
    avg_popularity   = mean(popularity, na.rm = TRUE),
    avg_danceability = mean(danceability, na.rm = TRUE),
    avg_energy       = mean(energy, na.rm = TRUE),
    avg_tempo        = mean(tempo, na.rm = TRUE),
    avg_duration_min = mean(duration_ms, na.rm = TRUE) / 60000
  )


# (ii) Distribution of variables - target variable
ggplot(spotify_tracks_unique, aes(x = popularity)) +
  geom_histogram(binwidth = 3, fill = "#4C72B0", color = "white") +
  labs(title = "Distribution of Track Popularity", x = "Popularity", y = "Frequency")
# TODO: skew direction, where most tracks concentrate


# (iii) Distribution of variables - numeric audio features
ggplot(spotify_tracks_unique, aes(x = danceability)) +
  geom_histogram(binwidth = 0.03, fill = "#DD8452", color = "white") +
  labs(title = "Distribution of Danceability", x = "Danceability", y = "Frequency")
# TODO: shape, peak location

ggplot(spotify_tracks_unique, aes(x = energy)) +
  geom_histogram(binwidth = 0.02, fill = "#55A868", color = "white") +
  labs(title = "Distribution of Energy", x = "Energy", y = "Frequency")
# TODO: shape, peak location

ggplot(spotify_tracks_unique, aes(x = loudness)) +
  geom_histogram(binwidth = 1, fill = "#8172B2", color = "white") +
  labs(title = "Distribution of Loudness", x = "Loudness (dB)", y = "Frequency")
# TODO: shape, typical loudness range

ggplot(spotify_tracks_unique, aes(x = tempo)) +
  geom_histogram(binwidth = 3, fill = "#64B5CD", color = "white") +
  labs(title = "Distribution of Tempo", x = "Tempo (BPM)", y = "Frequency")
# TODO: shape, typical tempo range

ggplot(spotify_tracks_unique, aes(x = valence)) +
  geom_histogram(binwidth = 0.02, fill = "#CCB974", color = "white") +
  labs(title = "Distribution of Valence", x = "Valence", y = "Frequency")
# TODO: shape - skewed toward happy/sad tracks?

ggplot(spotify_tracks_unique, aes(x = speechiness)) +
  geom_histogram(binwidth = 0.01, fill = "#937860", color = "white") +
  labs(title = "Distribution of Speechiness", x = "Speechiness", y = "Frequency")
# TODO: shape - most tracks low speechiness (music, not podcasts)?

ggplot(spotify_tracks_unique, aes(x = acousticness)) +
  geom_histogram(binwidth = 0.03, fill = "#DA8BC3", color = "white") +
  labs(title = "Distribution of Acousticness", x = "Acousticness", y = "Frequency")
# TODO: shape

ggplot(spotify_tracks_unique, aes(x = instrumentalness)) +
  geom_histogram(binwidth = 0.04, fill = "#8C8C8C", color = "white") +
  labs(title = "Distribution of Instrumentalness", x = "Instrumentalness", y = "Frequency")
# TODO: shape - many near-zero (vocal tracks)?

ggplot(spotify_tracks_unique, aes(x = liveness)) +
  geom_histogram(binwidth = 0.02, fill = "#CCB974", color = "white") +
  labs(title = "Distribution of Liveness", x = "Liveness", y = "Frequency")
# TODO: shape - spike suggesting live recordings?


# (iv) Distribution of variables - track duration
ggplot(spotify_tracks_unique, aes(x = duration_ms / 60000)) +
  geom_histogram(binwidth = 0.7, fill = "#55A868", color = "white") +
  labs(title = "Distribution of Track Duration", x = "Duration (minutes)", y = "Frequency")
# TODO: typical length range, long tail of outliers?


# (v) Categorical variables - bar charts
top_genres <- spotify_tracks_unique %>%
  count(track_genre, sort = TRUE) %>%
  head(10)

ggplot(top_genres, aes(x = reorder(track_genre, n), y = n)) +
  geom_bar(stat = "identity", fill = "#C44E52") +
  coord_flip() +
  labs(title = "Top 10 Track Genres by Count", x = "Genre", y = "Number of Tracks")
# TODO: highest/lowest genre count, even or dominated?

ggplot(spotify_tracks_unique, aes(x = explicit)) +
  geom_bar(fill = "#C44E52") +
  labs(title = "Explicit vs Non-Explicit Tracks", x = "Explicit", y = "Count")
# TODO: proportion split, roughly what % explicit

# factor(x) -> Treat x as categories, not continuous numerical number
ggplot(spotify_tracks_unique, aes(x = factor(mode))) +
  geom_bar(fill = "#4C72B0") +
  labs(title = "Mode Distribution (0 = Minor, 1 = Major)", x = "Mode", y = "Count")
# TODO: which mode dominates?

ggplot(spotify_tracks_unique, aes(x = factor(time_signature))) +
  geom_bar(fill = "#DD8452") +
  labs(title = "Time Signature Distribution", x = "Time Signature", y = "Count")
# TODO: most common time signature?

ggplot(spotify_tracks_unique, aes(x = factor(key))) +
  geom_bar(fill = "#8172B2") +
  labs(title = "Key Distribution", x = "Key (0-11)", y = "Count")
# TODO: which key is most/least common?

# (vi) Boxplots - spread & outlier check with IQR
# Popularity
ggplot(spotify_tracks_unique, aes(y = popularity)) +
  geom_boxplot(fill = "#8172B2") +
  labs(title = "Boxplot of Popularity", y = "Popularity")
# TODO: median, IQR, outlier nature

Q1_popularity <- quantile(spotify_tracks_unique$popularity, 0.25)
Q3_popularity <- quantile(spotify_tracks_unique$popularity, 0.75)
IQR_popularity <- Q3_popularity - Q1_popularity
Q1_popularity
Q3_popularity
IQR_popularity

spotify_tracks_unique %>%
  filter(popularity > Q3_popularity + 1.5*IQR_popularity) %>%
  select(track_name, popularity)
# TODO: decide whether to remove, or keep the outlier(s), and justify

# Duration (minutes)
ggplot(spotify_tracks_unique, aes(y = duration_ms / 60000)) +
  geom_boxplot(fill = "#55A868") +
  labs(title = "Boxplot of Track Duration (minutes)", y = "Duration (minutes)")
# TODO: outliers - unusually long/short tracks?

Q1_duration <- quantile(spotify_tracks_unique$duration_ms / 60000, 0.25)
Q3_duration <- quantile(spotify_tracks_unique$duration_ms / 60000, 0.75)
IQR_duration <- Q3_duration - Q1_duration
Q1_duration
Q3_duration
IQR_duration

spotify_tracks_unique %>%
  filter(duration_ms/60000 > Q3_duration + 1.5*IQR_duration) %>%
  select(track_name, duration_ms)
# TODO: decide whether to remove, or keep the outlier(s), and justify

# Popularity by Top 10 Genres
spotify_tracks_unique %>%
  filter(track_genre %in% top_genres$track_genre) %>%
  ggplot(aes(x = track_genre, y = popularity)) +
  geom_boxplot(fill = "#64B5CD") +
  coord_flip() + # flip the chart sideways
  labs(title = "Popularity by Top 10 Genres", x = "Genre", y = "Popularity")
# TODO: highest/lowest median popularity genre, spread differences


# (vii) Relationship between 2 variables - numeric vs numeric
ggplot(spotify_tracks_unique, aes(x = energy, y = loudness)) +
  geom_point(alpha = 0.3, color = "#64B5CD") +
  geom_smooth(method = "lm", color = "#C44E52") +
  labs(title = "Energy vs Loudness", x = "Energy", y = "Loudness (dB)")
# TODO: correlation direction/strength visible?

# Energy vs Loudness - density view (shows concentration, dark = few tracks, yellow = many tracks)
ggplot(spotify_tracks_unique, aes(x = energy, y = loudness)) +
  geom_hex(bins = 40) +
  scale_fill_viridis_c() +
  labs(title = "Energy vs Loudness (Density)", x = "Energy", y = "Loudness (dB)", fill = "Count") +
  theme_minimal()
# TODO: where is the darkest/most concentrated region? does it match the scatter's trend line?

# Energy vs Loudness (split by Mode)
ggplot(spotify_tracks_unique, aes(x = energy, y = loudness)) +
  geom_point(alpha = 0.2, color = "#64B5CD") +
  facet_wrap(~ factor(mode, labels = c("Minor", "Major"))) +
  labs(title = "Energy vs Loudness by Mode")
# TODO: does the relationship look similar for Minor vs Major, or different?


# (viii) Correlation matrix - numeric variables
numeric_vars <- spotify_tracks_unique %>%
  select(popularity, danceability, energy, loudness, speechiness,
         acousticness, instrumentalness, liveness, valence, tempo,
         duration_ms)

cor_matrix <- cor(numeric_vars, use = "complete.obs")
cor_matrix

# Table view - correlation of each variable with popularity, sorted highest to lowest
popularity_correlation = data.frame(
  Variable = c("danceability", "energy", "loudness", "speechiness",
               "acousticness", "instrumentalness", "liveness", "valence",
               "tempo", "duration_ms"),
  Correlation_with_Popularity = c(
    cor_matrix["popularity", "danceability"],
    cor_matrix["popularity", "energy"],
    cor_matrix["popularity", "loudness"],
    cor_matrix["popularity", "speechiness"],
    cor_matrix["popularity", "acousticness"],
    cor_matrix["popularity", "instrumentalness"],
    cor_matrix["popularity", "liveness"],
    cor_matrix["popularity", "valence"],
    cor_matrix["popularity", "tempo"],
    cor_matrix["popularity", "duration_ms"]
  )
)

popularity_correlation$Correlation_with_Popularity = round(popularity_correlation$Correlation_with_Popularity, 3)
popularity_correlation = popularity_correlation[order(-popularity_correlation$Correlation_with_Popularity), ]

View(popularity_correlation)
# TODO: which variables correlate most with popularity (both positive and negative)?
# +1: Perfect positive (One goes up, another one always goes up)
# 0: No relationship
# -1: Perfect negative (One goes up, another one always goes down)


###############################################################
# (G) Data Analysis
###############################################################
library(dplyr)
library(ggplot2)
library(tidyr)
###############################################################
# Objective 01: Chua Yexun
# To examine the relationship between genre and selected musical characteristics and Spotify track popularity.
###############################################################
# 1.1 Average popularity by genre
genre_stats <- spotify_tracks_unique %>%
  group_by(track_genre) %>%  
  # n() - calculate the number of tracks in each genre
  summarise(n = n(), mean_popularity = mean(popularity, na.rm = TRUE)) %>% 
  arrange(desc(mean_popularity))

top10_genre_stats <- head(genre_stats, 10)

ggplot(top10_genre_stats, aes(x = reorder(track_genre, mean_popularity), y = mean_popularity)) +
  # reorder() = sort by mean_popularity (default = alphabetically)
  geom_bar(stat = "identity", fill = "#4C72B0") +
  coord_flip() +
  labs(title = "Top 10 Genres by Average Popularity", x = "Genre", y = "Average Popularity")
# TODO: which genres have the highest/lowest average popularity

# Simple test to check whether mean popularity differs across genres
anova_genre <- aov(popularity ~ track_genre, data = spotify_tracks_unique)
summary(anova_genre)
# p < 0.05 → significant difference between at least some group means
# p ≥ 0.05 → insufficient evidence that the group means differ


# 1.2 Correlation + Scatterplot: musical characteristics vs popularity
cor.test(spotify_tracks_unique$danceability, spotify_tracks_unique$popularity, method = "pearson")

ggplot(spotify_tracks_unique, aes(x = danceability, y = popularity)) +
  geom_point(alpha = 0.2, color = "#4C72B0") +
  geom_smooth(method = "lm", color = "#C44E52") +
  labs(title = "Danceability vs Popularity", x = "Danceability", y = "Popularity")
# TODO: check the correlation number, then confirm visually with the trend line

cor.test(spotify_tracks_unique$energy, spotify_tracks_unique$popularity)

ggplot(spotify_tracks_unique, aes(x = energy, y = popularity)) +
  geom_point(alpha = 0.2, color = "#55A868") +
  geom_smooth(method = "lm", color = "#C44E52") +
  labs(title = "Energy vs Popularity", x = "Energy", y = "Popularity")
# TODO: check the correlation number, then confirm visually with the trend line


# 1.3 Popularity spread across top 10 genres
genre_subset <- spotify_tracks_unique %>%
  filter(track_genre %in% top10_genre_stats$track_genre)

ggplot(genre_subset, aes(x = reorder(track_genre, popularity, median), y = popularity)) +
  geom_boxplot(fill = "#64B5CD") +
  coord_flip() +
  labs(title = "Popularity Spread by Top 10 Genres", x = "Genre", y = "Popularity")
# middle line = median popularity per genre
# taller box = popularity varies more within that genre


# 1.4 Danceability vs Popularity, split by genre
top5_genre_names <- head(top10_genre_stats$track_genre, 5)

genre_subset %>%
  filter(track_genre %in% top5_genre_names) %>%
  ggplot(aes(x = danceability, y = popularity)) +
  geom_point(alpha = 0.2, color = "#4C72B0") +
  geom_smooth(method = "lm", color = "#C44E52") +
  facet_wrap(~ track_genre) +
  labs(title = "Danceability vs Popularity — Top 5 Genres", x = "Danceability", y = "Popularity")
# TODO: does the danceability-popularity trend line look the same across genres, or does it slope differently (or even flip direction) for some?


###############################################################
# Objective 02: Lim Chee Xuan
# To investigate the relationship between the track duration and temporal characteristic and Spotify track popularity.
###############################################################
# factor - to arrange in the order we want
spotify_tracks_unique <- spotify_tracks_unique %>%
  mutate(
    duration_category = factor(duration_category, levels = c("Short", "Medium", "Long")),
    tempo_category = factor(tempo_category, levels = c("Slow", "Moderate", "Fast"))
  )


# 2.1  Correlation + Scatterplot: duration vs popularity
cor.test(spotify_tracks_unique$duration_min, spotify_tracks_unique$popularity)

ggplot(spotify_tracks_unique, aes(x = duration_min, y = popularity)) +
  geom_point(alpha = 0.2, color = "#4C72B0") +
  geom_smooth(method = "lm", color = "#C44E52") +
  labs(title = "Track Duration vs Popularity", x = "Duration (minutes)", y = "Popularity")
# TODO: check the correlation number, then confirm visually with the trend line


# 2.2 Correlation + Scatterplot: tempo vs popularity
cor.test(spotify_tracks_unique$tempo, spotify_tracks_unique$popularity)

ggplot(spotify_tracks_unique, aes(x = tempo, y = popularity)) +
  geom_point(alpha = 0.2, color = "#64B5CD") +
  geom_smooth(method = "lm", color = "#C44E52") +
  labs(title = "Tempo vs Popularity", x = "Tempo (BPM)", y = "Popularity")
# TODO: check the correlation number, then confirm visually with the trend line


# 2.3 Descriptive stats + Bar chart: popularity by duration_category
spotify_tracks_unique %>%
  group_by(duration_category) %>%
  summarise(n = n(), mean_popularity = mean(popularity, na.rm = TRUE))

spotify_tracks_unique %>%
  group_by(duration_category) %>%
  summarise(mean_popularity = mean(popularity, na.rm = TRUE)) %>%
  ggplot(aes(x = duration_category, y = mean_popularity)) +
  geom_bar(stat = "identity", fill = "#55A868") +
  labs(title = "Average Popularity by Duration Category", x = "Duration Category", y = "Average Popularity")
# TODO: do Short/Medium/Long tracks differ in average popularity?

# check if the duration groups are REALLY different, not just by eye
anova_duration <- aov(popularity ~ duration_category, data = spotify_tracks_unique)
summary(anova_duration)
# p < 0.05 → significant difference between at least some group means
# p ≥ 0.05 → insufficient evidence that the group means differ
# duration_min correlation in 2.1 is close to 0, but this test
# will still be significant - meaning "Medium" length tracks do better
# than both "Short" and "Long", it's just not a straight line.


# 2.4 Descriptive stats + Bar chart: popularity by tempo_category
spotify_tracks_unique %>%
  group_by(tempo_category) %>%
  summarise(n = n(), mean_popularity = mean(popularity, na.rm = TRUE))

spotify_tracks_unique %>%
  group_by(tempo_category) %>%
  summarise(mean_popularity = mean(popularity, na.rm = TRUE)) %>%
  ggplot(aes(x = tempo_category, y = mean_popularity)) +
  geom_bar(stat = "identity", fill = "#DD8452") +
  labs(title = "Average Popularity by Tempo Category", x = "Tempo Category", y = "Average Popularity")
# TODO: do Slow/Moderate/Fast tracks differ in average popularity?

anova_tempo <- aov(popularity ~ tempo_category, data = spotify_tracks_unique)
summary(anova_tempo)
# p < 0.05 → significant difference between at least some group means
# p ≥ 0.05 → insufficient evidence that the group means differ

anova_interaction <- aov(popularity ~ duration_category * tempo_category,
                         data = spotify_tracks_unique)
summary(anova_interaction)


###############################################################
# Objective 03: Paureen Tan Nie Nie
# To determine the relationship between the artist-related and track-related characteristics and Spotify track popularity.
###############################################################
# 3.1 Descriptive stats + Bar chart: top 10 artists by average popularity
artist_stats <- spotify_tracks_unique %>%
  separate_rows(artists, sep = ";\\s*|,\\s*") %>%   # handles both ; and , separators
  group_by(artists) %>%
  summarise(
    n_tracks = n(),
    avg_popularity = mean(popularity, na.rm = TRUE),
    sd_popularity = sd(popularity, na.rm = TRUE),
    median_popularity = median(popularity, na.rm = TRUE)
  ) %>%
  filter(n_tracks >= 5) %>%
  arrange(desc(avg_popularity))

top10_artists <- head(artist_stats, 10)

ggplot(top10_artists, aes(x = reorder(artists, avg_popularity), y = avg_popularity)) +
  geom_bar(stat = "identity", fill = "#8172B2") +
  geom_errorbar(aes(ymin = avg_popularity - sd_popularity, ymax = avg_popularity + sd_popularity),
                width = 0.3, color = "black") +
  coord_flip() +
  labs(
    title = "Top 10 Artists by Average Popularity (min. 5 tracks, collabs credited)",
    subtitle = "Error bars show \u00b11 SD",
    x = "Artist", y = "Average Popularity"
  ) +
  theme_minimal()
# TODO: which artists have the highest average popularity
# SD error bars show whether the artist is consistently popular or has mixed tracks


# 3.2 Comparison + Bar chart: explicit vs non-explicit tracks
explicit_stats <- spotify_tracks_unique %>%
  group_by(explicit) %>%
  summarise(n = n(), mean_popularity = mean(popularity, na.rm = TRUE), sd_popularity = sd(popularity, na.rm = TRUE))

t_explicit <- t.test(popularity ~ explicit, data = spotify_tracks_unique)
t_explicit

# Cohen's d (effect size) — how big the gap is, not just whether it's real
pooled_sd <- sqrt(((explicit_stats$n[1]-1)*explicit_stats$sd_popularity[1]^2 +
                     (explicit_stats$n[2]-1)*explicit_stats$sd_popularity[2]^2) /
                    (sum(explicit_stats$n) - 2))
cohens_d_explicit <- diff(explicit_stats$mean_popularity) / pooled_sd
cohens_d_explicit
# |d| ~0.2 = small, ~0.5 = medium, ~0.8 = large effect (Cohen's convention)

# TODO: t-test p-value < 0.05 -> difference is statistically significant,
# not due to random chance. Check mean values to see which group is higher and by how much.


# 3.3 Comparison + Bar chart: major vs minor mode
mode_stats <- spotify_tracks_unique %>%
  group_by(mode) %>%
  summarise(n = n(), mean_popularity = mean(popularity, na.rm = TRUE),
            sd_popularity = sd(popularity, na.rm = TRUE))

t_mode <- t.test(popularity ~ mode, data = spotify_tracks_unique)
t_mode

pooled_sd_mode <- sqrt(((mode_stats$n[1]-1)*mode_stats$sd_popularity[1]^2 +
                          (mode_stats$n[2]-1)*mode_stats$sd_popularity[2]^2) /
                         (sum(mode_stats$n) - 2))
cohens_d_mode <- diff(mode_stats$mean_popularity) / pooled_sd_mode
cohens_d_mode
# |d| ~0.2 = small, ~0.5 = medium, ~0.8 = large effect

# TODO: t-test p-value < 0.05 -> difference is statistically significant.
# Major mode (1) = brighter/happier sound; Minor mode (0) = darker/sadder sound.
# Check which mode has higher average popularity.


###############################################################
# Objective 04: Phang Shea Wen
# To discover the relationship between selected perceptual audio characteristics and Spotify track popularity.
###############################################################
library(tidyr)

# 4.1 Correlation table + Bar chart: perceptual features vs popularity
perceptual_vars <- c("danceability", "energy", "loudness", "valence",
                     "acousticness", "instrumentalness", "liveness", "speechiness")

perceptual_corr <- data.frame(
  Feature = perceptual_vars,
  Correlation_with_Popularity = sapply(perceptual_vars, function(v) {
    cor(spotify_tracks_unique[[v]], spotify_tracks_unique$popularity, use = "complete.obs")
  }),
  P_Value = sapply(perceptual_vars, function(v) {
    cor.test(spotify_tracks_unique[[v]], spotify_tracks_unique$popularity)$p.value
  })
)
perceptual_corr$Correlation_with_Popularity <- round(perceptual_corr$Correlation_with_Popularity, 3)
perceptual_corr$P_Value <- round(perceptual_corr$P_Value, 4)
perceptual_corr$Significant <- ifelse(perceptual_corr$P_Value < 0.05, "Yes", "No")
perceptual_corr <- perceptual_corr[order(-perceptual_corr$Correlation_with_Popularity), ]
perceptual_corr

ggplot(perceptual_corr, aes(x = reorder(Feature, Correlation_with_Popularity),
                            y = Correlation_with_Popularity,
                            fill = Correlation_with_Popularity > 0)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Correlation_with_Popularity),
            hjust = ifelse(perceptual_corr$Correlation_with_Popularity > 0, -0.1, 1.1), size = 3.5) +
  scale_fill_manual(values = c("TRUE" = "#55A868", "FALSE" = "#C44E52"),
                    labels = c("Negative", "Positive"), name = "Direction") +
  coord_flip() +
  labs(title = "Correlation of Perceptual Features with Popularity",
       subtitle = "Green = positive relationship, Red = negative relationship",
       x = "Feature", y = "Correlation (r)") +
  theme_minimal()

# TODO: ranks every perceptual feature by how strongly it
# relates to popularity. Green bars = higher feature -> more popular.
# Red bars = higher feature -> less popular.


## 4.2 Scatterplot: strongest-correlated feature vs popularity

# Identify strongest feature automatically from perceptual_corr
strongest_feature <- perceptual_corr$Feature[which.max(abs(perceptual_corr$Correlation_with_Popularity))]
cat("Strongest correlated feature:", strongest_feature, "\n")   # NEW: prints which feature

# Scatterplot for strongest feature
ggplot(spotify_tracks_unique, aes(x = .data[[strongest_feature]], y = popularity)) +
  geom_point(alpha = 0.2, color = "#64B5CD") +
  geom_smooth(method = "lm", color = "#C44E52") +
  labs(
    title    = paste(strongest_feature, "vs Popularity"),
    subtitle = paste("r =", perceptual_corr$Correlation_with_Popularity[
      perceptual_corr$Feature == strongest_feature]),
    x        = strongest_feature,
    y        = "Popularity"
  ) +
  theme_minimal()

weakest_feature <- perceptual_corr$Feature[which.min(abs(perceptual_corr$Correlation_with_Popularity))]
cat("Weakest correlated feature:", weakest_feature, "\n")

ggplot(spotify_tracks_unique, aes(x = .data[[weakest_feature]], y = popularity)) +
  geom_point(alpha = 0.2, color = "#8172B2") +
  geom_smooth(method = "lm", color = "#C44E52") +
  labs(
    title    = paste(weakest_feature, "vs Popularity (Weakest Correlation)"),
    subtitle = paste("r =", perceptual_corr$Correlation_with_Popularity[
      perceptual_corr$Feature == weakest_feature]),
    x        = weakest_feature,
    y        = "Popularity"
  ) +
  theme_minimal()
# TODO: visually confirms the correlation direction & strength.
# Steep trend line = stronger relationship, flat line = weak/no relationship.


# 4.3 High vs Low popularity group comparison + Bar chart
p25 <- quantile(spotify_tracks_unique$popularity, 0.25)
p75 <- quantile(spotify_tracks_unique$popularity, 0.75)

grouped <- spotify_tracks_unique %>%
  filter(popularity <= p25 | popularity >= p75) %>%
  mutate(pop_group = ifelse(popularity >= p75, "High Popularity", "Low Popularity"))

group_compare <- grouped %>%
  group_by(pop_group) %>%
  summarise(
    n = n(),
    mean_danceability = mean(danceability),
    mean_energy = mean(energy),
    mean_valence = mean(valence),
    mean_acousticness = mean(acousticness),
    mean_instrumentalness = mean(instrumentalness),
    mean_loudness = mean(loudness),
    mean_liveness = mean(liveness)
  )
group_compare

features_to_test <- c("danceability", "energy", "valence", "acousticness", "instrumentalness", "loudness", "liveness")

ttest_results <- data.frame(
  Feature = features_to_test,
  P_Value = sapply(features_to_test, function(f) t.test(grouped[[f]] ~ grouped$pop_group)$p.value),
  Mean_High = sapply(features_to_test, function(f) round(mean(grouped[[f]][grouped$pop_group == "High Popularity"]), 3)),
  Mean_Low = sapply(features_to_test, function(f) round(mean(grouped[[f]][grouped$pop_group == "Low Popularity"]), 3))
)
ttest_results$Significant <- ifelse(ttest_results$P_Value < 0.05, "Yes", "No")
ttest_results$P_Value <- round(ttest_results$P_Value, 4)
ttest_results

# FIX: pivot_longer() needs the "tidyr" package - add this ONE line
# near the top of your script (with your other library() lines)
# library(tidyr)
group_compare_long <- group_compare %>%
  select(-n) %>%
  pivot_longer(cols = -pop_group, names_to = "feature", values_to = "avg_value") %>%
  mutate(feature = gsub("mean_", "", feature))

ggplot(group_compare_long, aes(x = feature, y = avg_value, fill = pop_group)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.85) +
  geom_text(aes(label = round(avg_value, 2)), position = position_dodge(width = 0.9), vjust = -0.3, size = 3) +
  scale_fill_manual(values = c("High Popularity" = "#55A868", "Low Popularity" = "#C44E52")) +
  coord_flip() +
  labs(title = "High vs Low Popularity: Average Feature Values",
       subtitle = "Comparing top 25% vs bottom 25% popularity tracks",
       x = "Feature", y = "Average Value", fill = "Group") +
  theme_minimal()

# TODO: side-by-side bars per feature. A clear height difference
# between green (High) and red (Low) = that feature matters for popularity.
# Bars roughly same height = feature has little impact on popularity.
