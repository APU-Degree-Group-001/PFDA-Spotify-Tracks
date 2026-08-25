
###############################################################
# (A) Import CSV
###############################################################

getwd() #check current working directory
setwd("E:/Academics/APU/APD2F2606CS(AI)/Semester 1/Programming for Data Analysis/Assignment/CT127-3-2-PFDA Group 01/dataset")
setwd("D:/APU/Degree/APD2F2606CS(AI)/SEM 1/Programming for Data Analysis/Assignment/dataset") # For Paureen

# install packages (one-time job)
# library gone when power off, so need to load library at least one time in the script
# install.packages("data.table") #fread() - fast csv import

# install.packages("dplyr")
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


# (v) Data Imputation
rows_before_missing <- nrow(spotify_tracks_unique)

spotify_tracks_unique <- spotify_tracks_unique %>% na.omit()

rows_after_missing <- nrow(spotify_tracks_unique)
percentage_removed_missing <- round((rows_before_missing - rows_after_missing) / rows_before_missing * 100, 2)
percentage_removed_missing


# (vi) Data Validation - check for out-of-range / invalid values
validation_summary = data.frame(
  Column = c("popularity", "danceability", "energy", "valence", "speechiness",
             "acousticness", "instrumentalness", "liveness", "loudness",
             "tempo", "duration_ms"),
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
    sum(spotify_tracks_unique$duration_ms <= 0, na.rm = TRUE)
  )
)

validation_summary$Invalid_Percentage = round(
  validation_summary$Invalid_Count / nrow(spotify_tracks_unique) * 100, 2
)

View(validation_summary)


# (vi) Data Validation - remove rows with invalid values
rows_before_validation <- nrow(spotify_tracks_unique)

spotify_tracks_unique <- spotify_tracks_unique %>%
  filter(
    popularity >= 0 & popularity <= 100,
    danceability >= 0 & danceability <= 1,
    energy >= 0 & energy <= 1,
    valence >= 0 & valence <= 1,
    speechiness >= 0 & speechiness <= 1,
    acousticness >= 0 & acousticness <= 1,
    instrumentalness >= 0 & instrumentalness <= 1,
    liveness >= 0 & liveness <= 1,
    loudness >= -60 & loudness <= 0,
    tempo > 0,
    duration_ms > 0
  )

rows_after_validation <- nrow(spotify_tracks_unique)
rows_removed_validation <- rows_before_validation - rows_after_validation
percentage_removed_validation <- round(rows_removed_validation / rows_before_validation * 100, 2)

rows_removed_validation
percentage_removed_validation    # 2.47% of rows removed


###############################################################
# (D) Data Exploration
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
  geom_histogram(binwidth = 5, fill = "#4C72B0", color = "white") +
  labs(title = "Distribution of Track Popularity", x = "Popularity", y = "Frequency")
# TODO: skew direction, where most tracks concentrate


# (iii) Distribution of variables - numeric audio features
ggplot(spotify_tracks_unique, aes(x = danceability)) +
  geom_histogram(binwidth = 0.05, fill = "#DD8452", color = "white") +
  labs(title = "Distribution of Danceability", x = "Danceability", y = "Frequency")
# TODO: shape, peak location

ggplot(spotify_tracks_unique, aes(x = energy)) +
  geom_histogram(binwidth = 0.05, fill = "#55A868", color = "white") +
  labs(title = "Distribution of Energy", x = "Energy", y = "Frequency")
# TODO: shape, peak location

ggplot(spotify_tracks_unique, aes(x = loudness)) +
  geom_histogram(binwidth = 2, fill = "#8172B2", color = "white") +
  labs(title = "Distribution of Loudness", x = "Loudness (dB)", y = "Frequency")
# TODO: shape, typical loudness range

ggplot(spotify_tracks_unique, aes(x = tempo)) +
  geom_histogram(binwidth = 5, fill = "#64B5CD", color = "white") +
  labs(title = "Distribution of Tempo", x = "Tempo (BPM)", y = "Frequency")
# TODO: shape, typical tempo range

ggplot(spotify_tracks_unique, aes(x = valence)) +
  geom_histogram(binwidth = 0.05, fill = "#CCB974", color = "white") +
  labs(title = "Distribution of Valence", x = "Valence", y = "Frequency")
# TODO: shape - skewed toward happy/sad tracks?

ggplot(spotify_tracks_unique, aes(x = speechiness)) +
  geom_histogram(binwidth = 0.02, fill = "#937860", color = "white") +
  labs(title = "Distribution of Speechiness", x = "Speechiness", y = "Frequency")
# TODO: shape - most tracks low speechiness (music, not podcasts)?

ggplot(spotify_tracks_unique, aes(x = acousticness)) +
  geom_histogram(binwidth = 0.05, fill = "#DA8BC3", color = "white") +
  labs(title = "Distribution of Acousticness", x = "Acousticness", y = "Frequency")
# TODO: shape

ggplot(spotify_tracks_unique, aes(x = instrumentalness)) +
  geom_histogram(binwidth = 0.05, fill = "#8C8C8C", color = "white") +
  labs(title = "Distribution of Instrumentalness", x = "Instrumentalness", y = "Frequency")
# TODO: shape - many near-zero (vocal tracks)?

ggplot(spotify_tracks_unique, aes(x = liveness)) +
  geom_histogram(binwidth = 0.05, fill = "#CCB974", color = "white") +
  labs(title = "Distribution of Liveness", x = "Liveness", y = "Frequency")
# TODO: shape - spike suggesting live recordings?


# (iv) Distribution of variables - track duration
ggplot(spotify_tracks_unique, aes(x = duration_ms / 60000)) +
  geom_histogram(binwidth = 0.5, fill = "#55A868", color = "white") +
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

ggplot(spotify_tracks_unique, aes(x = factor(mode))) +
  geom_bar(fill = "#4C72B0") +
  labs(title = "Mode Distribution (0 = Minor, 1 = Major)", x = "Mode", y = "Count")
# TODO: which mode dominates?

ggplot(spotify_tracks_unique, aes(x = factor(time_signature))) +
  geom_bar(fill = "#DD8452") +
  labs(title = "Time Signature Distribution", x = "Time Signature", y = "Count")
# TODO: most common time signature?


# (vi) Boxplots - spread & outlier check
ggplot(spotify_tracks_unique, aes(y = popularity)) +
  geom_boxplot(fill = "#8172B2") +
  labs(title = "Boxplot of Popularity")
# TODO: median, IQR, outlier nature

ggplot(spotify_tracks_unique, aes(y = duration_ms / 60000)) +
  geom_boxplot(fill = "#55A868") +
  labs(title = "Boxplot of Track Duration (minutes)")
# TODO: outliers - unusually long/short tracks?

# filter to top 10 genres
spotify_tracks_unique %>%
  filter(track_genre %in% top_genres$track_genre) %>%
  ggplot(aes(x = track_genre, y = popularity)) +
  geom_boxplot(fill = "#64B5CD") +
  coord_flip() +
  labs(title = "Popularity by Top 10 Genres", x = "Genre", y = "Popularity")
# TODO: highest/lowest median popularity genre, spread differences


# (vii) Relationship between 2 variables - numeric vs numeric
ggplot(spotify_tracks_unique, aes(x = energy, y = loudness)) +
  geom_point(alpha = 0.3, color = "#64B5CD") +
  labs(title = "Energy vs Loudness", x = "Energy", y = "Loudness (dB)")
# TODO: correlation direction/strength visible?


# (viii) Correlation matrix - numeric variables
numeric_vars <- spotify_tracks_unique %>%
  select(popularity, danceability, energy, loudness, speechiness,
         acousticness, instrumentalness, liveness, valence, tempo,
         duration_ms)

cor_matrix <- cor(numeric_vars, use = "complete.obs")
cor_matrix
# TODO: which variables correlate most with popularity?
# +1: Perfect positive (One goes up, another one always goes up)
# 0: No relationship
# -1: Perfect negative (One goes up, another one always goes down)