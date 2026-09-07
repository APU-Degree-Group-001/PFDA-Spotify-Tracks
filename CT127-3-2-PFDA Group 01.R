
###############################################################
# (A) Import CSV
###############################################################

getwd() # check current working directory

# For Shea Wen
setwd("E:/Academics/APU/APD2F2606CS(AI)/Semester 1/Programming for Data Analysis/Assignment/CT127-3-2-PFDA Group 01/dataset")
# For Paureen
setwd("D:/APU/Degree/APD2F2606CS(AI)/SEM 1/Programming for Data Analysis/Assignment/dataset")
# For Chee Xuan
setwd("C:/Users/ASUS/OneDrive/Documents/APU/SEM 1 PFDA Programming for Data Analysis/PFDA Assignment/Assignment Data Set")

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


# Brief exploration
head(spotify_tracks)
tail(spotify_tracks)
str(spotify_tracks)
names(spotify_tracks)
# summary(spotify_tracks$Price)
summary(spotify_tracks) # get summary for entire dataset
View(spotify_tracks)



###############################################################
# (B) Data Cleaning
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

# is.na() only catches true NA, not empty strings
spotify_tracks_unique$artists[trimws(spotify_tracks_unique$artists) == ""] <- NA
spotify_tracks_unique$album_name[trimws(spotify_tracks_unique$album_name) == ""] <- NA
spotify_tracks_unique$track_name[trimws(spotify_tracks_unique$track_name) == ""] <- NA
spotify_tracks_unique$track_genre[trimws(spotify_tracks_unique$track_genre) == ""] <- NA

colSums(is.na(spotify_tracks_unique))

# - put in dataframe > better view of No. of N/A & (%)
missing_values_summary = data.frame(
  Column = names(spotify_tracks_unique),
  Missing_Values_Count = sapply(spotify_tracks_unique, function(col) {
    if (is.character(col)) sum(is.na(col) | trimws(col) == "") else sum(is.na(col))
  })
)
missing_values_summary$Missing_Percentage = round(
  missing_values_summary$Missing_Values_Count / nrow(spotify_tracks_unique) * 100, 2
)

View(missing_values_summary)


# (iv) Calculate no. of rows with 1 or more null values (not specific to which column)
rows_with_missing_values = sum(!complete.cases(spotify_tracks_unique))
rows_with_missing_values
rows_missing_percentage = (rows_with_missing_values / nrow(spotify_tracks_unique)) * 100
rows_missing_percentage
# 3.067674% rows with missing values


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

# (vi) Remove Data
nrow_before <- nrow(spotify_tracks_unique)

spotify_tracks_unique <- na.omit(spotify_tracks_unique)
colSums(is.na(spotify_tracks_unique))

nrow_after <- nrow(spotify_tracks_unique)
nrow_before - nrow_after
round((nrow_before - nrow_after) / nrow_before * 100, 3)


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
# mode = 1 is more common (Based on (C)(v))
spotify_tracks_unique$mode[spotify_tracks_unique$mode != 0 & spotify_tracks_unique$mode != 1] <- 1
# key = 7 is more common (Based on (C)(v))
spotify_tracks_unique$key[spotify_tracks_unique$key < 0 | spotify_tracks_unique$key > 11] <- 7
# time_signature = 4 is more common (Based on (C)(v))
spotify_tracks_unique$time_signature[spotify_tracks_unique$time_signature < 3 | spotify_tracks_unique$time_signature > 7] <- 4

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

# (ii) Outlier exploration
# (a) Popularity - Boxplot and IQR
ggplot(spotify_tracks_unique, aes(y = popularity)) +
  geom_boxplot(fill = "#8172B2") +
  labs(title = "Boxplot of Popularity", y = "Popularity")

Q1_popularity <- quantile(spotify_tracks_unique$popularity, 0.25)
Q3_popularity <- quantile(spotify_tracks_unique$popularity, 0.75)
IQR_popularity <- Q3_popularity - Q1_popularity
Q1_popularity
Q3_popularity
IQR_popularity

spotify_tracks_unique %>%
  filter(popularity > Q3_popularity + 1.5*IQR_popularity) %>%
  select(track_name, popularity)
# The popularity=100 point is a statistical outlier 
# but represents a valid maximum score, so it was retained.

# (b) Duration (minutes) - Boxplot and IQR
ggplot(spotify_tracks_unique, aes(y = duration_ms / 60000)) +
  geom_boxplot(fill = "#55A868") +
  labs(title = "Boxplot of Track Duration (minutes)", y = "Duration (minutes)")

Q1_duration <- quantile(spotify_tracks_unique$duration_ms / 60000, 0.25)
Q3_duration <- quantile(spotify_tracks_unique$duration_ms / 60000, 0.75)
IQR_duration <- Q3_duration - Q1_duration
Q1_duration
Q3_duration
IQR_duration

spotify_tracks_unique %>%
  filter(duration_ms/60000 > Q3_duration + 1.5*IQR_duration) %>%
  arrange(desc(duration_ms)) %>%
  select(track_name, artists, track_genre, duration_ms) %>%
  head(15)
# Most tracks are 2.9 to 4.36 minutes
# The most extreme duration outliers belong to 'Continuous DJ Mix' tracks and sleep/ambient/white-noise genres
# where multi-hour length is a defining characteristic of the genre
# so the outliers are retained

# (iii) Table view - correlation of each variable with popularity, sorted highest to lowest
# Correlation matrix - numeric variables
numeric_vars <- spotify_tracks_unique %>%
  select(popularity, danceability, energy, loudness, speechiness,
         acousticness, instrumentalness, liveness, valence, tempo,
         duration_ms)

cor_matrix <- cor(numeric_vars, use = "complete.obs")

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
# +1: Perfect positive (One goes up, another one always goes up)
# 0: No relationship
# -1: Perfect negative (One goes up, another one always goes down)
# PSW: This correlation table shows the relationship between each numeric variable and popularity. 
# - All correlations are negligible (well below the ~0.1 threshold typically considered even "weak"), 
# - indicating none of these variables have a meaningful linear relationship with popularity individually
# - Loudness (0.048) and danceability (0.034) show the slightly strongest positive associations
# - instrumentalness (-0.094) and speechiness (-0.045) show the slightly strongest negative associations
# - Overall: none stand out as a meaningful individual predictor of popularity


###############################################################
# Objective 01: Chua Yexun
# Explore Genre & Musical Characteristics
# Variables: track_genre, explicit, key, mode, speechiness, acousticness, instrumentalness, liveness, popularity
###############################################################
# 1.1 Genre Distribution (Categorical variables - bar charts)
top_genres <- spotify_tracks_unique %>%
  count(track_genre, sort = TRUE) %>%
  head(10)

ggplot(top_genres, aes(x = reorder(track_genre, n), y = n)) +
  geom_bar(stat = "identity", fill = "#C44E52") +
  coord_flip() +
  labs(title = "Top 10 Track Genres by Count", x = "Genre", y = "Number of Tracks")
# TODO: highest/lowest genre count, even or dominated?

# 1.2 Genre vs Popularity - Popularity by Top 10 Genres
spotify_tracks_unique %>%
  filter(track_genre %in% top_genres$track_genre) %>%
  ggplot(aes(x = track_genre, y = popularity)) +
  geom_boxplot(fill = "#64B5CD") +
  coord_flip() + # flip the chart sideways
  labs(title = "Popularity by Top 10 Genres", x = "Genre", y = "Popularity")
# TODO: highest/lowest median popularity genre, spread differences

# 1.3 Explicit vs Popularity
spotify_tracks_unique %>% count(explicit)

ggplot(spotify_tracks_unique,
       aes(x = factor(explicit), y = popularity)) +
  geom_boxplot() +
  labs(title = "Popularity by Explicit Content", x = "Explicit", y = "Popularity")

# 1.4 Key vs Popularity
spotify_tracks_unique %>% count(key)

ggplot(spotify_tracks_unique,
       aes(x = factor(key), y = popularity)) +
  geom_boxplot() +
  labs(title = "Popularity by Key", x = "Key", y = "Popularity")

# 1.5 Mode vs Popularity
spotify_tracks_unique %>% count(mode)

ggplot(spotify_tracks_unique,
       aes(x = factor(mode), y = popularity)) +
  geom_boxplot() +
  labs(title = "Popularity by Mode", x = "Mode (0 = Minor, 1 = Major)", y = "Popularity")

# 1.6 Continuous audio characteristics - speechiness, acousticness, instrumentalness, liveness
ggplot(spotify_tracks_unique,
       aes(x = speechiness, y = popularity)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm") +
  labs(
    title = "Speechiness vs Popularity",
    x = "Speechiness",
    y = "Popularity"
  )

ggplot(spotify_tracks_unique,
       aes(x = acousticness, y = popularity)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm") +
  labs(
    title = "Acousticness vs Popularity",
    x = "Acousticness",
    y = "Popularity"
  )

ggplot(spotify_tracks_unique,
       aes(x = instrumentalness, y = popularity)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm") +
  labs(
    title = "Instrumentalness vs Popularity",
    x = "Inatrumentalness",
    y = "Popularity"
  )

ggplot(spotify_tracks_unique,
       aes(x = liveness, y = popularity)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm") +
  labs(
    title = "Liveness vs Popularity",
    x = "Liveness",
    y = "Popularity"
  )

###############################################################
# Objective 02: Lim Chee Xuan
# Explore Duration & Temporal Characteristics
# Variables: duration_ms, tempo, time_signature, popularity
###############################################################
# 2.1 Track Duration Distribution
ggplot(spotify_tracks_unique, aes(x = duration_ms / 60000)) +
  geom_histogram(binwidth = 0.7, fill = "#55A868", color = "white") +
  labs(title = "Distribution of Track Duration", x = "Duration (minutes)", y = "Frequency")
# TODO: typical length range, long tail of outliers?

# 2.2 Duration vs Popularity
ggplot(spotify_tracks_unique,
       aes(x = duration_ms / 60000, y = popularity)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm") +
  labs(
    title = "Track Duration vs Popularity",
    x = "Duration (minutes)",
    y = "Popularity"
  )

# 2.3 Temporal Characteristics - Tempo vs Popularity
ggplot(spotify_tracks_unique,
       aes(x = tempo, y = popularity)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm") +
  labs(
    title = "Tempo vs Popularity",
    x = "Tempo (BPM)",
    y = "Popularity"
  )

# 2.4 Temporal characteristics - Time Signature vs Popularity
spotify_tracks_unique %>% count(time_signature)

ggplot(spotify_tracks_unique,
       aes(x = factor(time_signature), y = popularity)) +
  geom_boxplot() +
  labs(title = "Popularity by Time Signature", x = "Time Signature", y = "Popularity")

###############################################################
# Objective 03: Paureen Tan Nie Nie
# Artist & Track Characteristics
# Variables: artists, album_name, track_name, popularity
###############################################################
# 3.1 Number of tracks by artist
top_artists <- spotify_tracks_unique %>%
  count(artists, sort = TRUE) %>%
  head(10)

ggplot(top_artists, aes(x = reorder(artists, n), y = n)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Top 10 Artists by Number of Tracks",
    x = "Artist",
    y = "Number of Tracks"
  )

# 3.2 Artist vs Popularity
spotify_tracks_unique %>%
  filter(artists %in% top_artists$artists) %>%
  ggplot(aes(x = artists, y = popularity)) +
  geom_boxplot() +
  coord_flip() +
  labs(
    title = "Popularity by Top 10 Artists",
    x = "Artist",
    y = "Popularity"
  )

# 3.3 Track Name and Album Name vs Popularity
spotify_tracks_unique %>%
  arrange(desc(popularity)) %>%
  select(track_name, artists, album_name, popularity) %>%
  head(10)

###############################################################
# Objective 04: Phang Shea Wen
# Perceptual Audio Characteristics
# Variables: danceability, energy, loudness, valence, popularity
###############################################################
# 4.1 Distributions 0 danceability, energy, loudness, valence
ggplot(spotify_tracks_unique, aes(x = danceability)) +
  geom_histogram(binwidth = 0.03, fill = "#DD8452", color = "white") +
  labs(title = "Distribution of Danceability", x = "Danceability", y = "Frequency")
# TODO: shape, peak location
# PSW:
# - Left-skewed (negatively skewed) with majority concentrated between 0.5 and 0.75
# - Indicating that most tracks have moderate to relatively high danceability

ggplot(spotify_tracks_unique, aes(x = energy)) +
  geom_histogram(binwidth = 0.02, fill = "#55A868", color = "white") +
  labs(title = "Distribution of Energy", x = "Energy", y = "Frequency")
# TODO: shape, peak location

ggplot(spotify_tracks_unique, aes(x = loudness)) +
  geom_histogram(binwidth = 1, fill = "#8172B2", color = "white") +
  labs(title = "Distribution of Loudness", x = "Loudness (dB)", y = "Frequency")
# TODO: shape, typical loudness range

ggplot(spotify_tracks_unique, aes(x = valence)) +
  geom_histogram(binwidth = 0.02, fill = "#CCB974", color = "white") +
  labs(title = "Distribution of Valence", x = "Valence", y = "Frequency")
# TODO: shape - skewed toward happy/sad tracks?

# 4.2 Relationship with Popularity
ggplot(spotify_tracks_unique,
       aes(x = danceability, y = popularity)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm") +
  labs(
    title = "Danceability vs Popularity",
    x = "Danceability",
    y = "Popularity"
  )

ggplot(spotify_tracks_unique,
       aes(x = energy, y = popularity)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm") +
  labs(
    title = "Energy vs Popularity",
    x = "Energy",
    y = "Popularity"
  )

ggplot(spotify_tracks_unique,
       aes(x = loudness, y = popularity)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm") +
  labs(
    title = "Loudness vs Popularity",
    x = "Loudness (dB)",
    y = "Popularity"
  )

ggplot(spotify_tracks_unique,
       aes(x = valence, y = popularity)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm") +
  labs(
    title = "Valence vs Popularity",
    x = "Valence",
    y = "Popularity"
  )

# 4.3 Correlation
numeric_vars_audio <- spotify_tracks_unique %>%
  select(danceability, energy, loudness, valence, popularity)
cor_matrix_audio <- cor(numeric_vars_audio, use = "complete.obs")
cor_matrix_audio


###############################################################
# (G) Data Analysis
###############################################################
library(dplyr)
library(ggplot2)
library(tidyr)
###############################################################
# Objective 01: Chua Yexun
# To examine the relationship between genre and selected musical characteristics and Spotify track popularity.
# Variables: track_genre, explicit, key, mode, speechiness, acousticness, instrumentalness, liveness, popularity
###############################################################
# 1.1 Descriptive statistics - popularity across genre & content groups
genre_stats <- spotify_tracks_unique %>%
  group_by(track_genre) %>%
  summarise(n = n(), mean_popularity = mean(popularity, na.rm = TRUE)) %>%
  arrange(desc(mean_popularity))

top10_genre_stats <- head(genre_stats, 10)
top10_genre_stats
tail(genre_stats, 10)

spotify_tracks_unique %>% group_by(explicit) %>%
  summarise(n = n(), mean_popularity = mean(popularity, na.rm = TRUE))

spotify_tracks_unique %>% group_by(mode) %>%
  summarise(n = n(), mean_popularity = mean(popularity, na.rm = TRUE))

# quick summary of the remaining content-related audio features
spotify_tracks_unique %>%
  summarise(
    mean_speechiness = mean(speechiness), mean_acousticness = mean(acousticness),
    mean_instrumentalness = mean(instrumentalness), mean_liveness = mean(liveness)
  )
# TODO: baseline averages before testing any group differences


# 1.2 ANOVA - does popularity differ across genres?
anova_genre <- aov(popularity ~ track_genre, data = spotify_tracks_unique)
summary(anova_genre)
# p < 0.05 -> significant difference between at least some genres' average popularity

ggplot(top10_genre_stats, aes(x = reorder(track_genre, mean_popularity), y = mean_popularity)) +
  geom_bar(stat = "identity", fill = "#4C72B0") +
  coord_flip() +
  labs(title = "Top 10 Genres by Average Popularity", x = "Genre", y = "Average Popularity")


# 1.3 Group Comparison - explicit vs non-explicit, major vs minor
t.test(popularity ~ explicit, data = spotify_tracks_unique)

spotify_tracks_unique %>%
  group_by(explicit) %>%
  summarise(mean_popularity = mean(popularity, na.rm = TRUE)) %>%
  ggplot(aes(x = explicit, y = mean_popularity)) +
  geom_bar(stat = "identity", fill = "#C44E52") +
  labs(title = "Average Popularity: Explicit vs Non-Explicit", x = "Explicit", y = "Average Popularity")

t.test(popularity ~ mode, data = spotify_tracks_unique)

spotify_tracks_unique %>%
  group_by(mode) %>%
  summarise(mean_popularity = mean(popularity, na.rm = TRUE)) %>%
  ggplot(aes(x = factor(mode, labels = c("Minor", "Major")), y = mean_popularity)) +
  geom_bar(stat = "identity", fill = "#4C72B0") +
  labs(title = "Average Popularity: Major vs Minor Mode", x = "Mode", y = "Average Popularity")
# TODO: t-test p-value < 0.05 = the group difference is unlikely
# random chance - check which group's mean is actually higher


# (Additional) Tukey HSD post-hoc - which genres differ significantly
genre_subset <- spotify_tracks_unique %>%
  filter(track_genre %in% top10_genre_stats$track_genre)

anova_genre_top10 <- aov(popularity ~ track_genre, data = genre_subset)
tukey_genre <- TukeyHSD(anova_genre_top10)
tukey_genre
plot(tukey_genre, las = 1)
# TODO: the overall ANOVA in 4.2 only says "genre matters
# somewhere" - Tukey HSD tells you EXACTLY which pairs of genres are
# significantly different (rows where "p adj" < 0.05)


###############################################################
# Objective 02: Lim Chee Xuan
# To investigate the relationship between the track duration and temporal characteristic and Spotify track popularity.
# Variables: duration_ms, tempo, time_signature, popularity
###############################################################
# factor - to arrange in the order we want
spotify_tracks_unique <- spotify_tracks_unique %>%
  mutate(
    duration_category = factor(duration_category, levels = c("Short", "Medium", "Long")),
    tempo_category = factor(tempo_category, levels = c("Slow", "Moderate", "Fast"))
  )


# 2.1  Descriptive statistics - duration, tempo, time_signature
spotify_tracks_unique %>%
  summarise(
    mean_duration_min = mean(duration_min, na.rm = TRUE),
    median_duration_min = median(duration_min, na.rm = TRUE),
    sd_duration_min = sd(duration_min, na.rm = TRUE),
    mean_tempo = mean(tempo, na.rm = TRUE),
    median_tempo = median(tempo, na.rm = TRUE),
    sd_tempo = sd(tempo, na.rm = TRUE)
  )

table(spotify_tracks_unique$time_signature) 
# TODO: baseline stats before checking any relationship with popularity


# 2.2 Correlation Analysis - duration/tempo vs popularity
cor.test(spotify_tracks_unique$duration_min, spotify_tracks_unique$popularity)

ggplot(spotify_tracks_unique, aes(x = duration_min, y = popularity)) +
  geom_point(alpha = 0.2, color = "#4C72B0") +
  geom_smooth(method = "lm", color = "#C44E52") +
  labs(title = "Track Duration vs Popularity", x = "Duration (minutes)", y = "Popularity")

cor.test(spotify_tracks_unique$tempo, spotify_tracks_unique$popularity)

ggplot(spotify_tracks_unique, aes(x = tempo, y = popularity)) +
  geom_point(alpha = 0.2, color = "#64B5CD") +
  geom_smooth(method = "lm", color = "#C44E52") +
  labs(title = "Tempo vs Popularity", x = "Tempo (BPM)", y = "Popularity")
# TODO: correlation number + direction, confirmed visually by the trend line


# 2.3 Group Comparison - popularity across duration & time-signature groups
spotify_tracks_unique %>%
  group_by(duration_category) %>%
  summarise(mean_popularity = mean(popularity, na.rm = TRUE)) %>%
  ggplot(aes(x = duration_category, y = mean_popularity)) +
  geom_bar(stat = "identity", fill = "#55A868") +
  labs(title = "Average Popularity by Duration Category",
       x = "Duration Category", y = "Average Popularity")

anova_duration <- aov(popularity ~ duration_category, data = spotify_tracks_unique)
summary(anova_duration)
# p < 0.05 -> at least some duration groups have significantly different average popularity

spotify_tracks_unique %>%
  group_by(time_signature = factor(time_signature)) %>%
  summarise(mean_popularity = mean(popularity, na.rm = TRUE)) %>%
  ggplot(aes(x = time_signature, y = mean_popularity)) +
  geom_bar(stat = "identity", fill = "#DD8452") +
  labs(title = "Average Popularity by Time Signature", x = "Time Signature", y = "Average Popularity")

anova_timesig <- aov(popularity ~ factor(time_signature), data = spotify_tracks_unique)
summary(anova_timesig)
# TODO: does track length group or time signature significantly relate to popularity?

# (Additional) Duration & Tempo Categorization
spotify_tracks_unique %>%
  group_by(duration_category, tempo_category) %>%
  summarise(n = n(), mean_popularity = mean(popularity, na.rm = TRUE), .groups = "drop")
# TODO: a combined view - e.g. are "Medium length + Fast tempo" tracks more popular than other duration/tempo combinations?


###############################################################
# Objective 03: Paureen Tan Nie Nie
# To determine the relationship between the artist-related and track-related characteristics and Spotify track popularity.
# Variables: artists, album_name, track_name, popularity
###############################################################
# install.packages("car")
library(car)
# (Additional/Advanced) Create artist_count & collaboration_status ----
# strsplit(artists, ";") breaks each artist string apart at every ";"
# and gives back a list, e.g. list("Nat King Cole", "John Legend")
# sapply(..., length) then just counts how many names came out of each split
spotify_tracks_unique <- spotify_tracks_unique %>%
  mutate(
    artist_count = sapply(strsplit(as.character(artists), ";"), length),
    collaboration_status = ifelse(artist_count > 1, "Collaboration", "Solo")
  )

table(spotify_tracks_unique$collaboration_status)
# TODO: how many tracks are solo vs multi-artist collaborations


# 3.1 Descriptive statistics - artist, album, track info
n_distinct(spotify_tracks_unique$artists)  
n_distinct(spotify_tracks_unique$album_name) 
n_distinct(spotify_tracks_unique$track_name) 

album_stats <- spotify_tracks_unique %>%
  group_by(album_name) %>%
  summarise(n_tracks = n(), avg_popularity = mean(popularity, na.rm = TRUE)) %>%
  filter(n_tracks >= 3) %>% 
  arrange(desc(avg_popularity))

head(album_stats, 10)  
tail(album_stats, 10) 
# TODO: basic counts + which albums score highest/lowest on average popularity


# 3.2 Artist/Collaboration Analysis - solo vs collaboration
collab_stats <- spotify_tracks_unique %>%
  group_by(collaboration_status) %>%
  summarise(n = n(), mean_popularity = mean(popularity, na.rm = TRUE))
collab_stats

t.test(popularity ~ collaboration_status, data = spotify_tracks_unique)

ggplot(collab_stats, aes(x = collaboration_status, y = mean_popularity)) +
  geom_bar(stat = "identity", fill = "#4C72B0") +
  labs(title = "Average Popularity: Solo vs Collaboration Tracks",
       x = "Track Type", y = "Average Popularity")
# TODO: do collaboration tracks tend to be more/less popular than solo tracks? t-test p-value < 0.05 = the difference is unlikely random


# 3.3 Artist-Level Comparison - top artists with enough tracks
artist_stats <- spotify_tracks_unique %>%
  # splits "Nat King Cole;John Legend" into 2 separate rows, so each artist gets individual credit for that track's popularity
  separate_rows(artists, sep = ";") %>%
  group_by(artists) %>%
  summarise(n_tracks = n(), avg_popularity = mean(popularity, na.rm = TRUE)) %>%
  filter(n_tracks >= 5) %>% 
  arrange(desc(avg_popularity))

top10_artists <- head(artist_stats, 10)
top10_artists

ggplot(top10_artists, aes(x = reorder(artists, avg_popularity), y = avg_popularity)) +
  geom_bar(stat = "identity", fill = "#8172B2") +
  coord_flip() +
  labs(title = "Top 10 Artists by Average Popularity (min. 5 tracks)",
       x = "Artist", y = "Average Popularity")
# TODO: which individual artists (with a reasonable track count) have the highest average popularity


###############################################################
# Objective 04: Phang Shea Wen
# To discover the relationship between selected perceptual audio characteristics and Spotify track popularity.
# Variables: danceability, energy, loudness, valence, popularity
###############################################################
library(tidyr)

# 4.1 Descriptive statistics - the 4 audio features
spotify_tracks_unique %>%
  summarise(
    mean_danceability = mean(danceability), sd_danceability = sd(danceability),
    mean_energy       = mean(energy),       sd_energy       = sd(energy),
    mean_loudness     = mean(loudness),     sd_loudness     = sd(loudness),
    mean_valence      = mean(valence),      sd_valence      = sd(valence)
  )
# TODO: typical (average) level and spread of each perceptual feature

# 4.2 Correlation Analysis - each feature vs popularity

# (a) Pooled correlation across all genres
perceptual_vars <- c("danceability", "energy", "loudness", "valence")

perceptual_corr <- data.frame(
  Feature = perceptual_vars,
  Correlation_with_Popularity = sapply(perceptual_vars, function(v) {
    cor(spotify_tracks_unique[[v]], spotify_tracks_unique$popularity, use = "complete.obs")
  })
)

perceptual_corr$Correlation_with_Popularity <- round(perceptual_corr$Correlation_with_Popularity, 3)
perceptual_corr <- perceptual_corr[order(-perceptual_corr$Correlation_with_Popularity), ]
perceptual_corr

ggplot(perceptual_corr, aes(x = reorder(Feature, Correlation_with_Popularity),
                            y = Correlation_with_Popularity)) +
  geom_bar(stat = "identity", fill = "#8172B2") +
  coord_flip() +
  labs(title = "Correlation of Audio Features with Popularity", x = "Feature", y = "Correlation")
# All four pooled correlations are weak (|r| < 0.1) - none is individually a meaningful predictor of popularity on its own.


# (b) Genre-conditional correlation
# Rationale: popularity is driven mostly by factors outside this dataset
# (artist fame, marketing, playlist placement), so a pooled correlation
# across all genres can mask a real relationship that only exists WITHIN
# a genre. Restrict to genres with enough tracks to be reliable (reuse
# top_genres from Objective 01).

genre_subset <- spotify_tracks_unique %>%
  filter(track_genre %in% top_genres$track_genre)

genre_corr <- genre_subset %>%
  group_by(track_genre) %>%
  summarise(
    n                = n(),
    cor_danceability = round(cor(danceability, popularity, use = "complete.obs"), 3),
    cor_energy       = round(cor(energy, popularity, use = "complete.obs"), 3),
    cor_loudness     = round(cor(loudness, popularity, use = "complete.obs"), 3),
    cor_valence      = round(cor(valence, popularity, use = "complete.obs"), 3),
    .groups = "drop"
  )

print(genre_corr)

# Heatmap
# install.packages("corrplot")
library(corrplot)

corr_matrix_m3 <- spotify_tracks_unique %>%
  select(danceability, energy, loudness, valence, popularity) %>%
  cor(use = "complete.obs")

print(round(corr_matrix_m3, 3))

corrplot(corr_matrix_m3, method = "color", type = "upper", addCoef.col = "black", tl.col = "black", 
         tl.srt = 45, title = "Correlation Heatmap - Perceptual Audio Features", mar = c(0,0,2,0))


# (c) Does allowing slopes to vary by genre improve the model？
# Flat model (same as pooled correlation implies: one slope for everyone)
lm_flat <- lm(popularity ~ danceability + energy + loudness + valence,
              data = genre_subset)

# Interaction model: each feature gets its own slope per genre
lm_inter <- lm(popularity ~ (danceability + energy + loudness + valence) * track_genre,
               data = genre_subset)

# F-test: is the improvement in fit statistically significant?
anova(lm_flat, lm_inter)
#  Res.DF > lm_inter is lower (9929)
# RSS > lm_Inter is lower (3978029)
# F 57.668 > no. error removed per parameter / no. error left per row
# p < 0.05, 2.2e^-16

# Effect size: HOW MUCH does it actually improve?
r2_flat  <- summary(lm_flat)$adj.r.squared
r2_inter <- summary(lm_inter)$adj.r.squared
cat("Adjusted R2 (flat):       ", round(r2_flat, 4), "\n") # 0.0563
cat("Adjusted R2 (interaction):", round(r2_inter, 4), "\n") # 0.2484
cat("R2 improvement:           ", round(r2_inter - r2_flat, 4), "\n") # 0.1922


# (d) per-genre slopes for whichever feature(s) looked genre-dependent in (b) => energy and loudness
# install.packages("emmeans")
library(emmeans)

# Energy slopes per genre
energy_slopes <- emtrends(lm_inter, ~ track_genre, var = "energy")
energy_slope_table <- as.data.frame(energy_slopes)[, c("track_genre", "energy.trend")]
colnames(energy_slope_table) <- c("Genre", "Energy_Slope")
energy_slope_table$Energy_Slope <- round(energy_slope_table$Energy_Slope, 3)
print(energy_slope_table)
# swedish 17.189 (largest positive gradient), reggae -26.551 (largest negative gradient)

# Loudness slopes per genre
loudness_slopes <- emtrends(lm_inter, ~ track_genre, var = "loudness")
loudness_slope_table <- as.data.frame(loudness_slopes)[, c("track_genre", "loudness.trend")]
colnames(loudness_slope_table) <- c("Genre", "Loudness_Slope")
loudness_slope_table$Loudness_Slope <- round(loudness_slope_table$Loudness_Slope, 3)
print(loudness_slope_table)
# soul 1.280 (largest positive gradient), disco -2.266 (largest negative gradient)


# (e) Visualize the strongest genre-dependent feature
# energy
ggplot(genre_subset, aes(x = energy, y = popularity)) +
  geom_point(alpha = 0.15) +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  facet_wrap(~ track_genre, scales = "free_y") +
  labs(title = "Energy vs Popularity, by Genre",
       x = "Energy", y = "Popularity")

# loudness
ggplot(genre_subset, aes(x = loudness, y = popularity)) +
  geom_point(alpha = 0.15) +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  facet_wrap(~ track_genre, scales = "free_y") +
  labs(title = "Loudness vs Popularity, by Genre",
       x = "Loudness", y = "Popularity")
  
# 4.3 (Additional/Advanced) Multiple Linear Regression + VIF + Diagnostics
vif_danceability <- 1 / (1 - summary(lm(danceability ~ energy + loudness + valence, data = genre_subset))$r.squared) # 1.301
vif_energy       <- 1 / (1 - summary(lm(energy ~ danceability + loudness + valence, data = genre_subset))$r.squared) # 2.858
vif_loudness     <- 1 / (1 - summary(lm(loudness ~ danceability + energy + valence, data = genre_subset))$r.squared) # 2.316
vif_valence      <- 1 / (1 - summary(lm(valence ~ danceability + energy + loudness, data = genre_subset))$r.squared) # 1.679

data.frame(
  Feature = c("danceability", "energy", "loudness", "valence"),
  VIF = round(c(vif_danceability, vif_energy, vif_loudness, vif_valence), 3)
)
# energy and loudness has stronger link with each other, but still < 5 or 10 (safe)

par(mfrow = c(2, 2)) # 2 x2 grid
plot(lm_flat)
par(mfrow = c(1, 1)) # 1 plot per figure
# Residuals vs Fitted: flat red horizontal line, but spread is slightly uneven with less on the edges
# Q-Q Residuals: S-curve not normal
# Scale-Location: flat horizontal red line
# Residual vs Leverage: majority points have low leverage, no dangerous outliers

par(mfrow = c(2, 2))
plot(lm_inter)
par(mfrow = c(1, 1))
# Residuals vs Fitted: slightly less flat, more uneven spread; 8696 still the biggest residual
# Q-Q Residuals: same non-normal S-curve, unresolved by the interaction model
# Scale-Location: mild increase in curvature, slightly more uneven variance
# Residuals vs Leverage: leverage up (more parameters), but no dangerous outliers

# check outlier - row 8696
genre_subset[8696, c("track_name", "artists", "track_genre", "popularity",
                     "danceability", "energy", "loudness", "valence")]