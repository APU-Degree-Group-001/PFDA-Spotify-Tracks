
###############################################################
# (A) Import CSV
###############################################################

getwd() #check current working directory
setwd("E:/Academics/APU/APD2F2606CS(AI)/Semester 1/Programming for Data Analysis/Assignment/CT127-3-2-PFDA Group 01/dataset")

# install packages (one-time job)
# library gone when power off, so need to load library at least one time in the script
# install.packages("data.table") #fread() - fast csv import

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