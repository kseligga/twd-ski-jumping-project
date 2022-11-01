path <- "C:/Users/kubas/RProjects/TWD_lab"

all_names <- read.csv(paste(path, "/Ski_jumping_data_center-main/all_names.csv", sep = ""))
all_comps <- read.csv(paste(path, "/Ski_jumping_data_center-main/all_comps.csv", sep = ""))
all_ratings <- read.csv(paste(path, "/Ski_jumping_data_center-main/all_ratings.csv", sep = ""))
all_results <- read.csv(paste(path, "/Ski_jumping_data_center-main/all_results.csv", sep = ""))
all_stats <- read.csv(paste(path, "/Ski_jumping_data_center-main/all_stats.csv", sep = ""))
improved_names <- read.csv(paste(path, "/Ski_jumping_data_center-main/improved_names.csv", sep = ""))

library(dplyr)
library(ggplot2)
library(forcats)

all_comps <- mutate(all_comps, id = trimws(id)) #There are spaces at the end of each tournament ids, but there were no spaces in other dataframes
improved_names<-improved_names %>% #whitebars problems fixed
  mutate(improved_names, nationality = trimws(nationality), 
         gender = trimws(gender),
         name = trimws(name))

real_comps <- filter(all_comps, training == 0)
# best ELOs of all time
df <- merge(all_ratings, all_names, by = 'codex') %>%
  arrange(-cumm_rating)
head(df)

# calculating distance to HS ratio for every jump
df5 <- merge(all_results, improved_names, by = 'codex') %>%
  merge(all_comps, by = 'id') %>%
  mutate(dist_HS_ratio = dist / hill_size_x)

df5

# ---------------------------------------------------
# GENDER WAR

# distance to HS ratio histogram: men vs women

df5 %>%
  ggplot(mapping = aes(x = dist_HS_ratio, fill = gender.x)) +
  geom_histogram(alpha = 0.5, bins = 100) +
  xlim(0.3, 1.3)

# ---------------------------------------------------
# average distance to HS ratio for BIB numbers

plot <- df5 %>%
  mutate(bib_num = as.numeric(bib)) %>%
  filter(is.na(bib_num) == F) %>%
  group_by(bib_num) %>%
  summarise(howmany = n(), med_frac_of_HS = median(dist_HS_ratio)) %>%
  filter(howmany > 50) %>%
  ggplot(mapping = aes(y = med_frac_of_HS, x = bib_num)) +
  geom_point()

plot #notice how it rises, shows obv thing that jumpers with higher BIB jump better
#its great near 30 bcs it used to be 30 jumpers in competiton

# ---------------------------------------------------
# SKI WARS

# calculating sum of jumped distance per competitor

sum_dist <- merge(all_results, improved_names, by = 'codex') %>%
  filter(gender=='M') %>% 
  group_by(codex, name) %>%
  summarise(sum_dist = sum(dist), apps = n()) %>%
  mutate(avg_jump = sum_dist / apps) %>%
  filter(apps > 50) %>%
  arrange(-sum_dist)

sum_dist # (poland mountain!)

# calculating sum of jumped distance per nation

real_comps <- all_comps %>%
  filter(training == 0) %>%
  mutate(id = trimws(id))

national_distance <- merge(all_results, improved_names, by = 'codex') %>%
  select(gender, nationality, dist, id) %>%
  merge(select(real_comps, id), by = 'id') %>%
  filter(gender == "M") %>%
  select(nationality, dist) %>%
  group_by(nationality) %>%
  summarise(sum_dist = sum(dist), apps = n()) %>%
  filter(apps > 1000) %>%
  mutate(avg_national_jump = sum_dist / apps) %>%
  arrange(-sum_dist)

as.data.frame(national_distance) %>%
  ggplot(mapping = aes(y = sum_dist, x = reorder(nationality, -sum_dist))) +
  geom_bar(stat = 'identity') # looks a bit like ski jump hill
# significant 'great 5' countries for only men's
# here we cut to the 'great 5' countries for next charts
great5 <- c("GER", "AUT", "SLO", "POL", "NOR")
# comparing speed values for every country

national_speed <- merge(all_results, improved_names, by = 'codex') %>%
  merge(real_comps, by = 'id') %>%
  filter(gender.x == 'M', k.point < 170, speed > 0) %>% # excluding mammoths and trainings
  select(nationality, speed) %>%
  filter(nationality %in% great5) %>%
  group_by(nationality) %>%
  summarise(national_apps = n(), speed, nationality)

national_speed %>%
  ggplot(mapping = aes(x = speed, y = reorder(nationality, speed))) +
  geom_boxplot() +
  xlim(70, 105)

national_speed %>%
  ggplot(mapping = aes(y = speed, x = nationality)) +
  geom_violin()

# all speeds plotted
ggplot(mapping = aes(x = national_speed$speed)) +
  geom_density() +
  xlim(75, 100)

# comparing notes values for every country

national_notes <- merge(all_results, improved_names, by = 'codex') %>%
  merge(real_comps, by = 'id') %>%
  filter(gender.x == 'M', note_points > 0) %>% # excluding trainings
  select(nationality, note_points) %>%
  filter(nationality %in% great5) %>%
  group_by(nationality) %>%
  summarise(national_apps = n(), note_points, nationality) %>%
  filter(national_apps > 100)

national_notes %>%
  ggplot(mapping = aes(x = note_points, y = reorder(nationality, note_points))) +
  geom_boxplot() +
  xlim(45, 60) #they're all the same, boring :/

# all notes plotted
ggplot(mapping = aes(x = national_notes$note_points)) +
  geom_density() +
  xlim(40, 60)


# ---------------------------------------------------
# WIND - THE BEST FRIEND AND WORST ENEMY

# wind values vs distance to HS ratio, correlation

df5 %>%
  filter(id != '2021JP3287RLT', dist > 0) %>%
  # this one competition is skewing results, they resized the hill irl but havent updated data,
  # so the distance to HS ratio is extraordinary there
  filter(nationality %in% great5) %>%
  ggplot(mapping = aes(x = wind, y = dist_HS_ratio)) +
  geom_point() +
  geom_smooth(method = lm) # takes some time to plot
