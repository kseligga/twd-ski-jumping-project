path<-"C:/Users/kubas/RProjects/TWD_lab"

all_names<-read.csv(paste(path,"/Ski_jumping_data_center-main/all_names.csv", sep=""))
all_comps<-read.csv(paste(path,"/Ski_jumping_data_center-main/all_comps.csv", sep=""))
all_ratings<-read.csv(paste(path,"/Ski_jumping_data_center-main/all_ratings.csv", sep=""))
all_results<-read.csv(paste(path,"/Ski_jumping_data_center-main/all_results.csv", sep=""))
all_stats<-read.csv(paste(path,"/Ski_jumping_data_center-main/all_stats.csv", sep=""))
improved_names<-read.csv(paste(path,"/Ski_jumping_data_center-main/improved_names.csv", sep=""))

library(dplyr)
library(ggplot2)
library(forcats)
library(hexbin)

# color scheme
colors3<-RColorBrewer::brewer.pal(n = 3, name = "Blues")

# best ELOs of all time
df<-merge(all_ratings, all_names, by='codex') %>% 
  arrange(-cumm_rating)
head(df)

# calculating distance to HS ratio for every jump
df5<-merge(all_comps, all_results, by='id') %>% 
  mutate(dist_HS_ratio=dist/hill_size_x)

df5

# ---------------------------------------------------
# GENDER WAR

# distance to HS ratio histogram: men vs women

df5 %>% 
  ggplot(mapping = aes(x=dist_HS_ratio, fill=gender))+
  geom_histogram(alpha=0.5, bins=100)+
  xlim(0.3, 1.3)

# ---------------------------------------------------
# average distance to HS ratio for BIB numbers

plot<-df5 %>% 
  mutate(bib_num = as.numeric(bib)) %>% 
  filter(is.na(bib_num)==F) %>% 
  group_by(bib_num) %>% 
  summarise(howmany=n(), med_frac_of_HS=median(dist_HS_ratio)) %>% 
  filter(howmany>50) %>% 
  ggplot(mapping = aes(y=med_frac_of_HS, x=bib_num))+
  geom_point() 

plot #notice how it rises, shows obv thing that jumpers with higher BIB jump better
     #its great near 30 bcs it used to be 30 jumpers in competiton

# ---------------------------------------------------
# SKI WARS

# calculating sum of jumped distance per competitor

sum_dist<-merge(all_results, improved_names, by='codex') %>% 
  # filter(gender=='M') %>% 
  group_by(codex, name) %>% 
  summarise(sum_dist=sum(dist), apps = n()) %>% 
  mutate(avg_jump=sum_dist/apps) %>% 
  filter(apps>50) %>% 
  arrange(-sum_dist)

sum_dist # (poland mountain!)

# calculating sum of jumped distance per nation
all_results
national_distance<-merge(all_results, improved_names, by='codex') %>% 
  merge(all_comps, by='id') %>% 
  filter(gender.x=='M', training==0) %>% 
  group_by(nationality) %>%
  summarise(sum_dist=sum(dist), apps = n()) %>% 
  filter(apps>1000) %>% 
  mutate(avg_national_jump=sum_dist/apps) %>% 
  arrange(-sum_dist)

as.data.frame(national_distance) %>% 
  ggplot(mapping=aes(y=sum_dist, x=reorder(nationality, -sum_dist)))+
  geom_bar(stat='identity') # looks a bit like ski jump hill
                            # significant 'great 5' countries for only men's

# comparing speed values for every country

national_speed<-merge(all_results, improved_names, by='codex') %>% 
  merge(all_comps, by='id') %>% 
  filter(gender.x=='M', k.point<170, training==0, speed>0) %>% # excluding mammoths and trainings
  group_by(nationality) %>% 
  summarise(national_apps=n(), speed, nationality) %>% 
  filter(national_apps>100)

national_speed %>% 
  ggplot(mapping=aes(x=speed, y=reorder(nationality,speed)))+
  geom_boxplot()+
  xlim(60,110)

national_speed %>% 
  ggplot(mapping = aes(y=speed, x=nationality))+
  geom_violin()

# all speeds plotted
ggplot(mapping=aes(x=national_speed$speed))+
  geom_density()

# plotting all speeds based on hill type
speed <- merge(all_results, improved_names, by = 'codex') %>%
  merge(real_comps, by = 'id') %>%
  filter(gender.x == 'M', speed > 0, training==0) %>%
  mutate(hill_type=case_when(
    k.point<100 ~ 'normal',
    k.point<170 ~ 'large',
    TRUE ~ 'ski flying hill')) %>% 
  mutate(hill_type<-factor(hill_type, levels=c('normal', 'large', 'ski flying hill')))

speed %>% 
  select(speed, hill_type) %>%
  ggplot(mapping = aes(x = speed, fill=hill_type)) +
  geom_density(alpha=0.3)+
  scale_fill_discrete(name = "Type of hill")+
  scale_x_continuous(name='Speed distribution', limits = c(75,110))+
  scale_y_continuous(name='Density')+
  ggtitle('Distribution of speed by type of hill')+
  theme_minimal()+
  scale_fill_manual(values=colors3)

# ---------------------------------------------------

# comparing notes values for every country

national_notes<-merge(all_results, improved_names, by='codex') %>% 
  merge(all_comps, by='id') %>% 
  filter(gender.x=='M', training==0, note_points>0) %>% # excluding trainings
  group_by(nationality) %>% 
  summarise(national_apps=n(), note_points, nationality) %>% 
  filter(national_apps>100)

national_notes %>% 
  ggplot(mapping=aes(x=note_points, y=reorder(nationality,note_points)))+
  geom_boxplot()+
  xlim(35,60)

# all notes plotted
ggplot(mapping=aes(x=national_notes$note_points))+
  geom_density()+
  xlim(35,60)

# ---------------------------------------------------
# CORRELATIONS WITH DISTANCE

# correlation between distance and speed/notes
# for every jump we calculate deviation of speed/notes and distance, in terms of standard deviations from mean value in competition
dist_correlations<-speed %>% 
  group_by(id) %>% 
  summarise(meanspeed_incomp=mean(speed),meandist_incomp=mean(dist), meannotes_incomp=mean(note_points),
            speed_sd=sd(speed), dist_sd=sd(dist), notes_sd=sd(note_points)) %>% 
  merge(speed, by='id') %>% 
  mutate(speed_deviation=(speed-meanspeed_incomp)/speed_sd,
         dist_deviation=(dist-meandist_incomp)/dist_sd,
         note_deviation=(note_points-meannotes_incomp)/notes_sd) %>% 
  filter(speed_deviation>-20)

# plotting correlations
# speed
dist_correlations %>% 
  ggplot(aes(x=speed_deviation,y=dist_deviation, color=hill_type))+
  geom_point(alpha=0.1)+
  geom_density2d()+
  xlim(-4,4)+
  ylim(-4,4)+
  geom_smooth(method = "lm")

# notes
dist_correlations %>% 
  ggplot(aes(x=note_deviation,y=dist_deviation))+
  geom_point(alpha=0.1)+
  geom_density2d()+
  xlim(-5,4)+
  ylim(-5,4)+
  geom_smooth(method = "lm")

# hexagonal heatmaps - cool plot but shitty here
dist_correlations %>% 
  ggplot(aes(x=speed_deviation,y=dist_deviation))+
  stat_bin_hex(colour="black", na.rm=TRUE) +
  scale_fill_gradientn(colours=c("white","cyan"))+
  xlim(-2,2)

dist_correlations %>% 
  ggplot(aes(x=note_deviation,y=dist_deviation))+
  stat_bin_hex(colour="black", na.rm=TRUE) +
  scale_fill_gradientn(colours=c("white","cyan"))+
  xlim(-3,3)

# ---------------------------------------------------
# WIND - THE BEST FRIEND AND WORST ENEMY

# wind values vs distance to HS ratio, correlation

df5 %>% 
  filter(id!='2021JP3287RLT', dist>0) %>%
  # this one competition is skewing results, they resized the hill irl but havent updated data,
  # so the distance to HS ratio is extraordinary there
  ggplot(mapping=aes(x=wind, y=dist_HS_ratio))+
  geom_point()+
  geom_smooth(method=lm) # takes some time to plot
