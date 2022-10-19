all_names<-read.csv("H:/Desktop/TWD_lab/Ski_jumping_data_center-main/all_names.csv")
all_comps<-read.csv("H:/Desktop/TWD_lab/Ski_jumping_data_center-main/all_comps.csv")
all_ratings<-read.csv("H:/Desktop/TWD_lab/Ski_jumping_data_center-main/all_ratings.csv")
all_results<-read.csv("H:/Desktop/TWD_lab/Ski_jumping_data_center-main/all_results.csv")
all_stats<-read.csv("H:/Desktop/TWD_lab/Ski_jumping_data_center-main/all_stats.csv")
all_comps<-read.csv("H:/Desktop/TWD_lab/Ski_jumping_data_center-main/all_comps.csv")
namesU<-unique(names$name)
namesU

# kodzik ukradniety z analysis_script, robi jakies gowno
all_results <- all_results[all_results['speed']>50 & all_results['speed']<115,]
all_results <- all_results[all_results['dist']>40,]
dataset <- merge(all_results,all_comps,by=c('id'),all.y=FALSE)
dataset['short_id'] <- apply(dataset['id'], 2, substr ,start=1, stop=10)
short_ratings <- all_ratings
short_ratings['short_id'] <- apply(short_ratings['id'], 2, substr ,start=1, stop=10)
short_ratings <- short_ratings[!duplicated(short_ratings[,c('short_id','codex')]),c('short_id','codex','cumm_rating')]
dataset['norm_dist'] = dataset['dist']/dataset['hill_size_x']
dataset <- merge(dataset,short_ratings,by.x=c('short_id','codex.x'),by.y=c('short_id','codex'),all.y=FALSE,all.x=TRUE)
dataset$gender = as.integer(as.factor(dataset$gender))
dataset$date_new = as.integer(as.Date.character(dataset$date))
simple_model <- lm(norm_dist~speed+wind+hill_size_x+cumm_rating+gender+date_new+training, data=dataset)
summary(simple_model)
library(mgcv) # contains our prime model
model<-gam(norm_dist~s(speed)+wind+hill_size_x+s(cumm_rating)+gender+training+s(date_new), data=dataset)
summary(model)


# szukanie najlepszych ELO w hisotrii
all_names
library(dplyr)
colnames(ratings)
df1<-ratings %>% 
  arrange(-cumm_rating)
head(df1)

df2<-merge(df1, all_names, by='codex')
df2
df3<-df2 %>% 
  arrange(-cumm_rating)

df3
all_names
