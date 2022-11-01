library(xlsx)
library(dplyr)
library(ggplot2)

# really shitty way to take this data, didn't have time to make it csv's
excel_path <- "team-points.xlsx"
for(i in 21:8) { 
  snam <- paste("s", i, i+1, sep = "")
  nam<- paste(i, i+1, sep="")
  assign(snam, read.xlsx(excel_path, sheetName = as.character(nam)) %>%
           select(Name, Points, Apps))
}

df<-rbind(s89, s910, s1011, s1112, s1213, s1314, s1415, s1516, s1617, s1718, s1819, s1920, s2021, s2122)

# total nation points since 2008/09 graph

df<-df %>% 
  mutate(points=as.numeric(Points), apps=as.numeric(Apps)) %>% 
  group_by(Name) %>% 
  summarise(points_sum=sum(points), apps_sum=sum(apps)) %>% 
  arrange(-points_sum) %>% 
  as.data.frame()

df %>% 
  ggplot(aes(y=points_sum, x=reorder(Name, -points_sum)))+
  geom_bar(stat='identity')+
  scale_x_discrete(guide=guide_axis(title='Nationality', angle=35))+
  scale_y_continuous(guide=guide_axis(title='Total number of points'))+
  ggtitle('Total number of points by nationality',
          subtitle='since 2008/2009 season')
