#Libraries

library(ggplot2)
library(dplyr)
library(ggmap)
library(gridExtra)
library(ggthemes)
library(corrplot)
############################

# data sets

GDP_Percapita<-read.csv("GDP_Percapita.csv")
Happiness<-read.csv("Happiness.csv")
Life_Satisfaction<-read.csv("Life_satisfaction.csv")

world_map <- map_data("world")

######################################################
# GDp per capita XXI


GDP_XXI<-GDP_Percapita %>% filter(Year>1999)
names(GDP_XXI)[1]<- paste("Country")
names(GDP_XXI)[4]<- paste("GDP_Percapita")

GDP_average_XXI<-GDP_XXI %>% group_by(Country) %>% 
  summarise(Average_GDP=mean(GDP_Percapita),
            .groups = "drop")


GDP_XXI_average_coor<- world_map %>% left_join(GDP_average_XXI, by= c("region"= "Country"))
####################################################################################################
########################################################################################
#visualization of GDP per capita XXI

GDP_map_average<-ggplot(GDP_XXI_average_coor,aes(long,lat,group=group,
                                                 alpha = "No data"))+
  geom_polygon(aes(fill=Average_GDP),colour="white")+
  scale_fill_viridis_b(breaks=c(0, 1000, 2000, 5000, 10000, 20000, 50000),
                       na.value = "grey")+
  
  labs(fill="GDP per \ncapita ",
       title="GDP per capita in 21st century", na.value= "No data") +
  theme_map() +  scale_alpha_manual("", values = 1) +
  guides(fill = guide_colorsteps(order = 1),
    alpha = guide_legend(order = 2, override.aes = list(fill = "grey"))) 





#########################################################################################
#Life satisfaction average
names(Life_Satisfaction)[1] <- paste("Country")
names(Life_Satisfaction)[4] <- paste("Life_Satisfaction_rate")


Life_Satisfaction_XXI <- Life_Satisfaction %>% filter( Year>1999)

LS_average_XXI<-Life_Satisfaction_XXI %>% group_by(Country) %>% 
  summarise(Average_LS_rate=mean(Life_Satisfaction_rate),
            .groups = "drop")


LS_average_XXI_coor<- world_map %>% left_join(LS_average_XXI,
                                              by= c("region"= "Country"))


LS_map<-ggplot(LS_average_XXI_coor,aes(long,lat,group=group, 
                                       alpha = "No data"))+
  geom_polygon(aes(fill=Average_LS_rate),colour="white")+
  scale_fill_viridis_b(na.value = "grey")+
  theme_map()+  labs(fill="Life satisfaction rate",
                      title="Life satisfaction rate in 21st century")+ 
  scale_alpha_manual("", values = 1) +
  guides(fill = guide_colorsteps(order = 1),
         alpha = guide_legend(order = 2,
                              override.aes = list(fill = "grey"))) 

###########################################################
#Happiness average

names(Happiness)[1] <- paste("Country")
names(Happiness)[4] <- paste("Happiness_rate")



Happiness_XXI <- Happiness %>% filter( Year>1999)

Happiness_XXI_average<-Happiness_XXI %>% group_by(Country) %>% 
  summarise(Average_Happiness=mean(Happiness_rate),
            .groups = "drop")

Happiness_XXI_average_coor<- world_map %>% left_join(Happiness_XXI_average, 
                                                     by= c("region"= "Country"))


Happiness_map<-ggplot(Happiness_XXI_average_coor,aes(long,lat,group=group,
                                                     alpha = "No data"))+
  geom_polygon(aes(fill=Average_Happiness),colour="white")+
  scale_fill_viridis_b(na.value = "grey")+
  theme_map()+
  labs(fill="Happiness rate ",
       title="Happiness rate in 21st century")+ 
  scale_alpha_manual("", values = 1) +
  guides(fill = guide_colorsteps(order = 1),
         alpha = guide_legend(order = 2,
                              override.aes = list(fill = "grey"))) 
na.omit(Happiness_XXI_average_coor)

write.csv(Happiness_XXI_average_coor, "HP1.csv")
###################################################################################
########################################################################
# Merge data sets - and correlation analysis # average data sets


all_datasets_XXI<-GDP_average_XXI %>% left_join(LS_average_XXI, by="Country") %>% 
  left_join(Happiness_XXI_average,by= "Country" )

all_datasets_XXI<-na.omit(all_datasets_XXI)

cor(all_datasets_XXI$Average_GDP, all_datasets_XXI$Average_LS_rate)
cor(all_datasets_XXI$Average_GDP, all_datasets_XXI$Average_Happiness)
cor(all_datasets_XXI$Average_Happiness, all_datasets_XXI$Average_LS_rate)


corrplot_data_XXI<-all_datasets_XXI %>%select(Average_GDP,Average_Happiness,Average_LS_rate)

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#11ccAA","#4A10DD"))

colnames(corrplot_data_XXI)


colnames(corrplot_data_XXI) <- c("GDP per capita", "Happiness", "Life Satisfaction")


corrplot(cor(corrplot_data_XXI), method = "color",col=col(200), type = "upper",addCoef.col = "black",
         tl.col="black", tl.srt=45,  sig.level = 0.01, insig = "blank")


################## visualization
### happiness and GDP # average

H_gdp_XXI<-ggplot(all_datasets_XXI, aes( Average_GDP, Average_Happiness), group=1) + 
  geom_point(stat = "identity", aes(  color=Average_Happiness, size=Average_GDP)) +
  scale_color_viridis_c()+
  labs(title = "Happiness explained by GDP per capita", 
       subtitle = "Does money buy happiness?",
       y="Happiness rate", x= "GDP per capita",  color="Happiness", size="GDP per capita") + 
  theme_hc() 




#Life satisfaction and GDP # average

LS_gdp_XXI<-ggplot(all_datasets_XXI, aes( Average_GDP, Average_LS_rate), group=1) + 
  geom_point(stat = "identity", aes(  color=Average_LS_rate, size=Average_GDP)) + 
  
  scale_color_viridis_c()+
  labs(title = "Life satisfaction explained by GDP per capita", 
       subtitle = "Does money buy happiness?",
       y="Life satisfaction rate", x= "GDP per capita", 
       color="Life Satisfaction", size="GDP per capita") + 
  theme_hc(5)



############################################################################################################
############################################################################################################

#Grid vizualization

grid.arrange(GDP_map_average, LS_map, Happiness_map, Mosthappy_graph, Most_ls_graph,
 H_gdp_XXI, LS_gdp_XXI)

grid.arrange(GDP_map_average, LS_map, Happiness_map, Mosthappy_graph_theme, 
             Most_ls_graph_theme, LS_gdp_XXI)



grid.arrange(arrangeGrob(GDP_map_average, ncol = 1, nrow = 1),
             arrangeGrob( LS_map, Happiness_map, nrow = 2, ncol = 1),
             arrangeGrob(Happiness_map, nrow = 3, ncol = 1),
             arrangeGrob( LS_gdp_XXI,  nrow = 4, ncol = 1),
             arrangeGrob(H_gdp_XXI,  nrow = 5, ncol = 1))

grid.arrange(GDP_map_average, LS_map, Happiness_map,
             H_gdp_XXI, LS_gdp_XXI)













