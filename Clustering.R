
####################################################
# packages

library(tidyverse)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(cluster)
library(writexl)
library(factoextra)

#############################################################

# data sets

x2014<-read.csv("2014_Financial_Data.csv")
x2015<-read.csv("2015_Financial_Data.csv")
x2016<-read.csv("2016_Financial_Data.csv")
x2017<-read.csv("2017_Financial_Data.csv")
x2018<-read.csv("2018_Financial_Data.csv")
#############################################
###########################################################

#Choosing financial ratios and essential variables for analysis

# Asset Turnover = ATR, Debt Ratio = DR, Return on Equity = ROE, Dividend Yield = DY,
# Price Earnings ratio = PER, Cash Ratio= CR, Sector=Sector, 2015 Price Variation= PV2015


names(x2014)

x2014<- x2014 %>%  select(X, assetTurnover,debtRatio, returnOnEquity, Dividend.Yield, priceEarningsRatio, cashRatio,
                          Sector ,X2015.PRICE.VAR...., Class)

names(x2014)[1]<-paste("Name")
names(x2014)[2]<-paste("ATR")
names(x2014)[3]<-paste("DR")
names(x2014)[4]<-paste("ROE")
names(x2014)[5]<-paste("DY")
names(x2014)[6]<-paste("PER")
names(x2014)[7]<-paste("CR")
names(x2014)[8]<-paste("Sector")
names(x2014)[9]<-paste("PV2015")
#################################################################################

# droping NA values

x2014<-na.omit(x2014)

###### looking at zero values and finding their share in each variable

#DY
x2014_withzerovalues<-x2014  %>% filter(  DY==0) # 1686 zero values in DY

(1686/2818)*100
# 60%
#######################

#DR
x2014_withzerovalues<-x2014  %>% filter(  DR==0) # 625 zero values in DR

(625/2818)*100

#22%
#######################

#CR
x2014_withzerovalues<-x2014  %>% filter(  CR==0) # 7 zero values in CR

(7/2818)*100
#0.25%
#######################

#ATR
x2014_withzerovalues<-x2014  %>% filter(  ATR==0) # 155 zero values in ATR

(155/2818)*100
#5.5%
#######################

#PER
x2014_withzerovalues<-x2014  %>% filter(  PER==0) # 1010 zero values in PER

(1010/2818)*100
# 36%
#######################

#ROE
x2014_withzerovalues<-x2014  %>% filter(  ROE==0) # 3 zero values in ROE

(3/2818)*100
#0.1%
#######################

################################################################################
# cleaning 0 values because they are looking suspicious

x2014_withoutzerovalues<-x2014  %>% filter(ATR!=0, DR!=0, ROE!=0, DY!=0, PER!=0, CR!=0)




# Choosing the technology sector because our analysis focus on technology sector


tech2014<- x2014_withoutzerovalues  %>% filter( Sector == "Technology")

################################################################################
#outlier detection

# We should seperate the outliers to maximize our results
# We are using Interquartile range or box plot method to clean outliers



# First, let's look at the box plot to see general situation




b1<-ggplot(tech2014, aes(DR)) + geom_boxplot(col="black") + theme_bw() + labs(title = "Debt ratio boxplot", 
                                                                              subtitle = "Detecting outliers")

b2<-ggplot(tech2014, aes(ROE)) + geom_boxplot(col="black") + theme_bw() + labs(title = "Return on earnings boxplot", 
                                                                               subtitle = "Detecting outliers")

b3<-ggplot(tech2014, aes(DY)) + geom_boxplot(col="black") + theme_bw() + labs(title = "Dividend Yield boxplot", 
                                                                              subtitle = "Detecting outliers")

b4<-ggplot(tech2014, aes(PER)) + geom_boxplot(col="black") + theme_bw() + labs(title = "Price earnings boxplot", 
                                                                               subtitle = "Detecting outliers")

b5<-ggplot(tech2014, aes(CR)) + geom_boxplot(col="black") + theme_bw() + labs(title = "Cash ratio boxplot", 
                                                                              subtitle = "Detecting outliers")

b6<-ggplot(tech2014, aes(ATR)) + geom_boxplot(col="black") + theme_bw() + labs(title = "Asset turnover boxplot", 
                                                                               subtitle = "Detecting outliers")
grid.arrange(b1, b2, b3,b4,b5,b6, ncol=2)

# for DR  - finding outlier borders


boxplot(tech2014$DR, horizontal = TRUE)



summary(tech2014$DR)

IQR(tech2014$DR)




Tmin_DR <- 0.1241 - (1.5*0.172825)
Tmax_DR <- 0.2969 + (1.5*0.172825)




# for DY - finding outlier borders


boxplot(tech2014$DY, horizontal = TRUE)



summary(tech2014$DY)

IQR(tech2014$DY)




Tmin_DY <- 0.00960 - (1.5*0.0167)
Tmax_DY <- 0.02630  + (1.5*0.0167)



######################################
# for ATR- finding outlier borders


boxplot(tech2014$ATR, horizontal = TRUE)



summary(tech2014$ATR)

IQR(tech2014$ATR)




Tmin_ATR <- 0.5427  - (1.5*0.4464969)
Tmax_ATR <- 0.9892 + (1.5*0.4464969)




######################################
# ROE
####################################

boxplot(tech2014$ROE, horizontal = TRUE)



summary(tech2014$ROE)

IQR(tech2014$ROE)




Tmin_ROE <- 0.1004 - (1.5*0.113225)
Tmax_ROE <- 0.2136 + (1.5*0.113225)




##############################################################
# for PER
####################################################
boxplot(tech2014$PER, horizontal = TRUE)



summary(tech2014$PER)

IQR(tech2014$PER)




Tmin_PER <- 16.13  - (1.5*9.9152)
Tmax_PER <- 26.05 + (1.5*9.9152)

############################################################################################
#CR
###################################################################

boxplot(tech2014$CR, horizontal = TRUE)



summary(tech2014$CR)

IQR(tech2014$CR)




Tmin_CR <- 0.37128   - (1.5*0.8120277)
Tmax_CR <- 1.18331 + (1.5*0.8120277)


#####################################################################################################################
# removal of outliers


x2014_withoutoutlier<- tech2014 %>% select(Name,CR, PER, DR, DY, ROE, ATR, PV2015, Class) %>%
  filter(DR>Tmin_DR & DR<Tmax_DR , DY>Tmin_DY & DY<Tmax_DY,ATR>Tmin_ATR & ATR<Tmax_ATR,
         ROE>Tmin_ROE & ROE<Tmax_ROE, PER>Tmin_PER & PER<Tmax_PER,
         CR>Tmin_CR & CR<Tmax_CR )




#boxplot visualization after cleaning outliers

b1_IQR<-ggplot(x2014_withoutoutlier, aes(DR)) + geom_boxplot(col="black") + theme_bw() + labs(title = "Debt ratio boxplot", 
                                                                                              subtitle = "Without outliers")

b2_IQR<-ggplot(x2014_withoutoutlier, aes(ROE)) + geom_boxplot(col="black") + theme_bw() + labs(title = "Return on earnings boxplot", 
                                                                                               subtitle = "Without outliers")

b3_IQR<-ggplot(x2014_withoutoutlier, aes(DY)) + geom_boxplot(col="black") + theme_bw() + labs(title = "Dividend Yield boxplot", 
                                                                                              subtitle = "Without outliers")

b4_IQR<-ggplot(x2014_withoutoutlier, aes(PER)) + geom_boxplot(col="black") + theme_bw() + labs(title = "Price earnings boxplot", 
                                                                                               subtitle = "Without outliers")

b5_IQR<-ggplot(x2014_withoutoutlier, aes(CR)) + geom_boxplot(col="black") + theme_bw() + labs(title = "Cash ratio boxplot", 
                                                                                              subtitle = "Without outliers")

b6_IQR<-ggplot(x2014_withoutoutlier, aes(ATR)) + geom_boxplot(col="black") + theme_bw() + labs(title = "Asset turnover boxplot", 
                                                                                               subtitle = "Without outliers")
grid.arrange(b1_IQR, b2_IQR, b3_IQR,b4_IQR,b5_IQR,b6_IQR, ncol=2)

################################################################################################################
# outliers
# outlier or not 0= No, 1=Yes
##########################################

x2014_withoutoutlier$Outlierornot <- 0

x2014_outliers<- tech2014 %>% left_join(x2014_withoutoutlier, by= "Name")

x2014_outliers$Outlierornot[is.na(x2014_outliers$Outlierornot)] = 1

# I labelled outliers as 1 and non-outliers as 0
x2014_outliers_1<- x2014_outliers %>% select(Name,CR.x, PER.x, DR.x, DY.x, ROE.x, ATR.x, Outlierornot, PV2015.x, Class.x, Outlierornot) %>%
  filter(Outlierornot==1)

x2014_outliers_1<- x2014_outliers_1%>% select(Name,CR.x, PER.x, DR.x, DY.x, ROE.x, ATR.x, PV2015.x, Class.x,Outlierornot)

# Now we can easily see the financial ratios of outliers easily in  x2014_outliers_1 


summary(x2014_outliers_1)


#####################################################################
################################################################################################################
###############################################################################################################
# preparing data for analysis - keeping only numbers

x<- x2014_withoutoutlier %>% select(DR, DY, CR,PER, ATR,ROE)



##########################################################################################################################
#clustering
######################################################################################
# finding the optimal number of clusters - “silhouette”, “wss”, “gap_stat” method



fviz_nbclust(
  x,
  FUNcluster = kmeans,
  method ="silhouette",
  diss = NULL,
  k.max = 10,
  nboot = 1000,
  verbose = interactive(),
  barfill = "dark",
  barcolor = "steelblue",
  linecolor = "black",
  print.summary = TRUE) + labs(title = "Silhouette", subtitle = "Finding optimal number of clusters") + 
  theme_grey() + geom_vline(xintercept = 3, linetype = "dashed", col="red")


fviz_nbclust(
  x,
  FUNcluster = kmeans,
  method ="wss",
  diss = NULL,
  k.max = 10,
  nboot = 1000,
  verbose = interactive(),
  barfill = "steelblue",
  barcolor = "steelblue",
  linecolor = "black",
  print.summary = TRUE)+ labs(title = "WSS", subtitle = "Finding optimal number of clusters") +
  theme_grey() + geom_vline(xintercept = 3, linetype = "dashed", col="red")

fviz_nbclust(
  x,
  FUNcluster = kmeans,
  method ="gap_stat",
  diss = NULL,
  k.max = 10,
  nboot = 1000,
  verbose = interactive(),
  barfill = "steelblue",
  barcolor = "steelblue",
  linecolor = "black",
  print.summary = TRUE) +
  labs(title = "Gap Stat", subtitle = "Finding optimal number of clusters") + theme_grey()+ 
  geom_vline(xintercept = 1, linetype = "dashed", col="red")

#According to our analysis optimal numbers of clusters is 3

##########################################################
##################################################################
km<- kmeans(x, centers = 3) 

fviz_cluster(km, data = x) + labs(title = "Clustering")


kmeans(x, centers = 3)

km
mean(x2014_outliers_1$CR.x)

km$centers
km$size
km$ifault
km$iter
km$totss
km$tot.withinss


########################################################################################
# cluster 1
#####################################################

cluster1<-data.frame(x[km$cluster==1,])

a<-cluster1%>% left_join(tech2014, by="PER")

cluster1_main<- a %>% select(Name,DR.x,CR.x, PER,  DY.x, ROE.x, ATR.x )

########################################################################################################
#cluster 2
##################################################################

cluster2<-data.frame(x[km$cluster==2,])

a<-cluster2 %>% left_join(x2014_withoutoutlier, by="DR")

cluster2_main<- a %>% select(Name,DR,CR.x, PER.x,  DY.x, ROE.x, ATR.x )


########################################################################################################
#cluster 3
##################################################################

cluster3<-data.frame(x[km$cluster==3,])

a<-cluster3 %>% left_join(x2014_withoutoutlier, by="PER")

cluster3_main<- a %>% select(Name, DR.x,CR.x, PER,  DY.x, ROE.x, ATR.x )

# Kmcenters 

kmcenters <- data.frame(km$centers)

##############################
# exporting dataset from r to excel



write_xlsx(kmcenters,"IntroDS.xlsx")
write_xlsx(cluster1_main,"IntroDS1.xlsx" )
write_xlsx(cluster2_main,"IntroDS2.xlsx" )
write_xlsx(cluster3_main,"IntroDS3.xlsx" )
write_xlsx(x2014_outliers_1,"IntroDS_outliers.xlsx" )

########################################################################################################
# Price analysis for choosen stocks
#############################################
#Best performed stocks from each cluster
#Cluster 1 - MSFT, ATVI, AMOT
#Cluster 2 - BELFA, AYI,QADB
#Cluster 3 - DBD, DAKT, APH

#Best performed stocks from outliers - SABR, BDC

# After clustering we analyzed each cluster and realized that these stocks are the best performed stocks
# so we invested them. Now we will look what is our earnings in first and 5th years.

#######################################
#2015 Price variation

cluster1_stocks<- tech2014 %>% select(Name, PV2015) %>% filter(Name== "MSFT" | Name== "ATVI" |Name== "AMOT" )
cluster2_stocks<- tech2014 %>% select(Name, PV2015) %>% filter(Name== "BELFA" | Name== "AYI" |Name== "QADB" )
cluster3_stocks<- tech2014 %>% select(Name, PV2015) %>% filter(Name== "DBD" | Name== "DAKT" |Name== "APH" )


outlier_stocks<- tech2014 %>% select(Name, PV2015) %>% filter(Name== "SABR" | Name== "BDC" )


write_xlsx(cluster1_stocks, "PriceV1.xlsx")

write_xlsx(cluster2_stocks, "PriceV2.xlsx")

write_xlsx(cluster3_stocks, "PriceV3.xlsx")

write_xlsx(outlier_stocks, "PriceVOUT.xlsx")

# preparing other data sets to do final comparisons

#2016 price variation

x2015<- x2015 %>%  select(X ,X2016.PRICE.VAR....)

names(x2015)[1]<-paste("Name")
names(x2015)[2]<-paste("PV2016")

cluster1_stocks_2016<- x2015 %>% select(Name, PV2016) %>% filter(Name== "MSFT" | Name== "ATVI" |Name== "AMOT" )
cluster2_stocks_2016<- x2015 %>% select(Name, PV2016) %>% filter(Name== "BELFA" | Name== "AYI" |Name== "QADB" )
cluster3_stocks_2016<- x2015 %>% select(Name, PV2016) %>% filter(Name== "DBD" | Name== "DAKT" |Name== "APH" )


outlier_stocks_2016<- x2015 %>% select(Name, PV2016) %>% filter(Name== "SABR" | Name== "BDC" )

write_xlsx(cluster1_stocks_2016, "PriceV1_2016.xlsx")

write_xlsx(cluster2_stocks_2016, "PriceV2_2016.xlsx")

write_xlsx(cluster3_stocks_2016, "PriceV3_2016.xlsx")

write_xlsx(outlier_stocks_2016, "PriceVOUT_2016.xlsx")


#2017 price variation

x2016<- x2016 %>%  select(X ,X2017.PRICE.VAR....)

names(x2016)[1]<-paste("Name")
names(x2016)[2]<-paste("PV2017")

cluster1_stocks_2017<- x2016 %>% select(Name, PV2017) %>% filter(Name== "MSFT" | Name== "ATVI" |Name== "AMOT" )
cluster2_stocks_2017<- x2016 %>% select(Name, PV2017) %>% filter(Name== "BELFA" | Name== "AYI" |Name== "QADB" )
cluster3_stocks_2017<- x2016 %>% select(Name, PV2017) %>% filter(Name== "DBD" | Name== "DAKT" |Name== "APH" )


outlier_stocks_2017<- x2016 %>% select(Name, PV2017) %>% filter(Name== "SABR" | Name== "BDC" )

write_xlsx(cluster1_stocks_2017, "PriceV1_2017.xlsx")

write_xlsx(cluster2_stocks_2017, "PriceV2_2017.xlsx")

write_xlsx(cluster3_stocks_2017, "PriceV3_2017.xlsx")

write_xlsx(outlier_stocks_2017, "PriceVOUT_2017.xlsx")


#2018 price variation

x2017<- x2017 %>%  select(X ,X2018.PRICE.VAR....)

names(x2017)[1]<-paste("Name")
names(x2017)[2]<-paste("PV2018")

cluster1_stocks_2018<- x2017 %>% select(Name, PV2018) %>% filter(Name== "MSFT" | Name== "ATVI" |Name== "AMOT" )
cluster2_stocks_2018<- x2017 %>% select(Name, PV2018) %>% filter(Name== "BELFA" | Name== "AYI" |Name== "QADB" )
cluster3_stocks_2018<- x2017 %>% select(Name, PV2018) %>% filter(Name== "DBD" | Name== "DAKT" |Name== "APH" )


outlier_stocks_2018<- x2017 %>% select(Name, PV2018) %>% filter(Name== "SABR" | Name== "BDC" )

write_xlsx(cluster1_stocks_2018, "PriceV1_2018.xlsx")

write_xlsx(cluster2_stocks_2018, "PriceV2_2018.xlsx")

write_xlsx(cluster3_stocks_2018, "PriceV3_2018.xlsx")

write_xlsx(outlier_stocks_2018, "PriceVOUT_2018.xlsx")

#2019 price variation

x2018<- x2018 %>%  select(X ,X2019.PRICE.VAR....)

names(x2018)[1]<-paste("Name")
names(x2018)[2]<-paste("PV2019")

cluster1_stocks_2019<- x2018 %>% select(Name, PV2019) %>% filter(Name== "MSFT" | Name== "ATVI" |Name== "AMOT" )
cluster2_stocks_2019<- x2018 %>% select(Name, PV2019) %>% filter(Name== "BELFA" | Name== "AYI" |Name== "QADB" )
cluster3_stocks_2019<- x2018 %>% select(Name, PV2019) %>% filter(Name== "DBD" | Name== "DAKT" |Name== "APH" )


outlier_stocks_2019<- x2018 %>% select(Name, PV2019) %>% filter(Name== "SABR" | Name== "BDC" )

write_xlsx(cluster1_stocks_2019, "PriceV1_2019.xlsx")

write_xlsx(cluster2_stocks_2019, "PriceV2_2019.xlsx")

write_xlsx(cluster3_stocks_2019, "PriceV3_2019.xlsx")

write_xlsx(outlier_stocks_2019, "PriceVOUT_2019.xlsx")

###################################
# Looking at all price variations together

Price_Var_cluster1<-cluster1_stocks %>% left_join(cluster1_stocks_2016, by="Name") %>%
  left_join(cluster1_stocks_2017, by="Name") %>% left_join(cluster1_stocks_2018, by="Name") %>% 
  left_join(cluster1_stocks_2019, by="Name") 


Price_Var_cluster2<-cluster2_stocks %>% left_join(cluster2_stocks_2016, by="Name") %>%
  left_join(cluster2_stocks_2017, by="Name") %>% left_join(cluster2_stocks_2018, by="Name") %>% 
  left_join(cluster2_stocks_2019, by="Name") 

Price_Var_cluster3<-cluster3_stocks %>% left_join(cluster3_stocks_2016, by="Name") %>%
  left_join(cluster3_stocks_2017, by="Name") %>% left_join(cluster3_stocks_2018, by="Name")  %>% 
  left_join(cluster3_stocks_2019, by="Name") 

Price_Var_OUT<-outlier_stocks %>% left_join(outlier_stocks_2016, by="Name") %>%
  left_join(outlier_stocks_2017, by="Name") %>% left_join(outlier_stocks_2018, by="Name")  %>% 
  left_join(outlier_stocks_2019, by="Name") 

#################################################################

# Making visualisations to get into deep and see whole picture

#First, we would change the name of PV2015 to 2015, PV2016 to 2016 and etc...
#We would also change the Name to Years because we will transpose our data to make visualization
#It would be more clear when the whole work is done

#1 step change names - Price_Var_cluster1


names(Price_Var_cluster1)[1]<-paste("Years")
names(Price_Var_cluster1)[2]<-paste("2015")
names(Price_Var_cluster1)[3]<-paste("2016")
names(Price_Var_cluster1)[4]<-paste("2017")
names(Price_Var_cluster1)[5]<-paste("2018")
names(Price_Var_cluster1)[6]<-paste("2019")

#Price_Var_cluster2
names(Price_Var_cluster2)[1]<-paste("Years")
names(Price_Var_cluster2)[2]<-paste("2015")
names(Price_Var_cluster2)[3]<-paste("2016")
names(Price_Var_cluster2)[4]<-paste("2017")
names(Price_Var_cluster2)[5]<-paste("2018")
names(Price_Var_cluster2)[6]<-paste("2019")

#Price_Var_cluster3
names(Price_Var_cluster3)[1]<-paste("Years")
names(Price_Var_cluster3)[2]<-paste("2015")
names(Price_Var_cluster3)[3]<-paste("2016")
names(Price_Var_cluster3)[4]<-paste("2017")
names(Price_Var_cluster3)[5]<-paste("2018")
names(Price_Var_cluster3)[6]<-paste("2019")
#Price_Var_OUT
names(Price_Var_OUT)[1]<-paste("Years")
names(Price_Var_OUT)[2]<-paste("2015")
names(Price_Var_OUT)[3]<-paste("2016")
names(Price_Var_OUT)[4]<-paste("2017")
names(Price_Var_OUT)[5]<-paste("2018")
names(Price_Var_OUT)[6]<-paste("2019")


##### 2 step - Transposing the data

#PV_cluster1


PV_cluster1<- as.data.frame(t(Price_Var_cluster1))

PV_cluster1<-PV_cluster1 %>%
  rownames_to_column("Years") %>% 
  relocate(Years, .after = ncol(.))

colnames(PV_cluster1)<- PV_cluster1[1,]
PV_cluster1<-PV_cluster1[-1,]


#PV_cluster2

PV_cluster2<- as.data.frame(t(Price_Var_cluster2))

PV_cluster2<-PV_cluster2 %>%
  rownames_to_column("Years") %>% 
  relocate(Years, .after = ncol(.))

colnames(PV_cluster2)<- PV_cluster2[1,]
PV_cluster2<-PV_cluster2[-1,]

#PV_cluster3

PV_cluster3<- as.data.frame(t(Price_Var_cluster3))

PV_cluster3<-PV_cluster3 %>%
  rownames_to_column("Years") %>% 
  relocate(Years, .after = ncol(.))

colnames(PV_cluster3)<- PV_cluster3[1,]
PV_cluster3<-PV_cluster3[-1,]

#PV_OUT
PV_OUT<- as.data.frame(t(Price_Var_OUT))

PV_OUT<-PV_OUT %>%
  rownames_to_column("Years") %>% 
  relocate(Years, .after = ncol(.))

colnames(PV_OUT)<- PV_OUT[1,]
PV_OUT<-PV_OUT[-1,]


############ 3. step - Visualisations 

# 3.1 Visualization of only first year Price variance

x2015_overall_PV <- merge(cluster1_stocks,  cluster2_stocks, all = TRUE)
x2015_overall_PV <- merge(x2015_overall_PV,  cluster3_stocks, all = TRUE)
x2015_overall_PV <- merge(x2015_overall_PV,  outlier_stocks, all = TRUE)

names(x2015_overall_PV)[1] <- paste("Stocks")
names(x2015_overall_PV)[2] <- paste("Percent")


x2015_PV<-x2015_overall_PV %>% mutate(Color = ifelse(Percent <0, "red","green")) %>%
  ggplot(aes(x = Stocks, y = Percent, fill = Color))+
  geom_col()+
  scale_fill_identity(guide = FALSE)+ labs(title = "Price Variation in 2015")



# 3.2 Overall visualization of all years

#PV_cluster1

PV_cluster1$MSFT=as.numeric(PV_cluster1$MSFT)

PV_cluster1$AMOT=as.numeric(PV_cluster1$AMOT)

PV_cluster1$ATVI=as.numeric(PV_cluster1$ATVI)



cluster1_line_viz<-ggplot(PV_cluster1, aes(Years,c(MSFT, ATVI, AMOT)), group=1) + 
  geom_line(mapping = aes( y= MSFT, group=1, col="MSFT")) +
  geom_line(mapping = aes( y= ATVI, group=1, col="ATVI")) + 
  geom_line(mapping = aes( y= AMOT, group=1, col="AMOT"))+
  scale_y_continuous(breaks=c(-20, 0, 20, 40, 60, 80, 100)) + theme_bw() + 
  labs(title = "Cluster 1 stocks", subtitle = "MSFT, ATVI, AMOT", colour="Stocks", y= "Price Variation")



cluster1_bar_viz<-PV_cluster1 %>%
  pivot_longer(
    cols = MSFT:AMOT,
    values_to = "PV",
    names_to = "Stock"
  ) %>%
  ggplot(aes(x=Years, y=PV, fill=Stock))+
  geom_col(position = "dodge") + theme_bw() +geom_hline(aes(yintercept=0)) 

grid.arrange(cluster1_line_viz,cluster1_bar_viz, ncol= 1)

#PV_cluster2

PV_cluster2$AYI=as.numeric(PV_cluster2$AYI)

PV_cluster2$BELFA=as.numeric(PV_cluster2$BELFA)

PV_cluster2$QADB=as.numeric(PV_cluster2$QADB)


cluster2_line_viz<-ggplot(PV_cluster2, aes(Years,c(AYI, BELFA, QADB)), group=1) + 
  geom_line(mapping = aes( y= AYI, group=1, col="AYI")) +
  geom_line(mapping = aes( y= BELFA, group=1, col="BELFA")) + 
  geom_line(mapping = aes( y= QADB, group=1, col="QADB"))+
  scale_y_continuous(breaks=c(-30,-15, 0, 15, 30, 45, 60, 75)) + theme_bw() + 
  labs(title = "Cluster 2 stocks", subtitle = "AYI, BELFA, QADB", colour="Stocks", y= "Price Variation")



cluster2_bar_viz<-PV_cluster2 %>%
  pivot_longer(
    cols = AYI:QADB,
    values_to = "PV",
    names_to = "Stock"
  ) %>%
  ggplot(aes(x=Years, y=PV, fill=Stock))+
  geom_col(position = "dodge") + theme_bw() + 
  scale_y_continuous(breaks=c(-30,-15, 0, 15, 30, 45, 60, 75)) +geom_hline(aes(yintercept=0))

grid.arrange(cluster2_line_viz,cluster2_bar_viz, ncol= 1)

#PV_cluster3

PV_cluster3$DBD=as.numeric(PV_cluster3$DBD)
PV_cluster3$APH=as.numeric(PV_cluster3$APH)

PV_cluster3$DAKT=as.numeric(PV_cluster3$DAKT)


cluster3_line_viz<-ggplot(PV_cluster3, aes(Years,c(DBD, APH, DAKT)), group=1) + 
  geom_line(mapping = aes( y= DBD, group=1, col="DBD")) +
  geom_line(mapping = aes( y= APH, group=1, col="APH")) + 
  geom_line(mapping = aes( y= DAKT, group=1, col="DAKT"))+
  scale_y_continuous(breaks=c(-100,-50, 0, 50, 100, 150, 200, 250, 290)) + theme_bw() + 
  labs(title = "Cluster 3 stocks", subtitle = "DBD, APH, DAKT", colour="Stocks", y= "Price Variation")



cluster3_bar_viz<-PV_cluster3 %>%
  pivot_longer(
    cols = DBD:DAKT,
    values_to = "PV",
    names_to = "Stock"
  ) %>%
  ggplot(aes(x=Years, y=PV, fill=Stock))+
  geom_col(position = "dodge") + theme_bw() + 
  scale_y_continuous(breaks=c(-100,-50, 0, 50, 100, 150, 200, 250, 290)) +geom_hline(aes(yintercept=0))

grid.arrange(cluster3_line_viz,cluster3_bar_viz, ncol= 1)

#PV_OUT

PV_OUT$SABR=as.numeric(PV_OUT$SABR)
PV_OUT$BDC=as.numeric(PV_OUT$BDC)




PV_OUT_line_viz<-ggplot(PV_OUT, aes(Years,c(SABR, BDC)), group=1) + 
  geom_line(mapping = aes( y= SABR, group=1, col="SABR")) +
  geom_line(mapping = aes( y= BDC, group=1, col="BDC")) + 
  scale_y_continuous(breaks=c(-40,-20, 0, 20, 40, 60)) + theme_bw() + 
  labs(title = "Outlier stocks", subtitle = "SABR, BDC", colour="Stocks", y= "Price Variation")



PV_OUT_bar_viz<-PV_OUT %>%
  pivot_longer(
    cols = SABR:BDC,
    values_to = "PV",
    names_to = "Stock"
  ) %>%
  ggplot(aes(x=Years, y=PV, fill=Stock))+
  geom_col(position = "dodge") + theme_bw() + 
  scale_y_continuous(breaks=c(-100,-50, 0, 50, 100, 150, 200, 250, 290)) +geom_hline(aes(yintercept=0))



grid.arrange(PV_OUT_line_viz,PV_OUT_bar_viz, ncol= 1)

# Evaluating the stock returns - calculating the total growth for 5 years

# Cluster 1 evaluating

((((100 * (1.21) * 1.16) * 1.39) * 1.20) * 1.58) - 100 #   MSFT - Microsoft - 269.9104% growth
((((100 * (1.94) * 0.97) * 1.73) * 0.73) * 1.27) - 100  #   ATVI - 201.8187% growth
((((100 * (1.14) * 0.87) * 1.48) * 1.28) * 1.09) - 100 # AMOT - 104.7964% growth

# Cluster 2 evaluating - 

((((100 * (1.67) * 0.99) * 0.76) * 0.65) * 1.2) - 100 # AYI - (-1.992376%) growth
((((100 * (0.67) * 1.67) * 0.87) * 0.64) * 0.99) - 100 #  BELFA - (-38.32265%) growth 
((((100 * (0.99) * 1.45) * 1.22) * 0.97) * 1.29) - 100 # "QADB" - 119.1414% growth

################################################################ 

# Cluster 3 evaluating

((((100 * (0.92) * 0.89) * 0.65) * 0.25) * 2.91) - 100 # DBD - (-61.28099%) growth
((((100 * (0.98) * 1.33) * 1.31) * 0.94) * 1.36) - 100 #  APH - 118.2809% growth 
((((100 * (0.75) * 1.36) * 0.87) * 0.84) * 0.86) - 100 # DAKT - (-35.89422%) growth



# Price variance of outliers evaluating

((((100 * (1.41) * 0.93) * 0.85) * 1.07) * 1.07) - 100 # SABR- 27.61113% growth

((((100 * (0.61) * 1.61) * 1.01) * .54) * 1.31) - 100   #  BDC - (-29.83151%) growth 

#Creating data frame of 5 year Price variance

x2019_overall_PV<-data.frame(c( "MSFT","ATVI","AMOT","AYI", "BELFA","QADB", "DBD","APH", "DAKT","SABR","BDC" ), 
                             c( 269,201,104,-2,  -38, 119,-61, 118, -35, 27,-29 ))

names( x2019_overall_PV)[1]<- paste("Stocks")

names( x2019_overall_PV)[2]<- paste("Percent")

# Vizualisation of 5 year price variation

x2019_PV<-x2019_overall_PV %>% mutate(Color = ifelse(Percent <0, "red","green")) %>%
  ggplot(aes(x = Stocks, y = Percent, fill = Color))+
  geom_col()+
  scale_fill_identity(guide = FALSE) + labs(title = "Price Variation of 5 year", subtitle = "2015-2019") + theme_bw()

#compare of first and 5 year price variance

grid.arrange( x2015_PV, x2019_PV)




write_xlsx(x2015_overall_PV,"1st year price var.xlsx")
write_xlsx(x2019_overall_PV,"5 year price var.xlsx")



