# getting the data ready

library(dplyr) 
library(ggplot2)
library(ggcorrplot)
getwd()
hp<-read.csv("/media/srijan/OS/Users/srija/Datasets/house_prices.csv")
head(hp)

# quick overview

sapply(hp,function(x) sum(is.na(x)))  # no missing values
sum(is.na(hp))
str(hp)
summary(hp)

hp <- mutate(hp, price_scaled = price/1000)
head(hp)

all(hp$sqft_living == (hp$sqft_above+hp$sqft_basement)) #checks if both are equal
hpnew<-hp[,-c(1,11,12,15,17,18)]

hpnew <- mutate(hpnew,sqft_total=hpnew$sqft_living+hpnew$sqft_lot)

# removing outliers

outliers <- function(x) {
  Q1 <- quantile(x, probs=.25)
  Q3 <- quantile(x, probs=.75)
  IQR = Q3-Q1
  lower = Q1 - (IQR*1.5)
  upper = Q3 + (IQR*1.5)
  x > upper | x < lower
}

remove_outliers <- function(hpnew,cols) {
  for (i in cols) {
    hpnew <- hpnew[!outliers(hpnew[[i]]),]
  }
  hpnew
}

sum(outliers(hpnew$price))

hp2<-remove_outliers(hpnew,c('price','sqft_living','sqft_lot'))
hp2 <- hp2[!hp2[,1]==0,]
hp2$Renovated <- ifelse(hp2$yr_renovated==0,'Not Renovated','Renovated')
hp2$era <- cut(hp2$yr_built,6,labels = c("1900-1920","1920-1940","1940-1960",
                              "1960-1980","1980-2000","2000-2020"))
hp2$price_sqft <- hp2$price / hp2$sqft_total
hp2

# checking for correlation

cor(hp2$price,hp2[,c(2,3,4,14)])
hpnum<-hp2[,c(1,2,3,4,14)]
ggcorrplot(cor(hpnum))

# Area vs Price 

ggplot(hp2,aes(sqft_living,price_scaled)) +
  geom_point(aes(colour=yr_built)) +
  labs(title = 'Scatterplot of Area and Price',
       x = 'Living area (sq ft)',
       y = 'Price (in $1000)',
       colour = 'Year built') + 
  theme_bw(base_size = 20) + 
  theme(plot.title = element_text(hjust = 0.5))

# City-wise distribution of mean price

tab1 <- aggregate(hp2$price_scaled,by=list(hp2$city),function(x) mean(x)); tab1
City <- tab1[[1]]
Mean_Price <- tab1[[2]]
df<-data.frame(City,Mean_Price); df
df$City <- reorder(df$City, -df$Mean_Price)
ggplot(df, aes(Mean_Price,City, label = round(Mean_Price,1))) +
  geom_segment(aes(x=0, xend=Mean_Price, y=City, yend=City), size=2, color='darkgreen') +
  geom_point(colour = "brown",size = 5) + 
  geom_text(hjust= -0.4, size=5, fontface='bold') +
  labs(title = 'City-wise distribution of mean price',
       x = 'Mean price (in $1000)',
       y = 'City') + 
  theme_minimal(base_size = 20) +
  theme(plot.title = element_text(hjust = 0.5))

# City-wise distribution of median price

tab2 <- aggregate(hp2$price_scaled,by=list(hp2$city),function(x) median(x)); tab2
City2 <- tab2[[1]]
Median_Price2 <- tab2[[2]]
df2<-data.frame(City2,Median_Price2); df2
df2$City2 <- reorder(df2$City2, -df2$Median_Price2)
ggplot(df2,aes(Median_Price2,City2, label=round(Median_Price2,1))) + 
  geom_segment(aes(x=0, xend=Median_Price2, y=City2, yend=City2), size=2, color='darkgreen') + 
  geom_point(colour = "brown",size = 5) + 
  geom_text(hjust= -0.4, size=5, fontface='bold') +
  labs(title = 'City-wise distribution of median price',
       x = 'Median price (in $1000)',
       y = 'City') + 
  theme_minimal(base_size = 20) +
  theme(plot.title = element_text(hjust = 0.5))

# City-wise distribution of mean price per square feet

tab3 <- aggregate(hp2$price_sqft,by=list(hp2$city),function(x) mean(x)); tab3
City3 <- tab3[[1]]
Mean_Price3 <- tab3[[2]]
df3 <- data.frame(City3,Mean_Price3); df3
df3$City3 <- reorder(df3$City3, -df3$Mean_Price3)
ggplot(df3, aes(Mean_Price3,City3, label = round(Mean_Price3,1))) +
  geom_segment(aes(x=0, xend=Mean_Price3, y=City3, yend=City3), size=2, color='darkgreen') +
  geom_point(colour = "brown",size = 5) + 
  geom_text(hjust= -0.4, size=5, fontface='bold') +
  labs(title = 'City-wise distribution of mean price per square feet',
       x = 'Mean price per sq ft (in $)',
       y = 'City') + 
  theme_minimal(base_size = 20) +
  theme(plot.title = element_text(hjust = 0.5))

# City-wise distribution of median price per square feet

tab4 <- aggregate(hp2$price_sqft,by=list(hp2$city),function(x) median(x)); tab4
City4 <- tab4[[1]]
Median_Price4 <- tab4[[2]]
df4 <- data.frame(City4,Median_Price4); df4
df4$City4 <- reorder(df4$City4, -df4$Median_Price4)
ggplot(df4, aes(Median_Price4,City4, label = round(Median_Price4,1))) +
  geom_segment(aes(x=0, xend=Median_Price4, y=City4, yend=City4), size=2, color='darkgreen') +
  geom_point(colour = "brown",size = 5) + 
  geom_text(hjust= -0.4, size=5, fontface='bold') +
  labs(title = 'City-wise distribution of mean price per square feet',
       x = 'Median price per sq ft (in $)',
       y = 'City') + 
  theme_minimal(base_size = 20) +
  theme(plot.title = element_text(hjust = 0.5))

# City-wise distribution of mean area (in sqft) of a house

tab5 <- aggregate(hp2$sqft_total,by=list(hp2$city),function(x) mean(x)); tab5
City5 <- tab5[[1]]
Mean_area <- tab5[[2]]
df5 <- data.frame(City5,Mean_area); df5
df5$City5 <- reorder(df5$City5, -df5$Mean_area)
ggplot(df5, aes(Mean_area,City5, label = round(Mean_area,1))) +
  geom_segment(aes(x=0, xend=Mean_area, y=City5, yend=City5), size=2, color='darkgreen') +
  geom_point(colour = "brown",size = 5) + 
  geom_text(hjust= -0.4, size=5, fontface='bold') +
  labs(title = 'City-wise distribution of mean area of houses (in sqft)',
       x = 'Mean area (in sqft)',
       y = 'City') + 
  theme_minimal(base_size = 20) +
  theme(plot.title = element_text(hjust = 0.5))

# Frequency distribution of Price

ggplot(hp2,aes(price_scaled)) + 
  geom_histogram(bins=30, colour='black', fill='steelblue') +
  labs(title = 'Frequency distribution of Price',
       x = 'Price (in $1000)',
       y = 'Count') + 
  theme_light(base_size = 20) +
  theme(plot.title = element_text(hjust = 0.5))

# Count of houses by year built

ggplot(hp2,aes(era)) + 
  geom_bar(colour='black', fill='steelblue') + 
  labs(title = 'Count of houses by year built',
       x = 'Year Built',
       y = 'Count') + 
  theme_light(base_size = 20) +
  theme(plot.title = element_text(hjust = 0.5))

# Count of houses by number of bedrooms

ggplot(hp2,aes(factor(bedrooms))) + 
  geom_bar(colour='black', fill='steelblue') +
  labs(title = 'Count of houses by number of bedrooms',
       x = 'Bedrooms',
       y = 'Count') + 
  theme_light(base_size = 20) +
  theme(plot.title = element_text(hjust = 0.5))

# Bedrooms vs price
 
 # boxplot

ggplot(hp2,aes(factor(bedrooms), price_scaled, fill=factor(bedrooms))) + 
  geom_boxplot() +
  labs(title = 'Bedrooms vs Price',
       x = 'Number of Bedrooms',
       y = 'Price (in $1000)') + 
  theme_bw(base_size = 20) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "none")
 
 # violin plot

ggplot(hp2,aes(factor(bedrooms), price_scaled, fill=factor(bedrooms))) + 
  geom_violin() +
  labs(title = 'Bedrooms vs Price',
       x = 'Number of Bedrooms',
       y = 'Price (in $1000)') + 
  theme_bw(base_size = 20) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "none")

# Bathrooms vs price

 # boxplot

ggplot(hp2,aes(factor(bathrooms), price_scaled, fill=factor(bathrooms))) + 
  geom_boxplot() +
  labs(title = 'Bathrooms vs Price',
       x = 'Number of Bathrooms',
       y = 'Price (in $1000)') + 
  theme_bw(base_size = 20) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "none") +
  annotate(geom = 'label', x=15,y=125, size=7, fill='yellow',alpha=0.6,
           label = "A 'full' bathroom is considered to be one containing 
a sink, toilet, bathtub and a shower, each contributing to 0.25 of 
 the total portion. This explains the fractional number of bathrooms.")

 # violin plot

ggplot(hp2,aes(factor(bathrooms), price_scaled, fill=factor(bathrooms))) + 
  geom_violin() +
  labs(title = 'Bathrooms vs Price',
       x = 'Number of Bathrooms',
       y = 'Price (in $1000)') + 
  theme_bw(base_size = 20) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "none") +
  annotate(geom = 'label', x=15,y=90, size=7, fill='yellow',alpha=0.6,
           label = "A 'full' bathroom is considered to be one containing 
a sink, toilet, bathtub and a shower, each contributing to 0.25 of 
 the total portion. This explains the fractional number of bathrooms.")

# Effect of Renovation on Condition and Price

ggplot(hp2,aes(price_scaled, fill=factor(condition))) + 
  geom_density(alpha=0.65) + facet_wrap(~Renovated) +
  labs(title = 'Effect of Renovation on Condition and Price',
       x = 'Price (in $1000)',
       y = 'Density') + 
  scale_fill_brewer(palette = 'Spectral',
                    name = "Condition of the house", 
                    labels = c("Poor","Fair","Average","Good","Very Good")) +
  theme_light(base_size = 20) +
  theme(plot.title = element_text(hjust = 0.5)) 

# Effect of property view on Price

ggplot(hp2,aes(price_scaled, fill=factor(view))) + 
  geom_density(alpha=0.65) +
  labs(title = 'Effect of Property View on Price',
       x = 'Price (in $1000)',
       y = 'Density') + 
  scale_fill_brewer(palette = 'Spectral',
                    name = 'View of property',
                    labels = c('Poor','Satisfactory','Good','Very Good',
                               'Excellent')) +
  theme_light(base_size = 20) +
  theme(plot.title = element_text(hjust = 0.5))

