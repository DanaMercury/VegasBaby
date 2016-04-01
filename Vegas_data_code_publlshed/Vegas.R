### Data from Vegas ###

# Load
Jer <- read.csv("Data")   #Jeremy (Jer) is my husband :)
View(Jer)

# Clean
Jer <- Jer[-22,]
Jer$PnL = NULL
Jer$ROI <- Jer$Earned - Jer$Invested
colnames(Jer)[7] <- "Percent Change"


# new df to store long/lat of each casino
casino <- data.frame("Casino" = Jer$Casino, "address" = "TBD", "lat" = 0, "long" = 0)
View(casino)

# clean casino df
casino <- unique(casino)
sapply(casino, class)
casino[,c(1,2)] <- sapply(casino[, c(1,2)], as.character)

sapply(casino, class)
head(casino)

# add new data 
casino[1,]= c("The Mirage", "3400 S Las Vegas Blvd, Las Vegas, NV 89109", 36.120601, -115.176838)
casino[2,]= c("The Venetian", "3355 S Las Vegas Blvd, Las Vegas, NV 89109", 36.122084, -115.168032)
casino[3,]= c("Treasure Island", "3300 S Las Vegas Blvd, Las Vegas, NV 89109", 36.124586, -115.171613)
casino[4,]= c("Mandalay Bay", "3950 S Las Vegas Blvd, Las Vegas, NV 89119", 36.090754, -115.176670)
casino[5,]= c("New York, New York", "3790 S Las Vegas Blvd, Las Vegas, NV 89109", 36.102229, -115.174585)
casino[6,]= c("The Cosmopolitan","3708 S Las Vegas Blvd, Las Vegas, NV 89109",36.109935, -115.175416)
casino[7,]= c("Wynn", "3131 S Las Vegas Blvd, Las Vegas, NV 89109", 36.126595, -115.165365)
casino[8,]= c("Caesar's Palace", "3570 S Las Vegas Blvd, Las Vegas, NV 89109", 36.116219, -115.174572)
casino[9,]= c("Bellagio","3600 S Las Vegas Blvd, Las Vegas, NV 89109", 36.112238, -115.179084)
casino[10,]= c("Paris", "3655 S Las Vegas Blvd, Las Vegas, NV 89109", 36.112358, -115.170221)
casino[11,]= c("The Linq", "3535 S Las Vegas Blvd, Las Vegas, NV 89109", 36.118165, -115.170636)

View(casino)


install.packages("plyr")
library("plyr")


# merge data
allvegas <- join(Jer, casino, by= "Casino")
View(allvegas)

# clean full df
sapply(allvegas, class)
allvegas[,c(1,7,10:11)] <- sapply(allvegas[, c(1,7, 10:11)], as.numeric)


# start mapping
mapvegas <- allvegas
mapvegas <- ddply(allvegas, .(Casino, address, lat, long), summarize, "Total Invested" = sum(Invested), "Total ROI" = sum(ROI))

View(mapvegas)

install.packages("ggplot2")
install.packages("ggmap")

library("ggplot2")
library("ggmap")

#size and color map

Vegas <- get_map(location = c(-115.1825, 36.085, -115.1625, 36.13),
              zoom=16,
              maptype="toner",
              source="stamen")

ggmap(Vegas)+
    geom_point(data= mapvegas,
               aes(x= long, y= lat, size =`Total Invested`, color =`Total ROI`),
               alpha = .4)+
    scale_color_gradient(low = "firebrick2", high = "gold")+
    labs(x = NULL, y = NULL, size = "Amount Invested", color = "Amount Won/Lost")+
    scale_size_continuous(range = c(8,30), breaks=c(min(mapvegas$`Total Invested`), mean(mapvegas$`Total Invested`), max(mapvegas$`Total Invested`)))+
    theme(axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())+
    geom_text(data = mapvegas,
              aes(x= long, y= lat, label = Casino), color = "black")

    
# bar chart with percent change and running total


install.packages("RColorBrewer")
library(RColorBrewer) 
display.brewer.all()  

allvegas.order <- allvegas[order(allvegas$Order),]
allvegas.order$flow <- cumsum(allvegas$ROI)     #flow of money/running total
View(allvegas.order)

ggplot(allvegas.order,
       aes(x = Order, y= ROI))+
  geom_bar(stat = "identity", fill = "black")+ 
           #aes(fill = casino))+
  labs(x = "Visits", y = "ROI", fill = "Casino")+
  geom_area(data = allvegas.order,
            aes(x= Order, y= flow),
            alpha = .2,
            fill = "green") +
  theme_minimal()

                               
                            

### Maryland Live Data ###

Marylandlive = NULL

Marylandlive$ROI <- c(c(38.14, 9.81, 8.12,17.63, 19.36, 71.12)-20) 
Marylandlive$order <- c(1:6)
Marylandlive$total <- cumsum(Marylandlive$ROI)
Marylandlive <- as.data.frame(Marylandlive)
View(Marylandlive)

ggplot(Marylandlive,
       aes(x = order, y= ROI)) +
  geom_bar(stat = "identity")+
  geom_area(data = Marylandlive,
          aes(x= order, y= total),
          alpha = .3)
