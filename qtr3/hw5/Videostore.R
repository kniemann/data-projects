setwd("C:/Users/kevin/Google Drive/datasci/qtr3/hw5")

video_store <- read.csv("Video_store_Results_dataset.csv")

video_store$Cust.ID = NULL
video_store$Gender = NULL
video_store$Genre = NULL
video_store$Incidentals = NULL

x <- video_store[1:6]
y <- video_store[1:6]
correlation <- cor(x, y)

summary(video_store)

good_customers <- video_store[video_store$Rentals > 30,]

summary(good_customers)

video_store <- read.csv("Video_store_Results_dataset.csv")

cross_tab <- table(video_store$Genre,video_store$Gender)

plot(cross_tab)


