words = read.csv("C:/Users/sur216/Box Sync/school stuff/romantic Relationships/yelp data/comment_words.csv", header = T, sep = ",")
bigrams = read.csv("C:/Users/sur216/Box Sync/school stuff/romantic Relationships/yelp data/comment_bigrams.csv", header = T, sep = ",")
Romance = read.csv ("C:/Users/sur216/Box Sync/school stuff/romantic Relationships/yelp data/Romantic_Thirdplace2.csv",sep = ",", header = T, na.strings = c("NA","-","", "?"))
names(bigrams)[names(bigrams)=="Buisness_id"] =  "Business_ID"
names(words)
names(bigrams)
all_words = merge(bigrams,words, by = "Business_ID")
sel_col = as.vector(c(-11,-24,-32,-33,-40,-45,-54,-56,-57,-58,-60,-63,-65,-66,-68))
all_words = all_words[,sel_col]
names(all_words)
all_words$celebrate = all_words$celebrate+all_words$birthday
all_words = all_words[,-35]
all_words$expensive = all_words$expensive + all_words$pricey
all_words = all_words[,-36]
names(Romance)
business_sel = as.vector(c(-3,-9:-22,-42:-48))
business = Romance[,business_sel]
names(business)
#selecting the restaurants
Rests = business[,-9:-26 ][business$Restaurant ==1, ][,-8]
names(Rests)
dim(Rests)
word_rest = merge(all_words,Rests, by = "Business_ID")
names(word_rest)
word_rest50 = word_rest[word_rest$Review_count>50,][,-52]

dd = as.vector(c(12:22))
dd
for(i in dd){
  word_rest50[,i] = (word_rest50[,i]/word_rest50$words)* 100
}
names(word_rest50)
word_rest50$wifhus = word_rest50$husband + word_rest50$wife 
word_rest50 = word_rest50[-12:-13]
word_rest50$bfgf = word_rest50$boyfriend + word_rest50$girlfriend 
word_rest50 = word_rest50[-14:-15]
#heatmaps
library(ggmap)
library(ggplot2)
names(word_rest50)
head(word_rest50)



# making data for the maps 

romrom = word_rest50[,c(1,12,13,53,54,50,51)]
romrom$bfgf=romrom$bfgf+romrom$date
romrom = romrom[,c(-3)]
head(romrom)

BFBF = romrom[,c(1,4,5,6)]
BFBF$group = 'BF/GF'
colnames(BFBF)[which(names(BFBF) == "bfgf")] <- "value"
head(BFBF)

famfam = romrom[,c(1,2,5,6)]
famfam$group = 'Family'
colnames(famfam)[which(names(famfam) == "family")] <- "value"
head(famfam)

wifwif = romrom[,c(1,3,5,6)]
wifwif$group = 'Wife/Husband'
colnames(wifwif)[which(names(wifwif) == "wifhus")] <- "value"
head(wifwif)

df6 = rbind(BFBF,wifwif,famfam)
unique(df6$group)
head(romrom)



LL = nrow(df6)
LL
head(df6)
df2 = data.frame(Business_ID = c(NA), lon = c(NA), lat= c(NA), group = c(NA))
for(i in 1:LL){
  #change the number here 
  nn = df6[i,2]
  nn = round(nn*100)
  df = data.frame(Business_ID =as.character(df6[i,1]), lon = df6[i,4], lat = df6[i,3], group = df6[i,5])
  df = do.call("rbind", replicate(nn, df, simplify = F))
  df2 = rbind(df2,df)
}
dim(df2)
head(df2)





tiff(filename="test_final.tiff", bg="white", width=4000, height=4000, pointsize=1);

#myLocation <- c(-115.35, 35.96, -114.95, 36.3)

#map = get_map(location = "Las Vegas",zoom = 12,   source = "osm",color = "bw")
#map = get_map(location = myLocation ,zoom = 11, source = "stamen", maptype = "toner")
#vegas = (lon = -115.14, lat = 36.13)
# pittsburgh = (lon = -79.98, lat = 40.45)
#charlotte  (lon = -80.84, lat = 35,22)
map = get_googlemap(center = c(lon = -115.14, lat = 36.13),zoom = 11,
                size = c(640, 640), scale = 2, maptype = "roadmap", language = "en-EN", sensor = FALSE, messaging = FALSE,
                urlonly = FALSE, color = c("bw"),
                force = FALSE, style = c(feature = "all", element = "labels", visibility = "off")) 
ggmap(map)
pts = geom_point(data=df2, aes(x=lon, y=lat),size=5, alpha = 0.5)
ggmap(map, extent = "device") + geom_density2d(data = df2, aes(x = lon, y = lat, color=group, alpha = 0.4), size = 1.5, bins = 40)+
  scale_color_manual(values=c("Family"="#ff3300", "Wife/Husband"="#0000ff", "BF/GF" = "#28A828"))+ pts +
  theme(legend.title = element_text(size = 70), legend.position = 'right',legend.key.width = unit(6, "cm"),legend.key.height = unit(5, "cm"),
      legend.text = element_text(size = 50),legend.key = element_rect(fill = 'white'))
dev.off()

#

# + stat_density2d(data = df2, 
#               aes(x = lon, y = lat, fill = ..level..,breaks=10), size = 0.01, 
#              bins = 16, geom = "polygon") + scale_fill_gradient(limits = c(0,500),low = "green", high = "red") + pts
# scale_alpha_continuous(limits=c(0,500))#+
#scale_alpha(range = c(0, 0.5), guide = F)
#, alpha =cut(..level..,breaks=c(0,1e-1,Inf)))


(map, extent = "device") + geom_density2d(data = df2, aes(x = lon, y = lat), size = 0.3) + 
  stat_density2d(data = df2, 
                 aes(x = lon, y = lat, fill = ..level.., alpha = ..level..), size = 0.01, 


