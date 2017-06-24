# http://www.r-bloggers.com/parsing-complex-text-files-using-regular-expressions-and-vectorization/
require(shiny)
require(ggplot2)
require(lubridate)
require(dplyr)
require(tidyr)
require(scales)
require(magrittr)

line.raw <- read.csv('20170623HR_2.csv', sep = '\t', quote = "")

line.raw$date %<>% dmy()
# line.raw$time %<>% format(parse_date_time(.,"hm"), format="%H:%M") %>% hm()
# line.raw$message %<>% as.character()
line.raw$message %<>% as.factor()
line.raw$day <- wday(line.raw$date, label = TRUE, abbr = FALSE)
levels(line.raw$author) <- c("Hong-Ru", "Simon")

summary(line.raw)

# -------------------------------
# plot du nb de messages par jour
# -------------------------------

nbmsgjr <- ggplot(line.raw, aes(x=day))

# png("Tsukihime_NbMessParJour_DatesduJapon.png", width = 700, height = 480)
nbmsgjr +
  geom_bar() +
  facet_grid(. ~ author) +
  theme_light() +
  labs(x = "Day of the week", y = "Number of messages", title = paste("Number of messages per day from ",line.raw$date[1]," to ",line.raw$date[nrow(line.raw)]))
# dev.off()

# png("Maud_NbMessParJour.png", width = 700, height = 480)
nbmsgjr +
  geom_bar(aes(fill = author)) +
  theme_light() +
  labs(x = "Day of the week", y = "Number of messages", title = paste("Number of messages per day from ",line.raw$date[1]," to ",line.raw$date[nrow(line.raw)]))
# dev.off()

# -----------------------------
# plot du nb de message par h:m
# -----------------------------

nbmsghm <- ggplot(line.raw, aes(x = sort(time)))

# png("Tsukihime_NbMessParHM_HeuresduJapon.png", width = 700, height = 700)
nbmsghm +
  geom_bar(aes(fill = author)) +
  coord_polar() +
  theme_minimal() +
  scale_x_discrete(breaks = c("01:00","02:00","03:00","04:00","05:00","06:00","07:00","08:00","09:00","10:00","11:00","12:00","13:00","14:00","15:00","16:00","17:00","18:00","19:00","20:00","21:00","22:00","23:00","00:00"))+
  # scale_x_discrete(breaks = c("06:04","07:01","08:00","09:02","10:00","11:00","12:00","13:00","14:01","15:02","16:00","17:01","18:00","19:00","20:00","21:00","22:00","23:00","23:58"))+
  labs(x = "Hours", y = "Number of messages", title = paste("Number of messages per hour from ",line.raw$date[1]," to ",line.raw$date[nrow(line.raw)]))
# dev.off()

# -----------------------------------------
# plot du nb de message par h:m et par date
# -----------------------------------------

nbmsghmday <- ggplot(line.raw, aes(x = date, y = time))

# png("Tsukihime_HeureMessParDates_DatesduJapon.png", width = 700, height = 500)
nbmsghmday +
  geom_point(aes(color = author)) +
  theme_minimal() +
  scale_y_discrete(breaks = c("00:00","01:00","02:00","03:00","04:00","05:00","06:00","07:00","08:00","09:00","10:00","11:00","12:00","13:00","14:00","15:00","16:00","17:00","18:00","19:00","20:00","21:00","22:00","23:00")) +
  scale_x_datetime() +
  labs(x = "Date", y = "Hour", title = paste("Hours of messages per date, from ",line.raw$date[1]," to ",line.raw$date[nrow(line.raw)]))
# dev.off()

# ---------------------------
# Heatmap des heures par jour
# ---------------------------
# timeDay <- t(as.matrix(table(sort(line.raw$date), sort(line.raw$time))))
# heatmap(timeDay, Colv = NA, Rowv = NA, scale = "row")

# # -------------
# # Circular plot
# # -------------
# # http://rstudio-pubs-static.s3.amazonaws.com/3369_998f8b2d788e4a0384ae565c4280aa47.html
# line.raw$time <- as.POSIXct(strptime(line.raw2$datetime,"%H:%M"))
# line.raw %>% group_by(date)
# 
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# 
# line.raw2 <- read.csv('chat_2.csv', sep = '\t')
# # line.raw2$Hours <- format(as.POSIXct(strptime(line.raw2$datetime,"%Y/%m/%d %H:%M")) ,format = "%H:%M")
# line.raw2$Hours <- format(as.POSIXct(strptime(line.raw2$datetime,"%d/%m/%Y %H:%M")) ,format = "%H:%M")
# line.raw2$Hours <- as.POSIXct(strptime(line.raw2$Hours, "%H:%M"))
# # line.raw2$Dates <- format(as.POSIXct(strptime(line.raw2$datetime,"%Y/%m/%d %H:%M")) ,format = "%Y/%m/%d")
# # line.raw2$Dates <- as.POSIXct(strptime(line.raw2$Dates, "%Y/%m/%d"))
# line.raw2$Dates <- format(as.POSIXct(strptime(line.raw2$datetime,"%d/%m/%Y %H:%M")) ,format = "%d/%m/%Y")
# line.raw2$Dates <- as.POSIXct(strptime(line.raw2$Dates, "%d/%m/%Y"))
# # line.raw2$datetime <- as.POSIXct(strptime(line.raw2$datetime, "%Y/%m/%d %H:%M"))
# line.raw2$datetime <- as.POSIXct(strptime(line.raw2$datetime, "%d/%m/%Y %H:%M"))
# line.raw2$message <- as.character(line.raw$message)
# 
# frise <- ggplot(line.raw2, aes(x = datetime))
# 
# svg("Frise.svg", width = 14)
# frise+geom_histogram()+
#   scale_x_datetime(breaks=date_breaks("1 month"))+
#   theme(axis.text.x=element_text(angle=90)) +
#   labs(x = "Jour", y = "Nombre de messages", title = paste("Nombre de messages par jour du ",line.raw$date[1]," au ",line.raw$date[nrow(line.raw)]))
# dev.off()
# 
# svg("FriseAuthor.svg", width = 14)
# frise+geom_histogram(aes(fill=author), position=position_dodge())+
#   scale_x_datetime(breaks=date_breaks("1 month"))+
#   theme(axis.text.x=element_text(angle=90))+
#   labs(x = "Jour", y = "Nombre de messages", title = paste("Nombre de messages par jour et auteur du ",line.raw$date[1]," au ",line.raw$date[nrow(line.raw)]))
# dev.off()
# 
# timescat <- ggplot(line.raw2, aes(x=Dates, y=Hours))
# 
# svg("HeuresChaqueJour.svg", width = 14)
# timescat +geom_point() + 
#   scale_y_datetime(breaks=date_breaks("1 hour"), labels=date_format("%H:%M")) + 
#   scale_x_datetime(breaks=date_breaks("1 day")) + 
#   theme(axis.text.x=element_text(angle=90)) +
#   labs(x = "Jour", y = "Heure", title = paste("Heures des messages par jour du ",line.raw$date[1]," au ",line.raw$date[nrow(line.raw)]))
# dev.off()
# 
# 
# # -----------
# # Some number
# # -----------
# msgByDay <- line.raw2 %>% group_by(Dates) %>% summarize(count = n())
# qplot(data = msgByDay, Dates, geom = "histogram")
# qplot(data = msgByDay, count, geom = "histogram")
# line.raw2 %>% mutate(size = length(message)) %>% View

# -----------------------------------
# nb messages per date and per author
# -----------------------------------

# nbpDpA <- table(line.raw$date, line.raw$author) %>% as.data.frame()
# names(nbpDpA) <- c("date", "author", "count")
# nbpDpA$date %<>% ymd()
# # nbpDpA %>% gather(author, count, -date)
# nbpDpA %>% str
# 
# g <- ggplot(nbpDpA, aes(x = date, y = count))
# g +
#   geom_density(aes(fill = author)) +
#   theme_light()

nbmsgdate <- ggplot(line.raw, aes(x=date))

# png("Tsukihime_NbMessParDate_DateduJapon.png", width = 700, height = 480)
nbmsgdate +
  geom_bar(aes(fill = author)) +
  theme_light() +
  labs(x = "Date", y = "Number of messages", title = paste("Number of messages per date from ",line.raw$date[1]," to ",line.raw$date[nrow(line.raw)]))
# dev.off()

# ----------------------------------------
# content message per author and occurence
# ----------------------------------------

### Complete

topSaid <- line.raw$message %>% summary %>% head(20)
names(topSaid)
datatopSaid <- line.raw[line.raw$message %in% names(topSaid), ]

# png("Tsukihime_NbMessParMessage_Complet.png", width = 1400, height = 480)
msgAuthor <- ggplot(datatopSaid, aes(x=message))
msgAuthor +
  geom_bar(aes(fill = author)) +
  theme_light() +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
# dev.off()

### Word by word

segmentLine <- function(line){
  Date <- c()
  Time <- c()
  Author <- c()
  Word <- c()
  
  for(k in 1:ncol(result)){result[, k] %<>% as.character()}
  
  for(i in 1:nrow(line)){
    print(i)
    tempDate <- line$date[i]
    tempTime <- line$time[i] %>% as.character()
    tempAuthor <- paste(line$author[i])
    theseWords <- line$message[i] %>% as.character %>% strsplit(" ")
    theseWords <- theseWords[[1]]
    for(j in 1:length(theseWords)){
      Date %<>% append(paste(tempDate))
      Time %<>% append(paste(tempTime))
      Author %<>% append(paste(tempAuthor))
      Word %<>% append(paste(theseWords[j]))
    }
  }
  stopifnot(length(Date)==length(Time))
  stopifnot(length(Date)==length(Author))
  stopifnot(length(Date)==length(Word))
  stopifnot(length(Time)==length(Author))
  stopifnot(length(Time)==length(Word))
  stopifnot(length(Author)==length(Word))
  result <- data.frame(Date, Time, Author, Word)
  # had capitalize letters because of existing functions with similar names
  names(result) <- c("date", "time", "author", "word")
  result$date %<>% ymd()
  result$day <- wday(result$date, label = TRUE, abbr = FALSE)
  return(result)
}

#
# CAUTION: long execution
#
# lineWord <- segmentLine(line.raw)
#
#
#
dim(line.raw)
dim(lineWord)

topWord <- lineWord$word %>% table() %>% sort(decreasing = T) %>% head(200)
names(topWord)
datatopWord <- lineWord[lineWord$word %in% names(topWord), ]

png("Maud_NbMessParMessage_Mot.png", width = 2000, height = 480)
wordAuthor <- ggplot(datatopWord, aes(x=word))
wordAuthor +
  geom_bar(aes(fill = author)) +
  theme_light() +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
dev.off()
