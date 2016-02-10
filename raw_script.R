# http://www.r-bloggers.com/parsing-complex-text-files-using-regular-expressions-and-vectorization/
require(lubridate)
require(dplyr)
require(tidyr)
require(ggplot2)
require(scales)
line.raw <- read.csv('chat.csv', sep = '\t')
line.raw$date <- dmy(line.raw$date) # fomr LINE2CSV
# line.raw$date <- ymd(line.raw$date) # from line2csv
line.raw$message <- as.character(line.raw$message)
# summary(line.raw)

# -------------------------------
# plot du nb de messages par jour
# -------------------------------

nbmsgjr <- ggplot(line.raw, aes(x=wday(date, label = TRUE, abbr = FALSE)))

svg("NbMessParJour.svg")
nbmsgjr+geom_histogram()+
  labs(x = "Jour de la semaine", y = "Nombre de messages", title = paste("Nombre de messages par jour du ",line.raw$date[1]," au ",line.raw$date[nrow(line.raw)]))
dev.off()

# svg("NbMessParJourAuthor.svg")
# nbmsgjr+geom_histogram()+
#   facet_grid(author ~ .)+
#   labs(x = "Jour de la semaine", y = "Nombre de messages", title = paste("Nombre de messages par jour du ",line.raw$date[1]," au ",line.raw$date[nrow(line.raw)]))
# dev.off()

svg("NbMessParJourAuthor.svg")
nbmsgjr+geom_histogram(aes(fill=author), position=position_dodge())+
  labs(x = "Jour de la semaine", y = "Nombre de messages", title = paste("Nombre de messages par jour et auteur du ",line.raw$date[1]," au ",line.raw$date[nrow(line.raw)]))
dev.off()

# -----------------------------
# plot du nb de message par h:m
# -----------------------------

nbmsghm <- ggplot(line.raw, aes(x = sort(time)))

svg("NbMessParHM.svg", width = 14)
nbmsghm+geom_histogram()+
  scale_x_discrete(breaks = c("06:04","07:01","08:00","09:02","10:00","11:00","12:00","13:00","14:01","15:02","16:00","17:01","18:00","19:00","20:00","21:00","22:00","23:00","23:53"))+
  labs(x = "Heures", y = "Nombre de messages", title = paste("Nombre de messages par heures du ",line.raw$date[1]," au ",line.raw$date[nrow(line.raw)]))
dev.off()

# ---------------------------
# Heatmap des heures par jour
# ---------------------------
timeDay <- t(as.matrix(table(sort(line.raw$date), sort(line.raw$time))))
# heatmap(timeDay, Colv = NA, Rowv = NA, scale = "row")

# -------------
# Circular plot
# -------------
# http://rstudio-pubs-static.s3.amazonaws.com/3369_998f8b2d788e4a0384ae565c4280aa47.html
line.raw$time <- as.POSIXct(strptime(line.raw2$datetime,"%H:%M"))
line.raw %>% group_by(date)

------------------------------------------------------------------------------
------------------------------------------------------------------------------

line.raw2 <- read.csv('chat_2.csv', sep = '\t')
# line.raw2$Hours <- format(as.POSIXct(strptime(line.raw2$datetime,"%Y/%m/%d %H:%M")) ,format = "%H:%M")
line.raw2$Hours <- format(as.POSIXct(strptime(line.raw2$datetime,"%d/%m/%Y %H:%M")) ,format = "%H:%M")
line.raw2$Hours <- as.POSIXct(strptime(line.raw2$Hours, "%H:%M"))
# line.raw2$Dates <- format(as.POSIXct(strptime(line.raw2$datetime,"%Y/%m/%d %H:%M")) ,format = "%Y/%m/%d")
# line.raw2$Dates <- as.POSIXct(strptime(line.raw2$Dates, "%Y/%m/%d"))
line.raw2$Dates <- format(as.POSIXct(strptime(line.raw2$datetime,"%d/%m/%Y %H:%M")) ,format = "%d/%m/%Y")
line.raw2$Dates <- as.POSIXct(strptime(line.raw2$Dates, "%d/%m/%Y"))
# line.raw2$datetime <- as.POSIXct(strptime(line.raw2$datetime, "%Y/%m/%d %H:%M"))
line.raw2$datetime <- as.POSIXct(strptime(line.raw2$datetime, "%d/%m/%Y %H:%M"))
line.raw2$message <- as.character(line.raw$message)

frise <- ggplot(line.raw2, aes(x = datetime))

svg("Frise.svg", width = 14)
frise+geom_histogram()+
  scale_x_datetime(breaks=date_breaks("1 month"))+
  theme(axis.text.x=element_text(angle=90)) +
  labs(x = "Jour", y = "Nombre de messages", title = paste("Nombre de messages par jour du ",line.raw$date[1]," au ",line.raw$date[nrow(line.raw)]))
dev.off()

svg("FriseAuthor.svg", width = 14)
frise+geom_histogram(aes(fill=author), position=position_dodge())+
  scale_x_datetime(breaks=date_breaks("1 month"))+
  theme(axis.text.x=element_text(angle=90))+
  labs(x = "Jour", y = "Nombre de messages", title = paste("Nombre de messages par jour et auteur du ",line.raw$date[1]," au ",line.raw$date[nrow(line.raw)]))
dev.off()

timescat <- ggplot(line.raw2, aes(x=Dates, y=Hours))

svg("HeuresChaqueJour.svg", width = 14)
timescat +geom_point() + 
  scale_y_datetime(breaks=date_breaks("1 hour"), labels=date_format("%H:%M")) + 
  scale_x_datetime(breaks=date_breaks("1 day")) + 
  theme(axis.text.x=element_text(angle=90)) +
  labs(x = "Jour", y = "Heure", title = paste("Heures des messages par jour du ",line.raw$date[1]," au ",line.raw$date[nrow(line.raw)]))
dev.off()


# -----------
# Some number
# -----------
msgByDay <- line.raw2 %>% group_by(Dates) %>% summarize(count = n())
qplot(data = msgByDay, Dates, geom = "histogram")
qplot(data = msgByDay, count, geom = "histogram")
line.raw2 %>% mutate(size = length(message)) %>% View
