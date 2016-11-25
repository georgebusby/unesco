############################################
###  PLOT A NICE MAP OF ANCIENT GENOMES  ###
############################################

## LOAD MAPPING LIBRARIES
library(maptools)
library(RColorBrewer)
library(rgdal)
library(maps)
source("functions/plot.map.R")

## LOAD FONT LIBRARIES
## use same font as slides
library(showtext)
font.add.google("Montserrat")

setwd('~/repos/mapping_trials/shipping_map/')
wrld <- readOGR(".", "ne_110m_admin_0_countries")
setwd('~/repos/presentations/unesco/')

bdata <- read.csv2("data/lazaridis2016.csv", header = T, sep = ",",
                   stringsAsFactors = F, as.is = T)

## MATCH LABELLING TO LAZ PAPER
#write(unique(bdata$Analysis.Label),file = "data/lazaridis2016.analysislabels.txt",ncol =1)
collabs <- read.table("data/lazaridis2016.analysislabels.txt", header = T, stringsAsFactors = F, as.is = T)
collabs <- collabs[order(collabs$bg.col, collabs$pop, decreasing = T),]

labels <- bdata$Analysis.Label
ppch <- pcol <- rimcol <- indreg <- indpop <- c()
for(i in labels)
{
  ppch <- c(ppch,collabs$pch[collabs$pop==i])
  pcol <- c(pcol,collabs$bg.col[collabs$pop==i])
  rimcol <- c(rimcol,collabs$rim.col[collabs$pop==i])
  indpop <- c(indpop,collabs$pop[collabs$pop==i])
  indreg <- c(indreg,collabs$region[collabs$pop==i])
}  


country_col <- "#b5c7c2"
sea_col <- "#e2edf2"
  
## PLOT MAP OF THE WORLD
png("figures/worldMap.png",res = 100, height = 1800,width = 3200)
  #layout(matrix(c(1,2), 1,2), widths = c(5,2.5))
  # plot.map("world", center=200, col=country_col,bg=sea_col,
  #          fill=TRUE,projection = "mercator",
  #          ylim=c(-60,90),mar=c(0,0,0,0), lwd = 0.01)
  # 
  map("world", resolution=0.001, 
      col=country_col,bg=sea_col,
      fill=TRUE,projection = "mercator",
      mar=c(0,0,0,0), lwd = 0.01,
      orient=c(90,160,0), wrap = TRUE,
      ylim = c(-60,90))
dev.off()





png("figures/lazaridisMap.png",res = 350,
    height = 3*900, width = 3*1600) 
#layout(matrix(c(1,2), 1,2), widths = c(5,2.5))
par(family = "Montserrat")
par(mar=c(0,0,0,0))
plot(wrld, col = country_col, bg =sea_col,
     xlim = c(-5,55), ylim = c(25,70), lwd = 0.01)

points(jitter(as.numeric(bdata$Longitude), factor = 150),
       jitter(as.numeric(bdata$Latitude), factor = 150), pch = ppch, col = rimcol, bg=pcol)

legend("bottomleft", legend = collabs$pop, pch = collabs$pch,
       pt.bg = collabs$bg.col, col = collabs$rim.col,ncol = 3, bty = "n",
       title = "281 Ancient Eurasian Genomes")

#dev.off()

sample_age <- bdata$Date..One.of.two.formats...Format.1..95.4..CI.calibrated.radiocarbon.age..Conventional.Radiocarbon.Age..Lab.number..e.g..5983.5747.calBCE..6980.50.BP..Beta.226472..or..Format.2..Archaeological.context.date..e.g..2500.1700.BCE
sample_agetab <- c()
for(i in sample_age)
{
  tmp <- strsplit(i, split = " ")[[1]]
  if(tmp == "..")
  {
    age1 <- age2 <- age3 <- NA
  } else
  {
    age1 <- as.numeric(strsplit(tmp[1], split = "-")[[1]][1])
    age2 <- as.numeric(strsplit(tmp[1], split = "-")[[1]][2])
    age3 <- tmp[2]
  }
  sample_agetab <- rbind(sample_agetab,c(age1,age2))
}

sample_agetab <- cbind(sample_agetab,indreg, indpop)
sample_agetab <- data.frame(sample_agetab, stringsAsFactors = F)
colnames(sample_agetab) <- c("maxdate","mindate","region","pop")
sample_agetab$maxdate <- as.numeric(as.character(sample_agetab$maxdate))
sample_agetab$mindate <- as.numeric(as.character(sample_agetab$mindate))


date_data <- c()
for(i in unique(sample_agetab$pop))
{
  tmpmin <- min(sample_agetab$mindate[sample_agetab$pop==i], na.rm = T)
  tmpmax <- max(sample_agetab$maxdate[sample_agetab$pop==i], na.rm = T)
  reg <- unique(sample_agetab$region[sample_agetab$pop==i])
  date_data <- rbind(date_data,c(i,reg,tmpmin,tmpmax))
}

date_data <- data.frame(date_data, stringsAsFactors = F)
colnames(date_data) <- c("pop","region","min.data","max.date")
date_data$region <- factor(date_data$region,
                           levels = c("Levant","Iran","Anatolia","Caucasus","Mainland_Europe","Eurasian_Steppe","Russia"))
date_data <- date_data[order(date_data$region,as.numeric(date_data$max.date)),]  
plot_date <- date_data[date_data$region!="Russia",]


# par(mar = c(4,1,1,1))
# plot(x=rep(0,nrow(plot_date)),y = 1:nrow(plot_date),type = "n",
#      axes = F, xlim = c(0,15000), xlab = "Time (years BCE)", ylab = "")
# ## dashed lines
# for(i in unique(plot_date$region))
# {
#   abline(h=max(which(plot_date$region==i))+0.5, lty = 2)
#   text(x=12500,y = median(which(plot_date$region==i)), font = 2, labels = gsub("\\_"," ",i))
# }
# xat <- seq(0,15000,2500)
# axis(1, at = xat, labels = prettyNum(xat, big.mark = ","))
# 
# box()
# for(i in 1:nrow(plot_date))
# {
#   tmpmin <- as.numeric(plot_date$min.data[i])
#   tmpmax <- as.numeric(plot_date$max.date[i])
#   rect(tmpmin,i-0.2,tmpmax,i+0.2, col = "grey", border = NA)
#   text(tmpmax+500, y = i, labels = plot_date$pop[i], cex = 0.8, xpd = T, adj = c(0,0.5))
#   points(tmpmin-500,y = i,
#          pch = collabs$pch[collabs$pop==plot_date$pop[i]],
#          col = collabs$rim.col[collabs$pop==plot_date$pop[i]],
#          bg = collabs$bg.col[collabs$pop==plot_date$pop[i]])
# }

dev.off()