#####################################################################
## plot timeline only

## LOAD FONT LIBRARIES
## use same font as slides
library(showtext)
source("functions/makeTransparent.R")
font.add.google("Montserrat")


plot_date <- read.table("data/important_dates.txt", header = F, stringsAsFactors = F, as.is = T)
colnames(plot_date)<- c("type", "name","date_end","date_beg","colour")



plot_all <- T
plot_neander <- F
plot_people <- F
plot_ice <- F
plot_write <- F

if(plot_all == T)
{
  plot_name <- ""
}
if(plot_neander == T)
{
  plot_name <- "neander"
}
if(plot_people == T)
{
  plot_name <- "humans"
}
if(plot_ice == T)
{
  plot_name <- "ice"
}
if(plot_write == T)
{
  plot_name <- "write"
}

plot_name <- "CP"

png(paste0("figures/eurasiaTimeline",plot_name,".png"),
           res = 350, height = 2*900, width = 2*1600)

forecol <- "ghostwhite"
par(lwd = 3, bg = NA, fg = forecol, family = "Montserrat")

date_range <- c(-2000,15000)
date_split <- 2000
par(mar = c(4,2,4,2))
maxy <- 14 #nrow(plot_date) + 2
plot(x=0,y = 0,type = "n",
     axes = F, xlim = date_range, ylim = c(0,maxy),
     xlab = "", ylab = "", yaxs = "i", xaxs = "i")

##########################################################
## PUT IN AGES AND OTHERS
bar_count <- maxy - 1
line_count <- 1
for(i  in 1:nrow(plot_date))
{
  xl <- plot_date$date_end[i]
  yb <- 0
  xr <- plot_date$date_beg[i]
  yt <- maxy
  
  if(plot_date$type[i] == "age")
  {
    rect(xl,yb,xr,yt,col = makeTransparent(plot_date$colour[i],150), border = NA)
    text(x=mean(c(xl,xr)),y=maxy+0.2, labels = gsub("_"," ",plot_date$name[i]),
         col = plot_date$colour[i], xpd = T, srt = 45, adj  = 0)
    axis(3,at = c(xl,xr), labels = NA, col.ticks =  plot_date$colour[i], lwd.ticks = 1.5)
  }
  
  if(plot_date$type[i] == "bar")
  {
    if((plot_neander == T & plot_date$name[i] == "Neanderthals") || plot_people == T || plot_ice == T || plot_write == T)
    {
      rect(xl,bar_count-0.4,xr,bar_count+0.4,col = plot_date$colour[i], border = NA)
      text(x=mean(c(xl)),y=bar_count, labels = plot_date$name[i],
           col = "black", xpd = T, srt = 0, adj  = c(0,0.5))
      bar_count <- bar_count - 1
    }
  }
  
  if(plot_date$type[i] == "line")
  {
    if((plot_ice == T & plot_date$name[i] %in% c("end_of_Ice_Age","Last_Glacial_Maximum")) || plot_write == T)
    {
      abline(v = xl, col = plot_date$colour[i], lwd = 3)
      text(x=xl, y = line_count, col = "black", labels = gsub("_"," ",plot_date$name[i]),
           adj  = 0)
      line_count <- line_count + 1
    }
  }
}

xat <- c(date_range[1],seq(0,date_range[2],date_split))
xlabs <- prettyNum(xat, big.mark = ",")
xlabs[1] <- ""
axis(1, at = xat, labels = xlabs, padj = 1, col.axis = forecol)
mtext(1, text = "Time (years BCE)", col = forecol, line = 3)

box()
####################################

date_data <- read.table("data/ancient_genomes.txt", header = F, as.is = T, stringsAsFactors = F)
colnames(date_data) <- c("ind","type","country","period","min.date", "max.date")
plot_date <- date_data


for(i in 1:nrow(plot_date))
{
  tmpmax <- as.numeric(plot_date$min.date[i])
  tmpmin <- as.numeric(plot_date$max.date[i])
  box_col <- "hotpink"
  if(plot_date$type[i] == "EF") box_col <- "springgreen"
  rect(tmpmin,(i-2)-0.2,tmpmax,(i-2)+0.2, col = box_col, border = NA)
  ind_label <- paste0(plot_date$country[i]," [",plot_date$ind[i],"]")
  text(tmpmax+500, y = i-2, labels = ind_label, cex = 0.8, xpd = T, adj = c(0,0.5))
  # points(tmpmin-500,y = i, lwd = 1.5,
  #        pch = collabs$pch[collabs$pop==plot_date$pop[i]],
  #        col = collabs$rim.col[collabs$pop==plot_date$pop[i]],
  #        bg = collabs$bg.col[collabs$pop==plot_date$pop[i]])
}

legend("bottomleft", bty = "n", legend = c("Hunter-gatherer", "Farmer"),
       fill = c("hotpink", "springgreen"), border = c("hotpink", "springgreen"))



dev.off()
