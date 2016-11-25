#####################################################################
## plot timeline only

## LOAD FONT LIBRARIES
## use same font as slides
library(showtext)
source("functions/makeTransparent.R")
font.add.google("Montserrat")


plot_date <- read.table("data/important_dates.txt", header = F, stringsAsFactors = F, as.is = T)
colnames(plot_date)<- c("type", "name","date_end","date_beg","colour")



plot_all <- F
plot_neander <- F
plot_people <- F
plot_ice <- F
plot_write <- F
plot_name <- "Laz"

png(paste0("figures/eurasiaTimeline",plot_name,".png"),
           res = 350, height = 2.5*900, width = 2.5*1600)

forecol <- "ghostwhite"
par(lwd = 3, bg = NA, fg = forecol, family = "Montserrat")

date_range <- c(-2000,15000)
date_split <- 2000
par(mar = c(4,2,4,2))
maxy <- 28 #nrow(plot_date) + 2
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

tmp_type <- sapply(plot_date$pop,function(x){strsplit(x,split="\\_")[[1]][2]})
tmp_type[is.na(tmp_type)] <- "HG"
tmp_type[tmp_type != "HG"] <- "F"
plot_date <- cbind(plot_date,tmp_type)

# ## dashed lines
for(i in unique(plot_date$region))
{
  abline(h=max(which(plot_date$region==i))+0.5, lty = 2)
  text(x=13500,y = median(which(plot_date$region==i)), font = 2, labels = gsub("\\_"," ",i))
}
xat <- seq(0,15000,2500)
#axis(1, at = xat, labels = prettyNum(xat, big.mark = ","))


for(i in 1:nrow(plot_date))
{
  tmpmin <- as.numeric(plot_date$min.data[i])
  tmpmax <- as.numeric(plot_date$max.date[i])
  rect_col <- "hotpink"
  if(plot_date$tmp_type[i]=="F") rect_col <- "springgreen"
  rect(tmpmin,i-0.2,tmpmax,i+0.2, col = rect_col, border = NA)
  text(tmpmax+250, y = i, labels = plot_date$pop[i], cex = 0.8, xpd = T, adj = c(0,0.5))
  points(tmpmin-250,y = i, lwd = 1.5,
         pch = collabs$pch[collabs$pop==plot_date$pop[i]],
         col = collabs$rim.col[collabs$pop==plot_date$pop[i]],
         bg = collabs$bg.col[collabs$pop==plot_date$pop[i]])
}

legend("bottomleft", bty = "n", legend = c("Hunter-gatherer", "Farmer"),
       fill = c("hotpink", "springgreen"), border = c("hotpink", "springgreen"))

dev.off()
