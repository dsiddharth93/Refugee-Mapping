# Refugee Data Visualization
library(ggplot2)
library(reshape2)
library(grid)
library(wordcloud)
library(extrafont)
library(scales)

# http://popstats.unhcr.org/en/overview

setwd("~/Documents/UNHCR/UNHCR Data/") # ~ acts as base for home directory

list.files() # list files or folders in current working directory 
list.files(full.names=TRUE) # gives full path for files or folders - ./ shows that its a folder
# list files in the Canada directory
files.all <- list.files(path="All_Data/") #assigns name to file names in the All_Data folder
length(files.all) #checks how many objects there are in the /All_Data folder
files.all

ref.d <- read.csv(paste0("All_Data/",files.all), #insert filepath and name into 1st argument
                  header=T, #select the headers in 3rd row
                  skip=2, #skips the first rows (metadata in .csv file)
                  na.string=c("","-","*"), #convert all blanks, "i","*" cells into missing type NA
                  col.names=new.names #since we already made new names
)

#I don't like names, so we'll specify them
new.names <- c("Year", "Country", "Country_Origin", "Refugees", "Asylum_Seekers", "Returned_Refugees", "IDPs", "Returned_IDPs", "Stateless_People", "Others_of_Concern","Total") 
names(ref.d) <- new.names

for(i in 2:length(names(ref.d))){ # "2"-ignores the first column (we want to keep Year as an integer)
  if (class(ref.d[,i])=="factor"){
    ref.d[,i] <- as.character(ref.d[,i]) #converts all columns with factor type to string
  }
  if (class(ref.d[,i])=="integer"){
    ref.d[,i] <- as.numeric(ref.d[,i]) #converts all columns with integer to numeric
  }}

# list of the country names I want to change
old.countries <- c("Bolivia (Plurinational State of)",
                   "China, Hong Kong SAR",
                   "China, Macao SAR",
                   "Iran (Islamic Rep. of)",
                   "Micronesia (Federated States of)",
                   "Serbia and Kosovo (S/RES/1244 (1999))",
                   "Venezuela (Bolivarian Republic of)",
                   "Various/Unknown")

# replacement names
new.countries <- c("Bolivia","Hong Kong","Macao","Iran","Micronesia","Serbia & Kosovo","Venezuela","Unknown")

for (k in 1:length(old.countries)){ 
  ref.d$Country_Origin[ref.d$Country_Origin==old.countries[k]]<-new.countries[k]
  ref.d$Country[ref.d$Country==old.countries[k]]<-new.countries[k]
}

# Creating a list of the countries in Country
clist<-sort(unique(ref.d$Country)) #alphabetical
clist

# Creating a list of the countries in Country_Origin
or.clist<-sort(unique(ref.d$Country_Origin)) #alphabetical
or.clist

# We want to if there are any differences between these two vectors of country names
# http://stackoverflow.com/questions/9162134/comparing-character-vectors-in-r-to-find-unique-and-or-missing-values
clist[!clist %in% or.clist] #which countries from clist are not in or.clist
# this means that these countries either don't have data for refugees or there are no refugees in these countries
# this means that these countries have not produced refugees or there is no data on refugees from these countries
setdiff(clist,or.clist) 

or.clist[!or.clist %in% clist] #which countries from or.clist are not in clist
# this means that these are ONLY countries of origin and either there is no data that refugees seltted or there are no refugees in these countries
# this means that these countries (ie. NK) have only produced refugees (not taken in any refugees) or there is no data on this.
setdiff(or.clist,clist) 

NK<-ref.d[which(ref.d$Country_Origin=="Dem. People's Rep. of Korea"),] #retrieves the rows that match the criteria: country of origin as NK

# WORDCLOUDS

# COUNTRY OF ORIGIN
or.count.tot <- aggregate(cbind(Total)~Country_Origin,data=ref.d,FUN=sum)
or.count.tot

# NK
NK.tot<- aggregate(cbind(Total)~Country,data=NK,FUN=sum)
NK.tot[order(-NK.tot[,2]),][1:10,]

# Set the Palette
pal3 <- c("#274c56", #http://tools.medialab.sciences-po.fr/iwanthue/
          "#664c47",
          "#4e5c48",
          "#595668",
          "#395e68",
          "#516964",
          "#6b6454",
          "#58737f",
          "#846b6b",
          "#807288",
          "#758997",
          "#7e9283",
          "#a79486",
          "#aa95a2",
          "#8ba7b4")

# COUNTRY OF ORIGIN
png("wordcloud_count_or.png",width=600,height=850,res=200)
wordcloud(or.count.tot[,1], #list of words
          or.count.tot[,2], #frequencies for words
          scale=c(3,.5), #scale of size range 
          min.freq=100, #minimum frequency
          max.words=100, #maximum number of words show (others dropped)
          family="Garamond", font=2, #text edit (The font "face" (1=plain, 2=bold, 3=italic, 4=bold-italic))
          # https://www.stat.auckland.ac.nz/~paul/R/fonts.html
          random.order=F, #F-plotted in decreasing frequency 
          colors=rev(pal3)) #colours from least to most frequent
dev.off()

# PLOTS
# 1st prepare data in LONG form to use in ggplot2 http://stackoverflow.com/questions/21236229/stacked-bar-chart

# going to use melt() to assign long form for ggplot2 so that the fill() in aes() can be set to the levels in variable
no.count<-(ref.d[c(-2,-3,-11)]) #removes Country, Country of Origin and Total
lno.count<-melt(no.count,id=c("Year"))
head(lno.count)

# set the theme
blank_t <- theme_minimal()+
  theme(
    panel.border = element_blank(), #removes border
    panel.background = element_rect(fill = "#d0d1cf",colour=NA),
    panel.grid = element_line(colour = "#ffffff"),
    plot.title=element_text(size=20, face="bold",hjust=0,vjust=2), #set the plot title
    plot.subtitle=element_text(size=15,face=c("bold","italic"),hjust=0.01), #set subtitle
    legend.title=element_text(size=10,face="bold",vjust=0.5), #specify legend title
    axis.text.x = element_text(size=9,angle = 0, hjust = 0.5,vjust=1,margin=margin(r=10)),
    axis.text.y = element_text(size=10,margin = margin(r=5)),
    legend.background = element_rect(fill="gray90", size=.5),
    legend.position=c(0.15,0.65),
    plot.margin=unit(c(t=1,r=1.2,b=1.2,l=1),"cm")
  )

# set the palette
pal5 <- c("#B53C26", #Refugees 7 TOTAL
          "#79542D", #Asylum Seekers
          "#DA634D", #Returned Refugees
          "#0B4659", #IDPs
          "#4B8699", #Returned IDPs
          "#D38F47", #Stateless People
          "#09692A") #Others of Concern
# stacked barplot of refugee types by year
gg1 <-ggplot(lno.count,aes(Year,value)) + 
  geom_bar(aes(fill=variable),stat="identity") +
  labs(title="UNHCR Population Statistics Database",
       subtitle="(1951 - 2014)",
       x="Year",y="Number of People (Millions)") + blank_t +
  scale_fill_manual(guide_legend(title="Populations of Concern"),labels=gsub("*_"," ",names(ref.d)[c(-1,-2,-3,-11)]),values=pal5) #gsub removes the _ from the names

mil.func <- function(x) {x/1000000 }

require(scales)
gg2<-gg1+
  scale_y_continuous(limits=c(0,6e7), 
                     breaks=pretty(0:6e7,n=5), 
                     labels=mil.func, 
                     expand=c(0.025,0)) +
  
  scale_x_continuous(limits=c(1950,2015), 
                     breaks=seq(1950,2015,by=5), 
                     expand=c(0.01,0))

# save the plot
ggsave(plot=gg2,filename="UNHCR_Totals_Yr.png", width=9.5, height=5.5, dpi=200)