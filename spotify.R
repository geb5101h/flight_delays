library(magrittr)
library(glasso)
library(dplyr)
library(ggplot2)
library(tidyr)
library(huge)

NUM_AIRPORTS = 25

data <- read.csv("flight_data_jan2015.csv")

#set NA delays to 0
data$DEP_DELAY[data$DEP_DELAY %>% is.na] <- 0

#get largest airports
largest_airports <- data %>%
  group_by(ORIGIN) %>%
  tally %>%
  arrange(n %>% desc) %>%
  .[["ORIGIN"]]


#create time varianble
data = data %>%
  filter(ORIGIN %in% largest_airports[1:NUM_AIRPORTS]) %>%
  rowwise() %>%
  mutate(DT =  {
    DOM = ifelse(DAY_OF_MONTH <= 9,paste(0,DAY_OF_MONTH,sep = ""),DAY_OF_MONTH)
    ifelse(
      CRS_DEP_TIME %>% nchar == 3,
      paste("01",DOM,0,CRS_DEP_TIME,sep = "") %>%
        strptime(.,format = "%m%d%H%M") %>% as.numeric(),
      paste("01",DOM,CRS_DEP_TIME,sep = "") %>% strptime(.,"%m%d%H%M") %>% as.numeric
    )
  })

data %>%
  arrange(DT) %$%
  plot(DT,DEP_DELAY,type = "l")


#
# get airport data and merge with flight
# data
airports <- read.csv("airports.csv") %>%
  filter(!(state %in% c("AK","HI"))) %>%
  dplyr::select(lat,long,iata)

data <-
  merge(
    x = data,
    y = airports,
    by.x = "ORIGIN",
    by.y = "iata",
    all.x = T
  )


# discretize time variable
data$time_new <- data$DT %>%
  cut(.,breaks = 93,labels = F)

#plot
pdf("ts.pdf",width=10,height=7)
  data %>%
    filter(ORIGIN %in% c("ORD","LGA"))%>%
    ggplot(.,aes(x=time_new,y=DEP_DELAY,color=DEP_DELAY))+
    geom_point(size=.5,alpha=.9)+
      facet_grid(~ORIGIN)+
    scale_colour_gradient(low="#0072B2",high="red")+
    xlab("Time (Jan 1-31, 2015)")+
    ylab("Departure delay (minutes)")+
    scale_x_discrete(breaks=c(0,60),
                     labels=c("Jan 1","Jan 31"))
  dev.off()


#data aggregated by time bin
data_time_agg <- data %>%
  group_by(time_new,ORIGIN) %>%
  summarize(MEAN_DEP_DELAY = mean(DEP_DELAY,na.rm = T))

#reshape to wide format
wide <- data_time_agg %>%
  spread(.,key = ORIGIN,value = MEAN_DEP_DELAY)


######################
# model fitting
# stage
#
######################

wide[wide %>% is.na] = 0
huge_output <- huge(
  wide[-1]%>%(function(x) log(x+20)) %>% as.matrix(),
  method = "glasso",lambda.min.ratio = .01,nlambda = 20,scr=T,verbose=T)
huge_selected <- huge.select(huge_output,criterion="stars",rep.num=10)

huge_selected$icov[[4]] %>% plot

######################
#
# plotting
#


biggest_airports <- airports %>%
  rowwise() %>%
  filter(iata %in% largest_airports[1:NUM_AIRPORTS])

lo <- biggest_airports %>%
  dplyr::select(long,lat) %>% as.matrix


graph_origin <-
  graph.adjacency((huge_selected$icov[[7]] != 0) %>% as.matrix,
                  mode = "undirected",diag = F)

library(maps)

pdf("full.pdf")
map("state")
points(lo,pch = 16,cex = 1)

graph_origin %>% get.edgelist() %>%
  apply(.,1,function(x) {
    longlat1 = airports %>%
      rowwise %>%
      filter(iata %>% as.character == largest_airports[x[1]]) %>%
      .[c("long","lat")]
    longlat2 = airports %>%
      rowwise %>%
      filter(iata %>% as.character == largest_airports[x[2]]) %>%
      .[c("long","lat")]
    points(longlat1,col = "red",pch = 16,cex = 1)
    points(longlat2,col = "red",pch = 16,cex = 1)
    lines(c(longlat1[1],longlat2[1]),
          c(longlat1[2],longlat2[2]),cex = .3,col = "grey")
  })
dev.off()

#####################################
# some graph stats

for(i in 1:25){print(largest_airports[i]); print(graph_origin%>%neighbors(i))}

#####################################
#risk analysis

DISRUPTION = 60

delay_means <- wide[-1]%>%colMeans
delay_cov <- huge_selected$icov[[7]]%>%solve

new_mu<- delay_means[-20] +
  delay_cov[,20][-20] *
  (60-delay_means[-20])/delay_cov[20,20]

#
#miscellaneous calculations

data %$%
  lm(ARR_DELAY ~ DEP_DELAY) %>%
  summary()

#departure/arrivals
data %$%
  plot(DEP_DELAY %>% log,ARR_DELAY %>% log,pch = 16)

data %>%
  filter(DEP_DELAY == 0) %$%
  hist(ARR_DELAY,breaks = 100)

#histograms by day
data %>%
  ggplot(., aes(x = ARR_DELAY %>% log, fill = DAY_OF_MONTH %>% as.factor)) +
  geom_density(alpha = .3)

