#install.packages("network")
#install.packages("igraph")
#installed.packages("ggraph")
#install.packages("tidygraph")
#install.packages("cooccur")
#install.packages("visNetwork")
#install.packages("gplots")
#install.packages("RColorBrewer")
#install.packages("qpcR")

setwd("C:/Users/Yussuf Schwarz/Documents")
library("pheatmap")
library(qpcR)
library("gplots")
library("RColorBrewer")
library(cooccur)
library(visNetwork)
library(tidygraph)
library(igraph)
library(taskscheduleR)
library(twitteR)
library(tidytext)
library(rtweet)
library(dplyr)
library(network)


appname <- "twitter_analyse_get_on"
apikey <- "QW9bmRIjnQ2KaDLhmttx4C4Wf"
apisecret <- "rK5ElwwnC6gHs0qdBw5eDobqy08M68XglX1HMnXl21we4De9XK"
bearertoken <- "AAAAAAAAAAAAAAAAAAAAAGZkWQEAAAAAfecS4ZjaqlN2VVipwv%2BGD651tNE%3DXCddr0YeQWXaVrISGlCm3qOIjNtYKbsWorLFOLrwUE7DdCj8Aa"
accesstoken <- "1476654668696670217-xf9sDHymT3Rdf7eHMAkrD1aVJbGu0a"
accesssecret <- "WxfBsv9pubWstnnWCUW4OO1FMSMYFGWpbfzvOyR5xyp5v"

twitter_token <- create_token(app = appname,consumer_key = apikey,consumer_secret = apisecret,access_token = accesstoken,access_secret = accesssecret)


#######################################################################################
#######################################################################################
#######################################################################################
#######################################################################################
#######################################################################################
#######################################################################################
#######################################################################################
#######################################################################################
#############################GERMANY#####################################################

repeat{
trends <- get_trends(woeid = "Germany",lat = NULL,lng = NULL,exclude_hashtags = FALSE,token = NULL,parse = TRUE)
trends<-trends[1:15,]

for (g in 1:nrow(trends)){
  rstats_tweets <- search_tweets(q = toString(trends[g,1]),n = 1000,include_rts=FALSE)
  rstats_tweets1 <- rstats_tweets[!duplicated(rstats_tweets$user_id),]
  loc <- unique(rstats_tweets$location)
  ##
  
  s1 = matrix(nrow = 1,ncol = length(unique(rstats_tweets$source)))
  for (b in 1:length(unique(rstats_tweets$source))) {
    s1[1,b]= sum(unique(rstats_tweets$source)[b] == rstats_tweets$source)
  }
  
  names123 = (unique(rstats_tweets$source))
  colnames(s1) = s1[0,]
  colnames(s1) = names123
  ss = paste("source", g,".png" ,sep="")
  main123 = paste("Sources of n = 1000 tweets of the hashtag",trends[g,1],sep=" ")
  png(ss,width = 750,height = 650,res=100)
  par(mar=c(11,4,4,4))
  barplot(names.arg = colnames(s1),height=s1[1,],las=3,cex.names = 0.7,ylim = c(0,max(s1[1,]+50)),main=main123)
  dev.off()
  
  #tweeet2 = paste("Trend analysis: Sources of a trend. Devices used tweeting the n = 1000 most recent tweets. NOTE: If APIs are used to tweet, Twitter states the name of the API as the source",trends[g,1],sep=" ")
  #post_tweet(status = tweeet2,media = ss)
  

  ##########
  
  s2 = matrix(nrow = 1,ncol = length(unique(rstats_tweets$lang)))
  for (c in 1:length(unique(rstats_tweets$lang))) {
    s2[1,c]= sum(unique(rstats_tweets$lang)[c] == rstats_tweets$lang)
  }
  
  names1234 = (unique(rstats_tweets$lang))
  colnames(s2) = s2[0,]
  colnames(s2) = names1234
  langs = paste("lang", g,".png" ,sep="")
  main1234 = paste("Languages of n = 1000 tweets of the hashtag",trends[g,1],sep=" ")
  png(langs,width = 750,height = 650,res=100)
  par(mar=c(11,4,4,4))
  barplot(names.arg = colnames(s2),height=s2[1,],las=3,cex.names = 0.7,ylim = c(0,max(s2[1,]+50)),main=main1234,xlab = "Languages",sub = "Language code used: ISO 639 ")
  dev.off()
  #tweeetlang = paste("Trend analysis: Languages of n = 1000 most recent tweets of the hashtag. Note: Twitter automatically identifies the language of a tweet. Language codes used = ISO 639, de = German, en = Englisch, es = Spanisch, fr = French, it = italian, und = unidentified",trends[g,1],sep=" ")
  #post_tweet(status = tweeetlang,media = langs)
  
  ##################
  Sys.sleep(60)
  
  mostliked = rstats_tweets[order(rstats_tweets$retweet_count,decreasing=TRUE),]
  mostliked = mostliked[1:10,]
  mostliked = mostliked[order(mostliked$favorite_count,decreasing = TRUE),]
  
  png("retweeted_pic.png",width = 800,height = 650,res=100)
  par(mar=c(10,4,4,4))
  barplot(names.arg= mostliked$screen_name,height = mostliked$favorite_count,las=2,main = "10 Most liked tweet of a sample of n = 1000 tweets of the hashtag",cex.names=0.7,ylab="Number of likes")
  dev.off()
  
  tweeet1_mostliked = paste("A Thread of the 3 most liked tweets of the hashtag (n=1000) 1/x",trends[g,1],sep=" ")
  post_tweet(status = tweeet1_mostliked,media = "retweeted_pic.png")
  statusid = get_timeline("@Trendsanalysed",n = 1)
  statusid = statusid$status_id
  
  for (z in 1:3){
    retweetid = mostliked$status_id[z]
    textretweet = mostliked[z,]$text
    retweeteduser = mostliked[z,]$screen_name
    retweeteduser = gsub(" ","",x = retweeteduser)
    text_retweet = paste("Posted by:",retweeteduser,":",  mostliked[z,]$text)
    text_retweet = gsub("#"," ",text_retweet)
    text_retweet = substr(text_retweet,start = 0,stop = 280)
    post_tweet(status = text_retweet,in_reply_to_status_id = statusid)
  }
  
  Sys.sleep(60)
  
  ##
  

  location = c()
  for (i in 1:length(loc)) {
    location[i] = sum(rstats_tweets$location == loc[i])
  }
  
  length(rstats_tweets$location)
  herkunft <- as.data.frame(cbind(loc,location))
  herkunft$location <- as.numeric(herkunft$location)
  herkunft <- herkunft[order(herkunft$location),]
  herkunft <- herkunft[herkunft$location > 9,]
  herkunft$per <- (herkunft$location/(sum(herkunft$location)))*100
  pie_labels <- paste0(herkunft$loc, " = ", round(herkunft$per), "%")
  trend_name <- toString(trends[g,1])
  file_name = paste("pl", g,".png" ,sep="")
  png(file_name,width = 800,height = 500,res=100)
  par(mar=c(5,4,7,4))
  pie(herkunft$per,labels = pie_labels,sub="Location of n = 1000 most recent tweets of the hashtag in title",cex=0.6,radius = 1)
  title(main=trend_name,cex.main=0.9,line = 5)
  dev.off()
  #tweeet = paste("Trend analysis: Location of n = 1000 recent tweets of the hashtag. Note: Location can be modified manually. No label means user does not provide a location",trends[g,1],sep=" ")
  #post_tweet(status = tweeet,media = file_name)
  

  
  file_name1 = paste("pt", g,".png" ,sep="")
  png(file_name1,width = 700,height = 550)
  barplot(height = length(rstats_tweets1$user_id),col = c("lightblue","white"),ylim = c(0,1000),width = 1,xlim= c(0,2),names.arg = trends[g,1],xlab="Trend",ylab="Number of individual accounts",sub="n=1000",cex.names = 1)
  title("Number of individual accounts \nbehind n = 1000 most recent tweets of the hashtag")
  bar = toString(length(rstats_tweets1$user_id))
  text(0.7,length(rstats_tweets1$user_id)-50,bar,cex=2,col="red")
  dev.off()
  
  tweeet1 = paste("Trend analysis: Heterogeneity of a trend, Sources, Locations and Languages of the n = 1000 most recent tweets of the trend.",trends[g,1],sep=" ")
  post_tweet(status = tweeet1,media = c(file_name1,ss,file_name,langs))
  
  ######################################################################
  
  hash= data.frame(matrix(unlist(rstats_tweets1$hashtags), nrow=length(rstats_tweets1$hashtags), byrow=TRUE))
  hashdata = stack(hash,select = 1:ncol(hash))
  hashdat = as.matrix(cbind(trends[g,1],hashdata$values))
  hashdat = na.omit(hashdat)
  colnames(hashdat) = cbind("From","To")
  
  freq = matrix(nrow = length(unique(hashdat[,2])),ncol = 3)
  for (b in 1:length(unique(hashdat[,2]))){
    a=sum(hashdat[,2] == unique(hashdat[,2])[b])
    freq[b,2]= unique(hashdat[,2])[b]
    freq[b,3]=a
    freq[b,1] = toString(trends[g,1])
  }
  freq = as.data.frame(freq)
  colnames(freq) = c("from","to","freq")
  freq$freq = as.numeric(freq$freq)
  freq = freq[order(freq[,3],decreasing=TRUE),]
  freq = freq[1:13,]
  
  ig = graph_from_data_frame(d=freq, directed = FALSE)
  
  name_of_graphic = paste("net", g,".png" ,sep="")
  png(name_of_graphic,width = 1250,height = 1000,res=100)
  plot(ig, edge.width=sqrt(E(ig)$freq),vertex.label.dist=1,vertex.shape = "square", vertex.size=0, edge.color="lightgrey", edge.curved=0,vertex.label.font=2,vertex.label.cex=1.2)
  dev.off()
  
  tweeet1_graph = paste("Trend analysis: Interconnectivity between Trends. 13 most used Hashtags of the trend. Thicker arrows indicate higher interconnectivity",trends[g,1],sep=" ")
  post_tweet(status = tweeet1_graph,media = name_of_graphic)
  
  
  
  Sys.sleep(60)
}

#taskscheduler_create(taskname = "trendanalyse_10",rscript = "C:/Users/Yussuf Schwarz/Documents/trendanalyse.R",schedule = "DAILY",starttime = "10:00")
#taskscheduler_create(taskname = "trendanalyse_19",rscript = "C:/Users/Yussuf Schwarz/Documents/trendanalyse.R",schedule = "DAILY",starttime = "19:00")

Sys.sleep(500)

trends <- get_trends(woeid = "Germany",lat = NULL,lng = NULL,exclude_hashtags = FALSE,token = NULL,parse = TRUE)
trends<-trends[1:15,]
usid <- matrix(nrow = 3000,ncol = 1)
for (g in 1:nrow(trends)){
  rstats_tweets <- search_tweets(q = toString(trends[g,1]),n = 3000,include_rts=TRUE)
  usid = qpcR:::cbind.na(usid,rstats_tweets$user_id)
  Sys.sleep(600)
  
}
usid = usid[,-1]
#usid[is.na(usid)] <- 0
usid = na.omit(usid)

usidnet <- matrix(nrow = nrow(trends),ncol = nrow(trends))
for (g in 1:nrow(trends)){
  for (i in 1:nrow(trends)){
    usidnet[i,g]<- sum(usid[,g]==usid[,i])
  }
}
names12<-c()
for (g in 1:nrow(trends)){
  names12[g] = toString(trends[g,1])
}
usidnet<-cbind(c(names12),usidnet)
usidnet<-data.frame(usidnet)
row.names(usidnet) <- usidnet[,1]
usidnet <- usidnet[,-1]
for (g in 1:(nrow(trends))){
  usidnet[,g] <- as.numeric(usidnet[,g])
}

usidnet <- as.matrix(usidnet)
colnames(usidnet)=c(names12)
diag(usidnet) = -1


png("heat1.png",width = 700,height = 550)
pheatmap(usidnet, display_numbers = F, color = colorRampPalette(c('white','red'))(100), cluster_rows = F, cluster_cols = F, fontsize_number = 15,main = "Activity between trends n=1000 most recent trends")
dev.off()

for (g in 1:nrow(trends)){
  tweeet1 = paste("Trend analysis: Activity between trends. Heatmap of active accounts in n=3000 most recent tweets between trends. NOTE: White (-1) = Activity between same trend = 1000 / 100%",trends[g,1],sep=" ")
  post_tweet(status = tweeet1,media = "heat1.png")
}




Sys.sleep(4000)



}

#######################################################################################
#######################################################################################
#######################################################################################
#######################################################################################
#######################################################################################
#######################################################################################
#######################################################################################
#######################################################################################
#######################################################################################
#######################################################################################
#######################################################################################
#######################################################################################
#######################################################################################


##

s1 = matrix(nrow = 1,ncol = length(unique(rstats_tweets$source)))
s1[1,g]= sum(unique(rstats_tweets$source)[g] == rstats_tweets$source)
names123 = (unique(rstats_tweets$source))
colnames(s1) = s1[0,]
colnames(s1) = names123
ss = paste("source", g,".png" ,sep="")
png(ss,width = 700,height = 550)
par(mar=c(10,4,4,4))
main123 = paste("Sources of n = 1000 tweets of the hashtag",trends[g,1],sep=" ")
barplot(names.arg = colnames(s1),height=s1[1,],las=3,cex.names = 0.9,ylim = c(0,max(s1[1,]+50)),main=names123)



dev.off()
##






#############################New York#####################################################


trends <- get_trends(woeid = 2459115,lat = NULL,lng = NULL,exclude_hashtags = FALSE,token = NULL,parse = TRUE)
trends<-trends[1:5,]

for (g in 1:nrow(trends)){
  rstats_tweets <- search_tweets(q = toString(trends[g,1]),n = 10,include_rts=TRUE)
  rstats_tweets1 <- rstats_tweets[!duplicated(rstats_tweets$user_id),]
  loc <- unique(rstats_tweets$location)
  

  
  
  location = c()
  for (i in 1:length(loc)) {
    location[i] = sum(rstats_tweets$location == loc[i])
  }
  
  length(rstats_tweets$location)
  herkunft <- as.data.frame(cbind(loc,location))
  herkunft$location <- as.numeric(herkunft$location)
  herkunft <- herkunft[order(herkunft$location),]
  herkunft <- herkunft[herkunft$location > 9,]
  herkunft$per <- (herkunft$location/(sum(herkunft$location)))*100
  pie_labels <- paste0(herkunft$loc, " = ", round(herkunft$per), "%")
  trend_name <- toString(trends[g,1])
  file_name = paste("pl", g,".png" ,sep="")
  png(file_name,width = 1250,height = 500,res=100)
  pie(herkunft$per,labels = pie_labels,main = trend_name,sub="Location of n = 1000 most recent tweets of the hashtag in title")
  dev.off()
  tweeet = paste("Trend analysis: Location of n = 1000 recent tweets of the hashtag. Note: Location can be modified manually. No label means user does not provide a location",trends[g,1],sep=" ")
  #post_tweet(status = tweeet,media = file_name)
  
  file_name1 = paste("pt", g,".png" ,sep="")
  png(file_name1,width = 700,height = 550)
  barplot(height = length(rstats_tweets1$user_id),col = c("lightblue","white"),ylim = c(0,1000),width = 1,xlim= c(0,2),names.arg = trends[g,1],xlab="Trend",ylab="Number of individual accounts",sub="n=1000")
  title("Number of individual accounts \nbehind n = 1000 most recent tweets of the hashtag")
  bar = toString(length(rstats_tweets1$user_id))
  text(0.7,length(rstats_tweets1$user_id)-50,bar,cex=2,col="red")
  dev.off()
  
  tweeet1 = paste("Trend analysis: Heterogeneity of a trend. Number of accounts responsible for the n = 1000 most recent tweets.",trends[g,1],sep=" ")
  #post_tweet(status = tweeet1,media = file_name1)
  
}


########################################################################################


############################################################################################

install.packages('pheatmap') # if not installed already
library(pheatmap)
pheatmap(usidnet, display_numbers = T)

myCol <- c("blue", "green", "yellow", "orange", "red")
## Defining breaks for the color scale
myBreaks <- c(0, .3, 1, 1.3, 3, 3.5)

hm <- heatmap.2(usidnet, scale="none", Rowv=NA, Colv=NA,
                col = myCol, ## using your colors
                breaks = myBreaks, ## using your breaks
                dendrogram = "none",  ## to suppress warnings
                margins=c(5,5), cexRow=0.5, cexCol=1.0, key=TRUE, keysize=1.5,
                trace="none")




co <- cooccur(usidnet,thresh = FALSE,spp_names = TRUE,type = "site_spp")
plot(co)

plot(usidnet)



uusid <- as.data.frame(usid)

usid <- network(usidnet)
network.vertex.names(usid) =  colnames(usidnet)
set.vertex.attribute(colnames(usidnet))


plot(usidnet)
heatmap.2(usidnet)

nodes <- data.frame(id = 1:nrow(trends),label = rownames(usidnet),color = "#606482",shadow = TRUE) 
edges <- data.frame(from = co$sp1, to = co$sp2,color = ifelse(co$p_lt <= 0.05, "#B0B2C1", "#3C3F51"),dashes = ifelse(co$p_lt <= 0.05, TRUE, FALSE))


usid1 = usid

rstatneu = rstats_tweets[sort(rstats_tweets$retweet_count,decreasing = T),]

pie
if (!is.na(lab) && nzchar(lab)) {
  lines(c(1, 1.35) * P$x, c(1, 1.35) * P$y)
  text(1.5 * P$x, 1.5 * P$y, labels[i], xpd = TRUE, 
       adj = ifelse(P$x < 0, 1, 0), ...)
}
lines(c(1, 1.35) * P$x, c(1, 1.35) * P$y)
text(1.5 * P$x, 1.5 * P$y, labels[i], xpd = TRUE, 
     adj = ifelse(P$x < 0, 1, 0), ...)
pie2(slices,labels = lbls, col=rainbow(length(lbls)), radius=.2)

hashtagss = do_call_rbind(p)
hashtagss = cbind(replicate(trends[1,1],n = 1000),hashtagss)
rownames(hashtagss) = NULL
colnames(hashtagss) = c("from",replicate("to",n = ncol(hashtagss)-1))

hashtags = as.network(hashtagss,directed=FALSE,  hyper=FALSE, loops=FALSE, multiple=FALSE, bipartite = FALSE)
plot(network(hashtagss,directed=TRUE,  hyper=FALSE, loops=FALSE, multiple=FALSE, bipartite = FALSE))


####################################################################
#########################################################################
############################################################


repeat{
  trends <- get_trends(woeid = "Belgium",exclude_hashtags = FALSE,token = NULL,parse = TRUE)
  trends<-trends[1:15,]
  
  for (g in 1:nrow(trends)){
    rstats_tweets <- search_tweets(q = toString(trends[g,1]),n = 1000,include_rts=FALSE)
    rstats_tweets1 <- rstats_tweets[!duplicated(rstats_tweets$user_id),]
    loc <- unique(rstats_tweets$location)
    ##
    
    s1 = matrix(nrow = 1,ncol = length(unique(rstats_tweets$source)))
    for (b in 1:length(unique(rstats_tweets$source))) {
      s1[1,b]= sum(unique(rstats_tweets$source)[b] == rstats_tweets$source)
    }
    
    names123 = (unique(rstats_tweets$source))
    colnames(s1) = s1[0,]
    colnames(s1) = names123
    ss = paste("source", g,".png" ,sep="")
    main123 = paste("Sources of n = 1000 tweets of the hashtag",trends[g,1],sep=" ")
    png(ss,width = 750,height = 650,res=100)
    par(mar=c(11,4,4,4))
    barplot(names.arg = colnames(s1),height=s1[1,],las=3,cex.names = 0.7,ylim = c(0,max(s1[1,]+50)),main=main123)
    dev.off()
    
    #tweeet2 = paste("Trend analysis: Sources of a trend. Devices used tweeting the n = 1000 most recent tweets. NOTE: If APIs are used to tweet, Twitter states the name of the API as the source",trends[g,1],sep=" ")
    #post_tweet(status = tweeet2,media = ss)
    
    
    ##########
    
    s2 = matrix(nrow = 1,ncol = length(unique(rstats_tweets$lang)))
    for (c in 1:length(unique(rstats_tweets$lang))) {
      s2[1,c]= sum(unique(rstats_tweets$lang)[c] == rstats_tweets$lang)
    }
    
    names1234 = (unique(rstats_tweets$lang))
    colnames(s2) = s2[0,]
    colnames(s2) = names1234
    langs = paste("lang", g,".png" ,sep="")
    main1234 = paste("Languages of n = 1000 tweets of the hashtag",trends[g,1],sep=" ")
    png(langs,width = 750,height = 650,res=100)
    par(mar=c(11,4,4,4))
    barplot(names.arg = colnames(s2),height=s2[1,],las=3,cex.names = 0.7,ylim = c(0,max(s2[1,]+50)),main=main1234,xlab = "Languages",sub = "Language code used: ISO 639 ")
    dev.off()
    #tweeetlang = paste("Trend analysis: Languages of n = 1000 most recent tweets of the hashtag. Note: Twitter automatically identifies the language of a tweet. Language codes used = ISO 639, de = German, en = Englisch, es = Spanisch, fr = French, it = italian, und = unidentified",trends[g,1],sep=" ")
    #post_tweet(status = tweeetlang,media = langs)
    
    ##################
    
    ##
    
    
    location = c()
    for (i in 1:length(loc)) {
      location[i] = sum(rstats_tweets$location == loc[i])
    }
    
    length(rstats_tweets$location)
    herkunft <- as.data.frame(cbind(loc,location))
    herkunft$location <- as.numeric(herkunft$location)
    herkunft <- herkunft[order(herkunft$location),]
    herkunft <- herkunft[herkunft$location > 9,]
    herkunft$per <- (herkunft$location/(sum(herkunft$location)))*100
    pie_labels <- paste0(herkunft$loc, " = ", round(herkunft$per), "%")
    trend_name <- toString(trends[g,1])
    file_name = paste("pl", g,".png" ,sep="")
    png(file_name,width = 800,height = 500,res=100)
    par(mar=c(5,4,7,4))
    pie(herkunft$per,labels = pie_labels,sub="Location of n = 1000 most recent tweets of the hashtag in title",cex=0.6,radius = 1)
    title(main=trend_name,cex.main=0.9,line = 5)
    dev.off()
    #tweeet = paste("Trend analysis: Location of n = 1000 recent tweets of the hashtag. Note: Location can be modified manually. No label means user does not provide a location",trends[g,1],sep=" ")
    #post_tweet(status = tweeet,media = file_name)
    
    
    
    file_name1 = paste("pt", g,".png" ,sep="")
    png(file_name1,width = 700,height = 550)
    barplot(height = length(rstats_tweets1$user_id),col = c("lightblue","white"),ylim = c(0,1000),width = 1,xlim= c(0,2),names.arg = trends[g,1],xlab="Trend",ylab="Number of individual accounts",sub="n=1000",cex.names = 1)
    title("Number of individual accounts \nbehind n = 1000 most recent tweets of the hashtag")
    bar = toString(length(rstats_tweets1$user_id))
    text(0.7,length(rstats_tweets1$user_id)-50,bar,cex=2,col="red")
    dev.off()
    
    tweeet1 = paste("Trend analysis: Heterogeneity of a trend, Sources, Locations and Languages of the n = 1000 most recent tweets of the trend.",trends[g,1],sep=" ")
    post_tweet(status = tweeet1,media = c(file_name1,ss,file_name,langs),in_reply_to_status_id = "1485308529925926923")
    
    
    Sys.sleep(60)
  }
  
  #taskscheduler_create(taskname = "trendanalyse_10",rscript = "C:/Users/Yussuf Schwarz/Documents/trendanalyse.R",schedule = "DAILY",starttime = "10:00")
  #taskscheduler_create(taskname = "trendanalyse_19",rscript = "C:/Users/Yussuf Schwarz/Documents/trendanalyse.R",schedule = "DAILY",starttime = "19:00")
  
  Sys.sleep(500)
  
  trends <- get_trends(woeid = "Belgium",exclude_hashtags = FALSE,token = NULL,parse = TRUE)
  trends<-trends[1:15,]
  usid <- matrix(nrow = 1000,ncol = 1)
  for (g in 1:nrow(trends)){
    rstats_tweets <- search_tweets(q = toString(trends[g,1]),n = 1000,include_rts=TRUE)
    usid = qpcR:::cbind.na(usid,rstats_tweets$user_id)
  }
  usid = usid[,-1]
  #usid[is.na(usid)] <- 0
  usid = na.omit(usid)
  
  usidnet <- matrix(nrow = nrow(trends),ncol = nrow(trends))
  for (g in 1:nrow(trends)){
    for (i in 1:nrow(trends)){
      usidnet[i,g]<- sum(usid[,g]==usid[,i])
    }
  }
  names12<-c()
  for (g in 1:nrow(trends)){
    names12[g] = toString(trends[g,1])
  }
  usidnet<-cbind(c(names12),usidnet)
  usidnet<-data.frame(usidnet)
  row.names(usidnet) <- usidnet[,1]
  usidnet <- usidnet[,-1]
  for (g in 1:(nrow(trends))){
    usidnet[,g] <- as.numeric(usidnet[,g])
  }
  
  usidnet <- as.matrix(usidnet)
  colnames(usidnet)=c(names12)
  diag(usidnet) = -1
  
  
  png("heat1.png",width = 700,height = 550)
  pheatmap(usidnet, display_numbers = F, color = colorRampPalette(c('white','red'))(100), cluster_rows = F, cluster_cols = F, fontsize_number = 15,main = "Activity between trends n=1000 most recent trends")
  dev.off()
  
  tweeet1 = paste("Trend analysis: Activity between trends. Heatmap of active accounts in n=1000 most recent tweets between trends. NOTE: White (-1) = Activity between same trend = 1000 / 100%",trends[g,1],sep=" ")
  post_tweet(status = tweeet1,media = "heat1.png",in_reply_to_status_id = "1485308529925926923")

  
#  for (g in 1:nrow(trends)){
    tweeet1 = paste("Trend analysis: Activity between trends. Heatmap of active accounts in n=1000 most recent tweets between trends. NOTE: White (-1) = Activity between same trend = 1000 / 100%",trends[g,1],sep=" ")
    post_tweet(status = tweeet1,media = "heat1.png",in_reply_to_status_id = "1484561979608117251")
  }
  
  
  
  
  Sys.sleep(4000)
  
  
  
}





#################################################################################################
#################################################################################################
#################################################################################################
#################################################################################################
#################################################################################################
#################################################################################################
#################################################################################################
#################################################################################################
#################################################################################################
#################################################################################################




##########################################################################################
##########################################################################################
##########################################################################################
##########################################################################################



#install.packages("ggalluvial")
library("ggalluvial")

ggplot(freq,aes(y = freq, axis1 = from, axis2 = to))+
        geom_alluvium() + scale_x_discrete(limits = c("from", "to"), expand = c(.05, .05))+
        geom_stratum(width = 1/12, fill = "black", color = "grey") +
        geom_label(stat = "stratum", aes(label = after_stat(stratum))) +
        scale_fill_brewer(type = "qual", palette = "Set1") +
        ggtitle("UC Berkeley admissions and rejections, by sex and department")

name = freq[,2]
v.size = freq[,3]
ggraph(ig,layout = "fr") +
  geom_edge_arc(colour= "gray50",
                lineend = "round",
                strength = .1) +
  geom_node_point(size=log(v.size)*2) +
  geom_node_text(aes(label=name), 
                 repel = TRUE, 
                 point.padding = unit(0.2, "lines"), 
                 size=sqrt(v.size), 
                 colour="gray10") +
  scale_edge_width(range = c(0, 2.5)) +
  scale_edge_alpha(range = c(0, .3)) +
  theme_graph(background = "white") +
  guides(edge_width = FALSE,
         edge_alpha = FALSE)

remove.packages("rtweet")
