#US Real GDP (QoQ, %, SAAR)	
#US CPI (inflation) Urban Consumer YoY NSA	
#US CPI (inflation) Urban Consumer MoM SA	
#US Unemployment Rate (%)	
#US Initial Jobless Claims MoM SA	
#US Industrial Production MoM SA	
#Private Housing Units Started Total Monthly % Change, SA	
#US Nominal GDP (QoQ, %, SAAR)	
#US Employment on Nonfarm Payrolls Total (SA, Net Monthly Change, thousands)
library("Rblpapi")
library("xts")
library("imputeTS")
# ticker names, input value
indices = c("EHGDUS Index","CPI YOY Index","CPI CHNG Index", "EHUPUS Index","INJCJMOM Index","IP CHNG Index","NHCHSTCH Index","GDP CRCH Index","NFP TCH Index")

# extracted data function
data_extract = function(ticker_list){
  blpConnect()
  StartDate=as.Date("1914-01-01")
  opt <- c(
    #"periodicitySelection"= "MONTHLY",
           "nonTradingDayFillOption"="ACTIVE_DAYS_ONLY"
           )  
  dat = bdh(securities=ticker_list, fields=c("PX_LAST"),start.date=StartDate,options=opt)
  datxts <- lapply(dat, function(d) xts(d[,-1], order.by=as.Date(d[,1])))
  res <- do.call(merge, datxts)  
  colnames(res) <- ticker_list
  #blpDisconnect()
  res = as.data.frame(res)
  return(res)
}

dat = data_extract(indices)
save(dat,file ="eco_data.RData")
ym<-as.yearmon(row.names(dat))
data.xts = xts(dat, order.by = ym)
plot(data.xts,col=rainbow(ncol(data.xts)))
addLegend("bottomleft",col=rainbow(ncol(data.xts)),lty=1,lwd=2)
dat1967 = window(data.xts, start =as.yearmon("Jan 1967"))
#impute data-linear
dat1967 = na_interpolation(dat1967)
plot(dat1967)
#####classification#####
library(PerformanceAnalytics)
library(dygraphs)
library(GGally)
#####k-means#####
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
library(dendextend) # for comparing two dendrograms
##################################
df = window(dat1967, end=as.yearmon("Dec 2019"))

plot(scale(df))

#dygraph(df) %>% dyRangeSelector()
df=df[,c(2:7)]
#correlation plot
#ggpairs(as.data.frame(scale(df)))+theme_bw()
#PCA
ep <- endpoints(df,'years')
df.yearly = period.apply(df,INDEX=ep, FUN=mean)
df.yearly =as.data.frame(scale(df.yearly))
row.names(df.yearly) = seq(1967, 2019, 1)
PCdf = prcomp(df.yearly, scale =TRUE)
biplot(PCdf , scale =1)
fviz_pca_ind(PCdf,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)
fviz_pca_var(PCdf,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)
fviz_pca_biplot(PCdf, repel = TRUE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969"  # Individuals color
)

summary(PCdf)
plot(PCdf)
PCdf5 = PCdf$rotation[,1:4]
#kmeans
kmpc <- kmeans(as.data.frame(PCdf5), centers = 2, nstart = 25)
fviz_cluster(kmpc, data = PCdf5)
#Clustering Distance Measures
distance <- get_dist(df)
#fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

###Computing k-means clustering
k2 <- kmeans(df, centers = 2, nstart = 30)
str(k2)
k2
fviz_cluster(k2, data = df)
df %>%
  as_tibble() %>%
  mutate(cluster = k2$cluster,
         state = row.names(df)) %>%
  ggplot(aes(`EHGDUS Index`, `CPI YOY Index`, color = factor(cluster), label = state)) +
  geom_text()

#k2$cluster
df %>% mutate(cluster = k2$cluster) %>% head
cluster_kmean = df %>% mutate(cluster = k2$cluster)
xts0 = xts(cluster_kmean$cluster,order.by = as.yearmon(row.names(df)))
plot(xts0,grid.col = NA,type = "b")
dygraph(df) %>% dyRangeSelector()
# set.seed(123)
# gap_stat <- clusGap(df, FUN = kmeans, nstart = 25,
#                     K.max = 10, B = 50)
# # Print the result
# print(gap_stat, method = "firstmax")
#fviz_gap_stat(gap_stat)

# Dissimilarity matrix
d <- dist(df, method = "euclidean")

# Hierarchical clustering using Complete Linkage
hc1 <- hclust(d, method = "complete" )

# Plot the obtained dendrogram
plot(hc1, cex = 0.6, hang = -1)
hc3 <- agnes(df, method = "ward")
pltree(hc3, cex = 0.6, hang = -1, main = "Dendrogram of agnes") 
# compute divisive hierarchical clustering
hc4 <- diana(df)

# Divise coefficient; amount of clustering structure found
hc4$dc
## [1] 0.8514345

# plot dendrogram
pltree(hc4, cex = 0.6, hang = -1, main = "Dendrogram of diana")
# Ward's method
hc5 <- hclust(d, method = "ward.D2" )

# Cut tree into 4 groups
sub_grp <- cutree(hc5, k = 4)

# Number of members in each cluster
table(sub_grp)
## sub_grp
##  1  2  3  4 
##  7 12 19 12
df %>% mutate(cluster = sub_grp) %>% head
cluster = df %>% mutate(cluster = sub_grp)
xts1 = xts(cluster$cluster,order.by = as.yearmon(row.names(df)))
plot(xts1,grid.col = NA,type = "p")
plot(hc5, cex = 0.6)
rect.hclust(hc5, k = 4, border = 2:5)
fviz_cluster(list(data = df, cluster = sub_grp))
# Compute distance matrix
res.dist <- dist(df, method = "euclidean")

# Compute 2 hierarchical clusterings
hc1 <- hclust(res.dist, method = "complete")
hc2 <- hclust(res.dist, method = "ward.D2")

# Create two dendrograms
dend1 <- as.dendrogram (hc1)
dend2 <- as.dendrogram (hc2)

tanglegram(dend1, dend2)
fviz_nbclust(df, FUN = hcut, method = "silhouette")
