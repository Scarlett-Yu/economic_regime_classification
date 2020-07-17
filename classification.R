library("Rblpapi")
library("xts")
library("imputeTS")

Economic_Regime_Data <- read_excel("Economic Regime Data.xlsx", sheet = "New Data", skip = 1, n_max = 2)
des = as.vector(t(Economic_Regime_Data[1,-1]))
# index = as.vector(t(Economic_Regime_Data[2,-1]))
# ticker names, input value
indices = c("EHGDUS Index",	"CPI YOY Index",	"CPI CHNG Index",	"EHUPUS Index",	"IP CHNG Index",	"NHSPATOT Index",	"NFP TCH Index"	,"TMNOCHNG Index",	"LEI TOTL Index",	"PITL YOY Index",	"CICRTOT Index",	"USCABAL Index","M2% YOY Index")
indices_des = cbind(indices, des)

# extracted data function
data_extract = function(ticker_list){
  blpConnect()
  StartDate=as.Date("1914-01-01")
  opt <- c("periodicitySelection"= "QUARTERLY",
           "nonTradingDayFillOption"="ACTIVE_DAYS_ONLY")  
  dat = bdh(securities=ticker_list, fields=c("PX_LAST"),start.date=StartDate,options=opt)
  datxts <- lapply(dat, function(d) xts(d[,-1], order.by=as.yearqtr(as.Date(d[,1]))))
  res <- do.call(merge, datxts)  
  colnames(res) <- names(dat)
  #blpDisconnect()
  return(res)
}
dat = data_extract(indices)
save(dat,file ="eco_data.RData")
#########################################
#plot all data
plot(dat,col=rainbow(ncol(dat)))
addLegend("bottomleft",col=rainbow(ncol(dat)),lty=1,lwd=2)
#remove NA
dat.all = na.omit(dat)
plot(dat.all)
#standardized
plot(scale(dat.all))
#remove 2020 outlier
df = dat.all
plot(df)
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

#dygraph(df) %>% dyRangeSelector()
#correlation matrix
cor.matrix = cor(as.data.frame(df))
cor.pairs = which(cor.matrix>0.4&cor.matrix!=1, arr.ind=TRUE)
cor.pairs = unique(t(apply(cor.pairs, 1, sort)))
colnames(cor.pairs) <- c("row", "col")
for (i in 1:nrow(cor.pairs)) {
  p = c(cor.pairs[i, "row"], cor.pairs[i, "col"])
  x = plot(df[,p], main = paste(colnames(cor.matrix)[p], collapse=" vs. "),
           legend.loc = "topleft",)
  #addLegend("topleft", colnames(cor.matrix)[p] , col=1:2, lty=1,lwd=2)
  print(x)
  #cat("The correlation between", indices_des[p[1],"des"], "and", indices_des[p[2],"des"], "is", 
  #    round(cor.matrix[p[1],p[2]],3),".\n")
  cat("The correlation between", paste(colnames(cor.matrix)[p], collapse=" and "), "is", 
      round(cor.matrix[p[1],p[2]],3),".\n")
}
#PCA
ep <- endpoints(df,'years')
df.yearly = period.apply(df,INDEX=ep, FUN=mean)
df.yearly = as.data.frame(scale(df.yearly))
row.names(df.yearly) = format(as.Date(row.names(df.yearly)),"%Y")
PCdf = prcomp(df.yearly, scale =TRUE)
fviz_contrib(PCdf, choice = "var", axes = 1)
fviz_contrib(PCdf, choice = "var", axes = 1:2)

biplot(PCdf, scale = 1)
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

###k-means

###Computing k-means clustering
df2 = as.data.frame(df.yearly)
# The plot above represents the variance within the clusters. It decreases as k increases, but it can be seen a bend (or “elbow”) at k = 4. 
fviz_nbclust(df2,kmeans,method = "wss")+
  geom_vline(xintercept = 4, linetype = 2)

k2 <- kmeans(df2, centers = 4, nstart = 15)
fviz_cluster(k2, data = df2, palette = "Set2", ggtheme = theme_minimal())

#quarterly
#PCA
ep <- endpoints(df,'quarters')
df.qtr = period.apply(df,INDEX=ep, FUN=mean)
index(df.qtr) = as.yearqtr(index(df.qtr))
df.qtr =scale(df.qtr)
PCdf = prcomp(df.qtr, scale =TRUE)
#biplot(PCdf , scale =1)
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
###k-means

###Computing k-means clustering
df2 = as.data.frame(df.qtr[,c(2:7,9)])
# The plot above represents the variance within the clusters. It decreases as k increases, but it can be seen a bend (or “elbow”) at k = 4. 
fviz_nbclust(df2,kmeans,method = "wss")+
  geom_vline(xintercept = 3, linetype = 2)

k2 <- kmeans(df2, centers = 3, nstart = 30)
fviz_cluster(k2, data = df2, palette = "Set2", ggtheme = theme_minimal())

df.qtr$cluster = k2$cluster
library("autoplotly")
autoplotly(PCdf, data = df.qtr,colour="cluster",
           frame = TRUE)
summary(PCdf)
plot(PCdf)