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
indices = c("EHGDUS Index",	"CPI YOY Index",	"CPI CHNG Index",	"EHUPUS Index",	"IP CHNG Index",	"NHSPATOT Index",	"NFP TCH Index"	,"TMNOCHNG Index",	"LEI TOTL Index",	"PITL YOY Index",	"CICRTOT Index",	"USCABAL Index",	
            "M2% YOY Index")

# extracted data function
data_extract = function(ticker_list){
  blpConnect()
  StartDate=as.Date("1914-01-01")
  opt <- c("periodicitySelection"= "QUARTERLY",
           "nonTradingDayFillOption"="ACTIVE_DAYS_ONLY")  
  dat = bdh(securities=ticker_list, fields=c("PX_LAST"),start.date=StartDate,options=opt)
  datxts <- lapply(dat, function(d) xts(d[,-1], order.by=as.Date(d[,1])))
  res <- do.call(merge, datxts)  
  colnames(res) <- names(dat)
  #blpDisconnect()
  res = as.data.frame(res)
  return(res)
}
dat = data_extract(indices)
save(dat,file ="eco_data.RData")

ym<-as.yearmon(row.names(dat))
data.xts = xts(dat, order.by = ym)
plot(data.xts[,-2],col=rainbow(ncol(data.xts)))
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

###k-means

###Computing k-means clustering
df2 = as.data.frame(df.yearly[,c(2:7,9)])
# The plot above represents the variance within the clusters. It decreases as k increases, but it can be seen a bend (or “elbow”) at k = 4. 
fviz_nbclust(df2,kmeans,method = "wss")+
  geom_vline(xintercept = 4, linetype = 2)

k2 <- kmeans(df2, centers = 4, nstart = 15)
fviz_cluster(k2, data = df2, palette = "Set2", ggtheme = theme_minimal())

