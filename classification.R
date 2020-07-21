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



########LABEL############
df.label = as.data.frame(df)
df.label$label = NULL
recr = function(start, end){
  r = index(window(df, start = as.yearqtr(as.yearmon(start)), end = as.yearqtr(as.yearmon(end))))
  r = as.character(r)
  return(r)
}
#Recession of 1960–61: Another primarily monetary recession occurred after the Federal Reserve began raising interest rates in 1959. The government switched from deficit (or 2.6% in 1959) to surplus (of 0.1% in 1960). When the economy emerged from this short recession, it began the second-longest period of growth in NBER history. The Dow Jones Industrial Average (Dow) finally reached its lowest point on February 20, 1961, about 4 weeks after President Kennedy was inaugurated.
r = recr(start = "Apr 1960", end = "Feb 1961")
df.label[r, "label"] = "recession"
#The relatively mild 1969 recession followed a lengthy expansion. At the end of the expansion, inflation was rising, possibly a result of increased deficits. This relatively mild recession coincided with an attempt to start closing the budget deficits of the Vietnam War (fiscal tightening) and the Federal Reserve raising interest rates (monetary tightening).
r = recr(start = "Dec 1969", end = "Nov 1970")
df.label[r, "label"] = "recession"
#The 1973 oil crisis, a quadrupling of oil prices by OPEC, coupled with the 1973–1974 stock market crash led to a stagflation recession in the United States.
r = recr(start = "Nov 1973", end = "Mar 1975")
df.label[r, "label"] = "recession"
#The NBER considers a very short recession to have occurred in 1980, followed by a short period of growth and then a deep recession. Unemployment remained relatively elevated in between recessions. The recession began as the Federal Reserve, under Paul Volcker, raised interest rates dramatically to fight the inflation of the 1970s. The early 1980s are sometimes referred to as a "double-dip" or "W-shaped" recession.
r = recr(start = "Jan 1980", end = "Jul 1980")
df.label[r, "label"] = "recession"
#The Iranian Revolution sharply increased the price of oil around the world in 1979, causing the 1979 energy crisis. This was caused by the new regime in power in Iran, which exported oil at inconsistent intervals and at a lower volume, forcing prices up. Tight monetary policy in the United States to control inflation led to another recession. The changes were made largely because of inflation carried over from the previous decade because of the 1973 oil crisis and the 1979 energy crisis.
r = recr(start = "Jul 1981", end = "Nov 1982")
df.label[r, "label"] = "recession"
#	After the lengthy peacetime expansion of the 1980s, inflation began to increase and the Federal Reserve responded by raising interest rates from 1986 to 1989. This weakened but did not stop growth, but some combination of the subsequent 1990 oil price shock, the debt accumulation of the 1980s, and growing consumer pessimism combined with the weakened economy to produce a brief recession.
r = recr(start = "Jul 1990", end = "Mar 1991")
df.label[r, "label"] = "recession"
#The 1990s once were the longest period of growth in American history. The collapse of the speculative dot-com bubble, a fall in business outlays and investments, and the September 11th attacks, brought the decade of growth to an end. Despite these major shocks, the recession was brief and shallow.
r = recr(start = "Mar 2001", end = "Nov 2001")
df.label[r, "label"] = "recession"
#The subprime mortgage crisis led to the collapse of the United States housing bubble. Falling housing-related assets contributed to a global financial crisis, even as oil and food prices soared. The crisis led to the failure or collapse of many of the United States' largest financial institutions: Bear Stearns, Fannie Mae, Freddie Mac, Lehman Brothers, and AIG, as well as a crisis in the automobile industry. The government responded with an unprecedented $700 billion bank bailout and $787 billion fiscal stimulus package. The National Bureau of Economic Research declared the end of this recession over a year after the end date.[78] The Dow Jones Industrial Average (Dow) finally reached its lowest point on March 9, 2009.
r = recr(start = "Dec 2007", end = "Jun 2009")
df.label[r, "label"] = "recession"
#The first documented case of COVID-19 emerged in Wuhan, China in November 2019. The government in China first instituted travel restrictions, quarantines and stay-at-home orders. When efforts to contain the virus in China were unsuccessful, other countries instituted similar measures in an attempt to contain and slow the spread of the virus, which prompted many cities to close. The initial outbreak expanded into a global pandemic. The economic effects of the pandemic were severe. More than 24 million people lost jobs in the United States in just three weeks.[81] Official economic impact of the virus is still being determined but the stock market responded negatively to the shock to supply chains, primarily in technology industries.
r = index(window(df, start = as.yearqtr(as.yearmon("Feb 2020"))))
r = as.character(r)
df.label[r, "label"] = "recession"
df.label$label[is.na(df.label$label)]<- "other"
df.label$label = as.factor(df.label$label)

#########################################

#PCA
ep <- endpoints(df,'years')
df.yearly = period.apply(df,INDEX=ep, FUN=mean)
df.yearly = as.data.frame(df.yearly)
row.names(df.yearly) = format(as.Date(row.names(df.yearly)),"%Y")
rec = function(x){
  if(length(which(x=="recession")) > 1){
    return("recession")
  }else{
    return("other")
  }
}
df.yearly.label = as.data.frame(df.yearly)
df.yearly.label$label = as.factor(period.apply(df.label$label,INDEX=ep, FUN=rec))
df.yearly.label[nrow(df.yearly.label),]$label <-"recession"


PCdf = prcomp(df.yearly, scale =TRUE)
fviz_contrib(PCdf, choice = "var", axes = 1)
fviz_contrib(PCdf, choice = "var", axes = 2)

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


fviz_pca_biplot(PCdf,
                col.ind = df.yearly.label$label, 
                palette = c("#00AFBB",  "#FC4E07"),
                repel = TRUE     # Avoid text overlapping
)

# by removing the least 4 unimportant index and 2020
colnames(df.yearly)
df.yearly2.2020 = df.yearly[, !colnames(df.yearly) %in% c("CPI YOY Index", "EHUPUS Index", "M2% YOY Index")]
df.yearly2 = df.yearly2.2020[-nrow(df.yearly2.2020),]
#"TMNOCHNG Index"
PCdf2 = prcomp(df.yearly2, scale = TRUE)
summary(PCdf2)
fviz_contrib(PCdf2, choice = "var", axes = 1:2)

fviz_pca_biplot(PCdf2,
             col.ind = df.yearly.label$label[-nrow(df.yearly)], 
             palette = c("#00AFBB",  "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)
###k-means

###Computing k-means clustering
df2020 = as.data.frame(df.yearly2.2020)
set.seed(123)
# The plot above represents the variance within the clusters. It decreases as k increases, but it can be seen a bend (or “elbow”) at k = 5. 
#fviz_nbclust(df2,kmeans,method = "wss")+geom_vline(xintercept = 5, linetype = 2)

k2 <- kmeans(df2020, centers = 5, nstart = 20, iter.max = 100)
fviz_cluster(k2, data = df2020, palette = "jco", ggtheme = theme_classic())
df2019 = as.data.frame(df.yearly2)
set.seed(123)
# The plot above represents the variance within the clusters. It decreases as k increases, but it can be seen a bend (or “elbow”) at k = 5. 
#fviz_nbclust(df2,kmeans,method = "wss")+geom_vline(xintercept = 5, linetype = 2)

k2 <- kmeans(df2019, centers = 4, nstart = 20, iter.max = 100)
fviz_cluster(k2, data = df2019, palette = "jco", ggtheme = theme_classic())

#quarterly - using original data
#PCA
df.qtr = df[, !colnames(df) %in% c("CPI YOY Index","EHUPUS Index", "M2% YOY Index")]
#df.qtr2019 = df.qtr[-nrow(df), ]
PCdf.qtr = prcomp(df.qtr, scale =TRUE)
fviz_contrib(PCdf.qtr, choice = "var", axes = 1:2)
fviz_pca_biplot(PCdf.qtr,
                col.ind = df.label$label, 
                palette = c("#00AFBB",  "#FC4E07"),
                repel = T)

###k-means

###Computing k-means clustering
df2 = as.data.frame(df.qtr)
# The plot above represents the variance within the clusters. It decreases as k increases, but it can be seen a bend (or “elbow”) at k = 4. 
fviz_nbclust(df2,kmeans,method = "wss")+
  geom_vline(xintercept = 5, linetype = 2)

k2 <- kmeans(df2, centers = 4, nstart = 15)
fviz_cluster(k2, data = df2, palette = "jco", ggtheme = theme_classic())

res = cbind(k2$cluster,df.label$label)
res[which(res[,2]==2),1]

