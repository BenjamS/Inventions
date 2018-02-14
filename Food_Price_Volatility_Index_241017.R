#Set working directory
setwd("D:/OneDrive - CGIAR/Documents")
#Load necessary libraries
library(stats)
library(plyr)
library(ggplot2)
library(ggrepel)
library(dplyr)
library(tidyr)
#Load data
df_raw <- read.csv("ConsumerPriceIndices_E_All_Data.csv")
#Prepare data
df_raw$Months.Code <- NULL
df_raw$Item.Code <- NULL
df_raw$Area.Code <- NULL
df_raw$Unit <- NULL
df_raw$Item <- as.character(df_raw$Item)
df_raw$Area <- as.character(df_raw$Area)
u <- colnames(df_raw)
df_raw <- df_raw[, -grep("F", u)]
u <- colnames(df_raw)
df_raw <- df_raw[, -grep("N", u)]
colnames(df_raw)[4:ncol(df_raw)] <- as.character(c(2000:2017))
df_raw <- gather(df_raw,Year,Value,`2000`:`2017`)
df_raw$Year <- as.integer(df_raw$Year)
#summary(df_raw)

#--Africa
countries_NAfrica <- as.character(unique(read.csv("Country list - Northern Africa.csv")[,"Area"]))
countries_MAfrica <- as.character(unique(read.csv("Country list - Middle Africa.csv")[,"Area"]))
countries_WAfrica <- as.character(unique(read.csv("Country list - Western Africa.csv")[,"Area"]))
countries_EAfrica <- as.character(unique(read.csv("Country list - Eastern Africa.csv")[,"Area"]))
countries_SAfrica <- as.character(unique(read.csv("Country list - Southern Africa.csv")[,"Area"]))
#--Americas
countries_SAmer <- as.character(unique(read.csv("Country list - South America.csv")[,"Area"]))
countries_CAmer <- as.character(unique(read.csv("Country list - Central America.csv")[,"Area"]))
countries_Carib <- as.character(unique(read.csv("Country list - Caribbean.csv")[,"Area"]))
countries_NAmer <- as.character(unique(read.csv("Country list - Northern America.csv")[,"Country"]))
#--Asia
countries_EAsia <- as.character(unique(read.csv("Country list - Eastern Asia.csv")[,"Area"]))
#countries_EAsia <- countries_EAsia[!(countries_EAsia %in% c("China, Hong Kong SAR", "China, Macao SAR"))]
countries_SEAsia <- as.character(unique(read.csv("Country list - South-Eastern Asia.csv")[,"Area"]))
countries_SAsia <- as.character(unique(read.csv("Country list - Southern Asia.csv")[,"Area"]))
countries_WAsia <- as.character(unique(read.csv("Country list - Western Asia.csv")[,"Area"]))
countries_CAsia <- as.character(unique(read.csv("Country list - Central Asia.csv")[,"Area"]))
#--Europe
countries_NEurope <- as.character(unique(read.csv("Country list - Northern Europe.csv")[,"Area"]))
countries_WEurope <- as.character(unique(read.csv("Country list - Western Europe.csv")[,"Area"]))
countries_EEurope <- as.character(unique(read.csv("Country list - Eastern Europe.csv")[,"Area"]))
countries_SEurope <- as.character(unique(read.csv("Country list - Southern Europe.csv")[,"Area"]))
#--Oceania
countries_Oceania <- as.character(unique(read.csv("Country list - Oceania.csv")[,"Area"]))
countries_AusNZea <- c("Australia", "New Zealand")
countries_PacifIs <- setdiff(countries_Oceania, countries_AusNZea)
#--Create region groupings
u <- df_raw$Area
df_raw$Region <- NA
df_raw$Region[which(u %in% countries_NAmer)] <- "North America"
df_raw$Region[which(u %in% countries_SAmer)] <- "South America"
df_raw$Region[which(u %in% countries_CAmer)] <- "Central America"
df_raw$Region[which(u %in% countries_Carib)] <- "Caribbean"
df_raw$Region[which(u %in% countries_NAfrica)] <- "Northern Africa"
df_raw$Region[which(u %in% countries_SAfrica)] <- "Southern Africa"
df_raw$Region[which(u %in% countries_WAfrica)] <- "Western Africa"
df_raw$Region[which(u %in% countries_EAfrica)] <- "Eastern Africa"
df_raw$Region[which(u %in% countries_MAfrica)] <- "Middle Africa"
df_raw$Region[which(u %in% countries_CAsia)] <- "Central Asia"
df_raw$Region[which(u %in% countries_WAsia)] <- "Western Asia"
df_raw$Region[which(u %in% countries_SAsia)] <- "Southern Asia"
df_raw$Region[which(u %in% countries_EAsia)] <- "Eastern Asia"
df_raw$Region[which(u %in% countries_SEAsia)] <- "South-Eastern Asia"
df_raw$Region[which(u %in% countries_NEurope)] <- "Northern Europe"
df_raw$Region[which(u %in% countries_SEurope)] <- "Southern Europe"
df_raw$Region[which(u %in% countries_WEurope)] <- "Western Europe"
df_raw$Region[which(u %in% countries_EEurope)] <- "Eastern Europe"
df_raw$Region[which(u %in% countries_PacifIs)] <- "Pacific Islands"
df_raw$Region[which(u %in% countries_AusNZea)] <- "Australia & New Zealand"
#--See what countries escaped designation
#unique(df_raw$Area[which(is.na(df_raw$Region))])
#--Assign these to their proper regions
#(Leave out "China" as it is already covered under "China, mainlaind", "Hong Kong", etc.)
df_raw$Region[which(u %in% c("Åland Islands", "Isle of Man", "Greenland"))] <- "Northern Europe"
df_raw$Region[which(u %in% c("Anguilla", "Bermuda", "Cayman Islands", "Curaçao"))] <- "Caribbean"
df_raw$Region[which(u %in% c("Côte d'Ivoire"))] <- "Western Africa"
df_raw$Region[which(u %in% c("Palau"))] <- "Pacific Islands"
df_raw$Region[which(u %in% c("Maldives", "Réunion"))] <- "Southern Asia"
df_raw$Region[which(u %in% c("French Guiana"))] <- "South America"
df_raw <- df_raw[which(is.na(df_raw$Region) == F),]
rm(u)
#--
df <- df_raw %>% group_by(Region, Area, Year, Item) %>% summarize(Value_mu = mean(Value, na.rm = T), Value_sd = sd(Value, na.rm = T))
class(df$Value_mu)
df$cv <- df$Value_sd / df$Value_mu
df <- df[, c("Area", "Region", "Item", "Year", "cv")]
df <- df %>% spread(Item, cv)
colnames(df)[4:5] <- c("Food CPI CV", "General CPI CV")



df_region <- df %>% group_by(Region, Year) %>% summarize(Region_mean = mean(`General CPI CV`, na.rm = T))
df_CV_GenCPI <- df
df_CV_GenCPI$`Food CPI CV` <- NULL
df_CV_GenCPI <- merge(df_CV_GenCPI, df_region, by = c("Region", "Year"))
write.csv(df_CV_GenCPI, "Coefficient of Variation FAO General CPI.csv", row.names = F)


df_region <- df %>% group_by(Region, Year) %>% summarize(Region_mean = mean(`Food CPI CV`, na.rm = T))
df_CV_FoodCPI <- df
df_CV_FoodCPI$`General CPI CV` <- NULL
df_CV_FoodCPI <- merge(df_CV_FoodCPI, df_region, by = c("Region", "Year"))
class(df_CV_FoodCPI$Year)

df_CV_FoodCPI <- subset(df_CV_FoodCPI, Year %in% c(2011:2017))
write.csv(df_CV_FoodCPI, "Coefficient of Variation FAO Food CPI.csv", row.names = F)





df_plot <- subset(df, Region == "Eastern Africa")
gg <- ggplot(df_plot, aes(x = Year, y = `Food CPI CV`, group = Area, color = Area))
gg <- gg + geom_line()
gg

df_plot <- subset(df, Region == "Southern Asia")
gg <- ggplot(df_plot, aes(x = Year, y = `General CPI CV`, group = Area, color = Area))
gg <- gg + geom_line()
gg

df_plot <- subset(df, Region == "Eastern Africa")
gg <- ggplot(df_plot, aes(x = Year, y = `General CPI CV`, group = Area, color = Area))
gg <- gg + geom_line()
gg

df_plot <- subset(df, Region == "Northern Africa")
gg <- ggplot(df_plot, aes(x = Year, y = `General CPI CV`, group = Area, color = Area))
gg <- gg + geom_line()
gg


#--Get country CPI CVs for a single year
df_mod <- subset(df, Year == 2015)
#--Remove missing values
df_mod <- df_mod[which(is.na(df_mod$`Food CPI CV`) == F), ]
df_mod <- df_mod[which(is.na(df_mod$`General CPI CV`) == F), ]
#--Take logs
df_mod$`Food CPI CV` <- log(df_mod$`Food CPI CV`)
df_mod$`General CPI CV` <- log(df_mod$`General CPI CV`)
#--Plot the food CPI-based CVs against the general CPI-based CVs (in logs)
gg <- ggplot(df_mod, aes(x = `Food CPI CV`, y = `General CPI CV`, group = Region, color = Region))
gg <- gg + geom_point() + labs(x = "Logged Food CPI CV", y = "Logged General CPI CV")
gg
#--Estimate a model
mod <- lm(`Food CPI CV` ~ `General CPI CV`, df_mod)
print(summary(mod))

df_temp <- df %>% group_by(Region, Year) %>% summarise("Region level general CPI CV" = mean(`General CPI CV`, na.rm = T))
df <- merge(df, df_temp, by = c("Region", "Year"))
df$Ratio <- df$`Food CPI CV` / df$`General CPI CV`
u <- df$`General CPI CV`
df$`General CPI CV`[which(is.na(u))] <- df$`Region level general CPI CV`[which(is.na(u))]










#=============================
#--
library(FactoMineR)
library(factoextra)
library(corrplot)
library(zoo)
#unique(df_raw$Item)
df_u <- subset(df_raw, Item == "Consumer Prices, General Indices (2010 = 100)")
df_u$Item <- NULL
df_u <- df_u[which(is.na(df_u$Value) == F),]

u <- df_u$Months
df_u$Months_n <- NA
df_u$Months_n[u == "January"] <- 1
df_u$Months_n[u == "February"] <- 2
df_u$Months_n[u == "March"] <- 3
df_u$Months_n[u == "April"] <- 4
df_u$Months_n[u == "May"] <- 5
df_u$Months_n[u == "June"] <- 6
df_u$Months_n[u == "July"] <- 7
df_u$Months_n[u == "August"] <- 8
df_u$Months_n[u == "September"] <- 9
df_u$Months_n[u == "October"] <- 10
df_u$Months_n[u == "November"] <- 11
df_u$Months_n[u == "December"] <- 12

df_u <- subset(df_u, Year %in% c(2004:2016))

df_wide <- df_u
df_wide$Region <- NULL
df_wide <- df_wide %>% spread(Area, Value)
df_wide <- df_wide[order(df_wide$Year, df_wide$Months_n), ]
df_wide$Year <- as.character(df_wide$Year)
df_wide$Months <- as.character(df_wide$Months)
df_wide$Date <- as.yearmon(paste(df_wide$Year, df_wide$Months_n), "%Y %m")

df_num <- df_wide[, c(4:ncol(df_wide))]
rownames(df_num) <- df_num$Date
df_num$Date <- NULL

o <- c()
for(i in 1:ncol(df_num)){x <- df_num[,i]; o[i] <- length(which(is.na(x)))} 
o
rmcols <- which(o >= 124)
rm_these <- colnames(df_num[, rmcols])
# save_these <- c("Australia", "New Zealand", "Anguilla", "Vanuatu", "Papua New Guinea")
# rm_these <- setdiff(colnames(df_num[, rmcols]), save_these)
# rmcols <- which(colnames(df_num) %in% rm_these)
# colnames(df_num[, rmcols])
# df_num <- df_num[, -rmcols]
# df_num <- as.data.frame(scale(na.spline(df_num)))
o <- c()
for(i in 1:nrow(df_num)){x <- df_num[i, ]; o[i] <- length(which(is.na(x)))} 
o
rmrows <- which(o >= 38)

df_wide2 <- df_wide[-c(1:11), -which(colnames(df_wide) %in% rm_these)]
ind <- c(4:(ncol(df_wide2) - 1))
df_wide2[, ind] <- as.data.frame(scale(na.spline(df_wide2[, ind])) )
df_wide2 <- df_wide2 %>% group_by(Months_n) %>% summarise_at(vars(Afghanistan:Zimbabwe), mean)

df_pca <- as.data.frame(t(df_wide2))
colnames(df_pca) <- month.abb[as.integer(df_pca[1,])]
df_pca <- df_pca[-1, ]

fviz_pca_biplot(res)

df_u <- subset(df_u, !(Area %in% rm_these))
df_region <- df_u %>% group_by(Region, Year, Months_n) %>% summarise(Region_mu = mean(Value))
df_region <- df_region %>% spread(Region, Region_mu)
df_region$Date <- as.yearmon(paste(df_wide$Year, df_wide$Months_n), "%Y %m")
rownames(df_region) <- df_region$Date
df_region$Year <- NULL
df_region$Months_n <- NULL

df_region[, c(1:(ncol(df_region) - 1))] <- na.spline(df_region[, c(1:(ncol(df_region) - 1))])

zdf_region <- df_region[, c(1:(ncol(df_region) - 1))]
zdf_region <- as.data.frame(scale(zdf_region))
zdf_region_long <- zdf_region %>% gather(Region, zCPI)
n_regions <- ncol(zdf_region)
zdf_region_long$Date <- as.yearmon(rep(df_region$Date, n_regions))

df_region_long <- df_region[, c(1:(ncol(df_region) - 1))] %>% gather(Region, CPI)
n_regions <- length(unique(df_region_long$Region))
df_region_long$Date <- as.yearmon(rep(df_region$Date, n_regions))

ggplot(zdf_region_long, aes(x = Date, y = zCPI, group = Region, color = Region)) + geom_line()

df_wide2 <- df_wide[-c(1:11), -which(colnames(df_wide) %in% rm_these)]
ind <- c(4:(ncol(df_wide2) - 1))
df_wide2[, ind] <- as.data.frame(scale(na.spline(df_wide2[, ind])) )
df_wide2 <- df_wide2 %>% group_by(Months_n) %>% summarise_at(vars(Afghanistan:Zimbabwe), mean)

zdf_region$Date <- df_region$Date
zdf_region$Months <- months(zdf_region$Date)
df_pca <- zdf_region %>% group_by(Months) %>% summarise_at(vars(`Australia & New Zealand`:`Western Europe`), mean)
df_pca$`Australia & New Zealand` <- NULL
df_pca$`South Asia` <- NULL
df_pca <- as.data.frame(t(df_pca))
colnames(df_pca) <- as.character(unlist(df_pca[1,]))
df_pca <- df_pca[-1, ]
for(i in 1:ncol(df_pca)){df_pca[, i] <- as.numeric(as.character(df_pca[, i]))}
res <- PCA(df_pca)
fviz_pca_biplot(res)

str(df_pca)

zdf <- scale(df_wide[, 4:ncol(df_wide)])
#zdf <- scale(na.spline(df_wide[, 4:ncol(df_wide)]))
zdf <- data.frame(Month = df_wide$Month, zdf)
zdf_cp <- zdf_cp %>% group_by(Month) %>% summarize_all(mean)
class(zdf_cp)
zdf_cp <- as.data.frame(zdf_cp)
rownames(zdf_cp) <- zdf_cp$Month
zdf_cp$Month <- NULL
zdf_cp <- as.data.frame(t(zdf_cp))
zdf_cp$Symb <- rownames(zdf_cp)

# df_wide <- df_wide %>% spread(Months, Value)
# 
# df_num <- df_wide[,c(5: ncol(df_wide))]



zdf_pca_in <- scale(df_pca_in)
# corMatMy <- cor(zdf_pca_in)
# corrplot(corMatMy, order = "hclust")


#logistic curve plot
a <- -14.5
b <- 0.69
t <- c(0:30)
y <- 1 / (1 + exp(-(a + b * t)))

df_plot <- data.frame(t = t, y = y)

ggplot(df_plot, aes(x = t, y = y)) + geom_line(size = 2) + 
  geom_hline(yintercept = c(1, 0)) + theme_void() + theme(legend.position="none")

#--