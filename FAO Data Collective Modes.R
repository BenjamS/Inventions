setwd('D:/OneDrive - CGIAR/Documents')
library(stats)
library(plyr)
library(ggplot2)
library(ggrepel)
library(dplyr)
library(tidyr)
library(zoo)
library(quantmod)
source('./collectiveModes.R')
source('./FAOdat_createRegionGroups.R')
#=================================
# Exclude specific areas
exclude_these <- c("Brunei Darussalam", "China, Hong Kong SAR",
                   "China, Macao SAR", "China, Taiwan Province of",
                   "Democratic People's Republic of Korea", "Mongolia",
                   "Singapore", "Timor-Leste", "Lao People's Democratic Republic",
                   "Republic of Korea", "Myanmar", "Japan", "Malaysia")
#==========================================
# Get export price commodity groups ready
cerealPrimary_vec <- c("Maize", "Wheat", "Barley", "Rice", "Millet", "Rye", "Sorghum")
cerealSecondary_vec <- c("Flour, maize", "Flour, wheat", "Cake, maize")
RnTPrimary_vec <- c("Sweet potatoes", "Cassava Equivalent", "Potatoes")
RnTSecondary_vec <- c("Starch, cassava", "Cassava dried", "Flour, potatoes")
OilPrimary_vec <- c("Soybeans", "Cottonseed", "Linseed", "Coconuts")
OilSecondary_vec <- c("Oil, soybean", "Oil, palm", "Oil, maize", "Oil, coconut (copra)", "Olive Oil,Total")
SugarcropsPrimary_vec <- c("Sugar beet", "Sugar Raw Centrifugal")
SugarcropsSecondary_vec <- c("Beet pulp")
#---
Cereal_vec_ExpPrice <- c(cerealPrimary_vec, cerealSecondary_vec)
RnT_vec_ExpPrice <- c(RnTPrimary_vec, RnTSecondary_vec)
Fruit_vec_ExpPrice <- c("Oranges+Tang+Clem", "Tomatoes", "Plantains and others", "Mangoes, mangosteens, guavas", "Bananas")
Pulses_vec_ExpPrice <- c("Flour, pulses", "Lentils", "Beans, dry")
Oil_vec_ExpPrice <- c(OilPrimary_vec, OilSecondary_vec)
Sugar_vec_ExpPrice <- c(SugarcropsPrimary_vec, SugarcropsSecondary_vec)
Oil_vec <- c("Soybeans", "Palm kernels", "Cottonseed", "Oil, palm", 
             "Castor oil seed", "Oil, palm fruit", "Linseed", "Sunflower seed")
teaCoffeeCacao_vec_ExpPrice <- c("Tea", "Cocoa, butter", "Cocoa, paste", "Cocoa, beans", "Coffee, roasted", "Coffee, green")
textileIndustrial_vec_ExpPrice <- c("Cotton lint", "Wool, greasy", "Wool, degreased", "Rubber, natural", "Cotton, carded, combed")
#--------------------------
item_vec_ExpPrice <- c(Cereal_vec_ExpPrice, RnT_vec_ExpPrice, Oil_vec_ExpPrice, Fruit_vec_ExpPrice,
                       Pulses_vec_ExpPrice, Sugar_vec_ExpPrice, teaCoffeeCacao_vec_ExpPrice,
                       textileIndustrial_vec_ExpPrice)
#==========================================
# Get SUR item groups ready
Cereal_vec_FoodBal <- c("Wheat and products", "Rice (Milled Equivalent)",
                        "Barley and products", "Maize and products",
                        "Rye and products", "Sorghum and products")
RnT_vec_FoodBal <- c("Cassava and products", "Sweet Potatoes", "Yams",
                     "Potatoes and products")
Oil_vec_FoodBal <- c("Oilcrops")
Sugar_vec_FoodBal <- c("Sugar Crops")
Pulses_vec_FoodBal <- c("Pulses")
Fruit_vec_FoodBal <- c("Fruits - Excluding Wine")
teaCoffeeCacao_vec_FoodBal <- c("Coffee and products", 
                                "Cocoa Beans and products",
                                "Tea (including mate)")
textileIndustrial_vec_FoodBal <- c()
#--------------------------
item_vec_FoodBal <- c(Cereal_vec_FoodBal, RnT_vec_FoodBal, Oil_vec_FoodBal, Fruit_vec_FoodBal,
                      Pulses_vec_FoodBal, Sugar_vec_FoodBal, teaCoffeeCacao_vec_FoodBal,
                      textileIndustrial_vec_FoodBal)
#--------------------------
# Important for collectiveModes()
crop_vec <- c(cerealPrimary_vec, "Cassava", "Potatoes", "Sweet potatoes",
              "Soybean", "Cotton", "Linseed", "Coconut", "Sugar", "Palm",
              "Wool", "Cocoa", "Coffee", "Tea", "Olive")
#==========================================
#==========================================
#==========================================
#==========================================
# Get raw data sets ready
#==========================================
#==========================================
#==========================================
# Get oil price from World Bank Pink Sheet
# Avg. of Brent, WTI, Dubai, nominal $/bbl
df_in <- read.csv("CMOHistoricalDataAnnual.csv", stringsAsFactors = F)
df_crudeOilPrice <- df_in[-c(1:8, 67:nrow(df_in)), c(1:2)]
colnames(df_crudeOilPrice) <- c("Date", "Price")
df_crudeOilPrice$Price <- as.numeric(df_crudeOilPrice$Price)
df_crudeOilPrice$Date <- as.integer(df_crudeOilPrice$Date)
df_crudeOilPrice$zPrice <- scale(df_crudeOilPrice$Price)
zPriceDiff <- diff(df_crudeOilPrice$zPrice)
df_crudeOilPrice$`zPriceDiff` <- c(NA, zPriceDiff)
df_crudeOilPrice <- df_crudeOilPrice[-1, ]
#==========================================
#==========================================
#==========================================
#==========================================
# Get export price data ready
ExportData_raw <- read.csv("Trade_Crops_Livestock_E_All_Data.csv", stringsAsFactors = F)
#------------------------------------------
ExportData_raw <- subset(ExportData_raw, Item %in% item_vec_ExpPrice)
#------------------------------------------
ExportData_raw <- subset(ExportData_raw, Item.Code != 2928)
ExportData_raw$Area.Code <- NULL
ExportData_raw$Element.Code <- NULL
ExportData_raw$Item.Code <- NULL
ExportData_raw$Unit <- NULL
ExportData_raw$Item <- as.character(ExportData_raw$Item)
ExportData_raw$Element <- as.character(ExportData_raw$Element)
ExportData_raw$Area <- as.character(ExportData_raw$Area)
u <- colnames(ExportData_raw)
ExportData_raw <- ExportData_raw[, -grep("F", u)]
colnames(ExportData_raw)[4:ncol(ExportData_raw)] <- as.character(c(1961:2016))
ExportData_raw <- gather(ExportData_raw,Year,Value,`1961`:`2016`)
rm(u)
colnames(ExportData_raw)
ExportData_raw$Year <- as.integer(ExportData_raw$Year)
unique(ExportData_raw$Element)
ExportData_raw <- subset(ExportData_raw, Element %in% c("Export Quantity", "Export Value"))
#------------------------------------------
ExportData_raw <- FAOdat_createRegionGroups(ExportData_raw, exclude_these)
unique(ExportData_raw$Area[which(is.na(ExportData_raw$Region))])
# unique(ExportData_raw$Region)
# unique(ExportData_raw$Area)
# unique(ExportData_raw$Region[which(ExportData_raw$Area == "Pacific Islands Trust Territory")])
#------------------------------------------
ExportData_raw$Group <- NA
u <- ExportData_raw$Item
ExportData_raw$Group[which(u %in% Cereal_vec_ExpPrice)] <- "Cereals"
ExportData_raw$Group[which(u %in% RnT_vec_ExpPrice)] <- "RnT"
ExportData_raw$Group[which(u %in% Oil_vec_ExpPrice)] <- "Oils"
ExportData_raw$Group[which(u %in% Sugar_vec_ExpPrice)] <- "Sugars"
ExportData_raw$Group[which(u %in% Fruit_vec_ExpPrice)] <- "Fruit"
ExportData_raw$Group[which(u %in% Pulses_vec_ExpPrice)] <- "Pulses"
ExportData_raw$Group[which(u %in% teaCoffeeCacao_vec_ExpPrice)] <- "Tea, coffee, cacao"
ExportData_raw$Group[which(u %in% textileIndustrial_vec_ExpPrice)] <- "Industrial (textile, rubber)"
#------------------
ExportData_raw <- ExportData_raw %>% spread(Element, Value)
ExportData_raw$Price <- ExportData_raw$`Export Value` / ExportData_raw$`Export Quantity`
#==========================================
#==========================================
#==========================================
# Get SUR data ready
FoodBal_raw <- read.csv("FoodBalanceSheets_E_All_Data.csv", stringsAsFactors = F)
#unique(FoodBal_raw[which(FoodBal_raw$Item.Code == 2928),"Item"])
FoodBal_raw <- subset(FoodBal_raw, Item.Code != 2928) #Miscellaneous
FoodBal_raw <- subset(FoodBal_raw, !(Item.Code %in% c(2949, 2948))) #Eggs and Milk are repeated for some reason
#------------------------------------------
FoodBal_raw <- subset(FoodBal_raw, Item %in% item_vec_FoodBal)
#------------------------------------------
FoodBal_raw$Area.Code <- NULL
FoodBal_raw$Item.Code <- NULL
FoodBal_raw$Element.Code <-NULL
FoodBal_raw$Unit <-NULL
FoodBal_raw$Item <- as.character(FoodBal_raw$Item)
FoodBal_raw$Element <- as.character(FoodBal_raw$Element)
FoodBal_raw$Area <- as.character(FoodBal_raw$Area)
u <- colnames(FoodBal_raw)
FoodBal_raw <- FoodBal_raw[, -grep("F", u)]
colnames(FoodBal_raw)[4:ncol(FoodBal_raw)] <- as.character(c(1961:2013))
FoodBal_raw <- gather(FoodBal_raw,Year,Value,`1961`:`2013`)
FoodBal_raw$Year <- as.integer(FoodBal_raw$Year)
#------------------------------------------
FoodBal_raw <- FAOdat_createRegionGroups(FoodBal_raw, exclude_these)
unique(FoodBal_raw$Area[which(is.na(FoodBal_raw$Region))])
# unique(FoodBal_raw$Region)
# unique(FoodBal_raw$Area)
#------------------------------------------
u <- FoodBal_raw$Item
FoodBal_raw$Item[which(u == "Soyabeans")] <- "Soybeans"
unique(FoodBal_raw$Item)
#------------------------------------------
FoodBal_raw$Group <- NA
u <- FoodBal_raw$Item
FoodBal_raw$Group[which(u %in% Cereal_vec_FoodBal)] <- "Cereals"
FoodBal_raw$Group[which(u %in% RnT_vec_FoodBal)] <- "RnT"
FoodBal_raw$Group[which(u %in% Oil_vec_FoodBal)] <- "Oils"
FoodBal_raw$Group[which(u %in% Sugar_vec_FoodBal)] <- "Sugars"
FoodBal_raw$Group[which(u %in% Fruit_vec_FoodBal)] <- "Fruit"
FoodBal_raw$Group[which(u %in% Pulses_vec_FoodBal)] <- "Pulses"
FoodBal_raw$Group[which(u %in% teaCoffeeCacao_vec_FoodBal)] <- "Tea, coffee, cacao"
FoodBal_raw$Group[which(u %in% textileIndustrial_vec_FoodBal)] <- "Industrial (textile, rubber)"
#------------------------------------------
# Create stocks-to-use, supply-demand ratios, etc.
# See FAO documentation for guidance
# http://www.fao.org/economic/the-statistics-division-ess/methodology/methodology-systems/supply-utilization-accounts-and-food-balance-sheets-background-information-for-your-better-understanding/en/
# Helpful notes:
# from stocks + production + imports = exports + feed + seed + waste + processing for food + food + other utilization
# production + imports = exports + feed + seed + waste + processing for food + food + other utilization + to stocks
# Where "from stocks" means "carry over"
# and "to stocks" means "ending stocks"
# Note also that "Domestic supply quantity" is badly named. Should be called "Domestic demand" or "Domestic use".
# "Domestic supply quantity" = feed + seed + waste + processing for food + food + other utilization
# Thus:
# from stocks + production + imports = exports + Domestic supply quantity
# production + imports = exports + Domestic supply quantity + to stocks
#--> Carry over = exports + Domestic supply quantity - production - imports
#-->? Ending stocks = production + imports - exports - Domestic supply quantity
# ?And since by definition Supply = Carry over + production + imports, then:
#-->? Supply = (exports + Domestic supply quantity - production - imports) + production + imports
# = exports + Domestic supply quantity = Demand
#----------
#unique(FoodBal_raw$Element)
FoodBal_raw <- subset(FoodBal_raw, Element %in% c("Stock Variation", "Domestic supply quantity"))
FoodBal_raw <- FoodBal_raw %>% spread(Element, Value)
FoodBal_raw <- as.data.frame(FoodBal_raw %>% group_by(Area, Item) %>%
                               mutate(x = sum(`Stock Variation`, na.rm = T)))

this_fun <- function(S0, Svar){
  s <- c()
  s[1] <- S0
  for(i in 2:length(Svar)){
    s[i] <- s[i - 1] + Svar[i] 
  }
  return(s)
}
FoodBal_raw <- as.data.frame(FoodBal_raw %>% group_by(Area, Item) %>%
                               mutate(Stocks = ifelse(x < 0, this_fun(-x, `Stock Variation`), this_fun(0, `Stock Variation`))))
FoodBal_raw <- as.data.frame(FoodBal_raw %>% group_by(Area, Item) %>%
                               mutate(`Stock Variation Check` = c(NA, diff(Stocks, na.rm = T))))





FoodBal_raw <- FoodBal_raw[, c("Area", "Item", "Year",
                               "Region", "Group",
                               "SupplyDemandRatio")]
colnames(FoodBal_raw)[ncol(FoodBal_raw)] <- "Value"
#==========================================
#==========================================
#==========================================
#==========================================
#==========================================
# Now ready to begin analysis on areas and items of interest
#==========================================
#==========================================
#==========================================
# SUR analysis
#==========================================
these_items <- c(RnT_vec_FoodBal, Cereal_vec_FoodBal,
                 Sugar_vec_FoodBal, Oil_vec_FoodBal)
these_regions <- c("Eastern Asia", "South-Eastern Asia",
                   "North America")
start_year <- 1968
#------------------------------------------
FoodBal <- subset(FoodBal_raw, Item %in% these_items)
FoodBal <- subset(FoodBal, Region %in% these_regions)
FoodBal <- subset(FoodBal, Year >= start_year)
#------------------------------------------
# Clean
FoodBal <- as.data.frame(FoodBal %>% mutate(Value = ifelse(is.nan(Value) | is.na(Value) | is.infinite(Value), NA, Value)))
FoodBal <- as.data.frame(FoodBal %>% 
                           mutate(isMiss = ifelse(is.na(Value), 1, 0)))
FoodBal <- as.data.frame(FoodBal %>% group_by(Area, Item) %>%
                           mutate(nMiss = sum(isMiss)))
FoodBal <- subset(FoodBal, nMiss <= 9)
unique(FoodBal$Area)
#unique(FoodBal$isMiss)
# FoodBal <- as.data.frame(FoodBal %>% group_by(Area, Item) %>%
#                                     mutate(Value = ifelse(isMiss == 1, mean(Value, na.rm = T), Value)))
# Lots of items have 0 variation for most of series. Get rid of these.
# FoodBal <- as.data.frame(FoodBal %>% group_by(Area, Item) %>%
#                                     mutate(nLowVar = length(which(abs(Value) <= 5))))
# FoodBal <- subset(FoodBal, nLowVar <= 20)
#------------------------------------------
# Scale
FoodBal <- as.data.frame(FoodBal %>% group_by(Area, Item) %>%
                           mutate(zValue = scale(Value)))
#------------------------------------------
# Visually inspect
gg <- ggplot(FoodBal, aes(x = Year, y = zValue, group = Item, color = Item))
gg <- gg + geom_line() + facet_wrap(~Area, ncol = 2, scales = "free")
gg
#------------------------------------------
FoodBal$`Country-Item` <- paste(FoodBal$Area, FoodBal$Item)
#FoodBal$CommodGroup <- FoodBal$Group
FoodBal$CommodGroup <- NA
u <- FoodBal$Item
FoodBal$Item[grep("beet", u, ignore.case = T)] <- paste(FoodBal$Item[grep("beet", u, ignore.case = T)], "(sugar)")
u <- FoodBal$Item
for(i in 1:length(crop_vec)){
  FoodBal$CommodGroup[grep(crop_vec[i], u, ignore.case = T)] <- crop_vec[i]
}
unique(FoodBal$CommodGroup)
#--careful
ind_stillNA <- which(is.na(FoodBal$CommodGroup))
unique(FoodBal$Item[ind_stillNA])
FoodBal$CommodGroup[ind_stillNA] <- FoodBal$Item[ind_stillNA]
#------------------------------------------
# Prepare input for collectiveModes()
FoodBal_group <- FoodBal[, c("Country-Item", "Region", "CommodGroup")]
FoodBal_group <- FoodBal_group[-which(duplicated(FoodBal_group)), ]
col_order <- FoodBal_group$`Country-Item`
df_group <- FoodBal_group
# 
FoodBal_ts <- FoodBal[, c("Year", "Country-Item", "zValue")]
FoodBal_ts <- FoodBal_ts %>% spread(`Country-Item`, zValue)
colnames(FoodBal_ts)[1] <- "Date"
FoodBal_ts <- FoodBal_ts[, c("Date", col_order)]
df_ts <- FoodBal_ts
#------------------------------------------
# Merge with oil price series
# Modify df_group accordingly
df_ts <- merge(df_ts, df_crudeOilPrice[, c("Date", "zPriceDiff")], by = "Date")
df_group <- rbind(df_group, c("World oil, 1st order", "(Crude oil)", "Crude oil, 1st order"))
#------------------------------------------
date_vec <- df_ts$Date
df_ts$Date <- NULL
mat_diff <- diff(as.matrix(df_ts))
date_vec <- date_vec[-1]
nrow(df_ts) / ncol(df_ts)
out_cm <- collectiveModes(mat_diff, date_vec, df_group, 
                          Contrib_as_ModeSq = F,
                          AggregateContributions = F)
#------------------------------------------
#==========================================
#==========================================
#==========================================
#==========================================
# Export price analysis
#==========================================
#==========================================
these_items <- c(RnT_vec_ExpPrice, Cereal_vec_ExpPrice,
                 Sugar_vec_ExpPrice, Oil_vec_ExpPrice)
these_regions <- c("South-Eastern Asia", "Eastern Asia", "North America")#, "Northern Europe", "Western Europe")
# these_items <- c(RnT_vec_ExpPrice, Cereal_vec_ExpPrice, Sugar_vec_ExpPrice)
# these_regions <- c("South-Eastern Asia", "Eastern Asia", "North America", 
#                    "Northern Europe", "Western Africa", "Southern Africa")
start_year <- 1968
#------------------------------------------
ExportData <- subset(ExportData_raw, Item %in% these_items)
ExportData <- subset(ExportData, Region %in% these_regions)
ExportData <- subset(ExportData, Year >= start_year)
ExportData$`Export Quantity` <- NULL
ExportData$`Export Value` <- NULL
colnames(ExportData)[ncol(ExportData)] <- "Value"
#------------------------------------------
# Clean
ExportData <- as.data.frame(ExportData %>% mutate(Value = ifelse(is.nan(Value) | is.na(Value) | is.infinite(Value), NA, Value)))
ExportData <- as.data.frame(ExportData %>% 
                              mutate(isMiss = ifelse(is.na(Value), 1, 0)))
ExportData <- as.data.frame(ExportData %>% group_by(Area, Item) %>%
                              mutate(nMiss = sum(isMiss)))
ExportData <- subset(ExportData, nMiss <= 5)
unique(ExportData$Area)
# Lots of items have 0 variation for most of series. Get rid of these.
# ExportData <- as.data.frame(ExportData %>% group_by(Area, Item) %>%
#                            mutate(nLowVar = length(which(abs(Value) <= 1))))
# ExportData <- subset(ExportData, nLowVar <= 20)
#------------------------------------------
# Interpolate remaining missing values
ExportData <- as.data.frame(ExportData %>% group_by(Area, Item) %>%
                              mutate(Value = ifelse(isMiss == 1, mean(Value, na.rm = T), Value)))
#------------------------------------------
# Scale
ExportData <- as.data.frame(ExportData %>% group_by(Area, Item) %>%
                              mutate(zValue = scale(Value)))
#------------------------------------------
# Visually inspect
gg <- ggplot(ExportData, aes(x = Year, y = zValue, group = Item, color = Item))
gg <- gg + geom_line() + facet_wrap(~Area, ncol = 2, scales = "free")
gg
#------------------------------------------
ExportData$`Country-Item` <- paste(ExportData$Area, ExportData$Item)
#ExportData$CommodGroup <- ExportData$Group
ExportData$CommodGroup <- NA
u <- ExportData$Item
ind_beet <- grep("beet", u, ignore.case = T)
ExportData$Item[ind_beet] <- paste(ExportData$Item[ind_beet], "(sugar)")
u <- ExportData$Item
for(i in 1:length(crop_vec)){
  ExportData$CommodGroup[grep(crop_vec[i], u, ignore.case = T)] <- crop_vec[i]
}
unique(ExportData$CommodGroup)
#--careful
# ind_stillNA <- which(is.na(ExportData$CommodGroup))
# unique(ExportData$Item[ind_stillNA])
# ExportData$CommodGroup[ind_stillNA] <- ExportData$Item[ind_stillNA]
#-
u <- ExportData$CommodGroup
ind_changeToOil <- which(u %in% c("Coconut", "Cotton", "Palm", "Linseed", "Soybean", "Olive"))
ExportData$CommodGroup[ind_changeToOil] <- "Oilcrop"
unique(ExportData$CommodGroup)
#------------------------------------------
# Prepare input for collectiveModes()
# Prepare df_group
ExportData_group <- ExportData[, c("Country-Item", "Region", "CommodGroup")]
ExportData_group <- ExportData_group[-which(duplicated(ExportData_group)), ]
col_order <- ExportData_group$`Country-Item`
df_group <- ExportData_group
# Prepare mat_diff
ExportData_ts <- ExportData[, c("Year", "Country-Item", "Value")]
ExportData_ts <- ExportData_ts %>% spread(`Country-Item`, Value)
colnames(ExportData_ts)[1] <- "Date"
ExportData_ts <- ExportData_ts[, c("Date", col_order)]
#------------------------------------------
# Merge with oil price series
# Modify df_group accordingly
df_ts <- ExportData_ts
df_ts <- merge(df_ts, df_crudeOilPrice[, c("Date", "zPriceDiff")], by = "Date")
colnames(df_ts)[ncol(df_ts)] <- "Crude oil, 1st order"
df_group <- rbind(df_group, c("World oil, 1st order", "(Crude oil)", "Crude oil, 1st order"))
col_order <- c(col_order, "Crude oil, 1st order")
#------------------------------------------
date_vec <- df_ts$Date
df_ts$Date <- NULL
#-this way
mat_ts <- as.matrix(df_ts)
mat_diff <- diff(mat_ts)
date_vec <- date_vec[-1]
#-or this way
# per_ema = 3
# mat_diff <- apply(df_ts, 2, getSlope, slope_per = 3, per_ema, Programatic = T)
# date_vec <- date_vec[-c(1:per_ema)]
#nrow(df_ts)
#-
nrow(mat_diff) / ncol(mat_diff)
out_cm <- collectiveModes(mat_diff, date_vec, df_group,
                          Contrib_as_ModeSq = F,
                          AggregateContributions = F,
                          plot_eigenportfolio_ts = T)
#------------------------------------------
#
n_ts <- ncol(mat_ts)
ts_avg <- mat_ts %*% rep(1, n_ts) * 1 / n_ts
ts_avg <- ts_avg[-1]
mat_sigModes <- out_cm[[1]]
mat_mode_ts <- mat_ts %*% mat_sigModes
mat_mode_ts <- mat_mode_ts[-1, ]
df_mode_ts <- as.data.frame(mat_mode_ts)
acf(df_mode_ts)
pacf(df_mode_ts)
n_sig <- ncol(df_mode_ts)
colnames(df_mode_ts) <- as.character(1:n_sig)
df_mode_ts$`ts Avg.` <- ts_avg
gathercols <- colnames(df_mode_ts)
df_mode_ts$Date <- date_vec
df_mode_ts <- df_mode_ts %>% gather_("Mode", "Value", gathercols)
df_plot <- df_mode_ts
zdf_plot <- as.data.frame(df_plot %>% group_by(Mode) %>% mutate(Value = scale(Value)))
ggplot(zdf_plot, aes(x = Date, y = Value, group = Mode, color = Mode)) +
  geom_line()
#---
# Mode decomposition
#---
ExportData_group <- ExportData[, c("Country-Item", "Area", "Item", "Region", "Group", "CommodGroup")]
ExportData_group <- ExportData_group[-which(duplicated(ExportData_group)), ]
#ExportData_group$`Country-Item`
df_group <- rbind(ExportData_group, c("World oil, 1st order", "(Crude oil)", "Crude oil, 1st order"))
#col_order <- df_group$`Country-Item`
#---
mat_mode_ts <- mat_ts %*% diag(mat_sigModes[, 3])
mat_mode_ts <- mat_mode_ts[-1, ]
mat_mode_ts <- t(mat_mode_ts)
df_mode_ts <- as.data.frame(mat_mode_ts)
df_mode_ts <- cbind(df_mode_ts, df_group)
colnames(df_mode_ts)[1:ncol(mat_mode_ts)] <- date_vec
gathercols <- colnames(df_mode_ts[1:ncol(mat_mode_ts)])
df_mode_ts <- df_mode_ts %>% gather_("Date", "Value", gathercols)
df_mode_ts$Date <- as.integer(df_mode_ts$Date)
df_plot <- df_mode_ts
df_plot <- subset(df_plot, Group == "Cereals")
gg <- ggplot(df_plot, aes(x = Date, y = Value, group = Item, color = Item))
gg <- gg + geom_line() + facet_wrap(~Area, ncol = 2, scales = "free")
gg




#
mat_modeVolat <- mat_diff %*% mat_sigModes
sd_vec <- apply(mat_modeVolat, 2, sd)
#








#==========================================
#==========================================


ExportData <- subset(ExportData_raw, Item %in% item_vec)
#ExportData <- ExportData %>% group_by(Area, Year, Item, Element) %>% summarise(Value = sum(Value))
#------------------
# these_regions <- c("E. & S.E. Asia", "North America", 
#                    "W., N., & S. Europe", "Southern Asia",
#                    "Sub-Saharan Africa")
these_regions <- c("W., N., & S. Europe", "North America", "E. & S.E. Asia")
#these_groups <- c("Cereals")
#these_groups <- c("Oils")
#these_groups <- c("Industrial (textile, rubber)")
#these_groups <- c("Tea, coffee, cacao")
#these_groups <- c("Pulses")
#these_groups <- c("Fruit")
#these_groups <- c("RnT", "Cereals", "Sugars")
these_groups <- c("RnT", "Cereals")
ExportData_clean <- subset(ExportData, Region %in% these_regions & Year >= 1960)
ExportData_clean$`Export Quantity` <- NULL
ExportData_clean$`Export Value` <- NULL

unique(ExportData_clean$Area)
ExportData_clean <- subset(ExportData_clean, Group %in% these_groups)
unique(ExportData_clean$Item)

ExportData_clean <- as.data.frame(ExportData_clean %>% 
                                    mutate(Price = ifelse(is.nan(Price) | is.na(Price) | is.infinite(Price), NA, Price)))
ExportData_clean <- as.data.frame(ExportData_clean %>% 
                                    mutate(isMiss = ifelse(is.na(Price), 1, 0)))
ExportData_clean <- as.data.frame(ExportData_clean %>% group_by(Area, Item) %>%
                                    mutate(nMiss = sum(isMiss)))
ExportData_clean <- subset(ExportData_clean, nMiss <= 5)
unique(ExportData_clean$Area)

ExportData_clean <- as.data.frame(ExportData_clean %>% group_by(Area, Item) %>%
                                    mutate(Price = ifelse(isMiss == 1, mean(Price, na.rm = T), Price)))
#---
# detrend_per <- 3
# ExportData_clean <- as.data.frame(ExportData_clean %>% group_by(Area, Item) %>%
#                                     mutate(Price_ema = EMA(Price, detrend_per)))
# ExportData_clean <- subset(ExportData_clean, is.na(Price_ema) == F)
# ExportData_clean <- as.data.frame(ExportData_clean %>% group_by(Area, Item) %>%
#                                     mutate(Price_dt = Price - Price_ema))
# ExportData_clean <- as.data.frame(ExportData_clean %>% group_by(Area, Item) %>%
#                                     mutate(zPrice_dt = scale(Price_dt)))
#---
ExportData_clean <- as.data.frame(ExportData_clean %>% group_by(Area, Item) %>%
                                    mutate(zPrice = scale(Price)))
#----
# df_plot <- subset(ExportData_clean, Area == "Finland")
# gg <- ggplot(df_plot, aes(x = Year, y = zPrice, group = Item, color = Item))
# gg <- gg + geom_line() + facet_wrap(~Area, ncol = 2, scales = "free")
# gg
#----

ExportData_clean$`Country-Item` <- paste(ExportData_clean$Area, ExportData_clean$Item)
#ExportData_clean$CommodGroup <- ExportData_clean$Group
ExportData_clean$CommodGroup <- NA
ExportData_clean$Item[grep("beet", u, ignore.case = T)] <- paste(ExportData_clean$Item[grep("beet", u, ignore.case = T)], "(sugar)")
u <- ExportData_clean$Item
for(i in 1:length(crop_vec)){
  ExportData_clean$CommodGroup[grep(crop_vec[i], u, ignore.case = T)] <- crop_vec[i]
}
unique(ExportData_clean$CommodGroup)
#--careful
ind_stillNA <- which(is.na(ExportData_clean$CommodGroup))
unique(ExportData_clean$Item[ind_stillNA])
ExportData_clean$CommodGroup[ind_stillNA] <- ExportData_clean$Item[ind_stillNA]
#--

ExportData_ts <- ExportData_clean[, c("Year", "Country-Item", "zPrice")]
ExportData_group <- ExportData_clean[, c("Country-Item", "Area", "CommodGroup")]
ExportData_group <- ExportData_group[-which(duplicated(ExportData_group)), ]
ExportData_ts <- ExportData_ts %>% spread(`Country-Item`, zPrice)
colnames(ExportData_ts)[1] <- "Date"

df_ts <- ExportData_ts
df_group <- ExportData_group
out_cm <- collectiveModes(df_ts, df_group, AggregateContributions = T)






































































#=================================
# Monthly producer prices
#=================================
# Commodity vectors
Pulses_vec <- c("Beans, dry", "Lentils", "Cow peas, dry", "Broad beans, horse beans, dry", "Chick peas")
Cereal_vec <- c("Rice, paddy", "Sorghum", "Wheat", "Millet", "Maize", "Rye", "Barley")
RnT_vec <- c("Cassava", "Potatoes", "Sweet potatoes", "Yams")
Oil_vec <- c("Soybeans", "Palm kernels", "Cottonseed", "Oil, palm", 
             "Castor oil seed", "Oil, palm fruit", "Linseed", "Sunflower seed")
Fruit_vec <- c("Bananas", "Papayas", "Oranges", 
               "Tangerines, mandarins, clementines, satsumas", "Grapefruit (inc. pomelos)",
               "Coconuts", "Mangoes, mangosteens, guavas")
textileIndustrial_vec <- c("Wool, greasy", "Rubber, natural", "Cotton lint", "Sugar cane")
teaCoffeeCacao_vec <- c("Coffee, green", "Cocoa, beans", "Tea")
#--------------------------
item_vec <- c(Pulses_vec, Cereal_vec, RnT_vec, Oil_vec, Fruit_vec,
              textileIndustrial_vec, teaCoffeeCacao_vec)
#--------------------------
df_raw <- read.csv("Prices_Monthly_E_All_Data.csv", stringsAsFactors = F)
df_raw$Months.Code <- NULL
df_raw$Item.Code <- NULL
df_raw$Area.Code <- NULL
df_raw$Element.Code <-NULL
df_raw$Unit <- NULL
df_raw$Element <- NULL
df_raw$Item <- as.character(df_raw$Item)
df_raw$Area <- as.character(df_raw$Area)
u <- colnames(df_raw)
df_raw <- df_raw[, -grep("F", u)]
colnames(df_raw)[4:ncol(df_raw)] <- as.character(c(2010:2015))
df_raw <- gather(df_raw, Year, Value,`2010`:`2015`)
rm(u)
#-------------
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
LAC <- c("Central America", "Caribbean", "South America")
Europe_E <- "Eastern Europe"
Europe_WNS <- c("Southern Europe", "Western Europe", "Northern Europe")
SSA <- c("Eastern Africa", "Southern Africa", "Western Africa", "Middle Africa")
ESE_Asia <- c("South-Eastern Asia", "Eastern Asia")
u <- df_raw$Region
df_raw$Region[which(u %in% LAC)] <- "LAC"
df_raw$Region[which(u %in% Europe_E)] <- "E. Europe"
df_raw$Region[which(u %in% Europe_WNS)] <- "W., N., & S. Europe"
df_raw$Region[which(u %in% SSA)] <- "Sub-Saharan Africa"
df_raw$Region[which(u %in% ESE_Asia)] <- "E. & S.E. Asia"
#--
df_raw$Group <- NA
u <- df_raw$Item
df_raw$Group[which(u %in% Cereal_vec)] <- "Cereals"
df_raw$Group[which(u %in% RnT_vec)] <- "RnT"
df_raw$Group[which(u %in% Pulses_vec)] <- "Pulses"
df_raw$Group[which(u %in% teaCoffeeCacao_vec)] <- "Tea, Coffee, Cacao"
df_raw$Group[which(u %in% textileIndustrial_vec)] <- "Textiles & Industrial"
df_raw$Group[which(u %in% Oil_vec)] <- "Oil crops"
df_raw$Group[which(u %in% Fruit_vec)] <- "Fruit"
#--
df_raw$YearMonth <- as.character(paste(df_raw$Year, df_raw$Months))
df_raw$YearMonth <- as.yearmon(df_raw$YearMonth, format = "%Y %B")
#library(zoo)
#--
df <- subset(df_raw, Item %in% item_vec)
#--
#==========
these_regions <- c("E. & S.E. Asia")
#these_items <- c("Cassava", "Maize", "Wheat", "Rice, paddy")
#these_groups <- c("Cereals")
these_groups <- c("Cereals")
#these_groups <- c("Oil crops")
#these_groups <- c("Textiles & Industrial")
df_clean <- subset(df, Region %in% these_regions)
#df_plot <- subset(df_plot, Item %in% these_items)
df_clean <- subset(df_clean, Group %in% these_groups)
df_clean <- df_clean %>% group_by(Area, Item, Year) %>% mutate(zValue = scale(Value))
df_clean <- as.data.frame(df_clean)
# gg <- ggplot(df_clean, aes(x = YearMonth, y = zValue, group = Item, color = Item))
# gg <- gg + geom_line() + facet_wrap(~Area, ncol = 2, scales = "free")
# gg
months_vec <- unique(df_clean$Months)
df_clean <- as.data.frame(df_clean %>% group_by(Area, Item, Year) %>%
                            mutate(nMonths = length(Months)))
df_clean <- subset(df_clean, nMonths == 12)
unique(df_clean$Area)
gg <- ggplot(df_clean, aes(x = YearMonth, y = zValue, group = Item, color = Item))
gg <- gg + geom_line() + facet_wrap(~Area, ncol = 2, scales = "free")
gg
df_clean <- as.data.frame(df_clean %>% group_by(Area, Item) %>%
                            mutate(isMiss = ifelse(is.nan(Value) | is.na(Value) | is.infinite(Value), 1, 0)))
df_clean <- as.data.frame(df_clean %>% group_by(Area, Item) %>%
                            mutate(nMiss = sum(isMiss)))
df_clean <- subset(df_clean, nMiss <= 20)
# gg <- ggplot(df_clean, aes(x = YearMonth, y = zValue, group = Item, color = Item))
# gg <- gg + geom_line() + facet_wrap(~Area, ncol = 2)
# gg
df_clean <- df_clean %>% group_by(Area, Item, Year) %>%
  mutate(zValue = scale(Value))
df_clean <- as.data.frame(df_clean)


ind_interpol <- which(df_clean$nMiss > 0)
interpol_for_these <- unique(df_interpol$Area[ind_interpol])
interpol_for_these <- setdiff(interpol_for_these, NA)
for(i in 1:length(interpol_for_these)){
  this_area <- interpol_for_these[i]
  u <- df_clean$Area
  these_items <- unique(df_clean$Item[which(u == this_area)])
  for(j in 1:length(these_items)){
    this_item <- these_items[j]
    df_x <- subset(df_clean, Item == this_item & Area == this_area)
    df_x <- df_x[, c("YearMonth", "zValue")]
    #ggplot(df_x, aes(x = YearMonth, y = zValue)) + geom_line()
    ind_na_true <- which(is.na(df_x$zValue))
    ind_na_false <- which(is.na(df_x$zValue) == F)
    ts <- df_x$zValue[ind_na_false]
    t <- ind_na_false
    t_proj <- ind_na_true
    outwavfit <- fitWave(ts, t, t_proj, n_max_periods = 15, pval_thresh = 0.1, quietly = F)
    wavfit <- outwavfit[[1]]
    wavproj <- outwavfit[[2]]
    df_x$wavfit <- NA
    df_x$wavfit[which(is.na(df_x$zValue) == F)] <- wavfit
    df_x$wavfit_proj <- NA
    noise <- rnorm(length(wavfit), 0, 0.2)
    df_x$wavfit_proj[which(is.na(df_x$zValue) == T)] <- wavproj + noise
    df_plot <- df_x %>% gather(Type, ts, zValue:wavfit_proj)
    gg <- ggplot(df_plot, aes(x = YearMonth, y = ts, color = Type))
    gg <- gg + geom_line()
    gg
    u_Area <- df_clean$Area
    u_Item <- df_clean$Item
    ind_ts <- intersect(which(u_Area == this_area), which(u_Item == this_item))
    ind_replace_na <- ind_ts[ind_na_true]
    df_clean$zValue[ind_replace_na] <- wavproj
  }
  
  
}



df_clean$`Country-Item` <- paste(df_clean$Area, df_clean$Item)
df_ts <- df_clean[, c("YearMonth", "Country-Item", "zValue")]
df_group <- df_clean[, c("Country-Item", "Area", "Item")]
df_group <- df_group[-which(duplicated(df_group)), ]
df_ts <- df_ts %>% spread(`Country-Item`, zValue)
colnames(df_ts)[1] <- "Date"

out_cm <- collectiveModes(df_ts, df_group)
