
set.seed(321)

library(tidyverse)

"%ni%" = Negate("%in%")

UC_data <- read.csv("outputData/UC_nsf_merged_monitorsOpenAQWAQI.csv")

UC_gridded <- read.csv("outputData/UC_nsf_monitors_mergedGrid.csv")
UC_data <- merge(UC_data, UC_gridded, by = "ID_HDC_G0")

nonOECDUC <- subset(UC_data, oecd != 'OECD')
nonOECDUC <- subset(nonOECDUC, INCM_CMI %ni% c('HIC', 'Other') & !is.na(INCM_CMI)) # remove high income countries and NA
nonOECDUC <- subset(nonOECDUC, !is.na(vdem_bin)) # here I subset the countries that are not in the vdem dataset
# nonOECDUC_woINDCHN <- subset(nonOECDUC, CTR_MN_NM %ni% c('India', 'China')) # here I subset india and china

nonOECDUC$vdem_bin <- ifelse(nonOECDUC$vdem_bin == "non-democracy", "0non-democracy",
                              ifelse(nonOECDUC$vdem_bin == "democracy", "1democracy", NA))

nonOECDUC <- subset(nonOECDUC, GDP15_SM != 0) # here I subset the countries that are not in the vdem dataset

nonOECDUC$GDP15_SMpc <- nonOECDUC$GDP15_SM/nonOECDUC$P15

nonOECDUC$isMonitor <- as.factor(ifelse((nonOECDUC$numberMonitor - nonOECDUC$numberUSEmbassyMonitors) > 0, 1, 0))

# Try with a dummy IND and CHN
nonOECDUC$dummyCHN <- as.factor(ifelse(nonOECDUC$CTR_MN_NM == "China", 1, 0))
nonOECDUC$dummyIND <- as.factor(ifelse(nonOECDUC$CTR_MN_NM == "India", 1, 0))

# Below we sub-sample IND and CHN because they make up the whole dataset otherwise
# Step 1: Identify the number of rows of the third most present label
label_counts <- nonOECDUC %>%
  count(CTR_MN_NM) %>%
  arrange(desc(n))

# Number of rows to sample for "India" and "China"
target_rows <- label_counts$n[3]

# Subsample for "India" and "China"
# Step 2: Adjust the rows for "India"
df_india_adjusted <- nonOECDUC %>%
  filter(CTR_MN_NM == "India") %>%
  sample_n(min(n(), target_rows))

# Step 2: Adjust the rows for "China"
df_china_adjusted <- nonOECDUC %>%
  filter(CTR_MN_NM == "China") %>%
  sample_n(min(n(), target_rows))

# Combine adjusted "India" and "China" with other labels
nonOECDUC_adjusted <- nonOECDUC %>%
  filter(!CTR_MN_NM %in% c("India", "China")) %>%
  bind_rows(df_india_adjusted, df_china_adjusted)

fit <- glm(formula = isMonitor
           ~ log(GDP15_SMpc+1) + pm25VanD20002016 * vdem_bin + 
             conflict_cumulative_intensity_22 +
             CPI_2012_2022 +
             capital +
             log(P15)
           + dummyCHN + dummyIND
           , binomial(link = "logit"), data = nonOECDUC_adjusted)

summary(fit)

plot(residuals(fit))

fullCities <- nonOECDUC_adjusted %>% 
  filter(!if_any(c(isMonitor, GDP15_SMpc, pm25VanD20002016, vdem_bin, conflict_cumulative_intensity_22, CPI_2012_2022, capital, P15, GCPNT_LAT, GCPNT_LON), is.na))

fit <- glm(formula = isMonitor
           ~ log(GDP15_SMpc+1) + pm25VanD20002016 * vdem_bin + 
             conflict_cumulative_intensity_22 +
             CPI_2012_2022 +
             capital +
             log(P15)
           + dummyCHN + dummyIND
           , binomial(link = "logit"), data = fullCities)

summary(fit)

fullCities$resid_pearson <- residuals(fit)
# fullCities$resid_pearson <- residuals(fit, type = "pearson")

library(spdep)

cities_pts <- st_as_sf(
  fullCities,
  coords = c("GCPNT_LON", "GCPNT_LAT"),
  crs = 4326,
  remove = FALSE   # keep lon/lat columns
)

coords <- sf::st_coordinates(cities_pts)
nb <- knn2nb(knearneigh(coords, k = 5))   # choose k (e.g., 4–8) after checking connectivity
lw <- nb2listw(nb, style = "W", zero.policy = TRUE)

moran.test(fullCities$resid_pearson, lw, zero.policy = TRUE)

set.seed(42)
moran.mc(fullCities$resid_pearson, lw, nsim = 999, zero.policy = TRUE)

nb_50 <- dnearneigh(coords, 0, 50, longlat = TRUE)
nb_100 <- dnearneigh(coords, 0, 100, longlat = TRUE)
nb_500 <- dnearneigh(coords, 0, 500, longlat = TRUE)
nb_1000 <- dnearneigh(coords, 0, 1000, longlat = TRUE)

lw_50 <- nb2listw(nb_50, style = "W", zero.policy = TRUE)
lw_100 <- nb2listw(nb_100, style = "W", zero.policy = TRUE)
lw_500 <- nb2listw(nb_500, style = "W", zero.policy = TRUE)
lw_1000 <- nb2listw(nb_1000, style = "W", zero.policy = TRUE)

table(card(nb_50))
table(card(nb_100))
table(card(nb_500))
table(card(nb_1000))

neigh50 <- moran.mc(fullCities$resid_pearson, lw_50, nsim = 999, zero.policy = TRUE)
neigh100 <- moran.mc(fullCities$resid_pearson, lw_100, nsim = 999, zero.policy = TRUE)
neigh500 <- moran.mc(fullCities$resid_pearson, lw_500, nsim = 999, zero.policy = TRUE)
neigh1000 <- moran.mc(fullCities$resid_pearson, lw_1000, nsim = 999, zero.policy = TRUE)

neigh50
neigh100
neigh500
neigh1000
