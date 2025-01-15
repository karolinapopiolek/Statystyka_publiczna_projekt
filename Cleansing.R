library(readxl)
library(dplyr)
library(ggplot2)
library(patchwork)

# Importing the data
Gini <- read_excel("Gini.xlsx", sheet = "Czyste dane", 
                   col_types = c("text", "numeric", "numeric"))

chronicDisease2014 <- read_excel("ChronicDisease.xlsx", 
                                 sheet = "2014_czyste", col_types = c("text", 
                                                                      "numeric", "numeric", "numeric", 
                                                                      "numeric", "numeric", "numeric"))
chronicDisease2019 <- read_excel("ChronicDisease.xlsx", 
                                 sheet = "2019_czyste", col_types = c("text", 
                                                                      "numeric", "numeric", "numeric", 
                                                                      "numeric", "numeric", "numeric"))
# Cleaning data for Gini dataset

# Simplifying the column names 
colnames(chronicDisease2014) <- c("GEO", "backPain", "Asthma", "Respiratory", "Hypertension", "Diabetes", "Depression")
colnames(chronicDisease2019) <- c("GEO", "backPain", "Asthma", "Respiratory", "Hypertension", "Diabetes", "Depression")

# Remove rows with missing values or unncecessary data
Gini <- Gini %>% na.omit() %>%
  filter(!grepl("^Euro|Serb|Unit|Swit|North", 'GEO'))


# Cleaning data for chronicDisease2014 and 2019 datasets
# Remove rows with missing values or unncecessary data
chronicDisease2014 <- chronicDisease2014 %>% na.omit() %>% 
  filter(!grepl("^Euro|Serbia|United Kingdom", `GEO`))

chronicDisease2019 <- chronicDisease2019 %>% na.omit() %>% 
  filter(!grepl("^Euro|Serbia|United Kingdom", `GEO`))


# Create the mapping of countries and their two-letter codes
country_codes <- data.frame(
  Country = c(
    "Belgium", "Bulgaria", "Czechia", "Denmark", "Germany", "Estonia", 
    "Ireland", "Greece", "Spain", "France", "Croatia", "Italy", "Cyprus", 
    "Latvia", "Lithuania", "Luxembourg", "Hungary", "Malta", "Netherlands", 
    "Austria", "Poland", "Portugal", "Romania", "Slovenia", "Slovakia", 
    "Finland", "Sweden", "Iceland", "Norway", "Türkiye"
  ),
  Code = c(
    "BE", "BG", "CZ", "DK", "DE", "EE", "IE", "GR", "ES", "FR", "HR", "IT", 
    "CY", "LV", "LT", "LU", "HU", "MT", "NL", "AT", "PL", "PT", "RO", "SI", 
    "SK", "FI", "SE", "IS", "NO", "TR"
  )
)

# Replace the first column of Gini with codes
Gini <- Gini %>%
  left_join(country_codes, by = c("Year" = "Country")) %>%
  mutate(Year = Code) %>%
  select(-Code)

# Replace the first column of chronicDisease2014 with codes
chronicDisease2014 <- chronicDisease2014 %>%
  left_join(country_codes, by = c("GEO" = "Country")) %>%
  mutate(`GEO` = Code) %>%
  select(-Code)

# Replace the first column of chronicDisease2019 with codes
chronicDisease2019 <- chronicDisease2019 %>%
  left_join(country_codes, by = c("GEO" = "Country")) %>%
  mutate(`GEO` = Code) %>%
  select(-Code)



# Now the datasets are unified

# Calculating average Gini values for 2014 and 2019
gini2014Average <- mean(Gini$`2014`)
gini2019Average <- mean(Gini$`2019`)

# Breaking the countries into geographic groups for better visualization

# Calculating average disease values for 2014 and 2019
chronicDisease2014Avg <- chronicDisease2014 %>% 
  select(where(is.numeric)) %>% 
  colMeans(na.rm = TRUE)

print(chronicDisease2014Avg)

chronicDisease2019Avg <- chronicDisease2019 %>% 
  select(where(is.numeric)) %>% 
  colMeans(na.rm = TRUE)

print(chronicDisease2019Avg)

# Convert to a data frame for plotting
chronicDiseaseAvg <- data.frame(
  Year = c(rep("2014", 6), rep("2019", 6)),
  Variable = names(chronicDisease2014Avg),
  Value = c(chronicDisease2014Avg, chronicDisease2019Avg)
)

# Plot for avg chronic disease change
ggplot(chronicDiseaseAvg, aes(x = Variable, y = Value, fill = Year)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Average Chronic Disease Values Comparison",
       x = "Variables", 
       y = "Average Value")

# Create a list of country groups
country_groups <- list(
  "Northern Europe" = c("DK", "EE", "FI", "SE", "IS", "NO", "LV", "LT"),
  "Western Europe" = c("BE", "DE", "IE", "FR", "LU", "NL", "AT"),
  "Southern Europe" = c("GR", "ES", "IT", "CY", "MT", "PT", "HR", "SI"),
  "Eastern Europe" = c("BG", "CZ", "HU", "PL", "RO", "SK", "TR")
)

Gini$Region <- NA
for(region in names(country_groups)) {
  Gini$Region[Gini$GEO %in% country_groups[[region]]] <- region
}

chronicDisease2014$Region <- NA
for(region in names(country_groups)) {
  chronicDisease2014$Region[chronicDisease2014$GEO %in% country_groups[[region]]] <- region
}

chronicDisease2019$Region <- NA
for(region in names(country_groups)) {
  chronicDisease2019$Region[chronicDisease2014$GEO %in% country_groups[[region]]] <- region
}

# Calculate changes
Gini$change <- Gini$`2019` - Gini$`2014`


# Plot for Gini change by region
ggplot(Gini, aes(x = GEO, y = change)) +
  geom_bar(stat = "identity") +
  facet_wrap(~Region, scales = "free_x") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Gini Coefficient Change (2014-2019) by Region",
       x = "Country",
       y = "Change in Gini Coefficient")

Gini$Category2014 <- cut(Gini$`2014`, 
                         breaks = c(0, 28, 32, 100),
                         labels = c("Low", "Medium", "High"))


Gini$Category2019 <- cut(Gini$`2019`, 
                         breaks = c(0, 28, 32, 100),
                         labels = c("Low", "Medium", "High"))

