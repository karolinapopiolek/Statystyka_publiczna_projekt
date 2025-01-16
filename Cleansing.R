library(readxl)
library(dplyr)
library(ggplot2)
library(patchwork)
library(ggcorrplot)


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
colnames(Gini) <- c("GEO", "2014", "2019")

# Remove rows with missing values or unnecessary data
Gini <- Gini %>% na.omit() %>%
  filter(!grepl("^Euro|Serb|Unit|Swit|North", `GEO`))


# Cleaning data for chronicDisease2014 and 2019 datasets
# Remove rows with missing values or unnecessary data
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
  left_join(country_codes, by = c("GEO" = "Country")) %>%
  mutate(GEO = Code) %>%
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

# Create a list of country groups
country_groups <- list(
  "Northern Europe" = c("DK", "EE", "FI", "SE", "IS", "NO", "LV", "LT"),
  "Western Europe" = c("BE", "DE", "IE", "FR", "LU", "NL", "AT"),
  "Southern Europe" = c("GR", "ES", "IT", "CY", "MT", "PT", "HR", "SI"),
  "Eastern Europe" = c("BG", "CZ", "HU", "PL", "RO", "SK", "TR")
)

# Calculating average disease values for 2014 and 2019 in each country
chronicDisease2014Avg <- chronicDisease2014 %>% 
  select(where(is.numeric)) %>% 
  rowMeans(na.rm = TRUE)

print(chronicDisease2014Avg)

chronicDisease2019Avg <- chronicDisease2019 %>% 
  select(where(is.numeric)) %>% 
  rowMeans(na.rm = TRUE)

print(chronicDisease2019Avg)

# Convert to a data frame for plotting
chronicDiseaseAvg <- data.frame(
  Year = c(rep("2014", 6), rep("2019", 6)),
  Variable = names(chronicDisease2014Avg),
  Value = c(chronicDisease2014Avg, chronicDisease2019Avg)
)

# Create a GEO column
GEO <- paste0(country_codes$Code)

# Convert 2 average chronicDisease value lists into a data frame
countryDiseaseAvg <- data.frame(
  GEO = GEO,
  `Y2014` = chronicDisease2014Avg,
  `Y2019` = chronicDisease2019Avg)

countryDiseaseAvg$Change <- countryDiseaseAvg$`Y2019` - countryDiseaseAvg$`Y2014`
 

# Plot for the Change in countryDiseaseAvg between by Region
ggplot(countryDiseaseAvg, aes(x = GEO, y = Change)) +
  geom_bar(stat = "identity") +
  facet_wrap(~Region, scales = "free_x") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Change in Chronic Disease Values (2014-2019) by Region",
       x = "Country",
       y = "Change in Value")


# Adding a Region column to countryDiseaseAvg dataset
countryDiseaseAvg$Region <- NA
for(region in names(country_groups)) {
  countryDiseaseAvg$Region[countryDiseaseAvg$GEO %in% country_groups[[region]]] <- region
}




# Plot for avg chronic disease change ## ale to jest bez sensu, bo skupiamy się na typie choroby zamiast kraju
ggplot(chronicDiseaseAvg, aes(x = Variable, y = Value, fill = Year)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Average Chronic Disease Values Comparison",
       x = "Variables", 
       y = "Average Value")



# Adding a region column to the datasets
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

# Function to determine transition type
get_transition <- function(Category2014, Category2019) {
  levels <- c("Low", "Medium", "High")
  if (Category2014 == Category2019) return("No Change")
  
  level2014 <- match(Category2014, levels)
  level2019 <- match(Category2019, levels)
  
  if (level2019 > level2014) return("Increase")
  return("Decrease")
}

# Add transition column
Gini <- Gini %>%
  mutate(Transition = mapply(get_transition, Category2014, Category2019))

# Create visualization
ggplot(Gini, aes(x = Region, fill = Transition)) +
  geom_bar() +
  theme_minimal() +
  coord_flip() +
  scale_fill_manual(values = c("Decrease" = "#FF9999", 
                               "No Change" = "#99CC99", 
                               "Increase" = "#9999FF")) +
  labs(title = "Gini Category Shifts by Region (2014-2019)",
       x = "Region",
       y = "Count of Countries") +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5))


print(chronicDisease2014[16,])
print(chronicDisease2019[16,])

# Gini Coefficient Distribution
par(mfrow = c(1, 2))

hist(Gini$`2014`, main = "Rozkład Giniego (2014)", xlab = "Rozkład współczynnika Giniego (2014)", col = "lightblue") 

hist(Gini$`2019`, main = "Rozkład Giniego (2019)", xlab = "Rozkład współczynnika Giniego (2019)", col = "lightgreen") 


# Correlation matrix for 2014
chronicDisease2014$`2014` <- Gini$`2014` # adding a supportive column

korelacja2014 <- cor(chronicDisease2014[, c("backPain", "Asthma", "Respiratory", "Hypertension", "Diabetes", "Depression", "2014")], 
                     use = "complete.obs") 
print(korelacja2014)

# Corrplot for 2014
ggcorrplot(korelacja2014, method = "circle", 
           type = "lower", lab = TRUE,
           lab_size = 3,
           colors = c("red", "white", "green"),
           title = "Macierz korelacji (2014)",
           ggtheme = theme_minimal())

# Correlation matrix for 2019
chronicDisease2019$`2019` <- Gini$`2019` # adding a supportive column

korelacja2019 <- cor(chronicDisease2019[, c("backPain", "Asthma", "Respiratory", "Hypertension", "Diabetes", "Depression", "2019")], 
                     use = "complete.obs") 
print(korelacja2019)

# Corrplot for 2019
ggcorrplot(korelacja2019, method = "circle", 
           type = "lower", lab = TRUE,
           lab_size = 3,
           colors = c("red", "white", "green"),
           title = "Macierz korelacji (2019)",
           ggtheme = theme_minimal())

# Uśredniona wartość korelacji między wsp. Giniego a występowaniem chorób w wybranych krajach europejskich jest bliska 0, co oznacza, że nie ma związku
# między wsp. Giniego a chorobami przewlekłymi w tych krajach.

# Występuje natomiast korelacja między rodzajami chorób – np Asthmą a Depresją albo problemami z kręgosłupem a chorobami układu oddechowego.


Gini$changePP <- Gini$change / Gini$`2014` * 100
countryDiseaseAvg$changePP <- countryDiseaseAvg$Change / countryDiseaseAvg$`Y2014` * 100

# Plot for the change of Gini coefficient by country in 2014-2019
ggplot(Gini, aes(x = reorder(GEO, changePP),  y = changePP)) +
  geom_bar(stat = "identity") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Change in Gini Coefficient (2014-2019) by Country",
       x = "Country",
       y = "Change in Gini Coefficient, p.p.")

# Plot for the change of chronicDiseaseAvg by country in 2014-2019
ggplot(countryDiseaseAvg, aes(x = reorder(GEO, changePP), y = changePP)) +
  geom_bar(stat = "identity") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Change in Chronic Disease Values (2014-2019) by Country",
       x = "Country",
       y = "Change in average percentage of people suffering chronic disease, p.p.")

# Avg14 and Avg19 for ranking purposes
Ranking <- data.frame(
  GEO = country_codes$Code,
  Avg14 = (Gini$`2014` + countryDiseaseAvg$`Y2014`)/2, # the lower the better
  Avg19 = (Gini$`2019` + countryDiseaseAvg$`Y2019`)/2, # the lower the better
  Change = (Ranking$Avg19 - Ranking$Avg14) / Ranking$Avg14 * 100
)

# Combined plot for Avg14 and Avg19
ggplot(Ranking, aes(x = GEO)) +
  geom_point(aes(y = Avg14), color = "black", size = 4) +
  geom_point(aes(y = Avg19), color = "red", size = 4) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Ranking of countries according to Gini coefficient and chronic disease among population",
       x = "Country",
       y = "Rank (lower is better)")

# Plot for the change in ranking
ggplot(Ranking, aes(x = GEO, y = Change, fill = Change > 0)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("darkgreen", "darkred")) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") +
  labs(title = "Change in ranking of countries according to Gini coefficient and chronic disease among population",
       x = "Country", 
       y = "Change in rank, p.p.")
