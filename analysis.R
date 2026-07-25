# Financial inclusion and digital access, using World Bank data (2021)

library(jsonlite)
library(dplyr)
library(ggplot2)
dir.create("outputs", showWarnings = FALSE)

# Download one World Bank indicator and keep country name, code and value.
get_data <- function(indicator, variable_name) {
  url <- paste0("https://api.worldbank.org/v2/country/all/indicator/", indicator,
                "?date=2021&format=json&per_page=20000")
  raw_data <- fromJSON(url)[[2]]
  data.frame(country = raw_data$country$value,
             iso3c = raw_data$countryiso3code,
             value = raw_data$value) %>%
    rename(!!variable_name := value)
}

# Three indicators used in this study.
accounts <- get_data("FX.OWN.TOTL.ZS", "account_ownership")
internet <- get_data("IT.NET.USER.ZS", "internet_use")
gdp <- get_data("NY.GDP.PCAP.CD", "gdp_per_capita")

# World Bank income groups let us compare richer and poorer economies.
country_info <- fromJSON("https://api.worldbank.org/v2/country?format=json&per_page=400")[[2]]
country_info <- data.frame(iso3c = country_info$id,
                           income_group = country_info$incomeLevel$value,
                           region = country_info$region$value)

data <- accounts %>%
  inner_join(internet, by = c("country", "iso3c")) %>%
  inner_join(gdp, by = c("country", "iso3c")) %>%
  inner_join(country_info, by = "iso3c") %>%
  filter(region != "Aggregates", income_group != "Not classified") %>%
  na.omit() %>%
  mutate(income_band = ifelse(income_group %in% c("Low income", "Lower middle income"),
                              "Lower income", "Upper-middle/high income"))

cat("Number of countries analysed:", nrow(data), "\n\n")

# Do the two income bands have different average account ownership?
test <- t.test(account_ownership ~ income_band, data = data)
print(test)

# Simple model: association with internet use and GDP per capita.
model <- lm(account_ownership ~ internet_use + log(gdp_per_capita), data = data)
print(summary(model))

# Chart 1: internet access and account ownership.
plot1 <- ggplot(data, aes(internet_use, account_ownership, colour = income_band)) +
  geom_point(size = 2.5, alpha = 0.8) +
  geom_smooth(method = "lm", se = FALSE, colour = "black") +
  labs(title = "Internet use and financial inclusion",
       x = "Individuals using the internet (%)",
       y = "Adults with an account (%)",
       colour = "Income band") +
  theme_minimal()
print(plot1)
ggsave("outputs/internet_vs_account_ownership.png", plot1, width = 8, height = 5, dpi = 300)

# Chart 2: account ownership in the two income bands.
plot2 <- ggplot(data, aes(income_band, account_ownership, fill = income_band)) +
  geom_boxplot(show.legend = FALSE) +
  labs(title = "Account ownership is higher in richer economies",
       x = NULL, y = "Adults with an account (%)") +
  theme_minimal()
print(plot2)
ggsave("outputs/account_ownership_by_income_group.png", plot2, width = 8, height = 5, dpi = 300)

# Hypothesis:
# H0: Average account ownership is the same in lower-income and richer economies.
# H1: Average account ownership is different in lower-income and richer economies.
#
# Observation:
# Using World Bank data for 117 countries, lower-income economies have average
# account ownership of about 48%, while upper-middle/high-income economies have
# about 83%. The t-test gives p < 0.001, so I reject H0.
#
# The charts also show a positive relationship between internet use and account
# ownership. However, this is an association, not proof that internet access
# causes financial inclusion.
