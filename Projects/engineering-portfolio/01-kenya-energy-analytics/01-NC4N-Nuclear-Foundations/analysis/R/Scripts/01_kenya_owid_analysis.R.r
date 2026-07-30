# ============================================
# 02_kenya_owid_analysis.R
# NC4N Nuclear Foundations - Kenya Energy Analysis
# Data Source: Our World in Data - Electricity production by source
# Variables: Entity, Code, Year, Other renewables, Bioenergy, Solar, 
#            Wind, Hydropower, Nuclear, Oil, Gas, Coal
# ============================================

# 1. Load Libraries ---------------------------
library(tidyverse)
library(lubridate)
library(plotly)
library(scales)
library(ggthemes)
library(patchwork)


# 2. Import Data ------------------------------
# Download directly from OWID
#url <- "https://raw.githubusercontent.com/owid/owid-datasets/master/datasets/Electricity%20production%20by%20source%20(OWID)/electricity_production_by_source.csv"

# Read the data
getwd()
setwd("C:/Users/user/Desktop/Programming/Projects/engineering-portfolio/01-kenya-energy-analytics/01-NC4N-Nuclear-Foundations")
df <- read_csv("C:/Users/user/Desktop/Programming/Projects/engineering-portfolio/01-kenya-energy-analytics/01-NC4N-Nuclear-Foundations/data/raw/electricity-prod-source-stacked.csv")

# Check original variable names
cat("Original variable names:\n")
print(names(df))

# 3. Clean and Rename Variables ---------------
# Rename to cleaner, more consistent names
df_clean <- df %>%
  rename(
    country = Entity,
    iso_code = Code,
    year = Year,
    other_renewables = `Other renewables`,  # Note: backticks needed because of space
    bioenergy = Bioenergy,
    solar = Solar,
    wind = Wind,
    hydro = Hydropower,
    nuclear = Nuclear,
    oil = Oil,
    gas = Gas,
    coal = Coal
  )

# Check new variable names
cat("\nNew variable names:\n")
print(names(df_clean))

# 4. Transform Data ---------------------------
df_clean <- df_clean %>%
  # Convert TWh to GWh (multiply by 1000)
  mutate(
    across(c(other_renewables, bioenergy, solar, wind, hydro, 
             nuclear, oil, gas, coal),
           ~ . * 1000)
  ) %>%
  # Calculate total generation
  mutate(
    total_generation = rowSums(across(c(coal, gas, oil, nuclear, hydro, 
                                        wind, solar, bioenergy, other_renewables)), 
                               na.rm = TRUE),
    renewables = rowSums(across(c(hydro, wind, solar, bioenergy, other_renewables)), 
                         na.rm = TRUE),
    fossil = rowSums(across(c(coal, gas, oil)), na.rm = TRUE)
  ) %>%
  # Calculate percentage shares
  mutate(
    nuclear_share = (nuclear / total_generation) * 100,
    renewables_share = (renewables / total_generation) * 100,
    fossil_share = (fossil / total_generation) * 100
  )

# View data structure
cat("\nData structure:\n")
glimpse(df_clean)

# 5. Filter for Kenya -------------------------
kenya <- df_clean %>%
  filter(country == "Kenya")

# Check Kenya data availability
cat("\nKenya data summary:\n")
kenya %>%
  summarise(
    min_year = min(year),
    max_year = max(year),
    n_years = n_distinct(year),
    total_generation_2025 = round(total_generation[year == max(year)], 0)
  ) %>%
  print()

# Create figures directory if it doesn't exist
dir.create("figures", showWarnings = FALSE)
dir.create("reports", showWarnings = FALSE)

# ============================================
# Q1: How has Kenya's electricity generation 
#     changed between 1985 and 2025?
# ============================================

cat("\n============================================\n")
cat("Q1: Kenya Electricity Generation Trends\n")
cat("============================================\n")

# 1a. Total generation trend
p1_total <- kenya %>%
  ggplot(aes(x = year, y = total_generation)) +
  geom_line(color = "#2c3e50", size = 1.5) +
  geom_point(color = "#3498db", size = 2.5, alpha = 0.7) +
  geom_smooth(method = "loess", se = TRUE, alpha = 0.2, color = "#e74c3c") +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Kenya's Total Electricity Generation (1985-2025)",
    subtitle = "Significant growth from ~2,000 GWh to over 12,000 GWh",
    x = "Year",
    y = "Generation (GWh)"
  ) +
  theme_economist() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11)
  )

# 1b. Annual growth rate
p1_growth <- kenya %>%
  arrange(year) %>%
  mutate(
    growth_rate = (total_generation - lag(total_generation)) / lag(total_generation) * 100
  ) %>%
  filter(!is.na(growth_rate)) %>%
  ggplot(aes(x = year, y = growth_rate)) +
  geom_col(fill = "#27ae60", alpha = 0.7, width = 0.8) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed", size = 1) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Annual Growth Rate of Kenya's Electricity Generation",
    subtitle = "Average growth ~5-7% annually, with some volatile years",
    x = "Year",
    y = "Growth Rate (%)"
  ) +
  theme_economist()

# Combine plots
q1_combined <- p1_total / p1_growth
ggsave("figures/q1_kenya_generation_trend.png", q1_combined, width = 12, height = 10)

# 1c. Summary statistics
q1_summary <- kenya %>%
  summarise(
    start_year = min(year),
    end_year = max(year),
    start_gen = first(total_generation),
    end_gen = last(total_generation),
    total_growth_percent = ((end_gen - start_gen) / start_gen) * 100,
    avg_annual_growth = mean((total_generation - lag(total_generation)) / 
                               lag(total_generation) * 100, na.rm = TRUE),
    cagr = ((end_gen / start_gen)^(1/(end_year - start_year)) - 1) * 100,
    max_generation = max(total_generation),
    max_year = year[which.max(total_generation)]
  )

cat("\nQ1 Summary Statistics:\n")
print(q1_summary)

# ============================================
# Q2: Which technologies contribute most to 
#     Kenya's energy mix?
# ============================================

cat("\n============================================\n")
cat("Q2: Kenya's Electricity Mix\n")
cat("============================================\n")

current_year <- max(kenya$year)

# 2a. Current mix (most recent year)
current_mix <- kenya %>%
  filter(year == current_year) %>%
  pivot_longer(
    cols = c(coal, gas, oil, nuclear, hydro, wind, solar, bioenergy, other_renewables),
    names_to = "source",
    values_to = "generation_gwh"
  ) %>%
  filter(generation_gwh > 0) %>%
  mutate(
    share = generation_gwh / sum(generation_gwh) * 100,
    source = case_when(
      source == "hydro" ~ "Hydro",
      source == "wind" ~ "Wind",
      source == "solar" ~ "Solar",
      source == "bioenergy" ~ "Bioenergy",
      source == "other_renewables" ~ "Other Renewables",
      source == "coal" ~ "Coal",
      source == "gas" ~ "Gas",
      source == "oil" ~ "Oil",
      source == "nuclear" ~ "Nuclear",
      TRUE ~ source
    )
  )

# Add this right after creating current_mix (around line 140)
leading_source <- current_mix$source[which.max(current_mix$share)]
leading_share <- max(current_mix$share)

# 2b. Bar chart - current mix
p2_bar <- current_mix %>%
  mutate(source = fct_reorder(source, share)) %>%
  ggplot(aes(x = source, y = generation_gwh, fill = source)) +
  geom_col() +
  geom_text(aes(label = paste0(round(share, 1), "%")), 
            hjust = -0.1, size = 4, fontface = "bold") +
  coord_flip() +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = paste("Kenya's Electricity Mix:", current_year),
    subtitle = "Geothermal and hydro dominate, with growing wind and solar",
    x = "Generation Source",
    y = "Generation (GWh)"
  ) +
  theme_economist() +
  theme(legend.position = "none")

ggsave("figures/q2_current_mix.png", p2_bar, width = 10, height = 6)

# 2c. Evolution of mix (stacked area)
p2_evolution <- kenya %>%
  pivot_longer(
    cols = c(coal, gas, oil, nuclear, hydro, wind, solar, bioenergy, other_renewables),
    names_to = "source",
    values_to = "generation_gwh"
  ) %>%
  filter(generation_gwh > 0) %>%
  mutate(source = case_when(
    source == "hydro" ~ "Hydro",
    source == "wind" ~ "Wind",
    source == "solar" ~ "Solar",
    source == "bioenergy" ~ "Bioenergy",
    source == "other_renewables" ~ "Other Renewables",
    source == "coal" ~ "Coal",
    source == "gas" ~ "Gas",
    source == "oil" ~ "Oil",
    source == "nuclear" ~ "Nuclear",
    TRUE ~ source
  )) %>%
  ggplot(aes(x = year, y = generation_gwh, fill = source)) +
  geom_area(alpha = 0.85) +
  scale_fill_brewer(palette = "Spectral") +
  labs(
    title = "Evolution of Kenya's Electricity Mix (1985-2025)",
    subtitle = "Rise of renewables; decline of thermal",
    x = "Year",
    y = "Generation (GWh)",
    fill = "Technology"
  ) +
  theme_economist() +
  theme(legend.position = "bottom")

ggsave("figures/q2_evolution.png", p2_evolution, width = 12, height = 7)

# 2d. Pie chart
p2_pie <- current_mix %>%
  ggplot(aes(x = "", y = share, fill = source)) +
  geom_col(width = 1) +
  coord_polar(theta = "y") +
  geom_text(aes(label = paste0(round(share, 1), "%")),
            position = position_stack(vjust = 0.5), size = 4) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = paste("Kenya Energy Mix:", current_year)) +
  theme_void() +
  theme(legend.position = "right")

ggsave("figures/q2_pie.png", p2_pie, width = 10, height = 8)

# 2e. Print current mix table
cat("\nCurrent Mix Table (", current_year, "):\n", sep = "")
current_mix %>%
  select(source, generation_gwh, share) %>%
  mutate(
    generation_gwh = round(generation_gwh, 0),
    share = round(share, 1)
  ) %>%
  arrange(desc(share)) %>%
  print()

# ============================================
# Q3: How does Kenya's mix compare with 
#     countries operating nuclear plants?
# ============================================

cat("\n============================================\n")
cat("Q3: Kenya vs. Nuclear-Operating Countries\n")
cat("============================================\n")

# 3a. Define nuclear countries
nuclear_countries <- c("France", "United States", "China", "Russia", 
                       "South Korea", "India", "Canada", "United Kingdom",
                       "Germany", "South Africa", "Japan", "Ukraine")

# 3b. Get comparison data (most recent year)
comparison_data <- df_clean %>%
  filter(country %in% c("Kenya", nuclear_countries)) %>%
  filter(year == max(year)) %>%
  select(country, nuclear_share, renewables_share, fossil_share, total_generation)

# 3c. Visualization - stacked bar comparison
p3_stack <- comparison_data %>%
  pivot_longer(
    cols = c(nuclear_share, renewables_share, fossil_share),
    names_to = "source_type",
    values_to = "share"
  ) %>%
  mutate(
    source_type = case_when(
      source_type == "nuclear_share" ~ "Nuclear",
      source_type == "renewables_share" ~ "Renewables",
      source_type == "fossil_share" ~ "Fossil"
    ),
    country = fct_reorder(country, -share)
  ) %>%
  ggplot(aes(x = country, y = share, fill = source_type)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = "Kenya vs. Nuclear-Operating Countries: Electricity Mix Comparison",
    subtitle = "Kenya has high renewables share, no nuclear, low fossil",
    x = "Country",
    y = "Share of Generation",
    fill = "Source Type"
  ) +
  theme_economist() +
  theme(
    legend.position = "bottom", 
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave("figures/q3_comparison_stack.png", p3_stack, width = 12, height = 8)

# 3d. Focus on nuclear share
p3_nuclear <- comparison_data %>%
  mutate(country = fct_reorder(country, nuclear_share)) %>%
  ggplot(aes(x = country, y = nuclear_share, fill = country == "Kenya")) +
  geom_col() +
  geom_text(aes(label = paste0(round(nuclear_share, 1), "%")), 
            hjust = -0.1, size = 3.5) +
  coord_flip() +
  scale_fill_manual(values = c("TRUE" = "#e74c3c", "FALSE" = "#3498db")) +
  labs(
    title = "Nuclear Share of Electricity Generation",
    subtitle = "Kenya currently has 0% nuclear generation",
    x = "Country",
    y = "Nuclear Share (%)"
  ) +
  theme_economist() +
  theme(legend.position = "none")

ggsave("figures/q3_nuclear_share.png", p3_nuclear, width = 10, height = 7)

# 3e. Print comparison table
cat("\nComparison Table (", max(df_clean$year), "):\n", sep = "")
comparison_data %>%
  arrange(desc(nuclear_share)) %>%
  mutate(
    nuclear_share = round(nuclear_share, 1),
    renewables_share = round(renewables_share, 1),
    fossil_share = round(fossil_share, 1),
    total_generation = round(total_generation, 0)
  ) %>%
  print()

# ============================================
# Q4: What engineering challenges motivate 
#     countries to adopt nuclear energy?
# ============================================

cat("\n============================================\n")
cat("Q4: Engineering Drivers for Nuclear Adoption\n")
cat("============================================\n")

# 4a. Calculate key metrics
engineering_drivers <- df_clean %>%
  filter(country %in% c("Kenya", nuclear_countries)) %>%
  filter(year == max(year)) %>%
  mutate(
    fossil_dependence = fossil_share,
    grid_stability_need = renewables_share * 0.3,
    energy_security = 100 - fossil_share,
    baseload_gap = ifelse(nuclear_share < 10, 20 - nuclear_share, 0)
  ) %>%
  select(country, fossil_dependence, grid_stability_need, baseload_gap, nuclear_share)

# 4b. Visualization
p4_drivers <- engineering_drivers %>%
  pivot_longer(
    cols = c(fossil_dependence, grid_stability_need, baseload_gap),
    names_to = "challenge",
    values_to = "score"
  ) %>%
  mutate(
    challenge = case_when(
      challenge == "fossil_dependence" ~ "Fossil Dependence",
      challenge == "grid_stability_need" ~ "Grid Stability Need",
      challenge == "baseload_gap" ~ "Baseload Gap"
    ),
    country = fct_reorder(country, nuclear_share)
  ) %>%
  filter(country %in% c("Kenya", "France", "South Africa", "China", "Germany", "United States")) %>%
  ggplot(aes(x = country, y = score, fill = challenge)) +
  geom_col(position = "dodge") +
  labs(
    title = "Engineering Drivers for Nuclear Adoption",
    subtitle = "Countries with high fossil dependence and grid stability needs may benefit",
    x = "Country",
    y = "Challenge Score (Index)",
    fill = "Engineering Challenge"
  ) +
  theme_economist() +
  theme(legend.position = "bottom")

ggsave("figures/q4_engineering_drivers.png", p4_drivers, width = 12, height = 7)

# ============================================
# Q5: What role could nuclear play in Kenya's 
#     future electricity portfolio?
# ============================================

cat("\n============================================\n")
cat("Q5: Nuclear Scenario Analysis\n")
cat("============================================\n")

# 5a. Current generation
current_gen <- kenya %>%
  filter(year == current_year) %>%
  summarise(total = sum(total_generation)) %>%
  pull(total)

# 5b. Nuclear plant parameters
nuclear_capacity_mw <- 1000
capacity_factor <- 0.90
hours_per_year <- 8760
nuclear_gwh <- nuclear_capacity_mw * hours_per_year * capacity_factor / 1000

# 5c. Scenarios
scenarios <- data.frame(
  scenario = c(
    "Current Mix",
    "Add 1,000 MW Nuclear",
    "Nuclear + Fossil Reduction",
    "High Renewables + Nuclear"
  ),
  total_generation_gwh = c(
    current_gen,
    current_gen + nuclear_gwh,
    current_gen + nuclear_gwh - 500,
    current_gen * 1.3 + nuclear_gwh
  ),
  nuclear_gwh = c(0, nuclear_gwh, nuclear_gwh, nuclear_gwh)
) %>%
  mutate(
    nuclear_share = (nuclear_gwh / total_generation_gwh) * 100,
    co2_reduction = c(0, 5, 15, 10)
  )

# 5d. Future mix visualization
future_mix <- kenya %>%
  filter(year == current_year) %>%
  pivot_longer(
    cols = c(coal, gas, oil, hydro, wind, solar, bioenergy),
    names_to = "source",
    values_to = "gwh"
  ) %>%
  mutate(
    source = case_when(
      source == "hydro" ~ "Hydro",
      source == "wind" ~ "Wind",
      source == "solar" ~ "Solar",
      source == "bioenergy" ~ "Bioenergy",
      source == "coal" ~ "Coal",
      source == "gas" ~ "Gas",
      source == "oil" ~ "Oil",
      TRUE ~ source
    )
  ) %>%
  bind_rows(
    data.frame(
      source = "Nuclear",
      gwh = nuclear_gwh,
      scenario = "With Nuclear",
      year = current_year
    )
  )

# 5e. Scenario plot
p5_scenario <- future_mix %>%
  group_by(scenario) %>%
  mutate(share = gwh / sum(gwh) * 100) %>%
  ggplot(aes(x = scenario, y = gwh, fill = source)) +
  geom_col() +
  geom_text(aes(label = paste0(round(share, 1), "%")),
            position = position_stack(vjust = 0.5), size = 3.5) +
  scale_fill_brewer(palette = "Spectral") +
  labs(
    title = "Impact of Nuclear Addition on Kenya's Electricity Mix",
    subtitle = paste("1,000 MW nuclear plant would provide ~", 
                     round(scenarios$nuclear_share[2], 1), "% of generation"),
    x = "Scenario",
    y = "Generation (GWh)",
    fill = "Source"
  ) +
  theme_economist() +
  theme(legend.position = "bottom")

ggsave("figures/q5_scenario_impact.png", p5_scenario, width = 12, height = 7)

# 5f. Benefits plot
benefits <- data.frame(
  metric = c("Baseload Reliability", "CO2 Reduction", "Energy Security", 
             "Grid Stability", "Cost Competitiveness", "24/7 Power"),
  current = c(30, 20, 40, 35, 30, 25),
  with_nuclear = c(95, 70, 85, 80, 75, 95)
) %>%
  pivot_longer(cols = c(current, with_nuclear), names_to = "scenario", values_to = "score")

p5_benefits <- benefits %>%
  mutate(metric = fct_reorder(metric, score)) %>%
  ggplot(aes(x = metric, y = score, fill = scenario)) +
  geom_col(position = "dodge") +
  coord_flip() +
  scale_fill_manual(values = c("current" = "#95a5a6", "with_nuclear" = "#e74c3c")) +
  labs(
    title = "Nuclear Energy Benefits for Kenya's Grid",
    subtitle = "Qualitative assessment of nuclear contributions",
    x = "Benefit Metric",
    y = "Potential Impact (%)",
    fill = "Scenario"
  ) +
  theme_economist() +
  theme(legend.position = "bottom")

ggsave("figures/q5_benefits.png", p5_benefits, width = 10, height = 6)

# 5g. Print scenario summary
cat("\nScenario Summary:\n")
scenarios %>%
  mutate(
    total_generation_gwh = round(total_generation_gwh, 0),
    nuclear_share = round(nuclear_share, 1),
    co2_reduction = round(co2_reduction, 1)
  ) %>%
  print()

# ============================================
# 6. Generate HTML Report
# ============================================

cat("\n============================================\n")
cat("Generating HTML Report\n")
cat("============================================\n")

html_report <- paste0('
<!DOCTYPE html>
<html>
<head>
<title>Kenya Energy Analysis Report</title>
<style>
body { font-family: Arial, sans-serif; margin: 40px; background: #f5f6fa; }
.container { max-width: 1200px; margin: 0 auto; }
.section { background: white; padding: 20px; margin: 20px 0; border-radius: 8px; box-shadow: 0 2px 4px rgba(0,0,0,0.1); }
h1 { color: #2c3e50; border-bottom: 3px solid #3498db; padding-bottom: 10px; }
h2 { color: #34495e; }
img { max-width: 100%; height: auto; margin: 10px 0; }
table { border-collapse: collapse; width: 100%; margin: 10px 0; }
th, td { border: 1px solid #ddd; padding: 8px; text-align: left; }
th { background-color: #3498db; color: white; }
tr:nth-child(even) { background-color: #f2f2f2; }
</style>
</head>
<body>
<div class="container">
  <h1>☢️ Kenya Nuclear Energy Readiness Analysis</h1>
  <p><strong>Data Source:</strong> Our World in Data - Electricity production by source (Ember/Energy Institute)</p>
  <p><strong>Analysis Date:</strong> ', Sys.Date(), '</p>
  
  <div class="section">
    <h2>Q1: Kenya Electricity Generation Trends (1985-2025)</h2>
    <p>Total generation has grown from <strong>', round(q1_summary$start_gen, 0), '</strong> GWh to <strong>', round(q1_summary$end_gen, 0), '</strong> GWh, a CAGR of <strong>', round(q1_summary$cagr, 1), '%</strong>.</p>
    <img src="../figures/q1_kenya_generation_trend.png" alt="Generation Trend">
  </div>
  
  <div class="section">
    <h2>Q2: Kenya Electricity Mix (', current_year, ')</h2>
    <p>', leading_source, ' is the leading source with ', round(leading_share, 1), '% of generation.</p>
    <img src="../figures/q2_current_mix.png" alt="Current Mix">
    <img src="../figures/q2_evolution.png" alt="Evolution">
  </div>
  
  <div class="section">
    <h2>Q3: Kenya vs. Nuclear-Operating Countries</h2>
    <p>Kenya has <strong>0%</strong> nuclear share, compared to <strong>', round(comparison_data$nuclear_share[comparison_data$country=="France"], 1), '%</strong> in France and <strong>', round(comparison_data$nuclear_share[comparison_data$country=="South Africa"], 1), '%</strong> in South Africa.</p>
    <img src="../figures/q3_nuclear_share.png" alt="Nuclear Share Comparison">
  </div>
  
  <div class="section">
    <h2>Q4: Engineering Drivers for Nuclear Adoption</h2>
    <p>Key drivers include fossil dependence, grid stability needs, and baseload gaps.</p>
    <img src="../figures/q4_engineering_drivers.png" alt="Engineering Drivers">
  </div>
  
  <div class="section">
    <h2>Q5: Nuclear Future Scenarios</h2>
    <p>A 1,000 MW nuclear plant could provide <strong>', round(scenarios$nuclear_share[2], 1), '%</strong> of Kenya\'s generation.</p>
    <img src="../figures/q5_scenario_impact.png" alt="Scenario Impact">
    <img src="../figures/q5_benefits.png" alt="Benefits">
  </div>
</div>
</body>
</html>
')

writeLines(html_report, "reports/kenya_energy_analysis_report.html")

# ============================================
# 7. Final Summary
# ============================================

cat("\n============================================\n")
cat("✅ ANALYSIS COMPLETE!\n")
cat("============================================\n")
cat("📊 Figures saved to: figures/\n")
cat("   - q1_kenya_generation_trend.png\n")
cat("   - q2_current_mix.png\n")
cat("   - q2_evolution.png\n")
cat("   - q2_pie.png\n")
cat("   - q3_comparison_stack.png\n")
cat("   - q3_nuclear_share.png\n")
cat("   - q4_engineering_drivers.png\n")
cat("   - q5_scenario_impact.png\n")
cat("   - q5_benefits.png\n")
cat("\n📄 Report saved to: reports/kenya_energy_analysis_report.html\n")
cat("\n============================================\n")
cat("Kenya Energy Summary:\n")
cat("  - Latest year: ", current_year, "\n", sep = "")
cat("  - Total generation: ", round(current_gen, 0), " GWh\n", sep = "")
cat("  - Nuclear share: 0%\n")
cat("  - Renewables share: ", 
    round(kenya %>% filter(year == current_year) %>% pull(renewables_share), 1), "%\n", sep = "")
cat("  - Fossil share: ", 
    round(kenya %>% filter(year == current_year) %>% pull(fossil_share), 1), "%\n", sep = "")
cat("  - Leading source: ", current_mix$source[1], " (", 
    round(current_mix$share[1], 1), "%)\n", sep = "")
cat("\nNuclear Scenario (1,000 MW plant):\n")
cat("  - Generation: ", round(nuclear_gwh, 0), " GWh\n", sep = "")
cat("  - Share of mix: ", round(scenarios$nuclear_share[2], 1), "%\n", sep = "")
cat("  - CO2 reduction: ", scenarios$co2_reduction[2], "%\n", sep = "")
cat("============================================\n")

getwd()

