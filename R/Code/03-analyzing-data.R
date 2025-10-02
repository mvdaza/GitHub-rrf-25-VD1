# Reproducible Research Fundamentals 
# 03. Data Analysis


# Libraries -----
library(haven)
library(dplyr)
library(modelsummary)
library(stargazer)
library(ggplot2)
library(tidyr)

# Load data -----
#household level data
data_path <- "C:/Users/wb618037/OneDrive - WBG/Desktop/DataWork/DataWork/Data"
hh_data   <- read_dta(file.path(data_path, "Final/TZA_CCT_analysis.dta"))

# secondary data 
secondary_data <- read_dta(file.path(data_path, "Final/TZA_amenity_analysis.dta")) %>%
    rename(district = adm2_en) %>% 
    mutate(district = as_factor(district))

# Exercise 1 and 2: Create graph of area by district -----

# Bar graph by treatment for all districts
# Ensure treatment is a factor for proper labeling
hh_data_plot <- hh_data %>%
    mutate(treatment = factor(treatment, labels = c("Control", "Treatment")), 
           district = as_factor(district))

# Create the bar plot
# Create the bar plot
library(ggplot2)

ggplot(hh_data_plot, aes(
  x = district,
  y = area_acre_w,
  fill = treatment
)) +
  geom_bar(stat = "summary", fun = "mean",
           position = position_dodge(width = 0.8), # barras más juntas
           width = 0.85) +                        # ancho mayor de cada barra
  stat_summary(
    fun.data = mean_cl_boot,   # error bars (bootstrap CI)
    geom = "errorbar",
    position = position_dodge(width = 0.8),
    width = 0.15,
    color = "black"
  ) +
  labs(
    title = "Average Cultivated Area by District and Treatment",
    x = "District",
    y = "Mean cultivated area (acres)",
    fill = "Treatment"
  ) +
  scale_fill_brewer(palette = "Set2") +
  theme_classic(base_size = 14) +
  theme(
    legend.position = "top",
    legend.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    axis.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

 ggsave(file.path("Outputs", "fig1.png"), width = 10, height = 6)
       
       
# Exercise 3: Create a density plot of non-food consumption -----
       
# Calculate mean non-food consumption for female and male-headed households
mean_female <- hh_data %>% 
   filter(female_head == 1) %>% 
   summarise(mean = mean(nonfood_cons_usd_w, na.rm = TRUE)) %>% 
   pull(mean)

mean_male <- hh_data %>% 
   filter(female_head == 0) %>% 
   summarise(mean = mean(nonfood_cons_usd_w, na.rm = TRUE)) %>% 
   pull(mean)

# Create the density plot
ggplot(hh_data, 
      aes(......)) +
   geom_density(......) +  # Density plot
   geom_vline(xintercept = ......, color = "purple", linetype = "dashed", size = 1) +  # Vertical line for female mean
   geom_vline(xintercept = ......, color = "grey", linetype = "dashed", size = 1) +  # Vertical line for male mean
   labs(title = "Distribution of Non-Food Consumption",
        x = "Non-food consumption value (USD)", 
        y = "Density",
        color = "Household Head:") +  # Custom labels
   theme_minimal() +
   ...... # Add other customization if needed

ggsave(file.path("Outputs", "fig2.png"), width = 10, height = 6)
       

# Exercise 4: Summary statistics ----

# Create summary statistics by district and export to CSV
library(modelsummary)

summary_table <- datasummary(
  (area_acre_w + food_cons_usd_w + nonfood_cons_usd_w + sick + read + days_sick) ~ as_factor(district) * (Mean + SD),
  data = hh_data,
  title = "Summary Statistics by District",
  output = file.path("Outputs", "summary_table.csv")
)



# Exercise 5: Balance table ----

balance_table <- datasummary_balance(
    (area_acre_w + food_cons_usd_w + nonfood_cons_usd_w + sick + read + days_sick) ~ treatment,
    data = hh_data,
    stars = TRUE,
    title = "Balance by Treatment Status",
    note = "Includes HHs with observations for baseline and endline",
    output = file.path("Outputs", "balance_table.csv")
)


# Exercise 6: Regressions ----

# Model 1: Food consumption regressed on treatment
model1 <- lm(food_cons_usd_w ~ treatment, data = hh_data)

# Model 2: Add controls (crop_damage, drought_flood)
model2 <- lm(food_cons_usd_w ~ treatment + crop_damage + drought_flood, data = hh_data)

# Model 3: Add FE by district
model3 <- lm(food_cons_usd_w ~ treatment + crop_damage + drought_flood + as_factor(district),
             data = hh_data)


library(stargazer)
library(sandwich)
library(lmtest)

# Robust SEs (HC1) for Models 1 and 2; Clustered by district for Model 3 (with FE)
se1 <- sqrt(diag(vcovHC(model1, type = "HC1")))
se2 <- sqrt(diag(vcovHC(model2, type = "HC1")))
se3 <- sqrt(diag(vcovCL(model3, cluster = hh_data$district, type = "HC1")))

# Create regression table using stargazer
stargazer(
    model1, model2, model3,
    se = list(se1, se2, se3),
    title = "Food Consumption Effects",
    column.labels = c("(1)", "(2)", "(3)"),
    keep = c("treatment", "crop_damage", "drought_flood"),
    covariate.labels = c("Treatment",
                         "Crop Damage",
                         "Drought/Flood"),
    dep.var.labels = c("Food Consumption (USD)"),
    dep.var.caption = "",
    add.lines = list(c("District Fixed Effects", "No", "No", "Yes")),
    header = FALSE,
    keep.stat = c("n", "adj.rsq"),
    notes = "Robust SEs in parentheses; Model (3) clustered by district.",
    notes.append = TRUE,
    out = file.path("Outputs","regression_table.tex")
)

# Exercise 7: Combining two plots ----

long_data <- secondary_data %>%
    ungroup() %>% 
    select(-c(n_hospital, n_clinic)) %>% 
    pivot_longer(cols = c(n_school, n_medical), names_to = "amenity", values_to = "count") %>%
    mutate(amenity = recode(amenity, n_school = "Number of Schools", n_medical = "Number of Medical Facilities"),
           in_sample = if_else(district %in% c("Kibaha", "Chamwino", "Bagamoyo"), "In Sample", "Not in Sample"))

# Create the facet-wrapped bar plot
ggplot(long_data,
       aes(......)) +
    geom_bar(......) +
    coord_flip() +
    facet_wrap(......) +  # Create facets for schools and medical facilities
    labs(title = "Access to Amenities: By Districts",
         x = "District", y = NULL, fill = "Districts:") +
    scale_fill_brewer(palette="PuRd") +
    theme_minimal() +
    ...... # Add other customization if needed

ggsave(file.path("Outputs", "fig3.png"), width = 10, height = 6)
