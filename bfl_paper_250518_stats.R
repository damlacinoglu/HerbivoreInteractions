#######################################################
#Interactions with insect herbivores impact the plant biodiversity-ecosystem functioning relationship
#Damla Cinoglu | March 6, 2025
#######################################################
library(plyr)
library(readr)
library(data.table)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(ggpubr)
library(gridExtra)
library(devtools)
library(plotrix)
library(lme4)
library(readxl)
library(lmerTest)
library(openxlsx)
library(cowplot)
library(viridis)
library(emmeans)
library(multcompView)
library(MuMIn)
library(glmmTMB)
library(FactoMineR)  
library(factoextra)  
library(performance)
library(corrplot)

setwd("~/Desktop/final_bfl_data")
gen_bio = read.xlsx("BIOMASS_2022.xlsx")
gen_bio_24 = read.xlsx("BIOMASS_2024.xlsx")
conti_pd = read.csv(file = "new_pds230515-2.csv",sep=",")
#merged = read.table(file = "merged*.txt", sep = ",")
gen_cover = read.xlsx("COVER_END_2022.xlsx")
gen_cover_23 =  read.xlsx("COVER_END_2023.xlsx")
gen_cover_mid_23 = read.xlsx("percent cover sheet _ mid season - 2023.xlsx")
gen_cover_mid_22 = read.xlsx("percent cover sheet _ mid season - 2022.xlsx")
gen_cover_24 = read.xlsx("BFL_cover_endseason_2024_raw.xlsx")
gen_cover_mid_2024 = read.xlsx("BFL_cover_midseason_2024.xlsx")

herbivory = read.csv(file = "HERBIVORY_2023.csv",sep=",")

#correct:
total_chem = read.table("trait_herbivory_250306.txt", sep=",")
total_trait = read.table("total_trait_250306.txt", sep=",")
merged = read.table("MERGED_231122")
sticky_trap = read.csv("STICKY_TRAP_2022.csv",sep=",",header=TRUE)
continuous_pd = read.csv("CONTINOUS_PD_2023.csv",sep=",",header=TRUE)

sla_23 = read.table("sla_trait_250306.txt")
trichome_23 = read.table("trichome_trait_250306.txt")
toughness_23 = read.table("toughness_trait_250306.txt")


ind_chem2 = read.table("chem_trait_250306.txt")

ind_chem = read.table("compiled_alpina_MAY16_BFL_ALL.txt")
ind_chem = alpinaDiv

#######################################################
#PROCESS THE INCOMING TRAIT DATA MORE
#######################################################

add_characteristic = function(data) {
	#add block
data = cbind(data, block = rep(NA, nrow(data)))
for(i in 1:nrow(data)){ 
  plot = data[i,"plot"] 
if(grepl("A",plot)) {data[i,"block"] = "A"}
  if(grepl("B",plot)) {data[i,"block"] = "B"}
  if(grepl("C",plot)) {data[i,"block"] = "C"}
}

#add watering 
data = data %>%
  mutate(watering = ifelse(grepl("W", plot), "W", "No W"))

#add net
data = data %>%
  mutate(net = ifelse(grepl("net", treatment), "net", "No net"))

#add watering pair
data = data %>%
  mutate(water_pair = gsub("-W","", data $plot))

return(data)
}

#deal with sla, trichome, total_trait
herbivory = add_characteristic(herbivory)
herbivory$veg.height = as.numeric(herbivory$veg.height )

sla_23 = add_characteristic(sla_23)

trichome_23 = add_characteristic(trichome_23)
trichome_23 = cbind(trichome_23, total_hair = rep(NA, nrow(trichome_23)))
trichome_23 $total_hair = trichome_23 $final_hair_front + trichome_23 $final_hair_back

total_trait = add_characteristic(total_trait)
total_trait = cbind(total_trait, total_hair = rep(NA, nrow(total_trait)))
total_trait $total_hair = total_trait $final_hair_front + total_trait $final_hair_back

#deal with ind_chem
ind_chem = cbind(ind_chem, block = rep(NA, nrow(ind_chem)))
for(i in 1:nrow(ind_chem)){ 
  plot = ind_chem[i,"plot"] 
if(grepl("A",plot)) {ind_chem[i,"block"] = "A"}
  if(grepl("B",plot)) {ind_chem[i,"block"] = "B"}
  if(grepl("C",plot)) {ind_chem[i,"block"] = "C"}
}
ind_chem = cbind(ind_chem, richness = rep(NA, nrow(ind_chem)))
for(i in 1:nrow(ind_chem)){ 
  plot = ind_chem[i,"plot"] 
if(grepl("MON",plot)) {ind_chem[i,"richness"] = 1} else {
	ind_chem[i,"richness"] = 12
}}
#add net
ind_chem = ind_chem %>%
  mutate(net2 = ifelse(grepl("NET", net), "net", "No net"))
ind_chem = ind_chem %>%
  mutate(water = ifelse(grepl("W", plot), "WATER", "CONTROL"))
#ind_chem$plot2 <- ifelse(grepl("-W", ind_chem$individual),
 #                        paste0(ind_chem$plot, "-W"),
  #                       ind_chem$plot)
 #ind_chem = ind_chem %>%
 # mutate(water_pair = gsub("-W","", ind_chem $plot))

ind_chem = ind_chem %>%
  mutate(water_pair = gsub("W","", ind_chem $plot))


#deal with toughness
#add watering 
toughness_23 = toughness_23 %>%
  mutate(watering = ifelse(grepl("W", indvidual), "W", "No W"))
toughness_23 = toughness_23 %>%
  mutate(net = ifelse(grepl("net", treatment), "net", "No net"))
toughness_23 = cbind(toughness_23, block = rep(NA, nrow(toughness_23)))
toughness_23$block <- sapply(toughness_23$ind, function(x) {
  if (grepl("^mon", x)) {
    return(toupper(substr(x, 4, 4)))  # Extract the letter after "mon" and capitalize
  } else if (grepl("^12", x)) {
    return(toupper(substr(x, 3, 3)))  # Extract the letter after "12" and capitalize
  } else {
    return(NA)  # Assign NA if the format is unexpected
  }
}
toughness_23 <- toughness_23 %>%
  mutate(block = case_when(
    grepl("^mon", ind) ~ toupper(substr(ind, 4, 4)),  # Extract and capitalize letter after "mon"
    grepl("^12", ind)  ~ toupper(substr(ind, 3, 3)),  # Extract and capitalize letter after "12"
    TRUE ~ NA_character_  # Assign NA if the format is unexpected
  ))
toughness_23$block <- as.character(toughness_23$block)
toughness_23 = cbind(toughness_23, plot = rep(NA, nrow(toughness_23)))
for(i in 1:nrow(toughness_23)) {
	if(toughness_23[i,"watering"] == "W") {toughness_23[i,"plot"] = paste0(toughness_23[i,"ind"],"-W")} else{
	toughness_23[i,"plot"] = toughness_23[i,"ind"]	
	}
}

#######################################################
#Sample sizes for herbivory and height
#######################################################
sample_size = NULL
for(i in unique(herbivory[,"species"])) {
	dat = herbivory[herbivory[,"species"] == i,]
	col_1 = nrow(dat[dat[,"richness"] == 12 & dat[,"treatment"] == "net" & !is.na(dat[,"herbivory.all"]), ])
	col_2 = nrow(dat[dat[,"richness"] == 12 & dat[,"treatment"] == "control" & !is.na(dat[,"herbivory.all"]), ])
	col_3 = nrow(dat[dat[,"richness"] == 12 & dat[,"treatment"] == "half" & !is.na(dat[,"herbivory.all"]), ])
	col_4 = nrow(dat[dat[,"richness"] == 12 & dat[,"treatment"] == "watered" & !is.na(dat[,"herbivory.all"]), ])
	col_5 = nrow(dat[dat[,"richness"] == 1 & dat[,"treatment"] == "control" & !is.na(dat[,"herbivory.all"]), ])
	sample_size = rbind(sample_size, c(i,col_1,col_2,col_3,col_4,col_5))
}

sample_size = NULL
for(i in unique(herbivory[,"species"])) {
	dat = herbivory[herbivory[,"species"] == i,]
	col_1 = nrow(dat[dat[,"richness"] == 12 & dat[,"treatment"] == "net" & !is.na(dat[,"veg.height"]), ])
	col_2 = nrow(dat[dat[,"richness"] == 12 & dat[,"treatment"] == "control" & !is.na(dat[,"veg.height"]), ])
	col_3 = nrow(dat[dat[,"richness"] == 12 & dat[,"treatment"] == "half" & !is.na(dat[,"veg.height"]), ])
	col_4 = nrow(dat[dat[,"richness"] == 12 & dat[,"treatment"] == "watered" & !is.na(dat[,"veg.height"]), ])
	col_5 = nrow(dat[dat[,"richness"] == 1 & dat[,"treatment"] == "control" & !is.na(dat[,"veg.height"]), ])
	sample_size = rbind(sample_size, c(i,col_1,col_2,col_3,col_4,col_5))
}

sample_size[order(sample_size[, 1]), ]

#######################################################
#TABLE 1
#######################################################
#QUESTION 2:
#Are plant traits correlated with herbivory? Only outside, 12sp
#All the traits in one model, do traits predict herb
#add leaf C:N
merged = cbind(merged, c_N = merged$percC/ merged$percN)
merged[merged[,"treatment"] == "C", "treatment"] = "control"
merged[merged[,"treatment"] == "W", "treatment"] = "watered"

#add leaf area to this sheet:
total_chem = cbind(total_chem, leaf_area = rep(NA, nrow(total_chem)))
for(i in 1:nrow(total_chem)) {
	sp = total_chem[i,"sp"]
	treatment = total_chem[i,"treatment"]
	dat = sla_23[sla_23[,"species"] == sp & sla_23[,"treatment"] == treatment ,"area_2"]
	total_chem[i,"leaf_area"] = mean(dat, na.rm = T)
}

unique(herbivory$treatment)
unique(total_chem$treatment)
total_chem$treatment = gsub("_1", "",total_chem$treatment)
total_chem$treatment = gsub("_12", "",total_chem$treatment)
total_chem$treatment = gsub("_2", "",total_chem$treatment)
total_chem$treatment = gsub("2", "",total_chem$treatment)

herbivory = cbind(herbivory, secondary_metabolome = rep(NA,nrow(herbivory)),total_metabolome = rep(NA,nrow(herbivory)),hill_div = rep(NA,nrow(herbivory)), alkaloid = rep(NA,nrow(herbivory)), 
terp= rep(NA,nrow(herbivory)), polyp= rep(NA,nrow(herbivory)), 
lma= rep(NA,nrow(herbivory)), mean_tough_area= rep(NA,nrow(herbivory)), final_hair_front= rep(NA,nrow(herbivory)),final_hair_back= rep(NA,nrow(herbivory)), 
veg_height = rep(NA,nrow(herbivory)),
leaf_n_weight = rep(NA,nrow(herbivory)),
c_n = rep(NA,nrow(herbivory)), leaf_area = rep(NA,nrow(herbivory)))

for(i in 1:nrow(herbivory)) {
	rich = herbivory[i,"richness"]
    treat = herbivory[i,"treatment"]
    sp = herbivory[i,"species"]
    if(rich %in% c(1,12)) {
    	
    	herbivory[i, "secondary_metabolome"] = total_chem[total_chem[,"sp"] == sp & 
total_chem[,"richness"] == rich &
total_chem[,"treatment"] == treat, "secondary_metabolome"]
  	herbivory[i, "total_metabolome"] = total_chem[total_chem[,"sp"] == sp & 
total_chem[,"richness"] == rich &
total_chem[,"treatment"] == treat, "total_metabolome"]
  	herbivory[i, "hill_div"] = total_chem[total_chem[,"sp"] == sp & 
total_chem[,"richness"] == rich &
total_chem[,"treatment"] == treat, "functional_hill_diversity"]
  	herbivory[i, "alkaloid"] = total_chem[total_chem[,"sp"] == sp & 
total_chem[,"richness"] == rich &
total_chem[,"treatment"] == treat, "alkaloid"]
  	herbivory[i, "terp"] = total_chem[total_chem[,"sp"] == sp & 
total_chem[,"richness"] == rich &
total_chem[,"treatment"] == treat, "terpenoid"]
  	herbivory[i, "polyp"] = total_chem[total_chem[,"sp"] == sp & 
total_chem[,"richness"] == rich &
total_chem[,"treatment"] == treat, "polyphenols"]

	herbivory[i, "lma"] = total_chem[total_chem[,"sp"] == sp & 
total_chem[,"richness"] == rich &
total_chem[,"treatment"] == treat, "lma"]
herbivory[i, "mean_tough_area"] = total_chem[total_chem[,"sp"] == sp & 
total_chem[,"richness"] == rich &
total_chem[,"treatment"] == treat, "mean_tough_area"]
herbivory[i, "final_hair_front"] = total_chem[total_chem[,"sp"] == sp & 
total_chem[,"richness"] == rich &
total_chem[,"treatment"] == treat, "final_hair_front"]
herbivory[i, "final_hair_back"] = total_chem[total_chem[,"sp"] == sp & 
total_chem[,"richness"] == rich &
total_chem[,"treatment"] == treat, "final_hair_back"]
herbivory[i, "veg_height"] = total_chem[total_chem[,"sp"] == sp & 
total_chem[,"richness"] == rich &
total_chem[,"treatment"] == treat, "veg_height"]
herbivory[i, "leaf_area"] = total_chem[total_chem[,"sp"] == sp & 
total_chem[,"richness"] == rich &
total_chem[,"treatment"] == treat, "leaf_area"]


herbivory[i, "leaf_n_weight"] = mean(merged[merged[,"species"] == sp & 
merged[,"richness"] == rich &
merged[,"treatment"] == treat, "percN"], na.rm = T)
herbivory[i, "c_n"] = mean(merged[merged[,"species"] == sp & 
merged[,"richness"] == rich &
merged[,"treatment"] == treat, "c_N"], na.rm = T)
    }
}

herbivory_12 = herbivory[herbivory[,"treatment"] == "control",]
herbivory_12 = herbivory_12[herbivory_12[,"richness"] == 12,]

herbivory_12$secondary_metabolome = as.numeric(herbivory_12$secondary_metabolome )
herbivory_12$veg_height = as.numeric(herbivory_12$veg_height )
herbivory_12$lma = as.numeric(herbivory_12$lma )
herbivory_12$mean_tough = as.numeric(herbivory_12$mean_tough )
herbivory_12$final_hair_front = as.numeric(herbivory_12$final_hair_front )
herbivory_12$final_hair_back = as.numeric(herbivory_12$final_hair_back )
herbivory_12$leaf_n_weight = as.numeric(herbivory_12$leaf_n_weight )
herbivory_12$c_N = as.numeric(herbivory_12$c_N )

herbivory_12 = cbind(herbivory_12, total_hair = rep(NA, nrow(herbivory_12)))
herbivory_12$total_hair = herbivory_12$final_hair_back + herbivory_12$final_hair_front

herbivory_12[is.na(herbivory_12[,"mean_tough"]),"species"]

#EDITED HERBIVORY 12 :
herbivory_12 <- herbivory_12[ , !grepl("^(secondary_metabolome|total_metabolome|hill_div|alkaloid|terp|polyp)", names(herbivory_12))]
herbivory_12 = cbind(herbivory_12, secondary_metabolome = rep(NA, nrow(herbivory_12)),
total_metabolome = rep(NA, nrow(herbivory_12)),
hill_div = rep(NA, nrow(herbivory_12)),
alkaloid = rep(NA, nrow(herbivory_12)),
terp = rep(NA, nrow(herbivory_12)),
polyp = rep(NA, nrow(herbivory_12))
)
for(i in 1:nrow(herbivory_12)) {
	sp = herbivory_12[i,"species"]
	water = herbivory_12[i,"watering"]
	dat = ind_chem[ind_chem[,"species"] == sp & ind_chem[,"richness"] == 12 &  ind_chem[,"net"] == "OPEN",]
	if(water == "No W") {water2 = "CONTROL"} else {water2 = "WATER"}
dat = dat[dat[,"water"] == water2, ]
	herbivory_12[i,"secondary_metabolome"] = mean(dat[,"secondary_metabolome"], na.rm = T)
	herbivory_12[i,"total_metabolome"] =mean(dat[,"total_metabolome"], na.rm = T)
	herbivory_12[i,"hill_div"] =mean(dat[,"FuncHillDiv"], na.rm = T)
	herbivory_12[i,"alkaloid"] =mean(dat[,"alkaloid"], na.rm = T)
	herbivory_12[i,"terp"] =mean(dat[,"terpenoid"], na.rm = T)
	herbivory_12[i,"polyp"] =mean(dat[,"polyphenols"], na.rm = T)
}

#TOUGHNESS DATA DOESN'T EXIST FOR 4 SPECIES: ARPU, BOGR, IPRU, DEIL

model_hurdle2 = glmmTMB(
  herbivory.all ~ scale(log(as.numeric(veg_height))) + 
                  scale(log(as.numeric(lma))) + 
                  scale(log(as.numeric(hill_div))) +  
                  scale(log(as.numeric(total_hair))) + 
                  scale(log(as.numeric(c_n))) + 
                  scale(log(as.numeric(leaf_area)))
                   + (1|species) + (1|block) + (1|block:plot) ,  #family = nbinom2,  
   family = tweedie(link = "log"),  # Gamma for positive values, binomial for zeros
  data = herbivory_12
)
summary(model_hurdle2)
performance::r2(model_hurdle2, tolerance = 0)

#######################################################
#FIGURE S3
#######################################################

# Define custom trait names
trait_names <- c(
  "secondary_metabolome" = expression("SMR"),
  "veg_height" = expression("Height"),
   "leaf_area" = "Leaf Area",
  "lma" = "LMA",
  "mean_tough" = expression("Toughness"),
  "hill_div" = expression("FHDiversity"),
  "total_hair" = expression("Trichomes"),
  "c_n" = "Leaf C:N",
  "total_metabolome" = expression("TMR"),
  "alkaloid" = "Alkaloids",
  "polyp" = "Polyphenols",
  "leaf_n_weight" = expression("Leaf N")
)

# Compute correlation matrix
cor_matrix <- cor(herbivory_12[, names(trait_names)], use = "pairwise.complete.obs")
# Replace column/row names in the plot
rownames(cor_matrix) <- trait_names[rownames(cor_matrix)]
colnames(cor_matrix) <- trait_names[colnames(cor_matrix)]

# Plot correlation matrix with customization
plot_1 = ggcorrplot(
  cor_matrix
  type = "full",
  method = "circle",        # Use circle method
  type = "upper",           # Show upper triangle
  lab = TRUE,               # Add correlation coefficients
  lab_col = "black",        # Label color
  lab_size = 3.5,           # Size of coefficients
  tl.col = "black",         # Trait label color
  tl.srt = 45               # Rotate trait labels
)

# Define custom trait names
trait_names <- c(
  #"secondary_metabolome" = expression("SMR"),
  "veg_height" = expression("Height"),
  "lma" = "LMA",
   "leaf_area" = "Leaf Area",
  "hill_div" = expression("FHDiversity"),
  "total_hair" = expression("Trichomes"),
  "c_n" = "Leaf C:N"
)

# Compute correlation matrix
cor_matrix <- cor(herbivory_12[, names(trait_names)], use = "pairwise.complete.obs")

# Rename matrix rows and columns for custom labels
rownames(cor_matrix) <- trait_names[rownames(cor_matrix)]
colnames(cor_matrix) <- trait_names[colnames(cor_matrix)]

# Plot correlation matrix with customization
plot_2 = ggcorrplot(
  cor_matrix,
  type = "full",
  method = "circle",        # Use circle method
  lab = TRUE,               # Add correlation coefficients
  lab_col = "black",        # Label color
  lab_size = 3.5,           # Size of coefficients
  tl.col = "black",         # Trait label color
  tl.srt = 45               # Rotate trait labels
)

combined_plot <- plot_grid(
  plot_1, plot_2,
  labels = c("A", "B"),
  label_size = 14,
  ncol = 2
)
ggsave("/Users/damlacinoglu/Desktop/final_bfl_data/figures/Figure_S3.png", combined_plot, width = 14, height = 6, dpi = 300)

#######################################################
#FIGURE S6
#######################################################
# Pivot data into long format
herbivory_long <- herbivory_12 %>%
  tidyr::pivot_longer(cols = c(veg_height, lma, leaf_area, mean_tough, total_hair, leaf_n_weight, c_n, 
                               total_metabolome, secondary_metabolome, hill_div, alkaloid, 
                               terp, polyp),
                      names_to = "predictor",
                      values_to = "value")

# Define predictors and their labels
predictors <- c("veg_height", "lma", "leaf_area","mean_tough", "total_hair", "leaf_n_weight", "c_n", 
                "total_metabolome", "secondary_metabolome", "hill_div", "alkaloid", "terp", "polyp")

custom_labels <- c("Vegetation Height", "Leaf Mass per unit Area","Leaf Area", "Mean Toughness", "Trichome Density", 
                   "Leaf N", "Leaf C:N", "Total Metabolite Richness", "Secondary Metabolite Richness", 
                   "Functional Hill Diversity", "Alkaloid Richness", "Terpenoid Richness", 
                   "Shikimate and Phenyl. Richness")

plot_list <- list()

get_r2_nakagawa <- function(model) {
  # Get variance components
  var_fixed <- var(predict(model, type = "link", re.form = NA))  # Variance from fixed effects only
  re_var <- sum(unlist(VarCorr(model)$cond))                     # Sum of random effects variances
  res_var <- sigma(model)^2                                      # Residual variance

  # Marginal R²: fixed effects only
  r2_marginal <- var_fixed / (var_fixed + re_var + res_var)

  # Conditional R²: fixed + random effects
  r2_conditional <- (var_fixed + re_var) / (var_fixed + re_var + res_var)

  return(list(R2_marginal = r2_marginal, R2_conditional = r2_conditional))
}


for (i in 1:length(predictors)) {
  # Subset data for the predictor
  data_subset <- herbivory_long %>%
    filter(predictor == predictors[i] & herbivory.all < 20)

  if (nrow(data_subset) < 10) {
    plot_list[[i]] <- ggplot() + ggtitle("Not enough data")
    next
  }

  # Fit glmmTMB model
  model <- 
    glmmTMB(herbivory.all ~ scale(log(as.numeric(value))) + (1 | species), 
           # ziformula = ~ scale(log(as.numeric(value))) + (1 | species) , 
            family = tweedie(link = "log"), data = data_subset)

  if (is.null(model)) {
    plot_list[[i]] <- ggplot() + ggtitle("Model failed to fit")
    next
  }
  # Extract p-value safely
  model_summary <- summary(model)
  p_value <- ifelse("scale(log(as.numeric(value)))" %in% rownames(model_summary$coefficients$cond),
                    model_summary$coefficients$cond["scale(log(as.numeric(value)))", "Pr(>|z|)"], NA)

  # Extract R² safely
  r2_values <- get_r2_nakagawa(model)
  r2_conditional <- if (!is.null(r2_values)) round(as.numeric(r2_values["R2_conditional"]), 3) else NA

  # Generate predicted values with confidence intervals
  range_value <- range(data_subset$value, na.rm = TRUE)
new_data <- data.frame(value = seq(range_value[1], range_value[2], length.out = 100))

   # Predict on the link scale and get standard errors
pred_values <- predict(model, newdata = new_data, type = "response", se.fit = TRUE, re.form = NA)

# Store predicted fit and SE directly (no back-transforming)
new_data$fit <- pred_values$fit
new_data$se <- pred_values$se.fit
new_data$lower <- new_data$fit - 1.96 * new_data$se
new_data$upper <- new_data$fit + 1.96 * new_data$se

  # Generate the plot
plot_list[[i]] <- ggplot(data_subset, aes(x = value, y = herbivory.all)) + 
  geom_jitter(size = 1, color = "grey") +               
  geom_ribbon(data = new_data, aes(x = value, ymin = lower, ymax = upper), alpha = 0.2, fill = "blue", inherit.aes = FALSE) + 
  geom_line(data = new_data, aes(x = value, y = fit), color = "black", inherit.aes = FALSE) +  
  ylab("Total Herbivory per Individual (%)") +
  xlab(custom_labels[i]) + 
  theme_bw(base_size = 12) + 
  theme(legend.position = "none", axis.text.x = element_text(size = 8)) +
  annotate("text", x = min(data_subset$value, na.rm = TRUE), 
           y = max(data_subset$herbivory.all, na.rm = TRUE) * 0.9,
           label = paste0("p = ", signif(p_value, 3), "\nR² = ", r2_conditional), 
           hjust = 0, size = 4)
}

# Remove NULL plots
plot_list <- plot_list[!sapply(plot_list, is.null)]

# Combine plots if enough valid plots exist
if (length(plot_list) > 0) {
  final_plot <- plot_grid(plotlist = plot_list, 
                          labels = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M")[1:length(plot_list)], 
                          label_size = 14,                 
                          nrow = 2)
  # Print the final plot
  print(final_plot)
} else {
  message("No valid plots generated.")
}

ggsave("/Users/damlacinoglu/Desktop/final_bfl_data/figures/trait_herbivory_plot.png", final_plot, width = 17, height = 8, dpi = 300)

#######################################################
#FIGURE 4
#######################################################
#Which traits are increasing with the treatments? Only outside

# Function to extract slopes and standard errors dynamically
extract_slope_se <- function(model, possible_terms) {
  summary_model <- summary(model)
  term <- intersect(possible_terms, rownames(summary_model$coefficients))
  if (length(term) > 0) {
    slope <- summary_model$coefficients[term, "Estimate"]
    se <- summary_model$coefficients[term, "Std. Error"]
       p <- summary_model$coefficients[term, "Pr(>|t|)"]
  } else {
    slope <- NA
    se <- NA
           p <- NA
  }
  return(list(slope = slope, se = se, p = p))
}

trichome_23 <- trichome_23 %>%  # Keep only relevant treatments
  filter(!is.na(total_hair), is.finite(total_hair))  # Remove NA and Inf values

for(i in 1:nrow(herbivory)) {
	if(herbivory[i,"treatment"] == "control" | herbivory[i,"treatment"] == "watered" ) {
		if(herbivory[i,"watering"] == "No W" & herbivory[i,"richness"] == 1) {
    herbivory[i,"treatment"] = "control_1"  }
  if(herbivory[i,"watering"] == "No W" & herbivory[i,"richness"] == 12) {
    herbivory[i,"treatment"] = "control_12"
  } 
  if(herbivory[i,"watering"] == "W" & herbivory[i,"richness"] == 12) {
    herbivory[i,"treatment"] = "watered_12"
  } 
  if(herbivory[i,"watering"] == "W" & herbivory[i,"richness"] == 1) {
    herbivory[i,"treatment"] = "watered_1"  }	
	}
  }
unique(herbivory$treatment)
unique(herbivory$watering)
herbivory[herbivory[,"treatment"] == "watered",]

#ADD TREATMENTS TO INDCHEM:
unique(ind_chem2[,"treatment"])

ind_chem = ind_chem[!is.na(ind_chem[ ,"net"]),]

for(i in 1:nrow(ind_chem)) {
if(ind_chem[i,"water"] == "CONTROL" & ind_chem[i,"richness"] == 1) {
    ind_chem[i,"treatment"] = "control_1"  }
  if(ind_chem[i,"water"] == "CONTROL" & ind_chem[i,"richness"] == 12) {
    ind_chem[i,"treatment"] = "control_12"
  } 
  if(ind_chem[i,"water"] == "WATER" & ind_chem[i,"richness"] == 12) {
    ind_chem[i,"treatment"] = "watered_12"
  } 
  if(ind_chem[i,"water"] == "WATER" & ind_chem[i,"richness"] == 1) {
    ind_chem[i,"treatment"] = "watered_1"  }	
    if(ind_chem[i,"net"] == "NET" ) {
    ind_chem[i,"treatment"] = "net"  }
    	if(ind_chem[i,"net"] == "HALF" ) {
    ind_chem[i,"treatment"] = "half"  }
  }
  
ind_chem$water[ind_chem$water == "WATER"] <- "W"
  

# Define the models
richness_models <- list(
  lma_model = lmer(log(lma) ~ richness + (1|block) + (1|block:plot) + (1| species) , data = sla_23[sla_23[,"treatment"] %in% c("control_1","control_12"),]),
    area_model = lmer(log(area_2) ~ richness + (1|block)+ (1|block:plot) + (1| species) , data = sla_23[sla_23[,"treatment"] %in% c("control_1","control_12"),]),
  trichome_model = lmer(log(total_hair+0.001) ~ richness + (1|block)+ (1|block:plot) + (1| species) , data = trichome_23[trichome_23[,"treatment"] %in% c("control_1","control_12"),]),
  height_model = lmer(log(as.numeric(veg.height)) ~ richness + (1|block)+ (1|block:plot) + (1| species) , data = herbivory[herbivory[,"treatment"]%in%c("control_1","control_12"),]),
  hill_model = lmer(log(FuncHillDiv) ~ richness + (1|block)+ (1|block:plot) + (1| species) , data = ind_chem[ind_chem[,"treatment"]%in%c("control_1","control_12"),]),
  total_model = lmer(log(total_metabolome) ~ richness + (1|block)+ (1|block:plot) + (1| species) , data = ind_chem[ind_chem[,"treatment"]%in%c("control_1","control_12"),]),
  secondary_model = lmer(log(secondary_metabolome) ~ richness + (1|block)+ (1|block:plot) + (1| species) , data = ind_chem[ind_chem[,"treatment"]%in%c("control_1","control_12"),]),
  alk_model = lmer(log(alkaloid) ~ richness + (1|block)+ (1|block:plot) + (1| species), data = ind_chem[ind_chem[,"treatment"]%in%c("control_1","control_12"),]),
  terp_model = lmer(log(terpenoid) ~ richness + (1|block)+ (1|block:plot) + (1| species) , data = ind_chem[ind_chem[,"treatment"]%in%c("control_1","control_12"),]),
  poly_model = lmer(log(polyphenols) ~ richness + (1|block)+ (1|block:plot) + (1| species) , data = ind_chem[ind_chem[,"treatment"]%in%c("control_1","control_12"),])
)

watering_models <- list(
 lma_model = lmer(log(lma) ~ watering + (1|block) + (1|block:water_pair) + (1| species) , data = sla_23[sla_23[,"treatment"] %in% c("watered_12","control_12"),]),
   area_model = lmer(log(area_2) ~ watering + (1|block) + (1|block:water_pair:plot) + (1| species), data = sla_23[sla_23[,"treatment"] %in% c("watered_12","control_12"),]),
  trichome_model = lmer(log(total_hair+0.001) ~ watering + (1|block) + (1|block:water_pair:plot)  + (1| species), data = trichome_23[trichome_23[,"treatment"] %in% c("watered_12","control_12"),]),
  height_model = lmer(log(veg.height) ~ watering + (1|block) + (1|block:water_pair:plot)  + (1| species) , data = herbivory[herbivory[,"treatment"]%in%c("watered_12","control_12"),]),
  hill_model = lmer(log(FuncHillDiv) ~ water + (1|block) + (1|block:water_pair:plot)  + (1| species) , data = ind_chem[ind_chem[,"treatment"]%in%c("watered_12","control_12"),]),
  total_model = lmer(log(total_metabolome) ~ water +  (1|block) + (1|block:water_pair:plot) + (1| species) , data = ind_chem[ind_chem[,"treatment"]%in%c("watered_12","control_12"),]),
  secondary_model = lmer(log(secondary_metabolome) ~ water + (1|block) + (1|block:water_pair:plot) + (1| species), data = ind_chem[ind_chem[,"treatment"]%in%c("watered_12","control_12"),]),
  alk_model = lmer(log(alkaloid) ~ water+ (1|block) + (1|block:water_pair:plot)  + (1| species) , data = ind_chem[ind_chem[,"treatment"]%in%c("watered_12","control_12"),]),
  terp_model = lmer(log(terpenoid) ~ water + (1|block) + (1|block:water_pair:plot) + (1| species) , data = ind_chem[ind_chem[,"treatment"]%in%c("watered_12","control_12"),]),
  poly_model = lmer(log(polyphenols) ~ water + (1|block) + (1|block:water_pair:plot) + (1| species) , data = ind_chem[ind_chem[,"treatment"]%in%c("watered_12","control_12"),])
)

interaction_models <- list(
  lma_model = lmer(log(lma) ~ richness * watering + (1|block) + (1|block:water_pair:plot) + (1| species), 
                   data = sla_23[sla_23[,"treatment"] %in% c("control_12", "watered_12", "watered_1", "control_1" ),]),
   area_model = lmer(log(area_2) ~ richness * watering + (1|block) + (1|block:water_pair:plot) + (1| species), data = sla_23[sla_23[,"treatment"] %in% c("control_12", "watered_12", "watered_1", "control_1"),]),
  trichome_model = lmer(log(total_hair+0.001) ~ richness * watering + (1|block) + (1|block:water_pair:plot) + (1| species), 
                        data = trichome_23[trichome_23[,"treatment"] %in% c("control_12", "watered_12", "watered_1", "control_1"),]),

  height_model = lmer(log(veg.height) ~ richness * watering + (1|block) + (1|block:water_pair:plot) + (1| species), 
                      data = herbivory[herbivory[,"treatment"] %in% c("control_12", "watered_12", "watered_1", "control_1"),]),

  hill_model = lmer(log(FuncHillDiv) ~ richness * water + (1|block) + (1|block:water_pair:plot) + (1| species),
                    data = ind_chem[ind_chem[,"treatment"] %in% c("control_12", "watered_12", "watered_1", "control_1"),]),

  total_model = lmer(log(total_metabolome) ~ richness * water + (1|block) + (1|block:water_pair:plot) + (1| species),
                     data = ind_chem[ind_chem[,"treatment"] %in% c("control_12", "watered_12", "watered_1", "control_1"),]),

  secondary_model = lmer(log(secondary_metabolome) ~ richness * water + (1|block) + (1|block:water_pair:plot) + (1| species), 
                         data = ind_chem[ind_chem[,"treatment"] %in% c("control_12", "watered_12", "watered_1", "control_1"),]),

  alk_model = lmer(log(alkaloid) ~ richness * water + (1|block) + (1|block:water_pair:plot) + (1| species), 
                   data = ind_chem[ind_chem[,"treatment"] %in% c("control_12", "watered_12", "watered_1", "control_1"),]),

  terp_model = lmer(log(terpenoid) ~ richness * water + (1|block) + (1|block:water_pair:plot) + (1| species), 
                    data = ind_chem[ind_chem[,"treatment"] %in% c("control_12", "watered_12", "watered_1", "control_1"),]),

  poly_model = lmer(log(polyphenols) ~ richness * water+ (1|block) + (1|block:water_pair:plot) + (1| species),
                    data = ind_chem[ind_chem[,"treatment"] %in% c("control_12", "watered_12", "watered_1", "control_1"),])
)

unique(sla_23$net)
unique(herbivory $net)
unique(trichome_23 $net)
unique(ind_chem $net2)

net_models <- list(
   lma_model = lmer(log(lma) ~ net + (1|block)+ (1|block:plot) + (1| species) , 
                   data = sla_23[sla_23[,"treatment"] %in% c("control_12", "half", "net" ),]),
   area_model = lmer(log(area_2) ~  net + (1|block:plot) + (1| species) , data = sla_23[sla_23[,"treatment"] %in% c("control_12", "half", "net"),]),

  trichome_model = lmer(log(total_hair+0.001) ~ net + (1|block)+ (1|block) + (1|block:plot) + (1| species) , 
                        data = trichome_23[trichome_23[,"treatment"] %in% c("control_12", "half", "net" ),]),

  height_model = lmer(log(veg.height) ~ net + (1|block) + (1|block:plot) + (1| species), 
                     data = herbivory[herbivory[,"treatment"] %in% c("control_12", "half", "net" ),]),

  hill_model = lmer(log(FuncHillDiv) ~ net2 + (1|block) + (1|block:plot) + (1| species), 
                    data = ind_chem[ind_chem[,"treatment"] %in% c("control_12", "half", "net" ),]),

  total_model = lmer(log(total_metabolome) ~ net2 + (1|block) + (1|block:plot) + (1| species), 
                     data = ind_chem[ind_chem[,"treatment"] %in% c("control_12", "half", "net" ),]),

  secondary_model = lmer(log(secondary_metabolome) ~ net2 + (1|block) + (1|block:plot) + (1| species), 
                         data = ind_chem[ind_chem[,"treatment"] %in% c("control_12", "half", "net" ),]),

  alk_model = lmer(log(alkaloid) ~ net2 + (1|block) + (1|block:plot) + (1| species) , 
                   data = ind_chem[ind_chem[,"treatment"] %in% c("control_12", "half", "net" ),]),

  terp_model = lmer(log(terpenoid) ~ net2 + (1|block) + (1|block:plot) + (1| species) , 
                    data = ind_chem[ind_chem[,"treatment"] %in% c("control_12", "half", "net" ),]),

  poly_model = lmer(log(polyphenols) ~ net2 + (1|block) + (1|block:plot) + (1| species), 
                    data = ind_chem[ind_chem[,"treatment"] %in% c("control_12", "half", "net" ),])
                    )

# Extract effects for different predictors (handling varied term names)
richness_effects <- lapply(richness_models, extract_slope_se, possible_terms = c("richness"))
watering_effects <- lapply(watering_models, extract_slope_se, possible_terms = c("wateringW", "waterW"))
interaction_effects <- lapply(interaction_models, extract_slope_se, possible_terms = c("richness:wateringW", "richness:waterW"))
net_effects <- lapply(net_models, extract_slope_se, possible_terms = c("netNo net", "net2No net"))

# Convert to data frames
create_effect_df <- function(effects_list, treatment, model) {
  data.frame(
    trait_label = names(model),
    estimate = sapply(effects_list, `[[`, "slope"),
    se = sapply(effects_list, `[[`, "se"),
    treatment = treatment
  )
}

richness_df <- create_effect_df(richness_effects, "Richness", richness_models)
watering_df <- create_effect_df(watering_effects, "Watering", watering_models)
interaction_df <- create_effect_df(interaction_effects, "Richness × Watering", interaction_models)
net_df <- create_effect_df(net_effects, "Net", net_models)

# Combine all effects into one dataframe for faceted plotting
all_effects <- bind_rows(richness_df, watering_df, interaction_df, net_df)

# Rename traits for clarity in the plot
trait_labels <- c(
  "lma_model" = "Leaf Mass per unit Area",
    "area_model" = "Leaf Area",
  "trichome_model" = "Trichome Density",
  "height_model" = "Height",
  "tough_model" = "Toughness",
  "hill_model" = "Functional Diversity",
  "total_model" = "Total Metabolome",
  "secondary_model" = "Secondary Metabolome",
  "alk_model" = "Alkaloids",
  "terp_model" = "Terpenoids",
  "poly_model" = "Polyphenols"
)

all_effects$trait_label <- factor(all_effects$trait_label, levels = names(trait_labels), labels = trait_labels)

all_effects$treatment <- factor(all_effects$treatment,
                                levels = c("Net", "Richness", "Watering", "Richness × Watering"))

combined_plot <- ggplot(all_effects, aes(x = estimate, y = trait_label)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = estimate - se, xmax = estimate + se), 
                 height = 0.2, color = "black") +
  geom_vline(xintercept = 0, color = "#D73027") +
  theme_bw(base_size = 12) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(face = "bold", size = 12),
        panel.spacing = unit(1, "lines")) +
  facet_wrap(~ treatment, scales = "free_x", nrow = 1)

ggsave("/Users/damlacinoglu/Desktop/final_bfl_data/figures/effect_sizes*.png", combined_plot, width = 13, height = 4, dpi = 300)

#######################################################
#Check if there is phlo sig in chemical traits but not others in polycultures
#######################################################
library(ape)

# Function to calculate Pagel's lambda for raw trait values using phylosig
calculate_lambda <- function(trait_values, phylo_tree) {
  # Use phylosig function to calculate Pagel's lambda
  result <- phylosig(phylo_tree, trait_values, method = "lambda", test = TRUE)
  
  # Extract lambda and p-value from the result
  lambda <- result$lambda
  p_value <- result$P

  return(list(lambda = lambda, p_value = p_value))
}

# Define your phylogenetic tree object
phylo_tree <- read.tree(text = "(((bogr:0.03625393,bocu:0.06249119):0.12157222,arpu:0.05852303)100:0.44168163,((((moci:0.27407229,mopu:1e-08)100:0.191537,(ipru:0.19916838,phco:0.22794257)19:0.02201392)14:0.02060074,(ceam:0.05590712,(gapu:0.11492468,coti:0.06760248)76:0.03307272)100:0.15903617)26:0.01972839,(deil:0.12726932,lute:0.22973094)100:0.14976214)100:0)Root;")

#treatment = "control_12"
treatment = "control_1"

# Create a list of raw trait values for each trait, including species names
trait_data <- list(
  lma = with(sla_23[sla_23$treatment == treatment, ], {
    vals <- lma
    names(vals) <- tolower(species)
    vals[complete.cases(vals)]
  }),
  
  area = with(sla_23[sla_23$treatment == treatment, ], {
    vals <- area_2
    names(vals) <- tolower(species)
    vals[complete.cases(vals)]
  }),
  
  trichome = with(trichome_23[trichome_23$treatment == treatment, ], {
    vals <- total_hair
    names(vals) <- tolower(species)
    vals[complete.cases(vals)]
  }),
  
  height = with(herbivory[herbivory$treatment == treatment, ], {
    vals <- veg.height
    names(vals) <- tolower(species)
    vals[complete.cases(vals)]
  }),
  
  hill = with(ind_chem[ind_chem$treatment == treatment, ], {
    vals <- FuncHillDiv
    names(vals) <- tolower(species)
    vals[complete.cases(vals)]
  }),
  
  total = with(ind_chem[ind_chem$treatment == treatment, ], {
    vals <- total_metabolome
    names(vals) <- tolower(species)
    vals[complete.cases(vals)]
  }),
  
  secondary = with(ind_chem[ind_chem$treatment == treatment, ], {
    vals <- secondary_metabolome
    names(vals) <- tolower(species)
    vals[complete.cases(vals)]
  }),
  
  alk = with(ind_chem[ind_chem$treatment == treatment, ], {
    vals <- alkaloid
    names(vals) <- tolower(species)
    vals[complete.cases(vals)]
  }),
  
  terp = with(ind_chem[ind_chem$treatment == treatment, ], {
    vals <- terpenoid
    names(vals) <- tolower(species)
    vals[complete.cases(vals)]
  }),
  
  poly = with(ind_chem[ind_chem$treatment == treatment, ], {
    vals <- polyphenols
    names(vals) <- tolower(species)
    vals[complete.cases(vals)]
  })
)


# Initialize an empty data frame to store lambda and p-values
lambda_results <- data.frame(
  trait = c("lma", "area", "trichome", "height", "hill", "total", "secondary", "alk", "terp", "poly"),
  lambda = NA,
  p_value = NA
)

# Calculate lambda and p-value for each trait
for (i in 1:nrow(lambda_results)) {
  trait <- lambda_results$trait[i]
  trait_values <- trait_data[[trait]]
  
  # Only compute if there are no NA values
  if (sum(is.na(trait_values)) == 0) {
    result <- calculate_lambda(trait_values, phylo_tree)
    lambda_results$lambda[i] <- result$lambda
    lambda_results$p_value[i] <- result$p_value
  } else {
    lambda_results$lambda[i] <- NA
    lambda_results$p_value[i] <- NA
  }
}

# Add significance based on p-value (p-value < 0.05 is significant)
lambda_results$significant <- ifelse(lambda_results$p_value < 0.05, "p<0.05", "")

 lambda_results$trait <- c( "LMA", "Leaf Area","Trichomes","Height",  "FHDiversity", 
                          "TMR", "SMR", 
                          "Alkaloids", "Terpenoids", "Polyphenols")
custom_order <- c( "Height","Leaf Area","LMA","Trichomes","FHDiversity", 
                          "TMR", "SMR", 
                          "Alkaloids", "Terpenoids", "Polyphenols")
lambda_results$trait <- factor(lambda_results$trait, levels = custom_order)

 final_poly = ggplot(lambda_results, aes(x = trait, y = lambda, color = significant)) +
  geom_point(size = 4) +
  labs(title = "",
       x = "Trait",
       y = "Pagel's λ") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

 final_mono = ggplot(lambda_results, aes(x = trait, y = lambda, color = significant)) +
  geom_point(size = 4) +
  labs(title = "",
       x = "Trait",
       y = "Pagel's λ") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("/Users/damlacinoglu/Desktop/final_bfl_data/figures/phylo_signal.png", final, 
       width = 5, height = 5, dpi = 300, units = "in")
       
#######################################################
#FIGURE 1
#######################################################
#Do plant species differ in their levels of insect herbivory? 

herbivory = herbivory[!is.na(herbivory[,"herbivory.all"]),]

#herbivory_just12 = herbivory[herbivory$richness != 1,]
herbivory_just12 = cbind(herbivory_just12, block = rep(NA, nrow(herbivory_just12)))
for(i in 1:nrow(herbivory_just12)){ 
  plot = herbivory_just12[i,"plot"] 
if(grepl("A",plot)) {herbivory_just12[i,"block"] = "A"}
  if(grepl("B",plot)) {herbivory_just12[i,"block"] = "B"}
  if(grepl("C",plot)) {herbivory_just12[i,"block"] = "C"}
}

herbivory_just12 = herbivory[herbivory$treatment %in% c("control_1", "control_12"),]

sla.wat = herbivory_just12 %>%
  group_by(species, treatment, richness) %>%
  summarise_at(vars(herbivory.all), list(herb.se = std.error,
                                         herb.mean = mean)) 
                                         
sla.wat.control = sla.wat[sla.wat$treatment %in% c("control_1", "control_12"), ]

model = glmmTMB(herbivory.all ~ species + (1 | block)+ (1 | block:plot), 
                         data = herbivory_just12[herbivory_just12$treatment == "control_12", ],
                         family = tweedie(link = "log"))
                         
           
species_pairs = emmeans(model, pairwise ~ species)

species_cld = cld(species_pairs$emmeans, Letters = letters)

species_cld_df = as.data.frame(species_cld)
species_cld_df$species = gsub("species", "", species_cld_df$species)  

plot_data = merge(sla.wat.control, species_cld_df, by = "species")

offset_value = max(plot_data$herb.se) * 0.6

# Reorder species based on herb.mean of richness 12
species_order = plot_data %>%
  filter(richness == 12) %>%
  arrange(desc(herb.mean)) %>%
  pull(species)

plot_data = plot_data %>%
  mutate(species = factor(species, levels = species_order))

# Convert richness to factor with custom labels
plot_data$richness = factor(plot_data$richness, levels = c(1, 12), labels = c("M", "P"))

plot_data[plot_data[,"richness"] == "M",".group"] = NA

sla.wat.plot = ggplot(plot_data, aes(x = species, y = herb.mean, color = richness)) +
  geom_point(size = 3, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = herb.mean - herb.se, ymax = herb.mean + herb.se), 
                width = 0.2, position = position_dodge(width = 0.5)) +
  scale_color_manual(values = c("M" = "grey", "P" = "black"), name = "Richness") +
    geom_text(aes(label = .group, 
                y = herb.mean + herb.se + offset_value), 
                            size = 4, color = "black") + 
  labs(x = "Species", y = "Total herbivory per individual (%)") + 
  theme(axis.ticks.x = element_line(color = "black")) + 
  theme_bw(base_size = 12)

ggsave("/Users/damlacinoglu/Desktop/final_bfl_data/figures/herbivory_species.png", sla.wat.plot, 
       width = 7, height = 4, dpi = 300, units = "in")

#######################################################
#FIGURE 2
#######################################################
#Do interactions with insect herbivores impact the plant biodiversity ecosystem functioning relationship? 
	
	gen_cover = gen_cover_24
	gen_cover_totals = gen_cover[gen_cover[,"spcode"] =="total",]
	
	for(i in 1:nrow(gen_cover_totals)) {
	  plot =  gen_cover_totals[i,"plot"]
	  
	  if(grepl("MON", plot, fixed=TRUE)) {
	    gen_cover_totals[i,"SR"] = 1 
	  } else {
	    if(grepl("6", plot, fixed=TRUE)) {gen_cover_totals[i,"SR"] = 6}
	    if(grepl("4", plot, fixed=TRUE)) {gen_cover_totals[i,"SR"] = 4}
	    if(grepl("2", plot, fixed=TRUE)) {gen_cover_totals[i,"SR"] = 2}
	    if(grepl("12", plot, fixed=TRUE)) {gen_cover_totals[i,"SR"] = 12}
	  }
	}
	unique(gen_cover_totals[,"SR"])
	
	for(i in 1:nrow(gen_cover_totals)) {
	  plot =  gen_cover_totals[i,"plot"]
	  if(grepl("12", plot, fixed=TRUE)) {gen_cover_totals[i,"PD"] = conti_pd[conti_pd[,"plot"] ==plot,"new_pd"]   } else
	    if(grepl("MON", plot, fixed=TRUE)) { gen_cover_totals[i,"PD"] = conti_pd[conti_pd[,"plot"] ==plot,"new_pd"]   } else 
	      if(grepl("6", plot, fixed=TRUE)) { gen_cover_totals[i,"PD"] = conti_pd[conti_pd[,"plot"] ==plot,"new_pd"]    } else
	        if(grepl("4", plot, fixed=TRUE)) { gen_cover_totals[i,"PD"] = conti_pd[conti_pd[,"plot"] ==plot,"new_pd"]    } else
	          if(grepl("2", plot, fixed=TRUE)) { gen_cover_totals[i,"PD"] = conti_pd[conti_pd[,"plot"] ==plot,"new_pd"]    }
	}
	gen_cover_totals[,c("plot","SR","PD")]
	
	colnames(gen_cover_totals)[7] = "out_net"
	colnames(gen_cover_totals)[8] = "net"
	colnames(gen_cover_totals)[9] = "half_net"
	
	masterdata  = data.frame(matrix(nrow = 3*nrow(gen_cover_totals), ncol =7))
	masterdata[,1] = c(gen_cover_totals$plot,gen_cover_totals$plot,gen_cover_totals$plot)
	masterdata[,2] = c(rep("IN", nrow(gen_cover_totals)), rep("OUT", nrow(gen_cover_totals)), rep("HALF", nrow(gen_cover_totals)))
	masterdata[,3] = c(gen_cover_totals$SR,gen_cover_totals$SR,gen_cover_totals$SR)
	masterdata[,4] = c(gen_cover_totals$PD,gen_cover_totals$PD,gen_cover_totals$PD)
	masterdata[,5] = rep(NA, nrow(masterdata))
	masterdata[,6] =c(gen_cover_totals$net,gen_cover_totals$out_net, gen_cover_totals$half_net)
	
	nrow(masterdata)
	colnames(masterdata) = c("plot","net","SR","PD","biomass","cover","biomass_24")
	
	#remove the weeds (we did not remove weeds from cover. )
	#gen_bio = gen_bio[gen_bio[,"Dry.biomass"] != "Weeds",]
	
	for(i in 1:nrow(masterdata)) {
		plot = masterdata[i,"plot"]
		status = masterdata[i,"net"]
		if(status == "IN") {status = "Full" }
		if(status == "OUT") {status = "Open" }
		if(status == "HALF") {status = "Half" }
		masterdata[i,"biomass"] = sum(na.omit(gen_bio[gen_bio[,"Plot.name"] == plot & gen_bio[,"Quadrat"] == status,"Total.weight.(g)"]))
			masterdata[i,"biomass_24"] = sum(as.numeric(na.omit(gen_bio_24[gen_bio_24[,"Plot.name"] == plot & gen_bio_24[,"Quadrat"] == status,"Total.weight.(g)"])))
	}
	
	masterdata = cbind(masterdata, block = rep(NA, nrow(masterdata)))
	for(i in 1:nrow(masterdata)) {
		if(grepl("A", masterdata[i,1])) {masterdata[i,"block"] = "A"}
		if(grepl("B", masterdata[i,1])) {masterdata[i,"block"] = "B"}
		if(grepl("C", masterdata[i,1])) {masterdata[i,"block"] = "C"}
	}
	
	masterdata = masterdata %>%
  mutate(W_presence = ifelse(grepl("W", plot), "W", "No W"))
masterdata = masterdata %>%
  mutate(water_pair = gsub("-W","", masterdata$plot))
length(unique(masterdata$water_pair))
	
masterdata[masterdata[,"net"] == "IN","net"] = "NET"
masterdata[masterdata[,"net"] == "HALF","net"] = "CONTROL"

masterdata_only_half_net = masterdata[masterdata[,2] %in% c("NET","CONTROL"),]

# Create a new grouping variable for color mapping
masterdata_only_half_net$Group <- with(masterdata_only_half_net, 
                                       paste(net, ifelse(W_presence == "W", "W", "NoW"), sep = "_"))

masterdata_only_half_net[masterdata_only_half_net[,"Group"] == "NET_W", "Group"] = "NET + W"
masterdata_only_half_net[masterdata_only_half_net[,"Group"] == "NET_NoW", "Group"] = "NET + No W" 
masterdata_only_half_net[masterdata_only_half_net[,"Group"] == "CONTROL_W", "Group"] = "NET CONTROL + W"
masterdata_only_half_net[masterdata_only_half_net[,"Group"] == "CONTROL_NoW", "Group"] = "NET CONTROL + No W"

model1 = lmer(formula = biomass_24 ~  SR *  net * W_presence + (1|block) + (1|block: water_pair:plot), data = masterdata_only_half_net) 
anova(model1)

pred_net_w_1 <- ggpredict(model1, terms = c("SR [all]"), 
                        condition = c(net = "NET", W_presence = "W"))
pred_net_w_2 <- ggpredict(model1, terms = c("SR [all]"), 
                        condition = c(net = "NET", W_presence = "No W"))
pred_net_w_3 <- ggpredict(model1, terms = c("SR [all]"), 
                        condition = c(net = "CONTROL", W_presence = "W"))
pred_net_w_4 <- ggpredict(model1, terms = c("SR [all]"), 
                        condition = c(net = "CONTROL", W_presence = "No W"))
                        
pred_net_w_1$group <- "NET + W"
pred_net_w_2$group <- "NET + No W"
pred_net_w_3$group <- "NET CONTROL + W"
pred_net_w_4$group <- "NET CONTROL + No W"

# Combine them all
pred_all <- rbind(pred_net_w_1, pred_net_w_2, pred_net_w_3, pred_net_w_4)

# Now plot them with geom_line(), mapping color to group
sr_plot <- ggplot(masterdata_only_half_net, aes(x = SR, y = biomass_24)) +
  geom_jitter(aes(color = Group), size = 1, alpha = 0.6) +
  geom_line(data = pred_all, aes(x = x, y = predicted, color = group), size = 1) +
  ylab('Biomass (g)') +
  xlab('Species Richness') +
  scale_color_manual(
    name = "Treatment",
    values = c("NET + W" = "#D73027",
               "NET + No W" = "#4575B4",
               "NET CONTROL + W" = "#F8766D",
               "NET CONTROL + No W" = "#91C3FF")
  ) +
  theme_bw(base_size = 12)+ 
  theme(legend.position = "none")

model2 = lmer(formula = biomass_24 ~  PD *  net * W_presence+ (1|block)  + (1|block: water_pair:plot)+ (1|block : water_pair : plot), data = masterdata_only_half_net) 
anova(model2)

pred_net_w_1 <- ggpredict(model2, terms = c("PD [all]"), 
                        condition = c(net = "NET", W_presence = "W"))
pred_net_w_2 <- ggpredict(model2, terms = c("PD [all]"), 
                        condition = c(net = "NET", W_presence = "No W"))
pred_net_w_3 <- ggpredict(model2, terms = c("PD [all]"), 
                        condition = c(net = "CONTROL", W_presence = "W"))
pred_net_w_4 <- ggpredict(model2, terms = c("PD [all]"), 
                        condition = c(net = "CONTROL", W_presence = "No W"))
                        
pred_net_w_1$group <- "NET + W"
pred_net_w_2$group <- "NET + No W"
pred_net_w_3$group <- "NET CONTROL + W"
pred_net_w_4$group <- "NET CONTROL + No W"

# Combine them all
pred_all <- rbind(pred_net_w_1, pred_net_w_2, pred_net_w_3, pred_net_w_4)

# Now plot them with geom_line(), mapping color to group
pd_plot <- ggplot(masterdata_only_half_net, aes(x = PD, y = biomass_24)) +
  geom_jitter(aes(color = Group), size = 1, alpha = 0.6) +
  geom_line(data = pred_all, aes(x = x, y = predicted, color = group), size = 1) +
  ylab('Biomass (g)') +
  xlab('Phylogenetic Diversity') +
  scale_color_manual(
    name = "Treatment",
    values = c("NET + W" = "#D73027",
               "NET + No W" = "#4575B4",
               "NET CONTROL + W" = "#F8766D",
               "NET CONTROL + No W" = "#91C3FF")
  ) +
  theme_bw(base_size = 12)+ 
  theme(legend.position = "right")

combined_plot <- plot_grid(
  sr_plot, pd_plot, 
  labels = c("A", "B"), 
  ncol = 2, 
  align = "h",
  rel_widths = c(0.7, 1)  # Adjust width ratio (A takes 70% of default width, B takes full width)
)

ggsave("/Users/damlacinoglu/Desktop/final_bfl_data/figures/combined_plot.png", combined_plot, width = 12, height = 6, dpi = 300)

#######################################################
#FIGURE 3
#######################################################
#Do plant species' herbivory levels / biomass differ in their dependence on diversity and watering?
####################Full model (12 SP CONTROL, MON CONTROL, 12 SP WATERED, MON WATERED)

herbivory_just12 = herbivory[herbivory$treatment %in% c("control_1","watered_1", "control_12","watered_12"),]

herbivory_just12 = cbind(herbivory_just12, block = rep(NA, nrow(herbivory_just12)))
for(i in 1:nrow(herbivory_just12)){ 
  plot = herbivory_just12[i,"plot"] 
if(grepl("A",plot)) {herbivory_just12[i,"block"] = "A"}
  if(grepl("B",plot)) {herbivory_just12[i,"block"] = "B"}
  if(grepl("C",plot)) {herbivory_just12[i,"block"] = "C"}
}
herbivory_just12 = herbivory_just12 %>%
  dplyr::mutate(W_presence = ifelse(grepl("W", plot), "W", "No W"))

herbivory_just12$richness = as.factor(herbivory_just12$richness)
herbivory_just12$watering = as.factor(herbivory_just12$watering)

# Fit hurdle model with tweedie distribution
hurdle_model <- glmmTMB(
  herbivory.all ~ richness * watering + (1 | block: water_pair) + (1 | block) + (1 | species) , 
  ziformula = ~ richness * watering + (1 | block: water_pair)+ (1 | block) + (1 | species) , 
  family = tweedie(link = "log"), 
  data = herbivory_just12
)
summary(hurdle_model)
anova(model)

herbivory_just12[herbivory_just12[,"treatment"] %in% c("watered_12","watered_1"),"treatment"] = "Watered"
herbivory_just12[herbivory_just12[,"treatment"]  %in%  c("control_1", "control_12"),"treatment"] = "Nonwatered"
herbivory_just12$richness <- ifelse(grepl("MON", herbivory_just12$plot), "Monoculture", "Polyculture")

summary_stats = herbivory_just12 %>%
  dplyr::group_by(richness, treatment) %>%
  dplyr::summarise(
    mean_herbivory = mean(herbivory.all, na.rm = TRUE),
    se_herbivory = sd(herbivory.all, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  ) %>%
  dplyr::mutate(x_label = paste(richness, "\n", treatment))  
  
 together = ggplot() +
  #geom_jitter(data = herbivory_just12, 
   #           aes(x = interaction(richness, treatment), y = herbivory.all), 
    #          color = "gray", size = 2, alpha = 0.3, width = 0.2) + 
  geom_errorbar(data = summary_stats, 
                aes(x = x_label, ymin = mean_herbivory - se_herbivory, ymax = mean_herbivory + se_herbivory), 
                width = 0.2, color = "black") + 
                 scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(0, NA)) +
  geom_point(data = summary_stats, 
             aes(x = x_label, y = mean_herbivory), 
             size = 3, color = "black") + 
  ylab("Total herbivory per individual (%)") +
  xlab("") +
  theme_bw(base_size = 12)
       
####################Do that same graph but for BIOMASS
masterdata_out_12 = masterdata[masterdata[,"SR"] %in% c(1,12),]
masterdata_out_12 = masterdata_out_12[masterdata_out_12[,"net"] == "OUT",]

masterdata_out_12 = cbind(masterdata_out_12, block = rep(NA, nrow(masterdata_out_12)))
for(i in 1:nrow(masterdata_out_12)) {
	if(grepl("A", masterdata_out_12[i,1])) {masterdata_out_12[i,"block"] = "A"}
	if(grepl("B", masterdata_out_12[i,1])) {masterdata_out_12[i,"block"] = "B"}
	if(grepl("C", masterdata_out_12[i,1])) {masterdata_out_12[i,"block"] = "C"}
}

masterdata_out_12 = masterdata_out_12 %>%
  mutate(W_presence = ifelse(grepl("W", plot), "W", "No W"))

masterdata_out_12[masterdata_out_12[,"W_presence"] == "W","W_presence"] = "Watered"
masterdata_out_12[masterdata_out_12[,"W_presence"] == "No W","W_presence"] = "Nonwatered"
masterdata_out_12[masterdata_out_12[,"SR"] == 1,"SR"] = "Monoculture"
masterdata_out_12[masterdata_out_12[,"SR"] == 12,"SR"] = "Polyculture"

summary_stats = masterdata_out_12 %>%
  dplyr::group_by(SR, W_presence) %>%
  dplyr::summarise(
    mean_herbivory = mean(biomass_24, na.rm = TRUE),
    se_herbivory = sd(biomass_24, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  ) %>%
  dplyr::mutate(x_label = paste(SR, "\n", W_presence))  
  
 together_bio = ggplot() +
  #geom_jitter(data = herbivory_just12, 
   #           aes(x = interaction(richness, treatment), y = herbivory.all), 
    #          color = "gray", size = 2, alpha = 0.3, width = 0.2) + 
  geom_errorbar(data = summary_stats, 
                aes(x = x_label, ymin = mean_herbivory - se_herbivory, ymax = mean_herbivory + se_herbivory), 
                width = 0.2, color = "black") +
                    scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(0, NA)) +
 
  geom_point(data = summary_stats, 
             aes(x = x_label, y = mean_herbivory), 
             size = 3, color = "black") + 
  ylab("Biomass (g)") +
  xlab("") +
  theme_bw(base_size = 12)

masterdata_out_12$richness = as.factor(masterdata_out_12$richness)
model = lmer(biomass_24 ~ SR * W_presence + (1|block) + (1|block:water_pair), data = masterdata_out_12)
summary(model)
anova(model)

  combined_plot = plot_grid(together, together_bio, labels = c("A", "B"), ncol = 2, nrow = 1, align = "h")  
ggsave("/Users/damlacinoglu/Desktop/final_bfl_data/figures/biomass_diversity_water_1_together.png", combined_plot, 
       width = 12, height = 6, dpi = 300, units = "in")

#######################################################
#FIGURE 5
#######################################################
#What are the mechanisms behind the observed plant biodiversity ecosystem functioning under insect exposure?
sticky_trap = sticky_trap[,1:6]
colnames(sticky_trap) = c("plot","net","side","date","percent_cover","total_sr")

sticky_trap[,5] = as.numeric(gsub("<","",sticky_trap[,5]))

#Edit this dataset so that each row is a unique plot - net combination, add the percent covers of front and back to make total percent cover

total_sticky_trap = sticky_trap
#total_sticky_trap = sticky_trap[sticky_trap$side == "FRONT",]

for(i in 1:nrow(total_sticky_trap)) {
	
  plot = total_sticky_trap[i,"plot"]
  net = total_sticky_trap[i,"net"]
  total_sticky_trap[i,5] = mean(sticky_trap[sticky_trap[,1] == plot & sticky_trap[,2] == net,5])
}

#Add plant species richness, PD, watering columns 
total_sticky_trap = cbind(total_sticky_trap, SR=rep(NA,nrow(total_sticky_trap)), PD_numeric=rep(NA,nrow(total_sticky_trap)), PD_categoric=rep(NA,nrow(total_sticky_trap)), water=rep(NA,nrow(total_sticky_trap)))
head(total_sticky_trap)

for(i in 1:nrow(total_sticky_trap)) {
  if(grepl("-W",total_sticky_trap[i,1])) {total_sticky_trap[i,"water"] = "WATER"} else {total_sticky_trap[i,"water"] = "AMBIENT"}
  if(grepl("MON",total_sticky_trap[i,1])) {total_sticky_trap[i,"SR"] = 1; total_sticky_trap[i,"PD_numeric"] = 0}
  if(grepl("12",total_sticky_trap[i,1])) {total_sticky_trap[i,"SR"] = 12; total_sticky_trap[i,"PD_numeric"] = 0}
  
  if(grepl("H2",total_sticky_trap[i,1])) {total_sticky_trap[i,"SR"] = 2; total_sticky_trap[i,"PD_categoric"] = "H"}
  if(grepl("H4",total_sticky_trap[i,1])) {total_sticky_trap[i,"SR"] = 4; total_sticky_trap[i,"PD_categoric"] = "H"}
  if(grepl("H6",total_sticky_trap[i,1])) {total_sticky_trap[i,"SR"] = 6; total_sticky_trap[i,"PD_categoric"] = "H"}
  
  if(grepl("L2",total_sticky_trap[i,1])) {total_sticky_trap[i,"SR"] = 2; total_sticky_trap[i,"PD_categoric"] = "L"}
  if(grepl("L4",total_sticky_trap[i,1])) {total_sticky_trap[i,"SR"] = 4; total_sticky_trap[i,"PD_categoric"] = "L"}
  if(grepl("L6",total_sticky_trap[i,1])) {total_sticky_trap[i,"SR"] = 6; total_sticky_trap[i,"PD_categoric"] = "L"}
  
  if(grepl("M2",total_sticky_trap[i,1])) {total_sticky_trap[i,"SR"] = 2; total_sticky_trap[i,"PD_categoric"] = "M"}
  if(grepl("M4",total_sticky_trap[i,1])) {total_sticky_trap[i,"SR"] = 4; total_sticky_trap[i,"PD_categoric"] = "M"}
  if(grepl("M6",total_sticky_trap[i,1])) {total_sticky_trap[i,"SR"] = 6; total_sticky_trap[i,"PD_categoric"] = "M"}

  for(j in 1:nrow(conti_pd)) {
    plot = conti_pd[j,1]
    if(grepl(plot, total_sticky_trap[i,"plot"]) ) {total_sticky_trap[i,"PD_numeric"] = conti_pd[j,2]}
  }
}

total_sticky_trap = total_sticky_trap[,-3]

total_sticky_trap = cbind(total_sticky_trap, block = rep(NA, nrow(total_sticky_trap)))
for(i in 1:nrow(total_sticky_trap)){ 
  plot = total_sticky_trap[i,"plot"] 
  if(grepl("A",plot)) {total_sticky_trap[i,"block"] = "A"}
  if(grepl("B",plot)) {total_sticky_trap[i,"block"] = "B"}
  if(grepl("C",plot)) {total_sticky_trap[i,"block"] = "C"}
}

total_sticky_trap = total_sticky_trap %>%
  mutate(water_pair = gsub("-W","", total_sticky_trap $plot))
length(unique(total_sticky_trap $water_pair))
  
  model = lmer(total_sr~ PD_numeric * net * water +  (1|block) + (1|block:water_pair:plot), data = total_sticky_trap)
anova(model)


pred_df <- ggpredict(model, terms = c("PD_numeric [all]"), 
                     condition = c(net = "OUT ", water = "AMBIENT"))
pd_plot <- ggplot() +
  # Raw data layer
  geom_jitter(data = total_sticky_trap, aes(x = PD_numeric, y = total_sr), 
              size = 1, color = "gray") +
  # Confidence ribbon
  geom_ribbon(data = pred_df, aes(x = x, ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  # Predicted line
  geom_line(data = pred_df, aes(x = x, y = predicted), color = "black", size = 1) +
  ylab('Insect species richness') +
  xlab('Phylogenetic Diversity') +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  theme_bw(base_size = 12)
    
 mean_se_data = total_sticky_trap %>%
  dplyr::group_by(water) %>%
  dplyr::summarise(mean_cover = mean(percent_cover, na.rm = TRUE),
            se_cover = sd(percent_cover, na.rm = TRUE) / sqrt(n()), .groups = 'drop')

water_plot <- ggplot() +
  geom_jitter(data = total_sticky_trap, aes(x = water, y = percent_cover), 
              color = "gray", size = 2, alpha = 0.3) + 
  geom_errorbar(data = mean_se_data, 
                aes(x = water, ymin = mean_cover - se_cover, ymax = mean_cover + se_cover), 
                width = 0.2, color = "black") + 
  geom_point(data = mean_se_data, 
             aes(x = water, y = mean_cover), 
             size = 3, color = "black") + 
  ylab("Insect abundance (% cover of the trap)") +
  xlab("Treatment") +
  scale_x_discrete(labels = c("AMBIENT" = "NONWATERED", "WATER" = "WATERED")) +  # Custom legend labels
  theme_bw(base_size = 12)

combined_plot = plot_grid(pd_plot, water_plot, labels = c("A", "B"), ncol = 2, align = "h")
ggsave("/Users/damlacinoglu/Desktop/final_bfl_data/figures/insect_result.png", combined_plot, width = 10, height = 6, dpi = 300)

model = lmer(total_sr~ as.factor(SR)  + (1|block:plot) +  (1|block), data = total_sticky_trap[total_sticky_trap[,"water"] == "AMBIENT"& total_sticky_trap[,"net"] == "OUT ",])

model = lmer(total_sr~ SR * net * water + (1|block: water_pair :plot) +  (1|block), data = total_sticky_trap)
model = lmer(percent_cover ~ SR* net * water + (1|block) + (1|block: water_pair:plot), data = total_sticky_trap)

model = lmer(percent_cover ~ SR* net * water + (1|block) + (1|block: water_pair:plot), data = total_sticky_trap)

summary(model)
anova(model)

library(emmeans)
library(ggplot2)

#######################################################
#FIGURE S4
#######################################################

#############################Just 12 SP CONTROL and 12 SP HALF
herbivory_just12 = herbivory[herbivory$richness != 1,]
herbivory_just12 = herbivory_just12[herbivory_just12 $treatment %in% c("control", "half"),]

herbivory_just12$herbivory.all = as.numeric(herbivory_just12$herbivory.all )
sla.wat = herbivory_just12 %>%
  group_by(species, treatment) %>%
  summarise_at(vars(herbivory.all), list(herb.se = ~std.error(.), 
                                         herb.mean = ~mean(., na.rm = TRUE)))

species_order_2 = sla.wat %>%
  filter(treatment == "control") %>%
  arrange(desc(herb.mean)) %>%
  pull(species)

sla.wat = sla.wat %>%
  mutate(species = factor(species, levels = species_order_2))

sla.wat.plot_2 = ggplot(sla.wat, aes(x = species, y = herb.mean)) +  
  geom_point(data = subset(sla.wat, treatment == "control"), 
             aes(color = treatment), size = 3) +  
  geom_errorbar(data = subset(sla.wat, treatment == "control"), 
                aes(ymin = herb.mean - herb.se, ymax = herb.mean + herb.se, color = treatment), 
                width = 0.2) +  
  geom_point(data = subset(sla.wat, treatment == "half"), 
             aes(x = as.numeric(factor(species)) - 0.2, color = treatment), 
             size = 3) +  
  geom_errorbar(data = subset(sla.wat, treatment == "half"), 
                aes(x = as.numeric(factor(species)) - 0.2, ymin = herb.mean - herb.se, ymax = herb.mean + herb.se, color = treatment), 
                width = 0.2) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  
        axis.ticks.x = element_line(color = "black")) + 
  ylab('Total herbivory per individual (%)') +
  xlab('Species') + 
  theme_bw(base_size = 12) +
  scale_color_manual(values = c("control" = "black", "half" = "gray"),
                     name = "Treatment",
                     labels = c("Out", "Control"))

#NONE ARE SIGNIFICANT!
t.test(herbivory.all ~ treatment, data = herbivory_just12, subset = species == "ARPU")
t.test(herbivory.all ~ treatment, data = herbivory_just12, subset = species == "BOCU")
t.test(herbivory.all ~ treatment, data = herbivory_just12, subset = species == "BOGR")
t.test(herbivory.all ~ treatment, data = herbivory_just12, subset = species == "CEAM")
t.test(herbivory.all ~ treatment, data = herbivory_just12, subset = species == "COTI")
t.test(herbivory.all ~ treatment, data = herbivory_just12, subset = species == "DEIL")
t.test(herbivory.all ~ treatment, data = herbivory_just12, subset = species == "GAPU")
t.test(herbivory.all ~ treatment, data = herbivory_just12, subset = species == "IPRU")
t.test(herbivory.all ~ treatment, data = herbivory_just12, subset = species == "LUTE")
t.test(herbivory.all ~ treatment, data = herbivory_just12, subset = species == "MOCI")
t.test(herbivory.all ~ treatment, data = herbivory_just12, subset = species == "MOPU")
t.test(herbivory.all ~ treatment, data = herbivory_just12, subset = species == "PHCO")

ggsave("/Users/damlacinoglu/Desktop/final_bfl_data/figures/herbivory_half_out.png", sla.wat.plot_2, 
       width = 12, height = 6, dpi = 300, units = "in")

#######################################################
#FIGURE S5
#######################################################

mean_se_data = total_sticky_trap %>%
  dplyr::group_by(net) %>%
  dplyr::summarise(mean_cover = mean(percent_cover, na.rm = TRUE),
            se_cover = sd(percent_cover, na.rm = TRUE) / sqrt(n()), .groups = 'drop')

cover = ggplot() +
  geom_jitter(data = total_sticky_trap, aes(x = net, y = percent_cover), 
              color = "gray", size = 2, alpha = 0.3) + 
  geom_errorbar(data = mean_se_data, 
                aes(x = net, ymin = mean_cover - se_cover, ymax = mean_cover + se_cover), 
                width = 0.2, color = "black") + 
  geom_point(data = mean_se_data, 
             aes(x = net, y = mean_cover), 
             size = 3, color = "black") + 
  ylab("Insect abundance (% cover of the trap)") +
  xlab("Treatment") +
  theme_bw(base_size = 12)

mean_se_data = total_sticky_trap %>%
  dplyr::group_by(net) %>%
  dplyr::summarise(mean_cover = mean(total_sr, na.rm = TRUE),
            se_cover = sd(total_sr, na.rm = TRUE) / sqrt(n()), .groups = 'drop')

richness = ggplot() +
  geom_jitter(data = total_sticky_trap, aes(x = net, y = total_sr), 
              color = "gray", size = 2, alpha = 0.3) + 
  geom_errorbar(data = mean_se_data, 
                aes(x = net, ymin = mean_cover - se_cover, ymax = mean_cover + se_cover), 
                width = 0.2, color = "black") + 
  geom_point(data = mean_se_data, 
             aes(x = net, y = mean_cover), 
             size = 3, color = "black") + 
  ylab("Insect species richness") +
  xlab("Treatment") +
  theme_bw(base_size = 12)
  
combined_plot = plot_grid(cover, richness, labels = c("A", "B"), ncol = 2, align = "h")
ggsave("/Users/damlacinoglu/Desktop/final_bfl_data/figures/insect_methods.png", combined_plot, width = 12, height = 6, dpi = 300)

#######################################################
#FIGURE S7
#######################################################        
####################Just richness (12 SP CONTROL, MON CONTROL)

herbivory_just12 = herbivory[herbivory$treatment == "control",]

herbivory_just12$herbivory.all = as.numeric(herbivory_just12$herbivory.all )
sla.wat = herbivory_just12 %>%
  group_by(species, richness) %>%
  summarise_at(vars(herbivory.all), list(herb.se = ~std.error(.), 
                                         herb.mean = ~mean(., na.rm = TRUE)))

sla.wat = data.frame(sla.wat)
sla.wat[,"richness"] = as.character(sla.wat[,"richness"])

asterisk_data = sla.wat %>%
  dplyr::group_by(species) %>%
  dplyr::mutate(max_herb = pmax(ifelse(richness == "1", herb.mean + herb.se, -Inf), 
                                 ifelse(richness == "12", herb.mean + herb.se, -Inf))) %>%
  dplyr::distinct(species, max_herb) %>%
  dplyr::mutate(asterisk_pos = max_herb + 0.5) 

asterisk_data = asterisk_data %>%
  dplyr::group_by(species) %>%
  dplyr::summarize(
    asterisk_pos = max(asterisk_pos)
  )
asterisk_data = data.frame(asterisk_data)
asterisk_data = asterisk_data[asterisk_data[,"species"] %in% c("BOCU", "BOGR", "COTI", "MOCI"),]

species_order = sla.wat %>%
  filter(richness == "12") %>%
  arrange(desc(herb.mean)) %>%
  pull(species)

sla.wat = sla.wat %>%
  mutate(species = factor(species, levels = species_order))

sla.wat.plot = ggplot(sla.wat, aes(x = species, y = herb.mean)) +  
  geom_point(data = subset(sla.wat, richness == "12"), 
             aes(color = richness), size = 3) +  
  geom_errorbar(data = subset(sla.wat, richness == "12"), 
                aes(ymin = herb.mean - herb.se, ymax = herb.mean + herb.se, color = richness), 
                width = 0.2) +  
  geom_point(data = subset(sla.wat, richness == "1"), 
             aes(x = as.numeric(factor(species)) - 0.2, color = richness), 
             size = 3) +  
  geom_errorbar(data = subset(sla.wat, richness == "1"), 
                aes(x = as.numeric(factor(species)) - 0.2, ymin = herb.mean - herb.se, ymax = herb.mean + herb.se, color = richness), 
                width = 0.2) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  
        axis.ticks.x = element_line(color = "black")) + 
  ylab('Total herbivory per individual (%)') +
  xlab('Species') + 
  theme_bw(base_size = 12) +
  geom_text(data = asterisk_data, 
            aes(x = species, 
                y = asterisk_pos, 
                label = "*"), 
            size = 7, color = "black") + 
  scale_color_manual(values = c("1" = "gray", "12" = "black"),
                     name = "Richness",
                     labels = c("Monoculture", "Polyculture"))

#BOCU, BOGR, COTI, MOCI (CONVERT TO ANOVAS?)
t.test(herbivory.all ~ richness, data = herbivory_just12, subset = species == "ARPU")
t.test(herbivory.all ~ richness, data = herbivory_just12, subset = species == "BOCU")
t.test(herbivory.all ~ richness, data = herbivory_just12, subset = species == "BOGR")
t.test(herbivory.all ~ richness, data = herbivory_just12, subset = species == "CEAM")
t.test(herbivory.all ~ richness, data = herbivory_just12, subset = species == "COTI")
t.test(herbivory.all ~ richness, data = herbivory_just12, subset = species == "DEIL")
t.test(herbivory.all ~ richness, data = herbivory_just12, subset = species == "GAPU")
t.test(herbivory.all ~ richness, data = herbivory_just12, subset = species == "IPRU")
t.test(herbivory.all ~ richness, data = herbivory_just12, subset = species == "LUTE")
t.test(herbivory.all ~ richness, data = herbivory_just12, subset = species == "MOCI")
t.test(herbivory.all ~ richness, data = herbivory_just12, subset = species == "MOPU")
t.test(herbivory.all ~ richness, data = herbivory_just12, subset = species == "PHCO")

#############################Just watering (12SP CONTROL VS 12SP WATERED)
herbivory_just12 = herbivory[herbivory$richness != 1,]
herbivory_just12 = herbivory_just12[herbivory_just12 $treatment %in% c("control", "watered"),]
sla.wat = herbivory_just12 %>%
  group_by(species, treatment) %>%
  summarise_at(vars(herbivory.all), list(herb.se = ~std.error(.), 
                                         herb.mean = ~mean(., na.rm = TRUE)))

asterisk_data = sla.wat %>%
  dplyr::group_by(species) %>%
  dplyr::mutate(max_herb = pmax(ifelse(treatment == "control", herb.mean + herb.se, -Inf), 
                                 ifelse(treatment == "watered", herb.mean + herb.se, -Inf))) %>%
  dplyr::distinct(species, max_herb) %>%
  dplyr::mutate(asterisk_pos = max_herb + 0.5) 

asterisk_data = asterisk_data %>%
  dplyr::group_by(species) %>%
  dplyr::summarize(
    asterisk_pos = max(asterisk_pos)
  )
asterisk_data = data.frame(asterisk_data)
asterisk_data = asterisk_data[asterisk_data[,"species"] %in% c("ARPU","BOGR", "CEAM", "LUTE"),]

species_order_2 = sla.wat %>%
  filter(treatment == "control") %>%
  arrange(desc(herb.mean)) %>%
  pull(species)

sla.wat = sla.wat %>%
  mutate(species = factor(species, levels = species_order_2))

sla.wat.plot_2 = ggplot(sla.wat, aes(x = species, y = herb.mean)) +  
  geom_point(data = subset(sla.wat, treatment == "control"), 
             aes(color = treatment), size = 3) +  
  geom_errorbar(data = subset(sla.wat, treatment == "control"), 
                aes(ymin = herb.mean - herb.se, ymax = herb.mean + herb.se, color = treatment), 
                width = 0.2) +  
  geom_point(data = subset(sla.wat, treatment == "watered"), 
             aes(x = as.numeric(factor(species)) - 0.2, color = treatment), 
             size = 3) +  
  geom_errorbar(data = subset(sla.wat, treatment == "watered"), 
                aes(x = as.numeric(factor(species)) - 0.2, ymin = herb.mean - herb.se, ymax = herb.mean + herb.se, color = treatment), 
                width = 0.2) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  
        axis.ticks.x = element_line(color = "black")) + 
  ylab('Total herbivory per individual (%)') +
  xlab('Species') + 
  theme_bw(base_size = 12) +
  geom_text(data = asterisk_data, 
            aes(x = species, 
                y = asterisk_pos, 
                label = "*"), 
            size = 7, color = "black") + 
  scale_color_manual(values = c("control" = "black", "watered" = "gray"),
                     name = "Treatment",
                     labels = c("Nonwatered", "Watered"))

#ARPU, BOGR, CEAM, LUTE
t.test(herbivory.all ~ treatment, data = herbivory_just12, subset = species == "ARPU")
t.test(herbivory.all ~ treatment, data = herbivory_just12, subset = species == "BOCU")
t.test(herbivory.all ~ treatment, data = herbivory_just12, subset = species == "BOGR")
t.test(herbivory.all ~ treatment, data = herbivory_just12, subset = species == "CEAM")
t.test(herbivory.all ~ treatment, data = herbivory_just12, subset = species == "COTI")
t.test(herbivory.all ~ treatment, data = herbivory_just12, subset = species == "DEIL")
t.test(herbivory.all ~ treatment, data = herbivory_just12, subset = species == "GAPU")
t.test(herbivory.all ~ treatment, data = herbivory_just12, subset = species == "IPRU")
t.test(herbivory.all ~ treatment, data = herbivory_just12, subset = species == "LUTE")
t.test(herbivory.all ~ treatment, data = herbivory_just12, subset = species == "MOCI")
t.test(herbivory.all ~ treatment, data = herbivory_just12, subset = species == "MOPU")
t.test(herbivory.all ~ treatment, data = herbivory_just12, subset = species == "PHCO")

combined_plot = plot_grid(sla.wat.plot, sla.wat.plot_2, labels = c("A", "B"), ncol = 1, nrow = 2, align = "h")

ggsave("/Users/damlacinoglu/Desktop/final_bfl_data/figures/herbivory_diversity.png", combined_plot, 
       width = 12, height = 10, dpi = 300, units = "in")
       





