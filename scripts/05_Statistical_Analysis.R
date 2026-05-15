
#### read packages ####
library(arrow) # read parquet
library(dplyr) # manipulate data
library(tidyr) # transform to long format
library(tibble) # manipulate names of rows and columns
library(readr) # write csv

library(mgcv) # do GAM
library(ggplot2) # create ggplots
library(pheatmap) # create heatmap

#### read and manipulate data ####
# put the parquet name here
data <- arrow::read_parquet(".parquet")

# integrate Male and Female
data <- data %>%
  dplyr::mutate(Morphology = pmin(Abstract_Morphology +
                                  Specific_Morphology +
                                  Conceptual_Morphology, 1)) %>%
  dplyr::mutate(People = pmin(People_Male + People_Female, 1))

num_cols <- c("Morphology", "Geography", "People", "Other")

# divide into each phylum (exclude duplication of Phylum)
phylum <- unique(data$"Phylum") 

phylum_list <- lapply(phylum, function(x){ 
  dplyr::filter(data, Phylum == x)})
names(phylum_list) <- phylum

# define Cultural Class
country_class <- list(
  "European Names" = c(
    "Albania", "Australia", "Austria", "Belarus", "Belgium", "Bosnia and Herzegovina",
    "Bulgaria", "Canada", "Croatia", "Cyprus", "Czechia", "Czech Republic", "Denmark",
    "Estonia", "Finland", "France", "Germany", "Greece", "Hungary", "Iceland",
    "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg", "Malta", "Moldova",
    "Montenegro", "Netherlands", "New Zealand", "North Macedonia", "Norway", "Poland",
    "Portugal", "Romania", "Russia", "Serbia", "Slovakia", "Slovenia", "Spain",
    "Sweden", "Switzerland", "Ukraine", "United Kingdom", "United States"),
  "East Asian Names" = c(
    "Cambodia", "China", "Hong Kong", "Indonesia", "Japan", "Korea", "Laos", 
    "Malaysia", "Mongolia","Myanmar", "Philippines", "Singapore", "South Korea",
    "Taiwan", "Thailand", "Vietnam"),
  "South Asian Names" = c(
    "Afghanistan", "Bangladesh", "Bhutan", "India", "Nepal",
    "Pakistan", "Sri Lanka"),
  "African Names" = c(
    "Algeria", "Angola", "Benin", "Burkina Faso", "Burundi", "Cameroon",
    "Central African Republic", "Chad", "Congo", "Democratic Republic of the Congo",
    "Eswatini", "Ethiopia", "Gabon", "Gambia", "Ghana", "Guinea", "Ivory Coast",
    "Kenya", "Lesotho", "Libya", "Madagascar", "Malawi", "Mali", "Mauritius",
    "Mozambique", "Namibia", "Nigeria", "Republic of the Congo", "Rwanda", "Senegal", 
    "Somalia", "South Africa", "Sudan", "Tanzania", "Togo", "Uganda",
    "Zambia", "Zimbabwe"),
  "Latin American Names" = c(
    "Argentina", "Bolivia", "Brazil", "Chile", "Colombia", "Costa Rica", "Cuba",
    "Dominican Republic", "Ecuador", "El Salvador", "Guatemala", "Haiti", 
    "Honduras", "Mexico", "Nicaragua", "Panama", "Paraguay", "Peru",
    "Puerto Rico", "Suriname", "Trinidad and Tobago", "Uruguay", "Venezuela"),
  "Middle Eastern Names" = c(
    "Armenia", "Azerbaijan", "Egypt", "Fiji", "Georgia", "Iraq", "Iran", "Israel",
    "Jordan", "Kazakhstan", "Kuwait", "Kyrgyzstan", "Kuqait", "Lebanon", "Morocco",
    "Oman", "Palestine", "Papua New Guinea", "Qatar", "Saudi Arabia", "Syria",
    "Tajikistan", "Tunisia", "Turkey", "Turkmenistan", "Uzbekistan", "Yemen"))

country_to_class <- country_class %>%
  tibble::enframe(name = "Author_Class", value = "Country") %>% 
  tidyr::unnest(Country)

data <- data %>%
  dplyr::left_join(country_to_class, by = "Country")

#### validation ####
# Time period classication based on Burgin et al., 2025
animalia <- animalia %>%
  # create Period column
  dplyr::mutate(Period = dplyr::case_when(
    Year >= 1758 & Year <= 1880 ~ "1758-1880",
    Year >= 1881 & Year <= 1939 ~ "1881-1939",
    Year >= 1940 & Year <= 1999 ~ "1940-1999",
    Year >= 2000 ~ "2000-Present",
    TRUE ~ NA_character_)) %>%
  filter(!is.na(Period)) %>%
  mutate(Period = factor(Period,
                         levels = c("1758-1880", 
                                    "1881-1939",
                                    "1940-1999", 
                                    "2000-Present")))

# define categoris and period levels used in validation
val_categories <- c("Abstract_Morphology",
                    "Specific_Morphology",
                    "Conceptual_Morphology",
                    "Geography",
                    "People",
                    "Other")
period_levels <- c("1758-1880",
                   "1881-1939",
                   "1940-1999",
                   "2000-Present")

# set random seed for reproducibility
seed_val <- 42

## epithets
# convert dataset to long format and keep labeled entries
val_long <- animalia %>%
  dplyr::select(Species,
                Genus,
                Year,
                Period,
                Author,
                Phylum,
                Country,
                Author_Class,
                dplyr::all_of(val_categories)) %>%
  tidyr::pivot_longer(cols = dplyr::all_of(val_categories),
                      names_to = "Category",
                      values_to = "Flag") %>%
  dplyr::filter(Flag == 1) %>%
  dplyr::mutate(Period = factor(Period,
                                levels = period_levels),
                Category = factor(Category,
                                  levels = val_categories)) %>%
  dplyr::filter(!is.na(Period),
                !is.na(Category))

# randomize order within each Period × Category
set.seed(seed_val)

val_que <- val_long %>%
  dplyr::group_by(Period, Category) %>%
  dplyr::mutate(rand = sample.int(dplyr::n())) %>%
  dplyr::arrange(rand, .by_group = TRUE) %>%
  dplyr::mutate(random_order = dplyr::row_number()) %>%
  dplyr::ungroup() %>%
  dplyr::select(-rand)

# select top N samples per group for validation
val_topN <- val_que %>%
  dplyr::group_by(Period, Category) %>%
  # take first N samples per group (or fewer if not available)
  dplyr::slice_head(n = "put any number here" ) %>%
  dplyr::ungroup()

# prepare validation sheet with annotation columns
val_topN_sheet <- val_topN %>%
  dplyr::mutate(manual_evaluate = NA_character_,
                manual_label = NA_character_,
                agreement = NA_character_)

# export results
readr::write_csv(val_topN_sheet, "animalia_validation.csv")

## nationality
region_levels <- c("European Names",
                   "East Asian Names",
                   "South Asian Names",
                   "African Names",
                   "Latin American Names",
                   "Middle Eastern Names")

nat_long <- animalia %>%
  dplyr::filter(!is.na(Period),
                !is.na(Author_Class)) %>%
  dplyr::mutate(Period = factor(Period, levels = period_levels),
                Author_Class = factor(Author_Class, levels = region_levels)) %>%
  dplyr::filter(!is.na(Period),
                !is.na(Author_Class)) %>%
  dplyr::select(Year, Period, Author, Author_Class)

set.seed(seed_val)

nat_que <- nat_long %>%
  dplyr::group_by(Period, Author_Class) %>%
  dplyr::mutate(rand = sample.int(dplyr::n())) %>%
  dplyr::arrange(rand, .by_group = TRUE) %>%
  dplyr::ungroup() %>%
  
  dplyr::distinct(Author, .keep_all = TRUE) %>%
  dplyr::group_by(Period, Author_Class) %>%
  dplyr::mutate(random_order = dplyr::row_number()) %>%
  dplyr::ungroup() %>%
  dplyr::select(-rand)

nat_topN <- nat_que %>%
  dplyr::group_by(Period, Author_Class) %>%
  dplyr::slice_head(n = "put any number") %>%
  dplyr::ungroup()

nat_topN_sheet <- nat_topN %>%
  dplyr::mutate(manual_evaluate = NA_character_)

readr::write_excel_csv(nat_topN_sheet, "nationality_validation.csv")

## GAMs
gam_diag_all <- list(All = list(k = mgcv::gam.check(prop_all$model),
                                concurvity = mgcv::concurvity(prop_all$model, full = TRUE)))

gam_diag_phylum <- lapply(prop_phylum, function(x){
  if(is.null(x$model)) return(NULL)
  list(k = mgcv::gam.check(x$model),
       concurvity = mgcv::concurvity(x$model, full = TRUE))})

#### Common Functions ####
## Temporal Count
temp_count <- function(df, label = "All"){
  # calculate the number of species each category per year
  temporal_sum <- df %>%
    dplyr::group_by(Year) %>%
    dplyr::summarise(
      Morphology = sum(Morphology, na.rm = TRUE),
      People = sum(People, na.rm = TRUE),
      Geography = sum(Geography, na.rm = TRUE),
      Other = sum(Other, na.rm = TRUE),
      .groups = "drop") 
  temporal_long <- temporal_sum %>%
    tidyr::pivot_longer(cols = -Year,
                        names_to = "Category", 
                        values_to = "Count") %>%
    dplyr::mutate(
      Year = as.integer(Year),
      Count = as.numeric(Count),
      Category = factor(Category, 
                        levels = c("Morphology",
                                   "Geography",
                                   "People",
                                   "Other")))
  p_temporal <- ggplot2::ggplot(temporal_long,
                                ggplot2::aes(x = Year, y = Count)) +
    ggplot2::geom_line(linewidth = 0.7, na.rm = TRUE, color = "red") +
    ggplot2::facet_wrap(~ Category, 
                        scales = "fixed") +
    ggplot2::labs(x = "Year", y = "Count") +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "none")
  list(label = label,
       temporal_table = temporal_sum,
       temporal_long = temporal_long,
       plot = p_temporal)}

## Common helper for GAM
.prep_gam_data <- function(df, num_cols){
  df <- df %>% dplyr::filter(!is.na(Year))
  df <- df %>% dplyr::mutate(
    dplyr::across(
      dplyr::all_of(num_cols),
      \(x) as.numeric(x > 0)))
  GAM_long <- df %>%
    dplyr::select(Year, dplyr::all_of(num_cols)) %>%
    tidyr::pivot_longer(
      dplyr::all_of(num_cols),
      names_to = "Category", 
      values_to = "Presence")
  # calculate proportions
  summary_GAM <- GAM_long %>%
    dplyr::group_by(Year, Category) %>%
    dplyr::summarise(
      n = dplyr::n(),
      successes = sum(Presence, na.rm = TRUE),
      Proportion = successes / n,
      .groups = "drop") %>%
    dplyr::mutate(
      Year = as.numeric(Year),
      Category = factor(
        Category,
        levels = c("Morphology",
                   "Geography",
                   "People", 
                   "Other"))) %>%
    dplyr::filter(!is.na(Category))
  summary_GAM}

.fit_gam <- function(summary_GAM){
  # check the data
  if(nrow(summary_GAM) == 0L)
    return(list(model = NULL, 
                dispersion = NA_real_,
                error = "empty summary_GAM"))
  if(dplyr::n_distinct(summary_GAM$Year) < 3)
    return(list(model = NULL, 
                dispersion = NA_real_,
                error = "too few unique Year"))
  by_cat <- summary_GAM %>%
    dplyr::group_by(Category) %>%
    dplyr::summarise(yrs = dplyr::n_distinct(Year),
                     w = sum(n),
                     .groups = "drop")
  # keep the data that have sufficient samples
  keep <- by_cat$Category[by_cat$yrs >= 2 & by_cat$w >= 5]
  #define errors 
  if(length(keep) == 0)
    return(list(model = NULL, 
                dispersion = NA_real_,
                error = "no category passes minimum data"))
 
   summary_GAM <- dplyr::filter(summary_GAM, Category %in% keep)
  
  # try catch
  fit <- tryCatch({
    mgcv::gam(
      Proportion ~ s(Year, by = Category, k = 10) + Category,
      family = quasibinomial(link = "logit"),
      weights = n,
      data = summary_GAM,
      method = "REML")},

    # if make error
    error = function(e){ 
    return(structure(list(.err = conditionMessage(e)),
                     class = "gam_fit_error"))})
  if(inherits(fit, "gam_fit_error"))
    return(list(model = NULL,
                dispersion = NA_real_,
                error = fit$.err))
  # if success fit
  list(model = fit,
       dispersion = stats::deviance(fit) / stats::df.residual(fit),
       error = NULL)}

# define colors, line-types, and shapes
.category_colors <- c("Abstract_Morphology" = "#A52A00",
                      "Geography" = "darkblue",
                      "People" = "#4B0082",
                      "Other" = "darkcyan")

.category_line <- c("Morphology" = "solid",
                    "Geography" = "dashed",
                    "People" = "dotdash",
                    "Other" = "dotted")

.category_shape <- c("Morphology" = 16,
                     "Geography" = 17,
                     "People" = 3,
                     "Other" = 15)

## GAM
gam <- function(df, num_cols, label = "All"){
  summary_GAM <- .prep_gam_data(df, num_cols)
  fit_info <- .fit_gam(summary_GAM)
  
  # if make error
  if(is.null(fit_info$model)){
    message(sprintf("[gam:%s] model = NULL; reason: %s",
                    label,
                    ifelse(is.null(fit_info$error), "unknown",
                           fit_info$error))) 
    return(list(label = label, 
                model = NULL,
                dispersion = NA_real_,
                table = summary_GAM, 
                pred_table = NULL,
                plot = NULL))}
 
  # if success fit
  pred_resp <- stats::predict(fit_info$model, 
                              newdata = summary_GAM,
                              type = "response",
                              se.fit = TRUE)
  pred_tbl <- summary_GAM %>%
    dplyr::mutate(Predicted = pred_resp$fit,
                  Lower_CI = pmax(0, Predicted - 1.96 * pred_resp$se.fit),
                  Upper_CI = pmin(1, Predicted + 1.96 * pred_resp$se.fit))
  
  p_prop <- ggplot2::ggplot(pred_tbl, 
                            ggplot2::aes(x = Year)) +
    ggplot2::geom_point(ggplot2::aes(y = Proportion, 
                                     color = Category, 
                                     shape = Category),
                        alpha = 0.25, size = 0.7) +
    ggplot2::geom_line(data = pred_tbl,
                       ggplot2::aes(y = Predicted,
                                    color = Category,
                                    linetype = Category),
                       linewidth = 0.9, na.rm = TRUE) +
    ggplot2::geom_ribbon(data = pred_tbl,
                         ggplot2::aes(ymin = Lower_CI, ymax = Upper_CI,
                                      fill = Category),
                        alpha = 0.18, color = NA) +
    ggplot2::scale_color_manual(values = .category_colors) +
    ggplot2::scale_fill_manual(values = .category_colors) +
    ggplot2::scale_linetype_manual(values = .category_line) +
    ggplot2::scale_shape_manual(values = .category_shape) +
    ggplot2::labs(x = "Year", y = "Proportion") +
    ggplot2::theme_minimal()

  list(label = label,
       model = fit_info$model,
       dispersion = fit_info$dispersion,
       table = summary_GAM,
       pred_table = pred_tbl,
       plot = p_prop)}

#### Visualization ####
## Temporal Count
temp_all <- temp_count(data, label = "All")

temp_phylum <- lapply(
  names(phylum_list),
  \(p) {temp_count(phylum_list[[p]], 
                   label = p)})
names(temp_phylum) <- names(phylum_list)
  # temp_phylum[["name of phylum"]]$plot

## GAM
prop_all <- gam(data, num_cols, label = "All")
prop_all$plot

prop_phylum <- lapply(
  names(phylum_list),
  \(p){gam(phylum_list[[p]],
                num_cols = num_cols,
                label = p)})
  # prop_phylum[["name of phylum"]]$plot
names(prop_phylum) <- names(phylum_list)

#### phylum × Category ####
phylum_profile <- data %>%
  dplyr::filter(!is.na(Phylum)) %>% 
  dplyr::group_by(Phylum) %>%
  dplyr::summarise(
    n = dplyr::n(),
      dplyr::across(dplyr::all_of(num_cols),
                    \(x) mean(x, na.rm = TRUE)),
      .groups = "drop") %>%  
  dplyr::filter(n >= 100) %>% # exclude phylum that include species fewer than 100
  dplyr::arrange(dplyr::desc(n))

phylum_mat <- phylum_profile %>%
  dplyr::select(Phylum, dplyr::all_of(num_cols)) %>%
  tibble::column_to_rownames("Phylum") %>%
  as.matrix()
    
heat_phylum <- pheatmap::pheatmap(
  phylum_mat,
  scale = "none",
  angle_col = 45,
  color = colorRampPalette(c("#FFF5F7", "#F768A1", "#7A003C"))(100),
  fontsize_row = 6)

#### Cultural Class × Category ####
Author_Class_profile <- data %>%
  dplyr::filter(!is.na(Author_Class)) %>%
  dplyr::group_by(Author_Class) %>%
  dplyr::summarise(dplyr::across(dplyr::all_of(num_cols),
                                 \(x) mean(x, na.rm = TRUE)),
                   .groups = "drop")

Author_Class_mat <- Author_Class_profile %>%
  dplyr::select(Author_Class, dplyr::all_of(num_cols)) %>%
  tibble::column_to_rownames("Author_Class") %>%
  as.matrix()
Author_Class_mat[is.na(Author_Class_mat)] <- 0

heat_class <- pheatmap::pheatmap(
  Author_Class_mat,
  scale = "none",
  angle_col = 45,
  color = colorRampPalette(c("#FFF5F7", "#F768A1", "#7A003C"))(100))

#### Stacking plot of Cultural Class ####
time_class <- data %>%
  dplyr::filter(!is.na(Year), !is.na(Author_Class)) %>%
  dplyr::count(Year, Author_Class, name = "n")

ratio_class <- time_class %>%
  dplyr::group_by(Year) %>%
  dplyr::mutate(
    prop = n / sum(n)) %>%
  dplyr::ungroup()

class_check <- ratio_class %>%
  dplyr::group_by(Year) %>%
  dplyr::summarise(total_prop = sum(prop))

full_class <- ratio_class %>%
  tidyr::complete(Year, Author_Class, fill = list(prop = 0))

# define colors
class_colors <- c(
  "European Names"       = "#1A1A1A",
  "East Asian Names"     = "#4E79A7",
  "Latin American Names" = "#E15759",
  "Middle Eastern Names" = "#76B7B2",
  "South Asian Names"    = "#F28E2B",
  "African Names"        = "#EDC948")


temp_class <- ggplot2::ggplot(full_class,
                              ggplot2::aes(x = Year, y = prop,
                                           fill = Author_Class)) +
  ggplot2::geom_area(alpha = 0.8) +
  ggplot2::scale_fill_manual(values = class_colors) +
  ggplot2::scale_y_continuous(labels = scales::percent_format()) +
  ggplot2::labs(x = "Year",
                y = "Proportion",
                fill = "Cultural Class") +
  ggplot2::theme_minimal()
