# DS4B 101-R ----
# PRODUCT RECOMMENDATION FUNCTIONS

# 1.0 LIBRARIES ----
library(tidyverse)
library(tidyquant)
library(parsnip)
library(plotly)

source("01_Scripts/separate_bikes_and_outlier_detection.R")

bike_orderlines_tbl <- read_rds("00_Data/bike_sales/data_wrangled/bike_orderlines.rds")

models_tbl <- read_rds("00_models/parsnip_models_tbl.rds")

# Pro tips:
# Why build separate file in a scripts rather than Rmd for functions?
# [1] It makes my Rmd files easier to follow and read
# [2] It allows me to focus on writing good code in a .R file first, then souring the result 
# [3] When resolving problems, you often make functions you will eventually want to reuse.
# Having modular code makes it easier to source for new projects and convert into an R Package 

# 2.0 BIKE FEATURES ----

get_bike_features <- function() {
    
    bike_features_tbl <- bike_orderlines_tbl %>% 
        select(price, model, category_1, category_2, frame_material) %>% 
        distinct() %>% 
        mutate(id = row_number()) %>% 
        select(id, everything()) %>% 
        separate_bike_model(keep_model_column = TRUE, append = TRUE)
    
    return(bike_features_tbl)
    
}

get_bike_features()


# fct_reorder(category_2, price) reorders by median. The order groups is based 
# on the median within group price.We can change this behaviour by chaning the
# .fun = median argument (to max, min, sum, or whatever other aggregation 
# function you desire)

plot_bike_features <- function(interactive = TRUE) {
    
    # DATA MANIPULATION
    bike_features_tbl <- get_bike_features()
    
    # VISUALIZATION
    g <- bike_features_tbl %>% 
        mutate(category_2 = fct_reorder(category_2, price)) %>% 
        mutate(label_text = str_glue("Model: {model}
                                     Price: {scales::dollar(price)}")) %>% 
        ggplot(aes(category_2, price)) +
        geom_violin() +
        geom_jitter(aes(text = label_text), width = 0.1, colour = palette_light()[1], alpha = 0.5) +
        facet_wrap(~frame_material) +
        coord_flip() +
        theme_tq() +
        theme(strip.text.x = element_text(margin = margin(5, 5, 5, 5, unit = "pt"))) +
        labs(title = "Product Gap Analysis",
             x = "",
             y = "") +
        scale_y_continuous(labels = scales::dollar_format())
    
    # Interactive vs Static
    if (interactive) {
        return(ggplotly(g, tooltip = "text"))
    } else{
        return(g)
    }
   
}

plot_bike_features()
plot_bike_features(interactive = FALSE)

# Return() is not technically required because the function will by default return
# any output that is not assigned. However, it is good idea to use return() to 
# notify others (and further you) what your function is returning.

# 3.0 SAVE FUNCTIONS ----

function_names <- c("get_bike_features", "plot_bike_features")

dump(function_names, file = "01_Scripts/plot_product_recommendation.R")
