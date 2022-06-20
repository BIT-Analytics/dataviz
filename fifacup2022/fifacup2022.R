# 0. Library and fonts management
library(tidyverse)
library(polite)
library(rvest)
library(glue)
library(ragg)
library(rlang)
library(scales)
library(ggpath)
library(ggtext)

# 1. Data download, load and handling
## Gets the groups of the World Cup 2022
grouping <- readr::read_delim("fifacup2022/data.csv")

## Connects to the transfermarkt website to extract data on the teams
session <- polite::bow("https://www.transfermarkt.com") %>% 
  polite::nod(path = "statistik/weltrangliste/statistik")

## Initializes the object that will hold the data
data <- rep(NA, 7) %>% t() %>% as.data.frame()
colnames(data) <- c("rank", "country", "n_players", "age_mean",
                    "value_total", "confederation", "points")

## Scrapes data from the website
for (i in 1:3) {
  
  ### Extracts the table
  result <- session %>% 
    polite::scrape(query = list(ajax="yw1", datum="2022-06-06", page=i)) %>% 
    rvest::html_element("table.items") %>% 
    rvest::html_table(header = TRUE)
  
  ### Defines the names of the columns
  colnames(result) <- colnames(data)
  
  ### Binds the extracted results together
  data <- data %>% rbind(result)
  
}

## Eliminates the first NA row
data <- data %>% dplyr::slice(-1L)

## Joins the datasets
df <- grouping %>% 
  dplyr::left_join(data)

## Converts and adequates the value of the players to numeric values
df <- df %>% 
  dplyr::mutate(unit = str_detect(value_total, "m"),
                unit = ifelse(unit, 1E+09, 1E+06),
                value_total = stringr::str_remove_all(value_total,"(€|[:alpha:])"),
                value_total = as.numeric(value_total),
                value_total = unit*value_total)

## Gets the mean value of players of the teams
df <- df %>% 
  dplyr::mutate(n_players = as.numeric(n_players),
                value_mean = value_total/n_players) %>% 
  dplyr::select(-unit, -rank, -n_players, -value_total)

## Gets the path to the flag images
df <- df %>% 
  dplyr::mutate(path = tolower(country),
                path = stringr::str_replace_all(path, "[:space:]", "-"),
                path = glue::glue("fifacup2022/flags/flag-{path}.png"))

## Converts the groups to numeric
df <- df %>% 
  dplyr::mutate(order = factor(group),
                order = as.numeric(order))

## Gets the mean of the metrics for each group
grp_means <- df %>% 
  dplyr::group_by(order, group) %>% 
  dplyr::summarise(across(.cols = c(age_mean, points, value_mean),
                          .fns = mean))

## Defines some values for the plots
namevar <- tibble(
  variable = c("age_mean", "points", "value_mean"),
  name = c("mean age of players", "FIFA ranking points", "mean value of players"),
  color = c("#cc009c", "#0007cc", "#00cc36")
)

# 2. Generates the plots
for (i in 1:3) {
  
  ## Gets the variable and converts it to a symbol
  var <- namevar$variable[i]
  var <- rlang::sym(var)
  
  ## Defines the scale type of the plots
  if (i == 3) {
    label_fun <- label_number(
      scale_cut = cut_short_scale(),
      suffix = "<span style='font-size:10px;'> euros</span>"
    )
  } else {
    label_fun <- waiver()
  }
  
  ## Creates the plots
  p <- df %>% 
    ggplot() +
    
    ### Places a label to represent the mean value for each group
    geom_segment(aes(x = order, xend = order+0.4, y = !!var, yend = !!var),
                 linetype = "dashed", color = "#cc8100", size = 1,
                 data = grp_means) +
    geom_point(aes(x = order+0.4, y = !!var),
               size = 8, color = "#cc8100", data = grp_means) +
    geom_text(aes(x = order+0.4, y = !!var, label = group), fontface = "bold",
              family = "Libre Baskerville", size = 4, color = "white", data = grp_means) +
    
    ### Places the flags and a contour around them
    geom_point(aes(x = order, y = !!var), size = 14.5) +
    ggpath::geom_from_path(aes(x = order, y = !!var, path = path), width = 0.03) +
    
    ### Defines breaks and labels for the x-axis
    scale_x_continuous(breaks = 1:8, labels = LETTERS[1:8]) +
    
    ### Defines the labels for the y-axis
    scale_y_continuous(labels = label_fun) + 
    
    ### Defines the texts of the title, subtitle and caption
    labs(
      title = glue::glue("2022 Fifa World Cup groups by <span style='color:{namevar$color[i]};'>*{namevar$name[i]}*</span>"),
      subtitle = "The average value for each group is represented in <span style='color:#cc8100;'>orange</span>",
      caption = "Data from Transfermarkt as of June 6, 2022 | Graphic by Ícaro Bernardes (@IcaroBSC) | Inspired by @datatec_"
    ) +
    
    ### Eliminates and customizes theme elements
    theme_minimal() +
    theme(
      text = element_text(family = "Graystroke"),
      
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(color = "black", size = 0.8),
      panel.grid.major.y = element_blank(),
      
      plot.background = element_rect(fill = "white", color = NA),
      plot.margin = margin(20,20,20,20),
      plot.title = ggtext::element_markdown(
        size = 41, hjust = 1,
        margin = margin(10, 0, 10, 0)
      ),
      plot.subtitle = ggtext::element_markdown(
        size = 20, hjust = 1,
        margin = margin(0, 0, 20, 0)
      ),
      plot.caption = ggtext::element_markdown(
        size = 10, family = "Libre Baskerville",
        margin = margin(20, 0, 0, 0)
      ),
      
      axis.title = element_blank(),
      axis.text = ggtext::element_markdown(size = 15),
      axis.text.y = ggtext::element_markdown()
    )
  
  ## Saves the plots
  path <- namevar$variable[i]
  path <- glue::glue("fifacup2022/fifa22_{path}.png")
  ggsave(path, plot = p, dpi = "retina", width = 15, height = 9)
  
}

