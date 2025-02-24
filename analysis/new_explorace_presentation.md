library(RSQLite)
library(dplyr)
library(ggplot2)
library(summarytools)
library(GGally)
library(readr)
library(rlang)
library(stringr)
library(gridExtra)
library(bipartite)
library(tidyr)
library(reshape2)
library(vegan)

# Define functions: ------------------------------------------------------------

# This function cleans a dataset of rows containing NAs in any of the columns 
# defined by either simple string aor a list of column name strings
clean_dataframe <- function(df, column_names) {
  # Ensure column_names is a character vector
  if (!is.character(column_names)) {
    stop("column_names must be a character vector or a single string.")
  }
  
  # Check if all column names are valid
  if (!all(column_names %in% names(df))) {
    stop("One or more column names are not in the dataframe.")
  }
  
  # Create a logical matrix for non-NA values in specified columns
  na_matrix <- sapply(df[, column_names, drop = FALSE], function(x) !is.na(x))
  
  # Create a vector indicating rows with no NA in any of the specified columns
  no_na_rows <- apply(na_matrix, 1, all)
  
  # Clean the dataframe by keeping only rows with no NA in specified columns
  cleaned_df <- df[no_na_rows, ]
  
  return(cleaned_df)
}

# Export modified datasets: ----------------------------------------------------
  ## To csv: ----------
write.csv(plants_with_indices, "species_traits_d.csv")

# Get Data from database: ------------------------------------------------------
  ## Get data from Combined table in the SQLite db: ----------------------------
setwd("D:/Dílna/Studium/DP - Bio/Diplomka - Analyses")
setwd("~/Rko/diplomka/diplomka-bio")
db_path <- "Data/Dataset.db"
con <- dbConnect(RSQLite::SQLite(), dbname = db_path)

# Set query
query <- "SELECT * FROM traits_unique_per_network WHERE species IS NOT NULL"  # Adjust your SELECT query as needed
df <- dbGetQuery(con, query)
dbDisconnect(con)

# Preliminary summary
df_summary <- summarytools::dfSummary(df)
summarytools::view(df_summary)

# Continuous variables: --------------------------------------------------------
  ## Boxplots for season and elevation: ----------------------------------------------------------
  
  # Removed duplicates based on sp_code.
  unique_species_across_df <- df %>% distinct(sp_code, .keep_all = TRUE)
  
  # Define a vector of strings
  list_of_strings <- tail(names(unique_species_across_df), 8)
  
  # Store each plot in a list
  plot_list <- list()
  
  # Loop over each string in the list
  for (i in 1:length(list_of_strings)) {
    
    # Clean dataset
    cleaned_df <- clean_dataframe(unique_species_across_df, list_of_strings[[i]])
    print(nrow(cleaned_df))
    
    # Prepare strings
    metric <- list_of_strings[[i]]
    label_name <- str_to_title(gsub("_", " ", metric))
    elev <- as.factor(cleaned_df$elevation)
    
    # Prepare plots
    plot_list[[i]] <- ggplot(cleaned_df, aes_string(x="elev", y=metric, color="season")) +
      geom_boxplot() +
      labs(x="Elevation", y=label_name, title = paste(label_name, "Distribution (All)")) +
      theme_classic()
    print(plot_list[[i]])
    
  } 
  
  ## Basic histograms for continuous traits across entire data sets: ---------------------------------------------
  
  # Removed duplicates based on sp_code.
  unique_species_across_df <- df %>% distinct(sp_code, .keep_all = TRUE)
  
  # Define a vector of strings
  list_of_strings <- tail(names(unique_species_across_df), 8)
  
  # Store each plot in a list
  plot_list <- list()
  
  # Loop over each string in the list
  for (i in 1:length(list_of_strings)) {
    
    # Clean dataset
    cleaned_df <- clean_dataframe(unique_species_across_df, list_of_strings[[i]])
    print(nrow(cleaned_df))
    
    # Prepare strings
    metric <- list_of_strings[[i]]
    label_name <- str_to_title(gsub("_", " ", metric))
    
    # Prepare plots
    plot_list[[i]] <- ggplot(cleaned_df, aes_string(x=metric)) +
      geom_histogram(bins=20, fill="forestgreen", color="white") +
      labs(x=label_name, y="Frequency") +
      theme_minimal() + labs(title = paste(label_name, "Distribution (All)")) + theme(
        panel.background = element_rect(fill = "white", colour = NA), # Set the background color here
        plot.background = element_rect(fill = "white", colour = NA) # Optional: Set the plot margin color he
      )
    
  } 
  
  # Arrange the plots in two rows and four columns
  grid.arrange(grobs=plot_list, nrow=2, ncol=4)
  
  ## Basic histograms for continuous traits for every season: ------------------------------------------------
  
  # Prepare data frames for each season
  unique_species_dry <- filter(df, season == "DRY") %>% distinct(sp_code, .keep_all = TRUE)
  unique_species_wet <- filter(df, season == "WET") %>% distinct(sp_code, .keep_all = TRUE)
  
  # Histograms
  list_of_dataframes <- list(unique_species_dry, unique_species_wet)
  color_list <- list("orange", "steelblue")
  season_list <- list("DRY", "WET")
  
  for (i in seq_along(list_of_dataframes)) {
    
    # Current dataframe
    current_df <- list_of_dataframes[[i]]
    
    # Define a vector of strings
    list_of_strings <- tail(names(current_df), 8)
    
    # Store each plot in a list
    plot_list <- list()
    
    # Loop over each string in the list
    for (j in 1:length(list_of_strings)) {
      
      # Clean dataset
      cleaned_df <- clean_dataframe(current_df, list_of_strings[[j]])
      print(nrow(cleaned_df))
      
      # Prepare strings
      metric <- list_of_strings[[j]]
      label_name <- str_to_title(gsub("_", " ", metric))
      
      # Prepare plots
      plot_list[[j]] <- ggplot(cleaned_df, aes_string(x=metric)) +
        geom_histogram(bins=20, fill=color_list[[i]], color="white") +
        labs(x=label_name, y="Frequency") +
        theme_minimal() + labs(title = paste(label_name, "Distribution", season_list[[i]])) + theme(
          panel.background = element_rect(fill = "white", colour = NA), # Set the background color here
          plot.background = element_rect(fill = "white", colour = NA) # Optional: Set the plot margin color he
        )
      
    } 
    
    # Arrange the plots in two rows and four columns
    grid.arrange(grobs=plot_list, nrow=2, ncol=4)
    
  }
  
  ## Basic histograms for continuous traits, per elevation, per season: -------------------------------------------
  
  # Prepare data frames for each season
  unique_species_dry <- filter(df, season == "DRY") %>% distinct(sp_code, .keep_all = TRUE)
  unique_species_wet <- filter(df, season == "WET") %>% distinct(sp_code, .keep_all = TRUE)
  
  # Prepare a list of dataframes pee season and elevation
  elevation_list <- list(filter(df, elevation == 650), filter(df, elevation == 1100), filter(df, elevation == 1450), filter(df, elevation == 2250))
  elevation_list_dry <- list(filter(df, elevation == 650, season == "DRY"), filter(df, elevation == 1100, season == "DRY"), filter(df, elevation == 1450, season == "DRY"), filter(df, elevation == 2250, season == "DRY"))
  elevation_list_wet <- list(filter(df, elevation == 650, season == "WET"), filter(df, elevation == 1100, season == "WET"), filter(df, elevation == 1450, season == "WET"), filter(df, elevation == 2250, season == "WET"))
  list_of_dataframes <- elevation_list
  
  
  # Prepare lists of colors and season labels
  color_list <- list("orange", "steelblue")
  season_list <- list("DRY", "WET")
  elevation_list_strings <- as.list(as.character(unique(df$elevation)))
  
  # Define a list of strings
  list_of_traits <- as.list(tail(names(df), 8))
  
  #For each trait
  for (i in seq_along(list_of_traits)) {
    
    # Store each plot in a list
    plot_list <- list()
    counter <- 1
    
    #For each elevation
    for (j in seq_along(elevation_list)) {
      
      # Current dataframe
      current_df <- elevation_list[[j]]
      
      # Prepare strings
      metric <- list_of_strings[[i]]
      label_name <- str_to_title(gsub("_", " ", metric))
      
      for (k in seq_along(season_list)) {
        
        filtered_df <- filter(current_df, season == season_list[[k]])
        
        # Prepare plots
        plot_list[[counter]] <- ggplot(filtered_df, aes_string(x=metric)) +
          geom_histogram(bins=20, fill=color_list[[k]], color="white") +
          labs(x=label_name, y="Frequency") +
          theme_minimal() + labs(title = paste("(", elevation_list_strings[[j]], ")", label_name, "Distribution", season_list[[k]])) + theme(
            panel.background = element_rect(fill = "white", colour = NA), # Set the background color here
            plot.background = element_rect(fill = "white", colour = NA) # Optional: Set the plot margin color he
          )
        
        counter <- counter + 1
      }
      
    }
    
    # Arrange the plots in two rows and four columns
    grid.arrange(grobs=plot_list, nrow=4, ncol=2)
    
  }
  
  
  ## Elaborate Size and Tube length plot ---------------------------------------------------------------------
  
  
  library(ggplot2)
  library(grid)
  library(png)
  
  # Prepare data frames for each season
  unique_species_dry <- filter(unique_species_across_df, season == "DRY")
  unique_species_wet <- filter(unique_species_across_df, season == "WET")
  
  
  # Assuming you have a PNG image named "background.png" in your working directory
  background <- readPNG("./img/tube_bg.png")
  g_background <- rasterGrob(background, interpolate=TRUE)
  
  # Create two histograms
  # Determine common scale for the size variable
  common_bins <- 10
  common_limit <- range(c(unique_species_wet$size, unique_species_dry$size), na.rm = TRUE)
  common_breaks <- seq(from = common_limit[1], to = common_limit[2], length.out = common_bins + 1)
  
  integer_breaks_x <- seq(floor(min(unique_species_wet$size)), 
                          ceiling(common_limit[2]-2), 
                          by = 1)
  
  # Histogram for wet conditions with common scale
  hist1a <- ggplot(unique_species_wet, aes(x=size)) +
    geom_histogram(fill="#2f2421", color="#fafcf4", bins=common_bins) +
    theme_void() +
    scale_x_continuous(limits = common_limit, breaks = integer_breaks_x, labels=integer_breaks_x) +
    theme(
      plot.subtitle = element_text(hjust = 0.5),
      axis.text.x = element_text(hjust = 3, vjust = 0.5),
      panel.grid.major.y = element_line(color = "#2f2421", size = 0.5), # Customize major Y grid lines (horizontal)
      #panel.grid.minor.y = element_line(color = "#2f2421", size = 0.25), # Customize minor Y grid lines (horizontal)
      panel.grid.major.x = element_line(color = "#2f2421", size = 0.5), # Customize major X grid lines (vertical)
      #panel.grid.minor.x = element_line(color = "#2f2421", size = 0.25) # Customize minor X grid lines (vertical)
    ) + 
    labs(subtitle = "Wet Season Flower Size")
  
  integer_breaks_x <- seq(floor(1), 
                          ceiling(common_limit[2]-2), 
                          by = 1)
  
  # Histogram for dry conditions with common scale
  hist1b <- ggplot(unique_species_dry, aes(x=size)) +
    geom_histogram(fill="#2f2421", color="#fafcf4", bins=common_bins) +
    theme_void() +
    #scale_x_continuous(limits = rev(common_limit), breaks = rev(common_breaks)) +  # Reverse the limits and breaks for mirroring
    scale_x_reverse(limits = rev(common_limit), breaks = rev(integer_breaks_x), labels=rev(integer_breaks_x)) +  # This will flip the x-axis
    theme(
      plot.subtitle = element_text(hjust = 0.5),
      axis.text.x = element_text(hjust = 3, vjust = 0.5),
      panel.grid.major.y = element_line(color = "#2f2421", size = 0.5), # Customize major Y grid lines (horizontal)
      #panel.grid.minor.y = element_line(color = "#2f2421", size = 0.25), # Customize minor Y grid lines (horizontal)
      panel.grid.major.x = element_line(color = "#2f2421", size = 0.5), # Customize major X grid lines (vertical)
      #panel.grid.minor.x = element_line(color = "#2f2421", size = 0.25) # Customize minor X grid lines (vertical)
    ) + 
    labs(subtitle = "Dry Season Flower Size")
  
  # Ensure both plots are displayed with the same y-axis limits if they are side by side
  common_y_limit <- range(
    c(
      ggplot_build(hist1a)$layout$panel_params[[1]]$y.range,
      ggplot_build(hist1b)$layout$panel_params[[1]]$y.range
    )
  )
  
  hist1a <- hist1a + scale_y_continuous(limits = common_y_limit)
  hist1b <- hist1b + scale_y_continuous(limits = common_y_limit)
  
  # Determine common scale for the size variable
  common_bins <- 10
  common_limit <- range(c(unique_species_wet$tube_length, unique_species_dry$tube_length), na.rm = TRUE)
  common_breaks <- seq(from = common_limit[1], to = common_limit[2], length.out = common_bins + 1)
  common_limit_y <- range(0,10)
  common_breaks_y <- seq(from = common_limit_y[1], to = common_limit_y[2], length.out = common_bins + 1)
  
  integer_breaks_x <- seq(floor(min(unique_species_wet$tube_length)), 
                          ceiling(common_limit[2]), 
                          by = 1)
  
  hist2a <- ggplot(unique_species_wet, aes(x=tube_length)) +
    geom_histogram(fill="#2f2421", color="#fafcf4", bins=common_bins) +
    coord_flip() + # Flip the axes to make histogram horizontal
    theme_void()  +
    scale_x_reverse(limits = rev(common_limit), breaks = rev(integer_breaks_x), labels = rev(integer_breaks_x)) + # Invert the x-axis values, which are now vertical due to coord_flip()
    scale_y_continuous(limits = common_limit_y, breaks = common_breaks_y) +
    # Keep the void theme
    theme(
      plot.subtitle = element_text(hjust = 0.5),
      axis.text.y = element_text(hjust = 3, vjust = 0.5),
      panel.grid.major.y = element_line(color = "#2f2421", size = 0.5), # Customize major Y grid lines (horizontal)
      #panel.grid.minor.y = element_line(color = "#2f2421", size = 0.25), # Customize minor Y grid lines (horizontal)
      panel.grid.major.x = element_line(color = "#2f2421", size = 0.5), # Customize major X grid lines (vertical)
      #panel.grid.minor.x = element_line(color = "#2f2421", size = 0.25) # Customize minor X grid lines (vertical)
    ) + 
    labs(subtitle = "Wet Season Tube Length")
  
  integer_breaks_x <- seq(floor(min(unique_species_dry$tube_length)), 
                          ceiling(common_limit[2]), 
                          by = 1)
  
  hist2b <- ggplot(unique_species_dry, aes(x=tube_length)) +
    geom_histogram(fill="#2f2421", color="#fafcf4", bins=common_bins) +
    coord_flip() + # Flip the axes to make histogram horizontal
    theme_void()  +
    scale_x_reverse(limits = rev(common_limit), breaks = rev(integer_breaks_x), labels = rev(integer_breaks_x), position="top") + # Invert the x-axis values, which are now vertical due to coord_flip()
    scale_y_reverse(limits = rev(common_limit_y), breaks = rev(common_breaks_y)) +
    theme(
      plot.subtitle = element_text(hjust = 0.5),
      axis.text.y = element_text(hjust = 3, vjust = 0.5),  # Add this line to bring back x-axis text (which is actually y-axis due to coord_flip)
      panel.grid.major.y = element_line(color = "#2f2421", size = 0.5), # Customize major Y grid lines (horizontal)
      #panel.grid.minor.y = element_line(color = "#2f2421", size = 0.25), # Customize minor Y grid lines (horizontal)
      panel.grid.major.x = element_line(color = "#2f2421", size = 0.5), # Customize major X grid lines (vertical)
      #panel.grid.minor.x = element_line(color = "#2f2421", size = 0.25) # Customize minor X grid lines (vertical)
    ) + 
    labs(subtitle = "Dry Season Tube Length")
  
  # Ensure both plots are displayed with the same y-axis limits if they are side by side
  #
  
  # Create a base plot with the background image
  base_plot <- ggplot() +
    annotation_custom(g_background, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) +
    theme_void()  # Keep the void theme
  
  # Horizontal line grob
  y_pos = 0.775
  hline_grob <- linesGrob(
    x = unit(c(0.2, 0.8), "npc"),  # Span from the left to the right of the plot
    y = unit(c(y_pos, y_pos), "npc"),  # Positioned at the middle, change this value as needed
    gp = gpar(
      col = "#2f2421",      # Line color
      lwd = unit(0.5, "mm")   # Line width in millimeters
    )
  )
  
  # Horizontal line grob
  y_pos = 0.375
  hline_grob_2 <- linesGrob(
    x = unit(c(0.2, 0.45), "npc"),  # Span from the left to the right of the plot
    y = unit(c(y_pos, y_pos), "npc"),  # Positioned at the middle, change this value as needed
    gp = gpar(
      col = "#2f2421",      # Line color
      lwd = unit(0.5, "mm")   # Line width in millimeters
    )
  )
  
  # Horizontal line grob
  y_pos = 0.375
  hline_grob_3 <- linesGrob(
    x = unit(c(0.55, 0.8), "npc"),  # Span from the left to the right of the plot
    y = unit(c(y_pos, y_pos), "npc"),  # Positioned at the middle, change this value as needed
    gp = gpar(
      col = "#2f2421",      # Line color
      lwd = unit(0.5, "mm")   # Line width in millimeters
    )
  )
  
  
  
  # Vertical line grob
  x_pos = 0.485
  vline_grob <- linesGrob(
    x = unit(c(x_pos, x_pos), "npc"),  # Positioned at the middle, change this value as needed
    y = unit(c(0.75, 0.9), "npc"),  # Span from the bottom to the top of the plot
    gp = gpar(
      col = "#2f2421",     # Line color
      lwd = unit(0.5, "mm")   # Line width in millimeters
    )
  )
  
  
  
  # Add the histograms onto the base plot
  # Here you need to determine the placement coordinates for the histograms
  final_plot <- base_plot +
    annotation_custom(ggplotGrob(hist1a), xmin=0.5, xmax=0.8, ymin=0.76, ymax=1) +
    annotation_custom(ggplotGrob(hist1b), xmin=0.2233, xmax=0.5233, ymin=0.76, ymax=1) +
    annotation_custom(ggplotGrob(hist2a), xmin=0.5, xmax=0.77, ymin=0.0, ymax=0.425) +
    annotation_custom(ggplotGrob(hist2b), xmin=0.195, xmax=0.465, ymin=0.0, ymax=0.425) +
    annotation_custom(hline_grob) +
    annotation_custom(hline_grob_2) +
    annotation_custom(hline_grob_3) +
    annotation_custom(vline_grob)
  
  
  print(final_plot)
  
  
  
  
  
# Categorical variables: -------------------------------------------------------  
  ## Barplots of categorials across elevation: -----------------------------------------------------------------------

# Prepare lists of colors and season labels
color_list <- list("orange", "steelblue")
season_list <- list("DRY", "WET")
elevation_list_strings <- as.list(c("650", "1100", "1450", "2250"))

# Define a list of strings
list_of_traits <- names(df)[5:13]

for (i in seq_along(list_of_traits)) {
  
  plot_list <- list()
  
  # Prepare strings
  metric <- list_of_traits[[i]]
  label_name <- str_to_title(gsub("_", " ", metric))
  
  #For each elevation
  for (j in seq_along(elevation_list_strings)) {
    
    plot_list[[j]] <- ggplot(filter(df, elevation == as.numeric(elevation_list_strings[[j]])), aes_string(x=metric, fill="season")) +
      geom_bar(position="dodge") +
      labs(x=paste(label_name, elevation_list_strings[[j]]), y="Count of Species") +
      theme_minimal()
  }
  
  # Arrange the plots in two rows and four columns
  grid.arrange(grobs=rev(plot_list), nrow=4, ncol=1)
  
}
--------------------------------------
  
  
# Species Richness: ------------------------------------------------------------
  ## Number of species in each network (elevation*season): -----------------------------------------------------
  
  # Species count per elevation and season
  species_count_per_network <- df %>% 
    count(elevation, season)
  
  # Define custom colors for seasons
  season_colors <- c("DRY" = "orange", "WET" = "steelblue")
  
  # Custom labels for elevation axis
  elevation_labels <- c("650" = "650m asl", "1100" = "1100m asl", "1450" = "1450m asl", "2250" = "2250m asl")
  
  # Create the plot with specified customizations
  ggplot(species_count_per_network, aes(x = factor(elevation), y = n, fill = season)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
    scale_fill_manual(values = season_colors) +
    scale_x_discrete(labels = elevation_labels) +  # Use custom labels for elevation axis
    labs(x = "Elevation", y = "Number of Unique Species", fill = "Season") +
    geom_text(aes(label = n), position = position_dodge(width = 0.9), vjust = -0.25) +
    theme_minimal()
  
  ## Turnover of species across elevations: ------------------------------------------------
  
  library(tidyr)
  library(dplyr)
  library(reshape2)
  
  # Filter df by season - do not get rid of duplicates - they will be used
  dry_df <- filter(df, season == "DRY")
  wet_df <- filter(df, season == "WET")
  
  # Turn the elevation column into a wide format, ready for melting
  plant_species_wide_dry <- dry_df %>%
    mutate(Presence = 1) %>%
    spread(key = elevation, value = Presence, fill = 0)
  
  plant_species_wide_wet <- wet_df %>%
    mutate(Presence = 1) %>%
    spread(key = elevation, value = Presence, fill = 0)
  
  # Create a unique identifier that combines species code and season
  df_1 <- df %>%
    unite(col = "sp_season_code", c("sp_code", "season"), sep = "_")
  
  plant_species_wide <- df_1 %>%
    mutate(Presence = 1) %>%
    spread(key = elevation, value = Presence, fill = 0)
  
  # Melt the data frame for ggplot2 heatmap
  species_melted_dry <- melt(select(plant_species_wide_dry, "sp_code", "650", "1100", "1450", "2250"), value.name = "elevation", id.vars = c("sp_code"))
  species_melted_wet <- melt(select(plant_species_wide_wet, "sp_code", "650", "1100", "1450", "2250"), value.name = "elevation", id.vars = c("sp_code"))
  species_melted <- melt(select(plant_species_wide, "sp_season_code", "650", "1100", "1450", "2250"), value.name = "elevation", id.vars = c("sp_season_code"))
  
  # Separate the combined species and season code back into two columns
  species_melted <- species_melted %>%
    separate(col = sp_season_code, into = c("sp_code", "season"), sep = "_")
  
  # Plotting the heatmap for species turnover
  ggplot(species_melted, aes(x = variable, y = sp_code, fill = elevation)) +
    geom_tile(color = "white") +
    scale_fill_gradient(low = "white", high = "steelblue") +
    facet_wrap(~season, ncol = 1) + # Separate by Season
    theme_minimal() +
    labs(x = "Elevation", y = "Species", fill = "Presence") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  ## Create UPSET plot to visualize turnover: ------------------------------------------------------------------------------
  
  install.packages("UpSetR")
  library(UpSetR)
  detach("package:ComplexUpset", unload=TRUE)
  
  
  df_2 <- df %>%
    mutate(Presence = 1)
  
  # Now, create a unique identifier for each combination of season and elevation
  df_2 <- df_2 %>%
    unite("SeasonElevation", season, elevation, sep = "_")
  
  # Next, pivot the data to wide format to get binary indicators
  species_binary_matrix <- df_2 %>%
    pivot_wider(
      id_cols = sp_code,
      names_from = SeasonElevation,
      values_from = Presence,
      values_fill = list(Presence = 0) # fill absent combinations with 0
    )
  
  species_binary_matrix <- data.frame(lapply(species_binary_matrix[,-1], function(x) as.numeric(as.character(x))))
  # Assuming df is your data frame and 'column_name' is the name of the column you want to turn into row names
  row.names(species_binary_matrix) <- species_binary_matrix$sp_code
  
  # After setting the row names, you might want to remove that column from the dataframe
  species_binary_matrix$sp_code <- NULL
  
  
  upset(species_binary_matrix, sets = c("DRY_650","DRY_1100", "DRY_1450", "DRY_2250"), keep.order=TRUE, set_size.show = TRUE)
  upset(species_binary_matrix, sets = c("WET_650","WET_1100", "WET_1450", "WET_2250"), keep.order=TRUE, set_size.show = TRUE)
  
# Specialization quantification: -----------------------------------------------
  ## Specialization indices: -------------------------------------------------------------------------------------------

  library(bipartite)

  ## Get data from the database
  db_path <- "Data/Dataset.db"
  con <- dbConnect(RSQLite::SQLite(), dbname = db_path)
  
  # Set query
  query <- "SELECT * FROM combined WHERE species IS NOT NULL"  # Adjust your SELECT query as needed
  visits <- dbGetQuery(con, query)
  dbDisconnect(con)

  # Filter visists
  visits <- visits[,4:62]
  visits <- filter(visits, include_in_network == 1)

  # Prepare data frames for each season
  visits_dry <- filter(visits, season == "DRY")
  visits_wet <- filter(visits, season == "WET")
  
  # Lists fro iterations
  list_of_dataframes <- list(visits_dry, visits_wet)
  color_list <- list("orange", "steelblue")
  season_list <- list("DRY", "WET")
  elevation_list_strings <- as.list(as.character(unique(visits$elevation)))
  
  # Lists of webs
  webs_order <- list()
  webs_group <- list()
  
  #For every season
  for (i in seq_along(list_of_dataframes)) {
    
    # Current dataframe
    current_df <- list_of_dataframes[[i]]
    
    webs_order[[i]]<-frame2webs(current_df, varnames = c("sp_code", "insect_order", "elevation"), type.out = "list", emptylist = TRUE)
    webs_group[[i]]<-frame2webs(current_df, varnames = c("sp_code", "functional_group", "elevation"), type.out = "list", emptylist = TRUE)
    
  }
  
  # Visualize web - select manually whether order or group
  for (i in 1:2) {
    for(j in 1:4) {
      
      web <- webs_order[[i]][[j]]
      
      plotweb(web, method = "cca", empty = TRUE, labsize = 1, ybig = 1, y.width.low = 0.1, 
              y.width.high = 0.1, low.spacing = NULL, high.spacing = NULL, arrow="no", 
              col.interaction=color_list[[i]], col.high = "gray0", col.low="forestgreen", bor.col.interaction ="black", 
              bor.col.high="white", bor.col.low="forestgreen", high.lablength = NULL, low.lablength = NULL, sequence=NULL, 
              low.abun=NULL, low.abun.col="green", bor.low.abun.col ="black", high.abun=NULL, high.abun.col="red", 
              bor.high.abun.col="black", text.rot=90, text.high.col="black", text.low.col="black", adj.high=NULL, 
              adj.low=NULL, plot.axes = FALSE, low.y=0.5, high.y=1.5, add=FALSE, y.lim=NULL, x.lim=NULL, low.plot=TRUE, 
              high.plot=TRUE, high.xoff = 0, low.xoff = 0, high.lab.dis = NULL, low.lab.dis = NULL, abuns.type="additional")
      
    }
  }
  
  
  # Visualize web, calculate indices and bind them to the filtered dataframe for each web
  joined_dfs_list <- list()
  
  # Each season
  for (i in 1:2) {
    
    # Current dataframe
    current_df <- list_of_dataframes[[i]]
    
    # Each elevation
    for(j in 1:4) {
      
      filtered_df <- filter(current_df, elevation == as.numeric(rev(elevation_list_strings)[[j]]))
      
      web <- webs_group[[i]][[j]]
      
      plotweb(web, method = "cca", empty = TRUE, labsize = 1, ybig = 1, y.width.low = 0.1, 
              y.width.high = 0.1, low.spacing = NULL, high.spacing = NULL, arrow="no", 
              col.interaction=color_list[[i]], col.high = "gray0", col.low="forestgreen", bor.col.interaction ="black", 
              bor.col.high="white", bor.col.low="forestgreen", high.lablength = NULL, low.lablength = NULL, sequence=NULL, 
              low.abun=NULL, low.abun.col="green", bor.low.abun.col ="black", high.abun=NULL, high.abun.col="red", 
              bor.high.abun.col="black", text.rot=90, text.high.col="black", text.low.col="black", adj.high=NULL, 
              adj.low=NULL, plot.axes = FALSE, low.y=0.5, high.y=1.5, add=FALSE, y.lim=NULL, x.lim=NULL, low.plot=TRUE, 
              high.plot=TRUE, high.xoff = 0, low.xoff = 0, high.lab.dis = NULL, low.lab.dis = NULL, abuns.type="additional")
      
      # Add a title after creating the plot
      title(main = elevation_list_strings[[j]], col.main = "black", font.main = 2)
      
      indices <- specieslevel(web, index="ALLBUTD", level="lower", logbase=exp(1), low.abun=NULL, high.abun=NULL, PDI.normalise=TRUE, 
                              PSI.beta=c(1,0), nested.method="NODF", nested.normalised=TRUE, nested.weighted=TRUE, empty.web=TRUE)
      # Convert row names of df2 to a column
      indices$sp_code <- rownames(indices)
      joined_dfs_list[[j]] <- merge(filtered_df %>% distinct(sp_code, .keep_all = TRUE), select(indices, sp_code, d), by = "sp_code")
      
      
    }
    
    list_of_dataframes[[i]] <- do.call(rbind, joined_dfs_list)
    
  }
  
  plants_with_indices <- do.call(rbind, list_of_dataframes)[,-c(5:42)]
  
  
  for (i in 1:2) {
    plot_list <- list()
    for(j in 1:4) {
      plot_list[[j]] <- ggplot(filter(plants_with_indices, season == season_list[[i]], elevation == elevation_list_strings[[j]]), aes(x=d)) +
        geom_histogram(bins=20, fill="blue", color="black") +
        labs(x="Specialization", y="Frequency", title = paste(season_list[[i]], " ", elevation_list_strings[[j]])) +
        theme_minimal()
    }
    grid.arrange(grobs=plot_list, nrow=2, ncol=2)
  }
  
  # Visualize distribution of specialization
  plants_with_indices_dry <- filter(plants_with_indices, season == "DRY")
  
  p <- ggplot(plants_with_indices, aes(x=size, y=d, color=factor(elevation), shape=factor(season))) +
    geom_point(alpha=0.7) +
    labs(x="Size", y="Specialization") +
    theme_minimal()
  print(p)
  
  # Visualize traits with specialization
  filtered_df <- filter(plants_with_indices, season == "DRY", elevation == 650)
  attach(filtered_df)
  boxplot(d~colour, col=c(sort(unique(colour))))
  boxplot(d~brightness)
  boxplot(d~nectar_guides)
  boxplot(d~odour)
  boxplot(d~anther_pos)
  boxplot(d~flower_pos)
  boxplot(d~shape_upd)
  plot(d~size)
  plot(d~tube_length)
  plot(d~sugar_amount)
  
  # Prepare lists of colors and season labels
  color_list <- list("orange", "steelblue")
  season_list <- list("DRY", "WET")
  elevation_list_strings <- as.list(as.character(unique(df$elevation)))
  
  #For each trait
  for (i in seq_along(season_list)) {
    
    # Filter season df
    season_df <- filter(plants_with_indices, season == season_list[[i]])
    
    # Store each plot in a list
    par(mfrow = c(4, 3))
    
    #For each elevation
    for (j in seq_along(elevation_list_strings)) {
      
      filtered_df <- filter(season_df, elevation == elevation_list_strings[[j]])
      attach(filtered_df)
      boxplot(d~colour, col=c(sort(unique(colour))))
      boxplot(d~brightness, col=c("brown", "red"))
      boxplot(d~nectar_guides)
      boxplot(d~odour)
      boxplot(d~anther_pos)
      boxplot(d~flower_pos)
      boxplot(d~shape_upd)
      boxplot(d~plant_group)
      plot(d~size)
      plot(d~tube_length)
      plot(d~sugar_amount)
      plot(d~concentration)
    }
    rm(season_df)
  }
  

# Multivariate analysis on trait data: -----------------------------------------
  ## Data preprocessing: -------------------------------------------------------------
  
  plants_with_indices <- plants_with_indices_backup
  plants_with_indices_backup <- plants_with_indices
  
  # Column mapping
  shape_col = "shape_upd"
  symmetry_col = "symmetry"
  flower_pos_col = "flower_pos"
  anther_pos_col = "anther_pos"
  odour_col = "odour"
  brightness_col = "brightness"
  colour_col = "colour"
  nectar_guides_col = "nectar_guides"
  
  # Openness
  plants_with_indices$shape_upd_num <- ifelse(plants_with_indices[[shape_col]] %in% c("Dish", "Open", "Bowl", "Stellate"), 1, 0)
  
  # Symmetry
  plants_with_indices$symmetry_num <- ifelse(plants_with_indices[[symmetry_col]] == "Zygomorphic", 1, 0)
  
  # Flower Position
  plants_with_indices$flower_pos_num <- ifelse(plants_with_indices[[flower_pos_col]] == "Upright", 0.5,
                                               ifelse(plants_with_indices[[flower_pos_col]] == "Horizontal", 1, 0))
  
  # Anther Position
  plants_with_indices$anther_pos_num <- ifelse(plants_with_indices[[anther_pos_col]] == "Partially exposed", 0.5,
                                               ifelse(plants_with_indices[[anther_pos_col]] == "Exposed", 1, 0))
  
  # Odour Strength
  plants_with_indices$odour_num <- ifelse(plants_with_indices[[odour_col]] == "Moderate", 0.5,
                                          ifelse(plants_with_indices[[odour_col]] == "Strong", 1, 0))
  
  # Brightness
  plants_with_indices$brightness_num <- ifelse(plants_with_indices[[brightness_col]] == "Vivid", 1, 0)
  
  # Nectar Guides
  plants_with_indices$nectar_guides_num <- ifelse(plants_with_indices[[nectar_guides_col]] == "Present", 1, 0)
  
  # Color Conversion
  vcols <- c("brown", "green", "orange", "pink", "purple", "red", "white", "yellow")
  rgb_values <- col2rgb(vcols)
  fcols <- as.data.frame(t(rgb_values)) # Transpose and convert to dataframe
  colnames(fcols) <- c("R", "G", "B") # Set column names for RGB
  rownames(fcols) <- vcols
  
  plants_with_indices$R <- NA
  plants_with_indices$G <- NA
  plants_with_indices$B <- NA
  
  for (i in 1:nrow(plants_with_indices)) {
    color <- tolower(plants_with_indices[i, which(names(plants_with_indices) == colour_col)])
    print(color)
    if (color %in% vcols) {
      print(fcols[color, "R"])
      plants_with_indices$R[i] <- fcols[color, "R"]
      plants_with_indices$G[i] <- fcols[color, "G"]
      plants_with_indices$B[i] <- fcols[color, "B"]
    }
  }
  
  # Create a column for each color and initialize with 0
  for (color in vcols) {
    plants_with_indices[[color]] <- 0
  }
  
  # Set the color column to 1 based on the plant's color
  for (i in 1:nrow(plants_with_indices)) {
    color <- tolower(plants_with_indices[i, which(names(plants_with_indices) == colour_col)])
    if (color %in% vcols) {
      plants_with_indices[i, color] <- 1
    }
  }
  
  # Replacing NA values for colors not found in the predefined list
  plants_with_indices$R[is.na(plants_with_indices$R)] <- 0
  plants_with_indices$G[is.na(plants_with_indices$G)] <- 0
  plants_with_indices$B[is.na(plants_with_indices$B)] <- 0
  
  # Final dataframe
  print("Data processing completed")
  
  # Add rownames based on sp_code, elevation and season
  rownames(plants_with_indices) <- paste(plants_with_indices$sp_code, plants_with_indices$elevation, plants_with_indices$season, sep="_")
  
  # Identify numeric columns, exclude elevation and d'
  plants_with_indices[, 14:40] <- lapply(plants_with_indices[, 14:40], function(x) as.numeric(as.character(x)))
  numeric_columns <- sapply(plants_with_indices, is.numeric)
  numeric_columns[2] <- FALSE
  numeric_columns[22] <- FALSE
  numeric_columns
  traits_numeric <- plants_with_indices[,numeric_columns]
  
  # exclude columns with NAs
  na_columns <- which(sapply(traits_numeric, function(x) any(is.na(x))))
  traits_numeric <- traits_numeric[, -na_columns]
  
  # Add hue
  traits_numeric$hue <- rgb2hsv(traits_numeric$R, traits_numeric$G, traits_numeric$B, maxColorValue = 255)[1,]
  traits_numeric <- traits_numeric[,!colnames(traits_numeric) %in% c("hue")]
  
  # Standardize over columns
  traits_numeric_stand = decostand(traits_numeric,method="stand") 
  
  # Select rows for individual network
  selected_network <- traits_numeric_stand[grepl("650_DRY", rownames(traits_numeric_stand)), ]
  
  ## Run PCA on entire dataset-------
  
  # Extract species code (everything before the first underscore)
  species_codes <- sub("_.*", "", rownames(traits_numeric_stand))
  
  # Find unique species codes
  unique_species <- unique(species_codes)
  
  # Select only the first occurrence of each species
  selected_rows <- traits_numeric_stand[!duplicated(species_codes), ]
  
  # Exclude some columns
  color_rgb <- c("R","G","B")
  color_columns <- c("brown", "green", "orange", "pink", "purple", "red", "white", "yellow")
  selected_rows <- selected_rows[,!colnames(selected_rows) %in% color_columns]
  colnames(selected_rows)
  
  # Plot the results
  attach(selected_rows)
  par(mfrow=c(1,1))
  rdout=rda(selected_rows)
  plot(rdout, choices = c(1, 3), type="n")
  text(rdout, disp="species", cex=0.7)
  points(rdout, disp="sites",pch=symmetry_num+16,cex=1.3, col = "black")
  points(rdout, disp="sites",pch=symmetry_num+16,cex=1.0, col = rgb(plants_with_indices$R/255, plants_with_indices$G/255,plants_with_indices$B/255))
  arrows(0,0,scores(rdout)$species["tube_length",1],scores(rdout)$species["tube_length",2],lwd=1,length=0.15)
  arrows(0,0,scores(rdout)$species["anther_pos_num",1],scores(rdout)$species["anther_pos_num",2],lwd=1,length=0.15)
  arrows(0,0,scores(rdout)$species["R",1],scores(rdout)$species["R",2],lwd=1,length=0.15)
  arrows(0,0,scores(rdout)$species["G",1],scores(rdout)$species["G",2],lwd=1,length=0.15)
  arrows(0,0,scores(rdout)$species["B",1],scores(rdout)$species["B",2],lwd=1,length=0.15)
  #arrows(0,0,scores(rdout)$species["hue",1],scores(rdout)$species["hue",2],lwd=1,length=0.15)
  arrows(0,0,scores(rdout)$species["position_num",1],scores(rdout)$species["position_num",2],lwd=1,length=0.15)
  arrows(0,0,scores(rdout)$species["odour_num",1],scores(rdout)$species["odour_num",2],lwd=1,length=0.15)
  arrows(0,0,scores(rdout)$species["symmetry_num",1],scores(rdout)$species["symmetry_num",2],lwd=1,length=0.15)
  arrows(0,0,scores(rdout)$species["size",1],scores(rdout)$species["size",2],lwd=1,length=0.15)
  arrows(0,0,scores(rdout)$species["brightness_num",1],scores(rdout)$species["brightness_num",2],lwd=1,length=0.15)
  ordispider(rdout, shape_upd_num, display = "sites", spiders = c("median"), col = shape_upd_num+3)
  
  barplot(as.numeric(eigenvals(rdout) / sum(eigenvals(rdout))))
  
  
  
