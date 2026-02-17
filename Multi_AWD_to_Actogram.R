# This code is meant to load multiple AWDs and create an actogram out of them
# Created by ChatGPT, Gogole Gemini, and Carter Bower
# Edited February 16th, 2026

library(tidyverse)
library(lubridate)

# ==============================================================================
# 1. HELPER: Read a single AWD file into a Dataframe with Datetimes
# ==============================================================================
read_awd_data <- function(file, tz = "America/Los_Angeles") {
  
  # Read file safely
  raw <- readLines(file, warn = FALSE) %>% trimws()
  raw <- raw[raw != ""]
  
  # Parse Header
  # AWD Format: Line 1=Title, 2=Date, 3=Time, 4=Interval code
  start_date_chr <- raw[2]
  start_time_chr <- raw[3]
  epoch_units    <- as.numeric(raw[4])
  
  # Calculate the exact interval in seconds based on Row 4
  # (ClockLab standard: Unit/4 = Minutes)
  interval_sec   <- (epoch_units / 4) * 60 
  
  # Parse start datetime
  start_datetime <- parse_date_time(
    paste(start_date_chr, start_time_chr),
    orders = c("d-b-Y HM", "d-B-Y HM", "d-m-Y HM", "Y-m-d HM"),
    locale = "C",
    tz = tz
  )
  
  if (is.na(start_datetime)) {
    warning(paste("Could not parse start date in:", file))
    return(NULL)
  }
  
  # Parse Activity Data (Lines 8 onwards)
  # CRITICAL FIX: Handle non-numeric strings safely
  data_strings <- raw[-(1:7)]
  activity <- suppressWarnings(as.numeric(data_strings))
  
  # Replace NA/NaN with 0 so the timeline doesn't break
  activity[is.na(activity)] <- 0
  
  # Calculate exact time for every data point
  datetime_seq <- start_datetime + seq(
    from = 0, 
    by = interval_sec, 
    length.out = length(activity)
  )
  
  # Return a clean table with the interval included
  tibble(
    datetime = datetime_seq, 
    activity = activity,
    interval_hours = interval_sec / 3600 # Save this for the plotter!
  )
}


# ==============================================================================
# 2. MAIN: Plotting Function (Accepts combined data)
# ==============================================================================
plot_actogram <- function(
    df,                  
    timescale = 24,
    dates = NULL,
    date_range = NULL,
    bar_width = NULL,     # Default to NULL -> Use the file's header info
    light_dark = FALSE,
    light_start = 6,
    dark_start = 18,
    vlines = NULL,       
    free_y = FALSE,
    hour_window = NULL
) {
  
  # ---- Prepare Data ----
  df <- df %>%
    mutate(
      day = as.Date(datetime),
      tod = hour(datetime) + minute(datetime)/60 + second(datetime)/3600
    )
  
  # ---- Determine Bar Width ----
  # If the user didn't force a width, use the one from the file header
  if (is.null(bar_width)) {
    if ("interval_hours" %in% names(df)) {
      # Use the interval we read from the file header
      width_val <- unique(df$interval_hours)[1]
      if(is.na(width_val)) width_val <- 0.1 # Fallback safety
      bar_width <- width_val
      cat("Using file header interval:", round(bar_width * 60, 2), "minutes.\n")
    } else {
      bar_width <- 0.1 # Fallback default (6 mins)
      warning("Interval not found in data. Defaulting to 6 mins.")
    }
  }
  
  # ---- Helper: Parse User Dates ----
  parse_user_dates <- function(x) {
    d <- parse_date_time(x, orders = c("d-b-Y","d-B-Y","d-m-Y","Y-m-d"), locale = "C")
    if (any(is.na(d))) stop("Check your date format.")
    as.Date(d)
  }
  
  # ---- Filter by Specific Dates ----
  if (!is.null(dates)) {
    dates <- parse_user_dates(dates)
    df <- df %>% filter(day %in% dates)
  }
  
  # ---- Filter by Date Range ----
  if (!is.null(date_range)) {
    date_range <- parse_user_dates(date_range)
    df <- df %>% filter(day >= min(date_range) & day <= max(date_range))
  }
  
  if (nrow(df) == 0) stop("No data left after date filtering.")
  
  # ---- 48h Duplication (Double Plot) ----
  if (timescale == 48) {
    df <- bind_rows(df, df %>% mutate(tod = tod + 24))
  }
  
  # ---- Light/Dark Shading Logic ----
  shade_df <- NULL
  if (light_dark) {
    days <- unique(df$day)
    n_cycles <- ceiling(timescale / 24)
    shade_df <- map_dfr(days, function(d) {
      map_dfr(0:(n_cycles-1), function(k) {
        offset <- k*24
        tibble(
          day = d,
          xmin = c(offset+0, offset+light_start, offset+dark_start),
          xmax = c(offset+light_start, offset+dark_start, min(offset+24, timescale)),
          type = c("dark","light","dark")
        )
      })
    }) %>% filter(xmin < timescale) 
  }
  
  # ---- Plotting ----
  x_breaks <- seq(0, max(df$tod), by = 6) 
  
  p <- ggplot(df, aes(x = tod, y = activity)) +
    # 1. Add Shading
    {if (!is.null(shade_df))
      geom_rect(data = shade_df, aes(xmin=xmin, xmax=xmax, ymin=-Inf, ymax=Inf, fill=type),
                inherit.aes = FALSE, alpha = 0.2)
    } +
    
    # 2. Add Activity Bars (PERFECT WIDTH)
    geom_col(aes(x = tod, y = activity), 
             width = bar_width,      # Uses the exact header width
             fill = "black", 
             color = NA,             # No border = crisp lines
             position = "identity") +
    
    # 3. Facet by Day
    facet_wrap(~ day, ncol = 1, scales = if(free_y) "free_y" else "fixed", strip.position = "right") +
    
    # 4. Formatting
    scale_fill_manual(values = c(light="gold", dark="gray70"), guide="none") +
    scale_x_continuous(breaks = x_breaks, labels = x_breaks %% 24) +
    labs(x="Time of Day (h)", y="Activity") +
    theme_minimal() +
    theme(
      panel.grid = element_blank(),
      strip.text.y.right = element_text(angle = 0, hjust=0)
    )
  
  # 5. Vertical Lines
  if (!is.null(vlines)) {
    p <- p + geom_vline(xintercept = vlines, linetype = "longdash", color = "black", linewidth = 0.5)
  }
  
  return(p)
}


# ==============================================================================
# 3. RUN THE SCRIPT
# ==============================================================================

# A. List your files here (in order)
my_files <- c(
  '/Volumes/CARTERBOWER/2026 ELCD Project/Control_Activity_Actogram_Graph Data.awd',
  '/Volumes/CARTERBOWER/2026 ELCD Project/Control2.5_Activity_Actogram_Graph Data.awd'
)

# B. Read and Combine all files
combined_data <- map_dfr(my_files, read_awd_data) %>% 
  arrange(datetime) %>%          # Sort by time
  distinct(datetime, .keep_all = TRUE) # Remove overlaps

# Check Data Quality
cat("Total rows:", nrow(combined_data), "\n")
cat("Date Range:", paste(range(combined_data$datetime), collapse=" to "), "\n")

# C. Generate the Graph
# Note: We removed 'bar_width' so it defaults to the header value
(p <- plot_actogram(combined_data, 
                    timescale = 48,
                    free_y = FALSE,
                    light_dark = TRUE, 
                    vlines = c(24),
                    light_start = 4,
                    dark_start = 18))

setwd('/Volumes/CARTERBOWER/2026 ELCD Project')

ggsave(
  filename = "actogram_Control.pdf",
  plot = p,
  device = cairo_pdf,
  width = 30,
  height = 30,
  units = "in"
)
