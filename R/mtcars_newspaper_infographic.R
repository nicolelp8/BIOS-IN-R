# ============================================================
# MTcars Newspaper-Style Infographic
# Uses only: base R + tidyverse
# ============================================================

# install.packages("tidyverse")  # Run once if necessary
library(tidyverse)

# -----------------------------
# 1. Data preparation
# -----------------------------
cars <- mtcars |>
  rownames_to_column("model") |>
  as_tibble() |>
  mutate(
    transmission = if_else(am == 1, "Manual", "Automatic"),
    cylinders = factor(cyl),
    model = str_replace_all(model, "_", " ")
  )

# Summary cards by cylinder count
cyl_summary <- cars |>
  group_by(cylinders) |>
  summarise(
    avg_mpg = mean(mpg),
    cars = n(),
    .groups = "drop"
  ) |>
  mutate(
    x = c(20, 50, 80),
    card_label = paste0(cylinders, "-CYLINDER"),
    detail = paste0(round(avg_mpg, 1), " MPG AVG • ", cars, " CARS")
  )

# Fuel-economy leaderboard
top_efficient <- cars |>
  arrange(desc(mpg)) |>
  slice_head(n = 5) |>
  mutate(
    model = str_trunc(model, 18),
    rank = row_number(),
    y = seq(37, 15, length.out = n()),
    
    # Scale bars from 0 MPG so the Fiat X1-9 has a visible bar
    bar_start = 70,
    bar_end = 70 + scales::rescale(
      mpg,
      from = c(0, max(cars$mpg)),
      to = c(0, 18)
    ),
    
    mpg_label = paste0(mpg, " mpg")
  )

# Key figures
mean_mpg <- mean(cars$mpg)
median_hp <- median(cars$hp)
manual_share <- mean(cars$am == 1)

best_car <- cars |>
  slice_max(mpg, n = 1, with_ties = FALSE)

# -----------------------------
# 2. Scatterplot layout values
# -----------------------------
# These values define the drawing area INSIDE the left panel.
scatter_left   <- 10
scatter_right  <- 57
scatter_bottom <- 16
scatter_top    <- 43

scatter_data <- cars |>
  mutate(
    x_plot = scales::rescale(
      wt,
      to = c(scatter_left + 0.8, scatter_right - 0.8)
    ),
    y_plot = scales::rescale(
      mpg,
      to = c(scatter_bottom + 0.8, scatter_top - 0.8)
    )
  )

# Tick locations
wt_breaks <- tibble(
  wt = c(2, 3, 4, 5),
  x = scales::rescale(
    wt,
    from = range(cars$wt),
    to = c(scatter_left + 0.8, scatter_right - 0.8)
  )
)

mpg_breaks <- tibble(
  mpg = c(10, 20, 30),
  y = scales::rescale(
    mpg,
    from = range(cars$mpg),
    to = c(scatter_bottom + 0.8, scatter_top - 0.8)
  )
)

best_car_plot <- best_car |>
  mutate(
    x_plot = scales::rescale(
      wt,
      from = range(cars$wt),
      to = c(scatter_left + 0.8, scatter_right - 0.8)
    ),
    y_plot = scales::rescale(
      mpg,
      from = range(cars$mpg),
      to = c(scatter_bottom + 0.8, scatter_top - 0.8)
    )
  )

# -----------------------------
# 3. Create the infographic
# -----------------------------
infographic <- ggplot() +
  
  # ---------------------------
# Full-page background
# ---------------------------
geom_rect(
  aes(xmin = 0, xmax = 100, ymin = 0, ymax = 100),
  fill = "#F7F2E8",
  color = NA
) +
  
  # ---------------------------
# Masthead
# ---------------------------
annotate(
  "text",
  x = 4, y = 96,
  label = "THE MOTOR DATA GAZETTE",
  hjust = 0,
  family = "serif",
  fontface = "bold",
  size = 7,
  color = "#1E1E1E"
) +
  annotate(
    "text",
    x = 4, y = 91.5,
    label = "A look at 32 classic performance cars",
    hjust = 0,
    family = "serif",
    size = 3.8,
    color = "#555555"
  ) +
  annotate(
    "text",
    x = 96, y = 95.5,
    label = "DATA DESK\nMTcars edition",
    hjust = 1,
    family = "mono",
    fontface = "bold",
    size = 3,
    color = "#9E2A2B"
  ) +
  geom_segment(
    aes(x = 4, xend = 96, y = 88.5, yend = 88.5),
    linewidth = 0.9,
    color = "#1E1E1E"
  ) +
  
  # ---------------------------
# Headline and story deck
# ---------------------------
annotate(
  "text",
  x = 4, y = 84,
  label = "LIGHTER CARS LEAD THE MILEAGE RACE",
  hjust = 0,
  family = "serif",
  fontface = "bold",
  size = 6.5,
  color = "#1E1E1E"
) +
  annotate(
    "text",
    x = 4, y = 79.5,
    label = paste(
      "In the mtcars sample, weight has a clearer relationship with fuel economy",
      "than cylinder count alone — though compact four-cylinder cars dominate the top of the table.",
      sep = "\n"
    ),
    hjust = 0,
    family = "serif",
    size = 3.4,
    color = "#383838",
    lineheight = 1.15
  ) +
  
  # ---------------------------
# Cylinder summary cards
# ---------------------------
geom_rect(
  data = cyl_summary,
  aes(xmin = x - 13, xmax = x + 13, ymin = 63, ymax = 74),
  fill = "#E8E1D3",
  color = "#1E1E1E",
  linewidth = 0.35
) +
  geom_text(
    data = cyl_summary,
    aes(x = x, y = 71.5, label = card_label),
    family = "mono",
    fontface = "bold",
    size = 3.3,
    color = "#9E2A2B"
  ) +
  geom_text(
    data = cyl_summary,
    aes(x = x, y = 68.4, label = sprintf("%.1f", avg_mpg)),
    family = "serif",
    fontface = "bold",
    size = 7,
    color = "#1E1E1E"
  ) +
  geom_text(
    data = cyl_summary,
    aes(x = x, y = 65.3, label = detail),
    family = "mono",
    size = 2.6,
    color = "#555555"
  ) +
  
  # Divider between headline/cards and chart area
  geom_segment(
    aes(x = 4, xend = 96, y = 59, yend = 59),
    linewidth = 0.45,
    color = "#1E1E1E"
  ) +
  
  # ---------------------------
# Left chart panel
# ---------------------------
geom_rect(
  aes(xmin = 4, xmax = 60, ymin = 8, ymax = 55),
  fill = "#FCFAF5",
  color = "#1E1E1E",
  linewidth = 0.4
) +
  annotate(
    "text",
    x = 7, y = 53,
    label = "WEIGHT IS THE BIG MPG STORY",
    hjust = 0,
    family = "mono",
    fontface = "bold",
    size = 3.5,
    color = "#9E2A2B"
  ) +
  annotate(
    "text",
    x = 7, y = 49.5,
    label = "Miles per gallon falls as vehicle weight rises",
    hjust = 0,
    family = "serif",
    size = 3.1,
    color = "#383838"
  ) +
  
  # Cylinder legend kept inside the left panel
  annotate(
    "text",
    x = 36, y = 53,
    label = "CYLINDERS:",
    hjust = 0,
    family = "mono",
    fontface = "bold",
    size = 2.4,
    color = "#555555"
  ) +
  annotate(
    "text",
    x = 47, y = 53,
    label = "4     6     8",
    hjust = 0,
    family = "mono",
    size = 2.4,
    color = "#555555"
  ) +
  
  # Scatterplot boundary
  geom_rect(
    aes(
      xmin = scatter_left,
      xmax = scatter_right,
      ymin = scatter_bottom,
      ymax = scatter_top
    ),
    fill = NA,
    color = "#8A8175",
    linewidth = 0.35
  ) +
  
  # Horizontal gridlines constrained to chart area
  geom_segment(
    data = mpg_breaks,
    aes(
      x = scatter_left,
      xend = scatter_right,
      y = y,
      yend = y
    ),
    color = "#D7D0C4",
    linewidth = 0.3
  ) +
  
  # Vertical gridlines constrained to chart area
  geom_segment(
    data = wt_breaks,
    aes(
      x = x,
      xend = x,
      y = scatter_bottom,
      yend = scatter_top
    ),
    color = "#D7D0C4",
    linewidth = 0.3
  ) +
  
  # Scatter points
  geom_point(
    data = scatter_data,
    aes(x = x_plot, y = y_plot, color = cylinders),
    size = 3.2,
    alpha = 0.9
  ) +
  
  # Best-MPG callout
  geom_label(
    data = best_car_plot,
    aes(
      x = x_plot,
      y = y_plot,
      label = paste0(model, "\n", mpg, " mpg")
    ),
    hjust = 0,
    nudge_x = 1.2,
    nudge_y = 1.4,
    family = "serif",
    size = 2.5,
    fill = "#FFF8DF",
    color = "#1E1E1E",
    label.size = 0.25
  ) +
  
  # X-axis tick labels below chart
  geom_text(
    data = wt_breaks,
    aes(x = x, y = 13.8, label = wt),
    family = "mono",
    size = 2.7,
    color = "#555555"
  ) +
  
  # Y-axis tick labels left of chart
  geom_text(
    data = mpg_breaks,
    aes(x = 8.4, y = y, label = mpg),
    family = "mono",
    size = 2.7,
    color = "#555555"
  ) +
  
  # Axis labels
  annotate(
    "text",
    x = (scatter_left + scatter_right) / 2,
    y = 10.5,
    label = "Vehicle weight (1,000 lbs)",
    family = "serif",
    size = 3,
    color = "#383838"
  ) +
  annotate(
    "text",
    x = 5.8,
    y = (scatter_bottom + scatter_top) / 2,
    label = "MPG",
    angle = 90,
    family = "serif",
    size = 3,
    color = "#383838"
  ) +
  
  # ---------------------------
# Right leaderboard panel
# ---------------------------
geom_rect(
  aes(xmin = 64, xmax = 96, ymin = 8, ymax = 55),
  fill = "#FCFAF5",
  color = "#1E1E1E",
  linewidth = 0.4
) +
  annotate(
    "text",
    x = 67, y = 53,
    label = "FUEL ECONOMY LEADERS",
    hjust = 0,
    family = "mono",
    fontface = "bold",
    size = 3.5,
    color = "#9E2A2B"
  ) +
  annotate(
    "text",
    x = 67, y = 49.5,
    label = "The five highest-mileage cars in the sample",
    hjust = 0,
    family = "serif",
    size = 3.1,
    color = "#383838"
  ) +
  
  # Bars start at a fixed x position, avoiding rank/model text
  geom_segment(
    data = top_efficient,
    aes(
      x = bar_start,
      xend = bar_end,
      y = y,
      yend = y
    ),
    linewidth = 4.5,
    color = "#C8B79A",
    lineend = "butt"
  ) +
  
  # Rank + model name appear above bars
  geom_text(
    data = top_efficient,
    aes(
      x = 67,
      y = y + 2.0,
      label = paste0(rank, ". ", model)
    ),
    hjust = 0,
    family = "serif",
    fontface = "bold",
    size = 2.8,
    color = "#1E1E1E"
  ) +
  
  # MPG values at far right
  geom_text(
    data = top_efficient,
    aes(
      x = 93,
      y = y,
      label = mpg_label
    ),
    hjust = 1,
    family = "mono",
    fontface = "bold",
    size = 2.8,
    color = "#9E2A2B"
  ) +
  
  # ---------------------------
# Footer
# ---------------------------
annotate(
  "text",
  x = 7, y = 5,
  label = paste0(
    "AT A GLANCE  •  Average economy: ",
    round(mean_mpg, 1),
    " mpg  •  Median output: ",
    median_hp,
    " hp  •  Manual transmissions: ",
    round(manual_share * 100),
    "%"
  ),
  hjust = 0,
  family = "mono",
  size = 2.7,
  color = "#555555"
) +
  annotate(
    "text",
    x = 96, y = 5,
    label = "Source: built-in R dataset `mtcars`",
    hjust = 1,
    family = "mono",
    size = 2.4,
    color = "#555555"
  ) +
  
  # Color mapping for scatterplot cylinder groups
  scale_color_manual(
    values = c(
      "4" = "#2A6F97",
      "6" = "#D27D2D",
      "8" = "#9E2A2B"
    ),
    guide = "none"
  ) +
  
  coord_cartesian(
    xlim = c(0, 100),
    ylim = c(0, 100),
    clip = "off"
  ) +
  theme_void() +
  theme(
    plot.background = element_rect(
      fill = "#F7F2E8",
      color = NA
    ),
    plot.margin = margin(12, 12, 12, 12)
  )

# Display in RStudio
infographic

# Save as high-resolution PNG
ggsave(
  filename = "mtcars_newspaper_infographic_fixed.png",
  plot = infographic,
  width = 13,
  height = 9,
  dpi = 300,
  bg = "#F7F2E8"
)

# Show the chart only when working interactively in RStudio
if (interactive()) {
  print(infographic)
}

# Always save the intended PNG output at the designed dimensions
ggsave(
  filename = "mtcars_newspaper_infographic_fixed.png",
  plot = infographic,
  device = "png",
  width = 13,
  height = 9,
  units = "in",
  dpi = 300,
  bg = "#F7F2E8"
)
