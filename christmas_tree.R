{required_packages <- c("ggplot2", "gganimate", "dplyr", "tidyr", "showtext", "av") 
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
  library(pkg, character.only = TRUE)
}
font_add_google("Great Vibes", "christmas_font")
showtext_auto()
current_style_id <- 1 # 【在此切换风格 1-6】
styles <- list(
  "1" = list( 
    bg_col = "#0f0505", tree_cols = c("#0B3D0B", "#144514", "#006400"), 
    trunk_col = "#3e2723", decor_cols = c("#FFD700", "#CCA43B", "#8B0000"),
    star_col = "#FFD700", text_col = "#FFD700", snow_col = "white",
    ribbon = TRUE, ribbon_col = "#D4AF37", ribbon_width = 1.5 
  ),
  "2" = list( 
    bg_col = "#F0F2F5", tree_cols = c("#2F4F4F", "#5F9EA0", "#708090"),
    trunk_col = "#696969", decor_cols = c("#B0C4DE", "#FFFAF0", "#E0FFFF"),
    star_col = "#B0C4DE", text_col = "#708090", snow_col = "#87CEFA",
    ribbon = TRUE, ribbon_col = "#B0C4DE", ribbon_width = 1.5 
  ),
  "3" = list( 
    bg_col = "#1a1a1a", tree_cols = c("#556B2F", "#6B8E23", "#808000"),
    trunk_col = "#5D4037", decor_cols = c("#CD853F", "#FFCC00"), 
    star_col = "#FFCC00", text_col = "#DEB887", snow_col = "white",
    ribbon = FALSE, ribbon_col = NA, ribbon_width = 0
  ),
  "4" = list(
    bg_col = "#101010", tree_cols = c("#006400", "#228B22"),
    trunk_col = "#4E342E", decor_cols = c("#FF0000", "#00FF00", "#0000FF", "#FFFF00"),
    star_col = "#FFD700", text_col = "#FF6347", snow_col = "white",
    ribbon = TRUE, ribbon_col = "#C0C0C0", ribbon_width = 1.0 
  ),
  "5" = list(
    bg_col = "#000000", tree_cols = c("#111111", "#050505"), 
    trunk_col = "#222222", decor_cols = c("#FFFFFF", "#FFFFE0"),
    star_col = "#FFFFFF", text_col = "#FFFFFF", snow_col = "gray30",
    ribbon = TRUE, ribbon_col = "#FFFFFF", ribbon_width = 0.8 
  ), 
  "6" = list( 
    title = "Pink Romance",
    bg_col = "#1F0F12",      
    tree_cols = c("#D87093", "#FF69B4", "#FFB6C1"), 
    trunk_col = "#4A3728",
    decor_cols = c("#FFFFFF", "#FFD700", "#FF1493"),
    star_col = "#FFD700",
    text_col = "#FFC0CB",
    snow_col = "#FFF0F5",
    ribbon = TRUE, 
    ribbon_col = "#F8F8FF", 
    ribbon_width = 0.8, 
    ribbon_burr = 0.04 
  )
)
cfg <- styles[[as.character(current_style_id)]]
set.seed(2025)
get_star_polygon <- function(x_center, y_center, radius) {
  angles <- seq(pi/2, 2.5 * pi, length.out = 11)[-11] 
  radii <- rep(c(radius, radius * 0.4), 5)
  data.frame(x = x_center + radii * cos(angles), y = y_center + radii * sin(angles))
}
generate_tree_data <- function(cfg) {
  n_leaves <- 8000 
  h <- runif(n_leaves, 0, 1) 
  base_r <- (1 - h)
  layer_cycle <- (h * 7) %% 1 
  r <- base_r * 0.65 * (0.4 + 0.6 * (1 - layer_cycle)^0.7)
  theta <- runif(n_leaves, 0, 2*pi)
  df_tree <- data.frame(
    x = r * cos(theta), y = h - 0.5, z = r * sin(theta),
    col = sample(cfg$tree_cols, n_leaves, replace = TRUE),
    size = runif(n_leaves, 0.6, 1.8), type = "tree", alpha = 0.95
  )
  n_trunk <- 1000
  h_trunk <- runif(n_trunk, -0.7, -0.45)
  r_trunk <- 0.12
  theta_trunk <- runif(n_trunk, 0, 2*pi)
  df_trunk <- data.frame(
    x = r_trunk * cos(theta_trunk), y = h_trunk, z = r_trunk * sin(theta_trunk),
    col = cfg$trunk_col, size = 1.2, type = "trunk", alpha = 1
  )
  n_decor <- 600
  h_dec <- runif(n_decor, 0, 0.95)
  base_r_dec <- (1 - h_dec)
  layer_cycle_dec <- (h_dec * 7) %% 1
  r_dec <- base_r_dec * 0.68 * (0.4 + 0.6 * (1 - layer_cycle_dec)^0.7)
  theta_dec <- runif(n_decor, 0, 2*pi)
  df_decor <- data.frame(
    x = r_dec * cos(theta_dec), y = h_dec - 0.5, z = r_dec * sin(theta_dec),
    col = sample(cfg$decor_cols, n_decor, replace = TRUE),
    size = runif(n_decor, 2, 4), type = "decor", alpha = 1
  )
  df_ribbon <- NULL
  if (cfg$ribbon) {
    n_rib <- 6000 
    h_rib <- seq(0, 0.95, length.out = n_rib)
    base_r_rib <- (1 - h_rib) * 0.65 * 1.05 
    theta_rib <- 10 * pi * h_rib 
    df_ribbon <- data.frame(
      x = base_r_rib * cos(theta_rib), 
      y = h_rib - 0.5, 
      z = base_r_rib * sin(theta_rib),
      col = cfg$ribbon_col, 
      size = cfg$ribbon_width, 
      type = "ribbon", 
      alpha = 1
    )
  }
  bind_rows(df_trunk, df_tree, df_decor, df_ribbon)
}
generate_snow <- function(n_flakes=250) {
  data.frame(
    x = runif(n_flakes, -1, 1), y = runif(n_flakes, -0.8, 1.2), z = runif(n_flakes, -1, 1),
    col = cfg$snow_col, size = runif(n_flakes, 0.5, 2),
    type = "snow", alpha = runif(n_flakes, 0.5, 0.9), speed = runif(n_flakes, 0.015, 0.035)
  )
}
static_data <- generate_tree_data(cfg)
snow_data <- generate_snow(250)
star_shape <- get_star_polygon(0, 0.4, 0.03) 
n_frames <- 90  
fps_val <- 24 
process_frame <- function(frame_id) {
  angle <- 2 * pi * (frame_id / n_frames)
  tree_rot <- static_data %>%
    mutate(
      x_rot = x * cos(angle) - z * sin(angle),
      z_rot = z * cos(angle) + x * sin(angle),
      y_final = y
    )
  snow_curr <- snow_data %>%
    mutate(
      y_final = -0.8 + (y - frame_id * speed - (-0.8)) %% 2,
      x_rot = x, z_rot = z
    )
  bind_rows(tree_rot, snow_curr) %>%
    mutate(
      depth = 1 / (2.5 - z_rot),
      x_proj = x_rot * depth * 2,
      y_proj = y_final * depth * 2,
      size_vis = size * depth * 1.5,
      alpha_vis = alpha * ifelse(type == "snow", 1, (z_rot + 1.2) / 2.2) 
    ) %>%
    arrange(depth) %>%
    mutate(frame = frame_id)
}
all_frames <- lapply(1:n_frames, process_frame) %>% bind_rows()
p <- ggplot() +
  geom_point(data = all_frames, 
             aes(x = x_proj, y = y_proj, color = I(col), size = I(size_vis), alpha = I(alpha_vis)), 
             shape = 19) +
  geom_polygon(data = star_shape, aes(x = x, y = y), 
               fill = cfg$star_col, color = "white", size = 0.3) +
  annotate("text", x = 0, y = 0.6, label = "Merry Christmas", 
           family = "christmas_font", color = cfg$text_col, size = 12) +
  scale_size_identity() +
  scale_alpha_identity() +
  coord_fixed(xlim = c(-0.8, 0.8), ylim = c(-0.8, 0.9)) + 
  theme_void() +
  theme(
    plot.background = element_rect(fill = cfg$bg_col, color = NA),
    panel.background = element_rect(fill = cfg$bg_col, color = NA)
  ) +
  transition_manual(frame)
animate(p, nframes = n_frames, fps = fps_val, width = 600, height = 750, 
        renderer = gifski_renderer(loop = TRUE))
}
