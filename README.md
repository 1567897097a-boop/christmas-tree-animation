# christmas-tree-animation
---
title: "3D Animated Christmas Tree with Multiple Styles"
date: "`r Sys.Date()`"
output: html_document 
---

## 一、项目说明

本项目使用 ggplot2 与 gganimate，
通过三维坐标模拟与透视投影的方法，
在二维画布中构建一个可旋转的 3D 圣诞树动画。

动画通过绕 Y 轴旋转树体，
并结合雪花、装饰物与文字，
最终可导出为 GIF 或 MP4 文件。

---
## 二、环境准备、全局参数与主题风格

```{r setup, message=FALSE, warning=FALSE}
required_packages <- c(
  "ggplot2",
  "gganimate",
  "dplyr",
  "tidyr",
  "showtext",
  "av",
  "gifski"
)

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE)
}

font_add_google("Great Vibes", "christmas_font")
showtext_auto()

set.seed(2025)

current_style_id <- 1

styles <- list(
  "1" = list(
    bg_col = "#0f0505",
    tree_cols = c("#0B3D0B", "#144514", "#006400"),
    trunk_col = "#3e2723",
    decor_cols = c("#FFD700", "#CCA43B", "#8B0000"),
    star_col = "#FFD700",
    text_col = "#FFD700",
    snow_col = "white",
    ribbon = TRUE,
    ribbon_col = "#D4AF37",
    ribbon_width = 1.5
  )
)

cfg <- styles[[as.character(current_style_id)]]
```
## 四、几何函数与数据生成

```{r geometry, message=FALSE, warning=FALSE}
get_star_polygon <- function(x_center, y_center, radius) {
  angles <- seq(pi/2, 2.5 * pi, length.out = 11)[-11]
  radii <- rep(c(radius, radius * 0.4), 5)
  data.frame(
    x = x_center + radii * cos(angles),
    y = y_center + radii * sin(angles)
  )
}

generate_tree_data <- function(cfg) {
  n_leaves <- 8000
  h <- runif(n_leaves)
  r <- (1 - h)^1.3 * 0.6
  theta <- runif(n_leaves, 0, 2 * pi)

  tree <- data.frame(
    x = r * cos(theta),
    y = h - 0.5,
    z = r * sin(theta),
    col = sample(cfg$tree_cols, n_leaves, TRUE),
    size = runif(n_leaves, 0.6, 1.8),
    type = "tree",
    alpha = 0.95
  )

  trunk <- data.frame(
    x = runif(800, -0.08, 0.08),
    y = runif(800, -0.7, -0.45),
    z = runif(800, -0.08, 0.08),
    col = cfg$trunk_col,
    size = 1.2,
    type = "trunk",
    alpha = 1
  )

  dplyr::bind_rows(tree, trunk)
}

generate_snow <- function(cfg, n = 250) {
  data.frame(
    x = runif(n, -1, 1),
    y = runif(n, -0.8, 1.2),
    z = runif(n, -1, 1),
    col = cfg$snow_col,
    size = runif(n, 0.5, 2),
    type = "snow",
    alpha = runif(n, 0.5, 0.9),
    speed = runif(n, 0.015, 0.035)
  )
}
```
## 五、动画帧计算（旋转 + 透视）

```{r functions, message=FALSE, warning=FALSE}
static_data <- generate_tree_data(cfg)
snow_data   <- generate_snow(cfg)
star_shape  <- get_star_polygon(0, 0.4, 0.03)

n_frames <- 90
fps_val  <- 24

process_frame <- function(frame_id) {

  angle <- 2 * pi * frame_id / n_frames

  tree_rot <- static_data %>%
    dplyr::mutate(
      x_rot   = x * cos(angle) - z * sin(angle),
      z_rot   = z * cos(angle) + x * sin(angle),
      y_final = y
    )

  snow_curr <- snow_data %>%
    dplyr::mutate(
      y_final = -0.8 + (y - frame_id * speed - (-0.8)) %% 2,
      x_rot   = x,
      z_rot   = z
    )

  dplyr::bind_rows(tree_rot, snow_curr) %>%
    dplyr::mutate(
      z_rot   = pmin(z_rot, 2.4),              # 防止透视爆炸
      depth   = 1 / (2.5 - z_rot),
      x_proj  = x_rot * depth * 2,
      y_proj  = y_final * depth * 2,
      size_vis  = size * depth * 1.5,
      alpha_vis = alpha
    ) %>%
    dplyr::arrange(depth) %>%
    dplyr::mutate(frame = frame_id)
}

all_frames <- lapply(seq_len(n_frames), process_frame) %>%
  dplyr::bind_rows()

```
## 六、绘图与动画输出

```{r plot, message=FALSE, warning=FALSE}
p <- ggplot(all_frames) +
  geom_point(
    aes(x_proj, y_proj, color = I(col), size = I(size_vis), alpha = I(alpha_vis)),
    shape = 19
  ) +
  geom_polygon(
    data = star_shape,
    aes(x, y),
    inherit.aes = FALSE,
    fill = cfg$star_col,
    color = "white"
  ) +
  annotate(
    "text",
    x = 0,
    y = 0.6,
    label = "Merry Christmas",
    family = "christmas_font",
    color = cfg$text_col,
    size = 12
  ) +
  scale_size_identity() +
  scale_alpha_identity() +
  coord_fixed(xlim = c(-0.8, 0.8), ylim = c(-0.8, 0.9)) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = cfg$bg_col, color = NA)
  ) +
  transition_manual(frame)

anim <- animate(
  p,
  nframes = n_frames,
  fps = fps_val,
  renderer = gifski_renderer()
)

anim_save("christmas_tree.gif", anim)

```
## 2. 动画效果展示
以下是生成的圣诞树动画：

![动态圣诞树动画](D:/Rpackagemyself/christmas_tree1.gif)
R 实现的动态圣诞树动画，支持 6 种风格切换
