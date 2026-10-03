## =====================================================================
## Simon 实验：三种 R 绘图体系可视化效果对比
##   1) base R（R 自带）   2) lattice（R 自带）   3) ggplot2（tidyverse）
## 数据清洗规则与 analyze_myTouchSimonNew.r 完全一致:
##   正确试次 + RT 200-1500ms + 被试内条件内 ±3SD 修剪
## 运行方式:
##   - 在 RStudio 中按 Source: 图直接显示在右下角 Plots 面板 (可用 Export 按钮导出)
##   - 用 Rscript 运行时: 保存为 PNG 到本文件夹
## =====================================================================
if (!dir.exists("experiment_data"))
    setwd("/Users/zhangxiaolan/Downloads/2周二午 5060 cognitive psy/PsyToolkitData_mySimon_2026_10_02_15_08")

## ---------- 数据准备与清洗 ----------
files <- list.files("experiment_data", full.names = TRUE)
raw <- do.call(rbind, lapply(files, function(f) {
    x <- read.table(f)
    names(x) <- c("block","condword","color","congruency","tablerow","status","RT","touchX","touchY")
    x$subject <- gsub("\\.data\\..*$", "", basename(f))
    x
}))
test <- raw[raw$block == "test" & raw$status == 1 & raw$RT >= 200 & raw$RT <= 1500, ]
trimmed <- do.call(rbind, lapply(split(test, list(test$subject, test$congruency)), function(d) {
    if (nrow(d) < 2) return(d)
    m <- mean(d$RT); s <- sd(d$RT)
    d[d$RT > m - 3*s & d$RT < m + 3*s, ]
}))
trimmed$cond <- factor(ifelse(trimmed$congruency == 1, "Congruent", "Incongruent"),
                       levels = c("Congruent", "Incongruent"))
subject_means <- aggregate(RT ~ subject + cond, trimmed, mean)
g <- aggregate(RT ~ cond, subject_means, mean)     # 组均值: Congruent, Incongruent
ci <- sapply(levels(subject_means$cond), function(cc) {
    v <- subject_means$RT[subject_means$cond == cc]
    qt(0.975, length(v) - 1) * sd(v) / sqrt(length(v))
})
g$ci <- ci
simon <- g$RT[2] - g$RT[1]
wide <- reshape(subject_means, idvar = "subject", timevar = "cond", direction = "wide")
wide$effect <- wide$RT.Incongruent - wide$RT.Congruent
cat("数据就绪: 组均值 =", round(g$RT, 2), " Simon effect =", round(simon, 2), "ms\n")

## =====================================================================
## 1) base R —— 传统绘图: 简单直接, 逐个命令手工拼装
## =====================================================================
toScreen <- interactive()   ## RStudio 中 Source 时为 TRUE
if (!toScreen) png("viz_1_base_R.png", width = 1400, height = 800, res = 130)
op <- par(mfrow = c(1, 2))
mp <- barplot(g$RT, names.arg = g$cond,
              col = c(adjustcolor("#2563eb", 0.85), adjustcolor("#dc2626", 0.85)),
              ylim = c(0, max(g$RT + g$ci) * 1.25),
              main = "Mean RT by condition (base R)", ylab = "RT (ms)")
arrows(mp, g$RT - g$ci, mp, g$RT + g$ci, angle = 90, code = 3, length = 0.08, lwd = 2)
text(mp, g$RT + g$ci + 40, round(g$RT, 1))
boxplot(RT ~ cond, trimmed,
        col = c(adjustcolor("#2563eb", 0.4), adjustcolor("#dc2626", 0.4)),
        main = "RT distribution (base R)", ylab = "RT (ms)")
par(op)
if (!toScreen) dev.off()

## =====================================================================
## 2) lattice —— Trellis 分面思想: 一个公式描述整张图
## =====================================================================
library(lattice)
if (!toScreen) png("viz_2_lattice.png", width = 1400, height = 800, res = 130)
print(barchart(RT ~ cond, data = g, horizontal = FALSE, origin = 0,
         col = c("#2563eb", "#dc2626"), ylim = c(0, max(g$RT + g$ci) * 1.25),
         main = "Mean RT by condition (lattice)", ylab = "RT (ms)",
         panel = function(x, y, ...) {
             panel.barchart(x, y, ...)
             panel.arrows(x, y - g$ci, x, y + g$ci, angle = 90, code = 3,
                          length = 0.06, lwd = 2)
             panel.text(x, y + g$ci + 40, round(y, 1))
         }))
print(bwplot(RT ~ cond, trimmed,
       fill = c(adjustcolor("#2563eb", 0.4), adjustcolor("#dc2626", 0.4)),
       main = "RT distribution (lattice)", ylab = "RT (ms)", xlab = ""))
if (!toScreen) dev.off()

## =====================================================================
## 3) ggplot2 —— 图形语法: 图层叠加, 默认即精美, 一致的设计语言
## =====================================================================
library(ggplot2)
library(grid)

p1 <- ggplot(g, aes(cond, RT, fill = cond)) +
    geom_col(width = 0.55) +
    geom_errorbar(aes(ymin = RT - ci, ymax = RT + ci), width = 0.12, linewidth = 1) +
    geom_text(aes(label = round(RT, 1)), vjust = -1.6, size = 4.5, fontface = "bold") +
    scale_fill_manual(values = c("#2563eb", "#dc2626"), guide = "none") +
    labs(title = "Mean RT by condition (ggplot2)",
         subtitle = sprintf("Error bars: 95%% CI | Simon effect = %.1f ms", simon),
         x = NULL, y = "RT (ms)") +
    theme_minimal(base_size = 15) +
    theme(plot.title = element_text(face = "bold"))

p2 <- ggplot(trimmed, aes(cond, RT, fill = cond)) +
    geom_violin(alpha = 0.55, color = NA) +
    geom_boxplot(width = 0.18, outlier.shape = NA, color = "grey20", alpha = 0.9) +
    scale_fill_manual(values = c("#2563eb", "#dc2626"), guide = "none") +
    coord_flip() +
    labs(title = "RT distribution (ggplot2)",
         subtitle = "Half-violin + boxplot, all cleaned trials pooled",
         x = NULL, y = "RT (ms)") +
    theme_minimal(base_size = 15) +
    theme(plot.title = element_text(face = "bold"))

p3 <- ggplot(wide, aes(x = reorder(subject, effect), y = effect,
                       fill = effect >= 0)) +
    geom_col(width = 0.6) +
    geom_hline(yintercept = mean(wide$effect), linetype = 2,
               color = "#7c3aed", linewidth = 0.9) +
    annotate("text", x = nrow(wide), y = mean(wide$effect),
             label = sprintf("Group mean: %.1f ms", mean(wide$effect)),
             hjust = 1, vjust = -0.6, color = "#7c3aed", size = 4) +
    scale_fill_manual(values = c("#ea580c", "#059669"), guide = "none") +
    labs(title = "Simon effect per subject (ggplot2)",
         subtitle = "Green = positive effect, orange = negative",
         x = "Subject", y = "Incongruent - Congruent (ms)") +
    theme_minimal(base_size = 15) +
    theme(plot.title = element_text(face = "bold"),
          axis.text.x = element_text(angle = 45, hjust = 1))

if (toScreen) {
    ## RStudio Plots 面板模式: 三张图依次显示, 可用面板左上角箭头翻页浏览
    print(p1)
    print(p2)
    print(p3)
} else {
    png("viz_3_ggplot2.png", width = 1400, height = 1800, res = 130)
    pushViewport(viewport(layout = grid.layout(3, 1)))
    print(p1, vp = viewport(layout.pos.row = 1))
    print(p2, vp = viewport(layout.pos.row = 2))
    print(p3, vp = viewport(layout.pos.row = 3))
    dev.off()
}

if (toScreen) {
    cat("已在 RStudio Plots 面板显示 6 页图 (base R 1页双面板 / lattice 2页 / ggplot2 3页),\n")
    cat("用面板左上角 <- -> 箭头翻页, Export 按钮导出. 终端运行则保存 PNG.\n")
} else {
    cat("已生成: viz_1_base_R.png, viz_2_lattice.png, viz_3_ggplot2.png\n")
}
