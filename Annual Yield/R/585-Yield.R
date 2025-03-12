library(readxl)
library(ggplot2)
library(dplyr)
library(trend)
library(extrafont)

# 加载额外字体
loadfonts()
# 设置字体
font_name <- "Times New Roman"

# 定义一个函数来绘制图形
plot_yield_trends <- function(region, nm_file, tm_file) {
  # 读取数据
  df_nm <- read_excel(nm_file, sheet = region)
  df_tm <- read_excel(tm_file, sheet = region)
  
  # 提取所需列
  df_nm <- df_nm %>% select(Year, mean_yield = GrainC_1_mean, 
                            sd_yield = GrainC_1_sd)
  df_tm <- df_tm %>% select(Year, mean_yield = GrainC_1_mean, 
                            sd_yield = GrainC_1_sd)
  
  # 计算置信区间（95%）
  df_nm <- df_nm %>% mutate(ci_low = mean_yield - 1.96 * sd_yield, 
                            ci_high = mean_yield + 1.96 * sd_yield)
  df_tm <- df_tm %>% mutate(ci_low = mean_yield - 1.96 * sd_yield,
                            ci_high = mean_yield + 1.96 * sd_yield)
  
  # Mann-Kendall test
  mk_test_nm <- mk.test(df_nm$mean_yield)
  mk_test_tm <- mk.test(df_tm$mean_yield)
  
  # 线性趋势线
  lm_nm <- lm(mean_yield ~ Year, data = df_nm)
  lm_tm <- lm(mean_yield ~ Year, data = df_tm)
  
  # 提取趋势线方程和p值
  lm_eqn_nm <- sprintf("y = %.2fx + %.2f", coef(lm_nm)[2], 
                       coef(lm_nm)[1])
  lm_eqn_tm <- sprintf("y = %.2fx + %.2f", coef(lm_tm)[2], 
                       coef(lm_tm)[1])
  p_value_nm <- formatC(mk_test_nm$p.value, format = "f", 
                        digits = 3)
  p_value_tm <- formatC(mk_test_tm$p.value, format = "f", 
                        digits = 3)
  
  # 根据地区动态生成标注
  labels <- c("内蒙古" = "d1", "陕西" = "d2", 
              "甘肃" = "d3", "宁夏" = "d4")
  label <- paste0("(", labels[[region]], ")")
  
  # 绘图
  plot <- ggplot() +
    geom_ribbon(data = df_nm, aes(x = Year, ymin = ci_low, 
                                  ymax = ci_high, fill = "NM"), 
                alpha = 0.2) +
    geom_line(data = df_nm, aes(x = Year, y = mean_yield, 
                                color = "NM"), size = 1) +
    geom_ribbon(data = df_tm, aes(x = Year, ymin = ci_low, 
                                  ymax = ci_high, fill = "TM"), 
                alpha = 0.2) +
    geom_line(data = df_tm, aes(x = Year, y = mean_yield, 
                                color = "TM"), size = 1) +
    geom_smooth(data = df_nm, aes(x = Year, y = mean_yield, 
                                  color = "NM"), method = "lm", 
                se = FALSE, linetype = "dashed", size = 1) +
    geom_smooth(data = df_tm, aes(x = Year, y = mean_yield, 
                                  color = "TM"), method = "lm",
                se = FALSE, linetype = "dashed", size = 1) +
    annotate("text", x = max(df_nm$Year), y = 2000, 
             label = paste("NM:", lm_eqn_nm, ", p = ", p_value_nm), 
             family = font_name, hjust = 1, size = 4) +
    annotate("text", x = max(df_nm$Year), y = 1700, 
             label = paste("TM:", lm_eqn_tm, ", p = ", p_value_tm), 
             family = font_name, hjust = 1, size = 4) +
    labs(x = "Year", y = expression("Tuber yield" ~ (kg * " " * C * " " * ha^-1))) +
    scale_x_continuous(limits = c(2020, 2100), 
                       breaks = seq(2020, 2100, by = 10), 
                       expand = c(0, 1.0)) +
    # 根据地区动态添加标注
    annotate("text", x = 2020, y = 4900, label = label, 
             hjust = 0, vjust = 1, family = font_name, 
             size = 5, fontface = "plain", color = "black") +
    scale_y_continuous(limits = c(1000, 5000), 
                       breaks = seq(1000, 5000, by = 1000), 
                       expand = c(0, 0.1)) +
    theme_minimal() +
    theme(text = element_text(family = font_name, size = 12), 
          plot.background = element_rect(fill = "white", 
                                         color = "black"), 
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          axis.title = element_text(size = 12, color = "black"), 
          axis.text = element_text(size = 12, color = "black"), 
          plot.title = element_text(hjust = 0.5), 
          axis.line = element_line(color = "black", size = 0.8), 
          axis.ticks.length = unit(0.2, "cm"), 
          panel.border = element_rect(color = "black", fill = NA, 
                                      size = 0.8), 
          axis.ticks = element_line(color = "black"), 
          legend.position = "top", 
          legend.title = element_text(size = 12), 
          legend.text = element_text(size = 12), 
          panel.grid = element_blank()) +
    scale_color_manual(values = c("NM" = "#5f5e62", 
                                  "TM" = "#2666fd")) +
    scale_fill_manual(values = c("NM" = "#5f5e62", 
                                 "TM" = "#2666fd"), 
                      guide = "none") +
    guides(color = guide_legend(title = element_blank())) +
    theme(legend.position = c(0.5, 1.05), 
          legend.justification = c("center", "top"), 
          legend.direction = "horizontal", 
          legend.box.margin = margin(t = 0.1, r = 0, 
                                     b = 0, l = 0, unit = "cm"), 
          legend.title = element_blank(), 
          legend.background = element_blank(), 
          legend.key.size = unit(0.8, "cm"), 
          legend.text = element_text(size = 12)) +
    theme(plot.margin = unit(c(0.25, 0.5, 0.1, 0), "cm"))
  
  
  # 保存图形
  ggsave(paste0("585_", region, "_yield.pdf"), plot, 
         width = 12, height = 8, units = "cm", 
         bg = "transparent", device = cairo_pdf, limitsize = TRUE)
  
  # 打印图形
  print(plot)
  
  # 输出完成信息
  cat(paste("绘图完成: ", region, "\n"))
}
# 地区列表
regions <- c("内蒙古", "陕西", "甘肃", "宁夏")
# 循环绘制每个地区的图形
for (region in regions) {
  plot_yield_trends(region, "585-NM_GrainC_1_result.xlsx", 
                    "585-TM_GrainC_1_result.xlsx")
}