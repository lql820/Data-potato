
library(readxl)
library(ggplot2)
library(dplyr)
library(trend)
library(extrafont)

# 加载额外字体
loadfonts()
# 设置字体
font_name <- "Times New Roman"
# 读取数据
nm_file <- "126-NM_GrainC_1_result.xlsx"
tm_file <- "126-TM_GrainC_1_result.xlsx"
df_nm <- read_excel(nm_file, sheet = "内蒙古")
df_tm <- read_excel(tm_file, sheet = "内蒙古")
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
lm_eqn_nm <- sprintf("y = %.2fx + %.2f", coef(lm_nm)[2], coef(lm_nm)[1])
lm_eqn_tm <- sprintf("y = %.2fx + %.2f", coef(lm_tm)[2], coef(lm_tm)[1])
p_value_nm <- formatC(mk_test_nm$p.value, format = "f", digits = 3)
p_value_tm <- formatC(mk_test_tm$p.value, format = "f", digits = 3)

# 绘图
plot <- ggplot() +
  # NM处理的年均产量及置信区间
  geom_ribbon(data = df_nm, aes(x = Year, ymin = ci_low, 
                                ymax = ci_high, fill = "NM"), 
              alpha = 0.2) +
  geom_line(data = df_nm, aes(x = Year, y = mean_yield, 
                              color = "NM"), size = 0.5) +
  # TM处理的年均产量及置信区间
  geom_ribbon(data = df_tm, aes(x = Year, ymin = ci_low, 
                                ymax = ci_high, fill = "TM"), 
              alpha = 0.2) +
  geom_line(data = df_tm, aes(x = Year, y = mean_yield, 
                              color = "TM"), size = 0.5) +
  # 趋势线
  geom_smooth(data = df_nm, aes(x = Year, y = mean_yield, 
                                color = "NM"), method = "lm", 
              se = FALSE, linetype = "dashed", size = 1) +
  geom_smooth(data = df_tm, aes(x = Year, y = mean_yield, 
                                color = "TM"), method = "lm", 
              se = FALSE, linetype = "dashed", size = 1) +
  # 添加趋势线方程和p值
  annotate("text", x = 2022, 
           y =  5000 - 680, 
           label = paste("NM:", lm_eqn_nm, ", p = ", p_value_nm),  
           # 用空格代替换行符
           family = font_name, 
           hjust = 0, size = 3.5) +
  annotate("text", x = 2022, 
           y=  5000 - 520, 
           label = paste("TM:",lm_eqn_tm, ", p = ", p_value_tm),  
           # 用空格代替换行符 
           family = font_name, 
           hjust = 0, size = 3.5) +
  # 图例和标签
  labs(x = "Year", 
   y = expression("Tuber yield" ~ (kg * " " * C * " " * ha^-1))) +
  scale_x_continuous(limits = c(2020, 2100),
                     breaks = seq(2020, 2100, by = 10),
                     expand = c(0, 1.0)) +
  # 标注(a) 和 趋势方程
  annotate("text", x = 2020, y = 4900, label = "(a)",
           hjust = 0, vjust = 1, family = font_name, 
           size = 5, fontface = "plain", color = "black") +
  # 设置y轴范围和刻度间隔
  scale_y_continuous(limits = c(1000, 5000), 
                     breaks = seq(1000, 5000, by = 1000),
                     expand = c(0, 0.1) ) +
  theme_minimal() +
  theme(text = element_text(family = font_name, size = 10),
        plot.background = element_rect(fill = "white", color = "black"),  # 设置背景色和边框颜色
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_text(size = 14, color = "black"),
        axis.text = element_text(size = 10,color = "black"),
        plot.title = element_text(hjust = 0.5),
        axis.line = element_line(color = "black",size=0.5),  # 设置轴线条的颜色
        axis.ticks.length = unit(0.2, "cm"),  # 设置刻度线的长度
        panel.border = element_rect(color = "black", fill = NA, 
                                    size = 0.5),#上下左右轴线显示
        axis.ticks = element_line(color = "black"),  # 设置刻度线的颜色
        legend.position = "top",  # 图例位置
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10),
        panel.grid = element_blank()) +  # 去掉背景网格
  
  # 颜色和填充比例尺，只设置趋势线的颜色，不显示图例名称，并隐藏fill图例
  scale_color_manual(values = c("NM" = "#5f5e62", 
                                "TM" = "#2666fd")) +
  scale_fill_manual(values = c("NM" = "#5f5e62", 
                               "TM" = "#2666fd"), guide = "none")+
  guides(color = guide_legend(title = element_blank()))+ # 隐藏颜色图例的标题
  theme(
    # 将图例放置在顶部边缘，稍微偏下1cm
    legend.position = c(0.5, 1),
    legend.justification = c("center", "top"),  # 图例在顶部右侧，垂直对齐到顶部
    legend.direction = "horizontal",  # 图例项水平排列
    # 使用margin函数调整图例与顶部边缘的距离，下移1cm
    legend.box.margin = margin(t = 0.1, r = 0, b = 0, l = 0, unit = "cm"),
    legend.title = element_blank(),  # 隐藏图例标题
    legend.background = element_blank(),  # 隐藏图例背景
    legend.key.size = unit(0.4, "cm"),  # 设置图例标记（颜色框）的大小
    legend.text = element_text(size = 10)  # 设置图例文本的字号
  ) +
  

# 设置图幅大小，移除画幅空白,上右（下（bottom）左（left）
  theme(plot.margin = unit(c(0.25, 0.5, 0.1, 0), "cm"))
# 将图保存为pdf
ggsave("a_内蒙古_yield.pdf", plot, width = 16, 
       height = 10, units = "cm",bg = "white", 
       device = cairo_pdf, 
       limitsize = TRUE)
# 显示绘图结果
print(plot)

cat("绘图完成")

