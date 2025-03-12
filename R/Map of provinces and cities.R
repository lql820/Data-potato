

# 加载包
library(sf)
library(ggplot2)
library(dplyr)
library(ggspatial)
library(readxl)
library(viridis)
library(extrafont)

# 导入字体（如果是第一次导入，需要运行并重启R会话）
# font_import()
# loadfonts(device = "win")

# 设定文件路径
county_shapefile_path <- "F:/学位论文/模型模拟/绘图-可视化/中国标准行政区划数据GS（2024）0650号/更改内蒙古新城区/蒙陕甘宁县区边界.shp"  # 请替换为实际路径
province_shapefile_path <- "F:/学位论文/模型模拟/绘图-可视化/中国标准行政区划数据GS（2024）0650号/蒙陕甘宁省边界.shp"  # 请替换为实际路径
excel_path <- "四省地图-NM_Yield.xlsx"                 # 请替换为实际路径
excel_path2 <- "经纬度.xlsx"  # 请替换为实际路径
# 读取县区Shape文件
county_map <- st_read(county_shapefile_path, quiet = FALSE)
county_map <- st_make_valid(county_map)  # 修复无效几何

# 读取省级边界Shape文件
province_borders <- st_read(province_shapefile_path, quiet = FALSE)
province_borders <- st_make_valid(province_borders)  # 修复无效几何
# 读取Excel文件
location_data <- read_excel(excel_path2)
# 读取Excel文件
potato_data <- read_excel(excel_path)

# 确认读取的数据
print(head(county_map))
print(head(province_borders))
print(head(potato_data))

# 确认数据的唯一性
cat("检查county_map中的唯一值:\n")
print(unique(county_map$NAME))
cat("检查potato_data中的唯一值:\n")
print(unique(potato_data$NAME))

# 处理列名
if (!"NAME" %in% names(potato_data)) {
  names(potato_data)[1] <- "NAME"  # 将第一列名称更改为 "NAME"
}

# 合并县区数据和产量数据
map_data <- dplyr::left_join(county_map, potato_data, by = "NAME")
# 定义度分到十进制度的转换函数
deg_to_dec <- function(deg) {
  if (grepl("°", deg)) {
    parts <- strsplit(gsub("[^0-9°']", "", deg), "°")[[1]]
    deg <- as.numeric(parts[1])
    min <- as.numeric(parts[2])
    dec <- deg + min / 60
    return(dec)
  } else {
    return(as.numeric(deg))
  }
}

# 转换经纬度数据
location_data$纬度 <- sapply(location_data$纬度, deg_to_dec)
location_data$经度 <- sapply(location_data$经度, deg_to_dec)

# 将经纬度数据转换为sf对象
locations_sf <- st_as_sf(location_data, coords = c("经度", "纬度"), crs = 4326, agr = "constant")

# 创建区间
map_data$Yield_category <- cut(map_data$Yield, 
                               breaks = c(1000, 2000, 3000, 4000, 5000), 
                               labels = c( "1000-2000","2000-3000", "3000-4000", 
                                           "4000-5000"))

colors <- c("#90EE90", "#E0C7A3",  "#F4A460", "#87CEEB")

# 绘制地图并加粗省边界
plot <- ggplot() +
  geom_sf(data = map_data, aes(fill = Yield_category), color = "black", size = 0.5) +  # 县区填充和边界线
  geom_sf(data = province_borders, color = "blue", size = 50, alpha = 0.1) +  # 加粗省边界
  geom_sf(data = locations_sf, color = "black", size = 0.5) +  # 标注点
  scale_fill_manual(
    name = expression("Province"),
    values = colors,
    labels = c("Inner Mongolia","Shaanxi", "Gansu", "Ningxia")
  ) +
  theme_minimal() +
  theme(
    legend.title = element_text(size = 8, family = "Times New Roman"),  # 设置图例标题的字体大小和字体
    legend.position = "bottom", # 设置图例位置在底部
    legend.direction = "vertical", # 设置图例为水平方向
    legend.key.width = unit(0.6, "cm"),       # 设置图例色带的宽度为1厘米
    legend.key.height = unit(0.2, "cm"),    # 设置图例色带的高度为0.2厘米
    legend.title.position = "top", # 将图例标题移动到默认位置（上方）
    legend.text = element_text(angle = 0, hjust = 0, family = "Times New Roman"),  # 将刻度标签倾斜30度并设置字体
    text = element_text(family = "Times New Roman"),  # 设置全局字体
    title = element_text(family = "Times New Roman"),  # 设置标题字体
    
    panel.background = element_rect(fill = "transparent", colour = NA),  # 设置背景为白色
    plot.background = element_rect(fill = "transparent", colour = NA),  # 设置背景为白色
    panel.border = element_rect(colour = "black", fill = NA, size = 0.5),  # 设置面板边框为黑色，无填充，大小为1
    plot.margin = margin(5, 5, 5, 5, "pt"),# 设置图表边缘的边距
    # 设置图例色带的边框粗度为0.5
    legend.key = element_rect(colour = NA, fill = NA, size = 0.2),
    # 设置图例的外边距，单位为厘米
    legend.margin = margin(t = 0, r = 1, b = 0, l = 0, unit = "cm")
  ) 

# 保存图片
ggsave("蒙陕甘宁—地图-有经纬度点.pdf", plot, width = 12, height = 12, units = "cm", 
       bg = "transparent", device = cairo_pdf, limitsize = TRUE)
cat("绘图完成")
