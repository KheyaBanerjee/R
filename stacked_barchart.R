library(ggplot2)
library(ggthemes)
library(plyr)
library(scales)
library(extrafont)

# Load the data
charts.data <- read.csv("copper-data-for-tutorial.csv")

# default plot
p4 <- ggplot() + geom_bar(aes(y = percentage, x = year, fill = product), data = charts.data, stat="identity")
p4

# add labels
p4 <- p4 + geom_text(data=charts.data, aes(x = year, y = percentage, label = paste0(percentage,"%")), size=4)
p4

# adjusting labels(using ddply function)
charts.data <- ddply(charts.data, .(year), transform, pos = percentage - (0.5 * percentage))

p4 <- ggplot() + geom_bar(aes(y = percentage, x = year, fill = product), data = charts.data, stat="identity")
p4 <- p4 + geom_text(data=charts.data, aes(x = year, y = pos, label = paste0(percentage,"%")), size=4)
p4

# adding legend
p4 <- p4 + theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank())
p4

# changing variables display
charts.data$product <- factor(charts.data$product, levels = c("copper","others"), labels = c("Copper","Pulp wood, Fruit, Salmon & Others"))

p4 <- ggplot() + geom_bar(aes(y = percentage, x = year, fill = product), data = charts.data, stat="identity") +
  geom_text(data=charts.data, aes(x = year, y = pos,
        label = paste0(percentage,"%")), size=4) +
  theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank())
p4

# adjusting x-axis scale
p4 <- p4 + scale_x_continuous(breaks=seq(2006,2014,1))
p4

# Adjusting axis, title & units
p4 <- p4 + labs(x="Year", y="Percentage") +
  scale_y_continuous(labels = dollar_format(suffix = "%", prefix = "")) + ggtitle("Composition of Exports to China (%)")
p4

# Adjusting color scale
fill <- c("#5F9EA0", "#E1B378")
p4 <- p4 + scale_fill_manual(values=fill)
p4

# Changing the theme
p4 <- ggplot() + theme_bw() +
  geom_bar(aes(y = percentage, x = year, fill = product), data = charts.data, stat="identity") +
  geom_text(data=charts.data, aes(x = year, y = pos, label = paste0(percentage,"%")), size=4) +
  theme(legend.position="bottom", legend.direction="horizontal",
        legend.title = element_blank()) +
  scale_x_continuous(breaks=seq(2006,2014,1)) +
  scale_y_continuous(labels = dollar_format(suffix = "%", prefix = "")) +
  labs(x="Year", y="Percentage") +
  ggtitle("Composition of Exports to China (%)")
p4

# Changing the theme “The Economist” (using with a 'safe" font - Verdana)
p4 <- ggplot() + theme_economist() + scale_fill_economist() +
  theme(plot.title=element_text(family="Verdana"),
        text=element_text(family="Verdana")) +
  geom_bar(aes(y = percentage, x = year, fill = product), data = charts.data, stat="identity") +
  geom_text(data=charts.data, aes(x = year, y = pos, label = paste0(percentage,"%")),
            colour="white", family="Verdana", size=4) +
  theme(legend.position="bottom", legend.direction="horizontal",
        legend.title = element_blank()) +
  scale_x_continuous(breaks=seq(2006,2014,1)) +
  scale_y_continuous(labels = dollar_format(suffix = "%", prefix = "")) +
  labs(x="Year", y="Percentage") +
  ggtitle("Composition of Exports to China (%)")
p4

# Changing the theme “Five Thirty Eight”
p4 <- ggplot() + theme_fivethirtyeight() + scale_fill_fivethirtyeight() +   
  theme(plot.title=element_text(family="Microsoft Sans Serif"),
        text=element_text(family="Microsoft Sans Serif")) +
  geom_bar(aes(y = percentage, x = year, fill = product), data = charts.data, stat="identity") +
  geom_text(data=charts.data, aes(x = year, y = pos, label = paste0(percentage,"%")),
            colour="white", family="Microsoft Sans Serif", size=4) +
  theme(legend.position="bottom", legend.direction="horizontal",
        legend.title = element_blank()) +
  scale_x_continuous(breaks=seq(2006,2014,1)) +
  scale_y_continuous(labels = dollar_format(suffix = "%", prefix = "")) +
  labs(x="Year", y="Percentage") +
  ggtitle("Composition of Exports to China (%)")
p4


# Changing the theme - create your own
fill <- c("#40b8d0", "#b2d183")
theme.my_chart <- 
  theme(legend.position="bottom", legend.direction="horizontal",
        legend.title = element_blank()) +
  theme(axis.line = element_line(size=1, colour = "black"),
        panel.grid.major = element_line(colour = "#d3d3d3"), panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank()) +
  theme(plot.title = element_text(size = 14, family = "Tahoma", face = "bold"),
        text=element_text(family="Tahoma"),
        axis.text.x=element_text(colour="black", size = 10),
        axis.text.y=element_text(colour="black", size = 10))

p4 <- ggplot() + theme.my_chart +
  geom_bar(aes(y = percentage, x = year, fill = product), data = charts.data, stat="identity") +
  geom_text(data=charts.data, aes(x = year, y = pos, label = paste0(percentage,"%")),
            colour="black", family="Tahoma", size=4) +
  scale_x_continuous(breaks=seq(2006,2014,1)) +
  scale_y_continuous(labels = dollar_format(suffix = "%", prefix = "")) +
  labs(x="Year", y="Percentage") +
  ggtitle("Composition of Exports to China (%)") +
  scale_fill_manual(values=fill)
p4
