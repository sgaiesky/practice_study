rm(list = ls())

source("basic_lib.R")
dat_pat <- c("C:/Users/User/OneDrive - Simon Fraser University (1sfu)/Plantiga Acceleration Reliability Paper/Stats")

dat <- read_xlsx(paste0(dat_pat, "/chris reliability results final.xlsx"), sheet = "Figures")

dat$time %<>% as.factor()
dat$variable %<>% as.factor()
dat$time <- relevel(dat$time,"baseline")
dat$speed %<>% as.factor()

short_rel <- dat %>%
  filter(time == "1-wk" & !grepl("icc", measure))

long_rel <- dat %>%
  filter(time == "3-mth")

plot_lab <- c("Baseline", "1-week", "3-months")
facet_lab <- as_labeller(c("peak_result" = "Peak Resultant",
                         "peak_vert" = "Peak Vertical",
                         "peak_ap" = "Peak Anterior-Posterior"))
text_size <- 10
text_angle <- 0

#facet_wrap based on variable...?

test_plot <- ggplot(data = filter(dat,
                     !grepl("cv", variable) & !grepl("asym", variable) &measure == "mean"),
               mapping = aes(x = time, y = value)) +
  geom_errorbar(mapping = aes(ymin = value - sd, ymax = value + sd,
                              colour = speed),
                width = 0.05) +
  geom_point(mapping = aes(colour = speed),
             shape = 19,
             size = 3) +
  geom_line(mapping = aes(group = speed,
                          colour = speed)) +
  labs(
    x = "",
    y = "Average Acceleration (g's)",
    colour = "Speed"
  ) +
  guides(group = "none") +
  scale_x_discrete(labels = plot_lab,
                   expand = c(0.15, 0)) +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0,15)) +
  scale_color_manual(labels = c("2.5 m/s", "3.0 m/s", "3.5 m/s"),
                     values = c("#FC8D59", "#D7301F", "#7F0000")) +
  theme_classic() +
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.text = element_text(face = "bold", size = text_size),
        legend.title = element_text(face = "bold", size = text_size),
        axis.text.x = element_text(colour = "black", face = "bold", 
                                   size = text_size, angle = text_angle),
        axis.title.y = element_text(face = "bold", size = text_size),
        strip.placement = "outside",
        strip.text = element_text(face = "bold", size = text_size + 2),
        strip.background = element_blank()) +
  facet_wrap(vars(variable),
             labeller = facet_lab)

print(test_plot)

ggsave("test_plot.png", plot = test_plot)

