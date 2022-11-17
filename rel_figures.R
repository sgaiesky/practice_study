rm(list = ls())

source("basic_lib.R")
dat_pat <- c("C:/Users/User/OneDrive - Simon Fraser University (1sfu)/Plantiga Acceleration Reliability Paper/Stats")

dat <- read_xlsx(paste0(dat_pat, "/chris reliability results final.xlsx"), sheet = "Figures")

dat$time %<>% as.factor()
dat$variable %<>% as.factor()
dat$time <- relevel(dat$time,"baseline")
dat$speed %<>% as.factor()

short_rel <- dat %>%
  filter(time == "1-wk")

long_rel <- dat %>%
  filter(time == "3-mth")

plot_lab <- c("Baseline", "1-week", "3-months")
#plots are busy with all the accelerations plotted in one 
ggplot(data = filter(dat,
                     grepl("peak_result", variable) & measure == "mean"),
       mapping = aes(x = time, y = value)) +
  geom_errorbar(mapping = aes(ymin = value - sd, ymax = value + sd, colour = speed),
                width = 0.05) +
  geom_point(mapping = aes(colour = speed),
             shape = 19, size = 3) +
  geom_line(mapping = aes(group = speed, colour = speed)) +
  labs(
    #title = "Mean Resultant Accelerations",
    x = "",
    y = "Acceleration (g's)"
  ) +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, 20)) +
  scale_x_discrete(labels = plot_lab) +
  theme_classic()



