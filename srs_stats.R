rm(list = ls())

source('basic_lib.R')

pivot_dat <- read_xlsx('run_data.xlsx') %>%
  filter(!grepl('remove', note)) %>%
  select(!contains("land") & !contains("take") & !contains('note')) %>%
  pivot_longer(cols = cadence:df, names_to = 'variables')

prelim.plot <- function(x) {

ggplot(data = filter(pivot_dat, id == x),
       mapping = aes(x = as.factor(session), y = value)) +
  geom_point(mapping = aes(colour = variables)) +
  geom_line(mapping = aes(colour = variables,
                          group = variables)) +
  labs(title = paste0('trial id:', x)) +
  facet_wrap(vars(as.factor(speed)))
  
}

lapply(unique(pivot_dat$id), prelim.plot)

