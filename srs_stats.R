rm(list = ls())

source('basic_lib.R')
library("psych")
library("xlsx")

pivot_dat <- read.xlsx('run_data.xlsx', sheetName = 'data') %>%
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

#lapply(unique(pivot_dat$id), prelim.plot)

#descriptives

descrip <- pivot_dat %>%
  group_by(session, speed, variables) %>%
  summarise(avg = mean(value, na.rm = TRUE),
            sd = sd(value, na.rm = TRUE))

sd_tbl <- descrip %>%
  select(!avg) %>%
  pivot_wider(names_from = session, values_from = sd) %>%
  rename('sd_1' = '1',
         'sd_2' = '2') %>%
  rowwise() %>%
  mutate(sd_mean = (sd_1+sd_2)/2) %>%
  mutate_if(is.numeric, round, 2)

icc_res <- list()

for (spd in unique(pivot_dat$speed)) {
  nam <- paste0('icc_', spd)
  
  icc_res[[paste0(nam)]] <- list()

  for (var in unique(pivot_dat$variables)) {
    dat <- pivot_dat %>%
      filter(speed == spd & grepl(paste0(var), variables)) %>%
      pivot_wider(names_from = session, values_from = value) %>%
      na.omit()
    
    x <- ICC(dat[4:5], missing = TRUE)
    
    icc_res[[nam]][[paste0(var)]] <- x$results
  }
}

file_name <- "icc_results.xlsx"

if (file.exists(file_name)) {
  unlink(file_name)
  print("File is deleted..")
} else{
  print("File not exists..")
}

for (i in 1:length(icc_res)) {
  
write.xlsx(icc_res[[i]], paste0('icc_results.xlsx'), sheetName = names(icc_res[i]), append = TRUE)
  
}

write.xlsx(sd_tbl, paste0('icc_results.xlsx'), sheetName = 'sd_table', append = TRUE)

