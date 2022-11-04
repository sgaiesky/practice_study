rm(list = ls())

source("basic_lib.R")

data.source <- c("C:/Users/User/OneDrive - Simon Fraser University (1sfu)/PhD/lab projects/practice study/test_data")

#filter based on speed
file.list <- list.files(data.source) %>%
  grep("30",., value = TRUE, fixed = FALSE)

#filter based on location
sacral.imu <- c("1714")
left.imu <- c("1766")
right.imu <- c("1779")

t1 <- 12.5
t2 <- 17.5

sacral.dat <- read.csv(
  paste0(data.source, "/", 
         grep(paste(sacral.imu, collapse = "|"), file.list, value = TRUE)[1])
  ) %>%
  filter(.,
         time_s >= t1,
         time_s <= t2)

left.tib.dat <- read.csv(
  paste0(data.source, "/", 
         grep(paste(left.imu, collapse = "|"), file.list, value = TRUE)[1])
) %>%
  filter(.,
         time_s >= t1,
         time_s <= t2)

right.tib.dat <- read.csv(
  paste0(data.source, "/", 
         grep(paste(right.imu, collapse = "|"), file.list, value = TRUE)[1])
) %>%
  filter(.,
         time_s >= t1,
         time_s <= t2)



ggplot() +
  geom_point(data = sacral.dat,
             mapping = aes(x = time_s,
                           y = ay_m.s.s),
             colour = "red") +
  geom_point(data = left.tib.dat,
             mapping = aes(x = time_s,
                           y = ay_m.s.s),
             colour = "blue") +
  geom_point(data = right.tib.dat,
             mapping = aes(x = time_s,
                           y = ay_m.s.s),
             colour = "green") 

