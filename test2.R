get_summary <- function(orientation, similarity_angle, num_samples) {
  refs <- seq(0, by = 45, length = 8)
  lower_bd <- (orientation - similarity_angle) %% 360
  upper_bd <- (orientation + similarity_angle) %% 360
  if ((orientation - similarity_angle) %/% 360 == 0 &&
      (orientation + similarity_angle) %/% 360 == 0) {
    angles <- refs[which(refs >= lower_bd & refs <= upper_bd)]
  } else {
    angles <- refs[which(refs >= lower_bd | refs <= upper_bd)]
  }
  
  offline %>% 
    filter(angle %in% angles) %>% 
    group_by(posX, posY, mac) %>% 
    # sample_n(num_samples) %>% 
    summarise(avg_signal = mean(signal), sd_signal = sd(signal)) %>% 
    ungroup()
}

summ <- offline %>% 
  group_by(posX, posY, mac) %>% 
  summarise(avg_signal = mean(signal), sd_signal = sd(signal)) %>% 
  ungroup()

beta <- 0.01

all_signals <- seq(min(offline$signal), max(offline$signal))
all_probs <- map_dbl(all_signals, ~ pnorm(.x + 0.5, summ$avg_signal[1], summ$sd_signal[1]) - pnorm(.x - 0.5, summ$avg_signal[1], summ$sd_signal[1]))


find_prob <- function(sig, orientation, similarity_angle, num_samples) {
  get_summary(orientation, similarity_angle, num_samples) %>% 
    group_by(posX, posY) %>% 
    left_join(sig, by = 'mac') %>% 
    mutate(g = pnorm(obs_signal + 0.5, avg_signal, sd_signal) - 
             pnorm(obs_signal - 0.5, avg_signal, sd_signal)) %>% 
    summarise(p = prod(g)) %>% 
    ungroup() %>% 
    mutate(pi = (p / n()),
           pi = pi / sum(pi)) %>%
    select(-p) %>% 
    arrange(desc(pi))
}

online_summary <- online %>% 
  group_by(posX, posY, orientation, mac) %>% 
  summarise(obs_signal = mean(signal))

get_measurements <- function(df) {
  df %>% group_by(posX, posY, orientation) %>% nest() %>% .$data
}

orientations <- online_summary %>% summarise(o = mean(orientation)) %>% .$o

Mlist <- get_measurements(online_summary)
find_prob(Mlist[[48]], orientations[48], 69, 20)
results <- map2_dfr(Mlist, orientations, find_prob, similarity_angle = 69, num_samples = 20, .id = 'point')

calculate_predictions <- function(results, k) {
  results %>% 
    group_by(pt = as.numeric(point)) %>% 
    top_n(k, pi) %>% 
    summarise(x = mean(posX), y = mean(posY))
}

error_by_k <- function(k, results, actual) {
  preds <- calculate_predictions(results, k) %>% select(x, y)
  sqrt(calc_error(preds, actual) / nrow(actual))
}

actual <- online_summary %>% summarise() %>% select(posX, posY)
errs <- map_dbl(1:20, error_by_k, results = results, actual = actual)
data_frame(k = 1:20, err = errs) %>% 
  ggplot(aes(k, err)) +
  geom_line() +
  scale_x_continuous(breaks = 1:20)

preds <- calculate_predictions(results, which.min(errs)) %>% select(x, y)

online2 <- online %>% group_by(posX, posY, orientation) %>% summarise() %>% bind_cols(preds)
offline %>% 
  group_by(posX, posY) %>% 
  summarise() %>% 
  ggplot(aes(posX, posY)) +
  geom_point(color = 'grey60', size = 0.5) +
  geom_point(data = ap, aes(x, y), shape = 15, size = 2) +
  geom_point(data = online2, aes(posX, posY)) +
  geom_point(data = online2, aes(x, y), shape = 8) +
  geom_segment(data = online2, aes(x = posX, y = posY, xend = x, yend = y), color = 'dodgerblue')


i <- 30
r <- results %>% filter(point == i)
smooth <- fields::Tps(cbind(r$posX, r$posY), r$pi)
viz <- fields::predictSurface(smooth)
fields::plot.surface(viz, type = 'C')
points(summarise(group_by(offline, posX, posY))$posX,
       summarise(group_by(offline, posX, posY))$posY,
       col = 'grey40')
points(online2$posX[i], online2$posY[i], pch = 15, cex = 3, col = 'grey80')

