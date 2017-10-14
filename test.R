library(class)

offline <- offline %>% filter(mac != top_macs[2])

prepare_data <- function(data, sample_angle = FALSE) {
  df <- data %>% 
    group_by(posX, posY, angle, mac) %>%
    summarise(avg_ss = mean(signal)) %>% 
    spread(mac, avg_ss) %>% 
    ungroup()
  if (sample_angle) {
    df <- df %>% 
      group_by(posX, posY) %>% 
      sample_n(1)
  }
  df
}

train <- prepare_data(offline)
test <- prepare_data(online)

select_train <- function(new_angle, data, m) {
  refs <- seq(0, by = 45, length = 8)
  nearest_angle <- round_orientation(new_angle)
  
  if (m %% 2) {
    angles <- seq(-45 * (m-1)/2, 45 * (m-1)/2, length = m)
  } else {
    angles <- seq(-45 * (m/2 - 1), 45 * m/2, length = m)
  }
  angles <- (angles + nearest_angle) %% 360
  
  data %>%
    filter(angle %in% angles) %>% 
    ungroup()
}

closest_pts <- function(train, testvec, k) {
  train %>% 
    gather(mac, signal, -(1:2)) %>% 
    group_by(posX, posY) %>% 
    mutate(diff = signal - testvec) %>% 
    summarise(dist = sum(diff*diff)) %>% 
    arrange(dist) %>% 
    head(k) %>% 
    # select(posX, posY) %>% 
    ungroup() %>% 
    inner_join(train, by = c('posX', 'posY'))
}

avg_neighbors <- function(neighbors) {
  neighbors %>%
    summarise_at(vars(posX:posY), mean)
}

weighted_avg_neighbors <- function(neighbors) {
  neighbors %>%
    mutate(wtdX = posX * (1/dist) / sum(1/dist),
           wtdY = posY * (1/dist) / sum(1/dist)) %>%
    summarise(posX = sum(wtdX), posY = sum(wtdY))
}

my_knn <- function(train, test, m = 1, k = 3) {
  # one_pred <- function(testrow) {
  #   testvec <- testrow[str_detect(names(testrow), '^00:')] %>% as.numeric()
  #   train_ss <- select_train(as.numeric(as.character(testrow$angle)), train, m) %>% 
  #     group_by(posX, posY) %>% 
  #     summarise_at(-1, mean)
  #   neighbors <- closest_pts(train_ss, testvec, k)
  #   weighted_avg_neighbors(neighbors)
  # }
  # 
  # test %>% 
  #   transpose() %>% 
  #   map_dfr(one_pred)
  pred_xy <- list()
  for (i in 1:nrow(test)) {
    testvec <- test[i, ] %>%
      select(starts_with('00:')) %>%
      unlist()
    train_ss <- select_train(as.numeric(as.character(test$angle[i])), train, m) %>%
      group_by(posX, posY) %>%
      summarise_at(-1, mean)
    neighbors <- closest_pts(train_ss, testvec, k)
    pred_xy[[i]] <- weighted_avg_neighbors(neighbors)
  }
  bind_rows(pred_xy)
}

plot_locations <- function(predictions) {
  ggplot(train, aes(posX, posY)) +
    geom_point(color = 'grey60', size = 0.5) +
    geom_point(data = test) +
    geom_point(data = as_data_frame(predictions), shape = 8) +
    geom_point(data = ap, aes(x, y), shape = 15, size = 2) +
    geom_segment(data = bind_cols(test, as_data_frame(predictions)),
                 aes(x = posX, y = posY, xend = posX1, yend = posY1),
                 color = 'dodgerblue') +
    labs(x = '', y = '')
}

# est_xy_k3 <- my_knn(train, test, m = 3, k = 3)
# 
# 
# plot_locations(est_xy_k3)
# 
# calc_error(est_xy_k3, select(test, posX, posY))
# head(est_xy_k3)




######

my_knn(offline_fold, online_fold, m = 3, k = 3)