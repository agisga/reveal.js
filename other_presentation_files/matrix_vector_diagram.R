library(tidyverse)

#--- This is for the prospectus presentation on 3/12/2018

set.seed(20180312)

# generate a Cov(X, Y) matrix
n_col_X <- 7
n_col_Y <- 5
Sigma <- matrix(NA, nrow = n_col_X, ncol = n_col_Y)
Sigma[1, 3] <- runif(1, min = -1, max = 1)
Sigma[2, 1:4] <- runif(4, min = -1, max = 1)
Sigma[3, c(1, 3, 4)] <- runif(3, min = -1, max = 1)
Sigma[7, 3] <- runif(1, min = -1, max = 1)

# generate "canonical vectors"
u <- c(NA, 0.8, -0.6, NA, -0.5, NA, 0.1)
v <- c(-0.3, NA, 0.7, NA, 0.2)

# put everything together and plot
everything_mat <- matrix(NA, nrow = n_col_X + 2, ncol = n_col_Y + 2)
everything_mat[3:nrow(everything_mat), 3:ncol(everything_mat)] <- Sigma
everything_mat[3:nrow(everything_mat), 1] <- u
everything_mat[1, 3:ncol(everything_mat)] <- v
everything_df <- as_data_frame(everything_mat) %>%
  mutate(y = 1:nrow(everything_mat)) %>%
  gather(x, value, -y) %>%
  mutate(x = as.integer(gsub("V", "", x)))

everything_df %>% ggplot(aes(x, y)) +
  geom_tile(aes(fill = value), color = "black", size = 2,
            show.legend = FALSE) +
  scale_y_reverse() +
  scale_fill_distiller(palette = "RdBu") +
  theme_void()

ggsave("cov_mat.png")
