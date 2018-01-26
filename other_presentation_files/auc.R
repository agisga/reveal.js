# load some data, fit a logistic regression classifier
data(iris)
versicolor_virginica <- iris[iris$Species != "setosa", ]
logistic_reg_fit <- glm(Species ~ Sepal.Width + Sepal.Length,
                        data = versicolor_virginica,
                        family = "binomial")
y <- ifelse(versicolor_virginica$Species == "versicolor", 0, 1)
y_pred <- logistic_reg_fit$fitted.values

# get TPR and FPR at different values of the decision threshold
threshold <- seq(0, 1, length = 1000)
FPR <- sapply(threshold,
  function(thresh) {
    sum(y_pred >= thresh & y != 1) / sum(y != 1)
  })
TPR <- sapply(threshold,
  function(thresh) {
    sum(y_pred >= thresh & y == 1) / sum(y == 1)
  })

# plot an ROC curve

library(tidyverse)

FPR_TPR_df <- data_frame(FPR, TPR) %>%
  group_by(FPR) %>% summarize(TPR = max(TPR)) %>% tbl_df()
df_areaStep <- bind_rows(old = FPR_TPR_df,
                         new = mutate(FPR_TPR_df, TPR = lag(TPR, default = 0)),
                         .id = "source") %>%
               arrange(FPR, source)
text_df <- data.frame(
  x = c(0.3, 0.5),
  y = c(0.87, 0.3),
  text = c("ROC curve", "AUC (area under the curve)")
)

data_frame(FPR = c(0), TPR = c(0)) %>%
  bind_rows(FPR_TPR_df) %>%
  ggplot(aes(FPR, TPR)) + geom_step() +
    geom_ribbon(aes(x = FPR, ymin = 0, ymax = TPR), data = df_areaStep, alpha = 0.4) +
    geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), linetype = 2, size = 0.3) +
    geom_text(aes(x = x, y = y, label = text), data = text_df,
              vjust = "outward")

ggsave("../img/20180211-SPIE-Medical-Imaging/AUC.png")
