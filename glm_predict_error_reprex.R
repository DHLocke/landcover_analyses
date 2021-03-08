# make data
set.seed(19870630)
n <- 1000
df <- data.frame(y = rgamma(n, shape = .5, rate = 1),
                 #runif(n, 0, 1), # trows same error
                 x1 = runif(n, 0, 100),
                 x2 = runif(n, 0, 100),
                 x3 = runif(n, -1, 1))

df$x2 <-  df$x1*df$x1

# refine data by scaling
df$x1 <- scale(df$x1, center = TRUE)
df$x2 <- scale(df$x2, center = TRUE)
df$x3 <- scale(df$x3, center = TRUE)

# double check
head(df); plot(df)

# fit model
mod <- glm(y ~ x1 + x2 + x3, data = df, family=Gamma(link="log"))

# confirm, success
summary(mod)


# make data to retain predictions
## first get realistic ranges of variables of interest, other vars will be held at mean
(x1_span <- c(rep(seq(min(df$x1), max(df$x1)), length = 50)))
(x2_span <- c(rep(seq(min(df$x2), max(df$x2)), length = 50)))


df_pred_x1_x2 <- data.frame(x1 = x1_span,
                            x2 = x2_span,
                            x3 = mean(df$x3))

# generate function for prediction ml predicted values
predict_fun <- function(my_glm) {
  predict(my_glm, newdata = df_pred_x1_x2)   # this is predict.glm
}

df_pred_x1_x2$y_value_pred <- predict_fun(mod) # error

# "Error: variables ‘x1’, ‘x2’, ‘x3’ were specified with different types from the fit"
                                                      
# End March 8, 2021