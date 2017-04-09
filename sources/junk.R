# Junk for later

## terrible residuals
```{r terrible}
mod <- lm(log(weight) ~ sex*(weight + drink + height), data = UK) ## notice weight used twice...
```

## cov
```{r cov matrix}
summary(mod_alien_lm)$sigma^2 * solve(t(X) %*% X)  ## same as vcov(mod_alien_lm)
```
