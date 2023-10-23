library(dplyr)
library(ggplot2)



# Run 1000 Simulations of a Coin Flip
ran_sam <- function(x){
  data.frame(
    result =
      replicate(
        n = 1000,    
        expr = sample(
          x = c("Heads","Tails"),
          size = 1,
          prob = c(.5,.5)))) |> 
    ggplot(aes(result)) +
    geom_bar(stat = "count") +
    theme_minimal()
}


flip_coin <-  function() {
  heads_cnt <- 0
  tails_cnt <- 0
  flips <- 0
  
  while (heads_cnt/flips !=0.5 ||
         tails_cnt/flips !=0.5) {
           result <- sample(c("Heads","Tails"),1)
        if (result == "Heads") {
          heads_cnt <- heads_cnt + 1
        } else {
          tails_cnt <- tails_cnt + 1
        }
        flips <- flips + 1
         }
    return(flips)
}


library(forcats)

tibble(
  data = runif(100,3,10) |> round(0),
  data_m = mean(data)) |> 
  mutate(dist = abs(data - data_m)) |> 
  ggplot(aes(data, dist)) +
  geom_point()



data.frame(
  degree = c("PhD","PsyD","Health Psych",
             "Social Psych","Child Psych"),
  duration = c(7,5,5,7,7)
) |> 
  ggplot(aes(fct_reorder(degree,duration),duration,
             fill = duration)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  coord_flip() +
  labs(x = "",
       y = "") +
  ylim(0,8) +
  theme(
    legend.position = "none"
  )
