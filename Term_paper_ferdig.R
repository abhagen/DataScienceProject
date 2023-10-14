### Semester project in Data Science

# Candidate number 218

setwd(" ~ /ECON4170 Data Science/Term Paper ECON4170") # Set own working directory, with ~ for anonymity

library(tidyverse) # several useful functions for plotting and wrangling
library(tidymodels) # for machine learning
library(jsonlite) # import JSON-files
library(gtrendsR) # trends
library(gridExtra) # add plots

# Google Trend Search for Expected Goals ----------------------------------
result <- gtrends(keyword = "Expected goals", geo = "", time = "all", low_search_volume = T)
interest_over_time <- result$interest_over_time
ggplot(data = interest_over_time, mapping = aes(x = date, y = hits)) + geom_line() + xlab("Year") + ylab("Popularity") + 
  labs(title = "Popularity of Expected Goals", subtitle = "Google search trend from 2005 to 2022")

# Importing the data ------------------------------------------------------

### import JSON-files
events_eur_ch <- fromJSON("events/events_European_Championship.json")
events_eng <- fromJSON("events/events_England.json")
events_wc <- fromJSON("events/events_World_Cup.json")
events_fra <- fromJSON("events/events_France.json")
events_ger <- fromJSON("events/events_Germany.json")
events_ita <- fromJSON("events/events_Italy.json")
events_spa <- fromJSON("events/events_Spain.json")

all_events <- bind_rows(events_eng, events_eur_ch, events_spa, events_ita, # merge the data
                        events_ger, events_fra, events_wc)
rm(events_eng, events_eur_ch, events_spa, events_ita, #remove all data I do not need anymore
   events_ger, events_fra, events_wc)
# Section 1: Prepare the data --------------------------------------------------------

events_filtered <- all_events %>%
  mutate(previous_event = lag(subEventName),
         matchId_lag = lag(matchId)) %>% 
  filter(eventId %in% 10)

n <- length(events_filtered$eventId)
obs <- c(1:n)

is_goal <- c(rep(0,n)) # whether the shot resulted in a goal or not
is_header <- c(rep(0,n)) # different xG for headers and shots with a foot
x_origin <- c(rep(0,n))  #origin of the shot, measured in percentage closeness to the opposition goal #from the attackers point of view
y_origin <- c(rep(0, n)) #origin of the shot, measured in percentage closeness to the right side of the pitch #from the attackers point of view

for (i in obs){
  is_goal[i] <- ifelse(events_filtered$tags[i][[1]][[1]][1]==101, yes = 1, no = 0) #dummy variable, if 1, the shot is a goal, if 0, the shot did not result in a goal
  is_header[i] <- ifelse(403 %in% events_filtered$tags[i][[1]][[1]], yes = 1, no = 0) #dummy variable, 1=header, 0=shot with foot, the event Id for header is 403
  x_origin[i] <- events_filtered$positions[i][[1]][2][[1]][1] 
  y_origin[i] <- events_filtered$positions[i][[1]][1][[1]][1]
}

x_origin_meters <- function(x){
x_meters <- 95.4 #standard length of pitch
x_origin_meters <- (100 - x_origin)*x_meters/100 # (100% - % from goal)*length in meters/100%
}

y_origin_meters <- function(y){
y_meters <- 76.25 # standard width
y_origin_meters <- abs(50 - y_origin)*y_meters/100 # absolute value of (50% (midpoint of the pitch) - % from goal) times width of pitch /100%
}

y_width_meters <- function(y_origin){ # To get width of field. For later plotting
  y_meters <- 76.25 #standard length of pitch
  y_width_meters <- (100 - y_origin)*y_meters/100 # (100% - % from sideline)*length in meters/100%
}

events_filtered <- events_filtered %>% 
  mutate(is_goal = is_goal,
         is_header = is_header,
         x_origin = x_origin,
         y_origin = y_origin,
         x_origin_meters = x_origin_meters(x_origin),
         y_origin_meters = y_origin_meters(y_origin),
         y_width = y_width_meters(y_origin),
         observation = obs) %>% 
  select(!tags) %>%  #remove the data frames within the columns now that I have extracted the information I need from tags
  select(!positions)  #same as above, but for the positions

# Add angle and distance to the data
distance <- function(a, b) {
  distance <- sqrt(a*a + b*b) # pythagoras
}
# trigonometric formula for calculating the angle of the shot relative to the goal posts. 
angle <- function(x_origin, y_origin){
  x_meters <- 95.4 # standard length
  y_meters <- 76.25 # standard width
  x_shift <- (100 - x_origin)*x_meters/100
  y_shift <- (50 - y_origin)*y_meters/100
  angle <- atan((7.32*x_shift)/(x_shift*x_shift + y_shift*y_shift - (7.32/2)*(7.32/2))) # trigonometric formula for calculating the angle of the shot relative to the goal posts. 
  angle <- ifelse(angle < 0, angle + pi, angle)
  angle_degrees <- angle*180/pi
}

#create shots df
shots <- events_filtered %>% 
  select(is_goal, is_header, x_origin, y_origin, x_origin_meters, y_origin_meters, y_width, previous_event, # outcome ~ predictors
          playerId, matchId, matchId_lag , teamId, observation) %>% # Ids
  mutate(is_goal = factor(is_goal),
         is_header = factor(is_header),
         distance = distance(x_origin_meters, y_origin_meters),# distance from goal mid-point
         angle = angle(x_origin, y_origin), 
         previous_event = factor(previous_event)) %>% 
  filter(matchId==matchId_lag) %>% 
  select(!matchId_lag) %>% #do not need the three following cols 
  select(!x_origin) %>%
  select(!y_origin)

 # write the csv for easy access to the data when returning to the project
 # still with ~ for anonymity
 # write_csv(shots, " ~ /ECON4170 Data Science/Term Paper ECON4170/shots_ferdig.csv")
 # removed from the code to run it as one

# Section 2: Analysis ---------------------------------------------------------------

 # shots <- read_csv("shots_ferdig.csv") %>% 
 # mutate(is_goal = factor(is_goal), #for importing I make these variables into factors straight away
 #      is_header = factor(is_header),
 #      previous_event = factor(previous_event))

set.seed(4170) # if you want to obtain the same results as I

shots_split <- initial_split(shots, prop = 0.8)
shots_train <- training(shots_split)
shots_test <- testing(shots_split)

# set a recipe ####
rec <- recipe(shots_train, is_goal ~ .) %>% 
  update_role(playerId, matchId, teamId, x_origin_meters, y_origin_meters, y_width, observation, new_role = "ID") %>% 
  step_dummy(all_nominal_predictors())

# logistic regression model ####

logreg_mod <- logistic_reg() %>% # model
  set_engine("glm") # engine

logreg_wf <- workflow() %>% #workflow
  add_model(logreg_mod) %>% 
  add_recipe(rec)

start_time_lr <- Sys.time()

logreg_fit <- logreg_wf %>% # fit the training data
  fit(data = shots_train)

end_time_lr <- Sys.time()

logreg_tidy <- logreg_fit %>% #tidy data for coeffs
  extract_fit_parsnip() %>% 
  tidy()

logreg_pred <- predict(logreg_fit, shots_test, type = "prob") %>% 
  rename("xG_logreg" = ".pred_1") %>%
  mutate(predicted_goal_logreg = factor(if_else(xG_logreg > .5, 1, 0))) %>% 
  select(xG_logreg, predicted_goal_logreg)

# Random Forest ####

rf_mod <- rand_forest(trees = 1500) %>% 
  set_engine("ranger") %>% 
  set_mode("classification")

rf_wf <- workflow() %>% 
  add_model(rf_mod) %>% 
  add_recipe(rec)

start_time_rf <- Sys.time() #check time usage

rf_fit <- rf_wf %>% 
  fit(data = shots_train)

end_time_rf <- Sys.time()

rf_pred <- predict(rf_fit, shots_test, type = "prob") %>% 
  rename("xG_rf" = ".pred_1") %>% 
  mutate(predicted_goal_rf = factor(ifelse(xG_rf > .5, 1, 0))) %>% 
  select(xG_rf, predicted_goal_rf)

# Extreme Gradient Boost model ####

xgb_mod <- boost_tree(trees = 30) %>% 
  set_engine("xgboost") %>% 
  set_mode("classification")

xgb_wf <- workflow() %>%
  add_model(xgb_mod) %>% 
  add_recipe(rec)

start_time_xgb <- Sys.time()

xgb_fit <- xgb_wf %>% 
  fit(shots_train)

end_time_xgb <- Sys.time()

xgb_pred <- predict(xgb_fit, shots_test, type = "prob") %>% 
  rename("xG_xgb" = ".pred_1") %>% 
  mutate(predicted_goal_xgb = factor(ifelse(xG_xgb > .5, 1, 0))) %>% 
  select(xG_xgb, predicted_goal_xgb)

# Time difference of the models ####

time_lr <- end_time_lr - start_time_lr
time_rf <- end_time_rf - start_time_rf
time_xgb <- end_time_xgb - start_time_xgb

# Combine results ####

shots_test <- shots_test %>% 
  bind_cols(logreg_pred, rf_pred, xgb_pred)

shots_test %>% 
  metrics(truth = is_goal, predicted_goal_logreg) %>%
  select(-.estimator) %>%
  filter(.metric == "accuracy")

shots_test %>% 
  metrics(truth = is_goal, predicted_goal_xgb) %>%
  select(-.estimator) %>%
  filter(.metric == "accuracy")

shots_test %>% 
  metrics(truth = is_goal, predicted_goal_rf) %>%
  select(-.estimator) %>%
  filter(.metric == "accuracy")

# Confusion Matrices ####

confmat1 <- shots_test %>% 
  conf_mat(truth = is_goal, predicted_goal_logreg) %>% # select which variables to construct conf.mat from
  pluck(1) %>%
  as_tibble() %>%
  ggplot(aes(Prediction, Truth, alpha = n)) +
  geom_tile(show.legend = FALSE) +
  geom_text(aes(label = n), colour = "white", alpha = 1, size = 8)+
  labs(title = "Confusion Matrix for Logistic Regression")

confmat2 <- shots_test %>% 
  conf_mat(truth = is_goal, predicted_goal_rf) %>%
  pluck(1) %>%
  as_tibble() %>%
  ggplot(aes(Prediction, Truth, alpha = n)) +
  geom_tile(show.legend = FALSE) +
  geom_text(aes(label = n), colour = "white", alpha = 1, size = 8)+
  labs(title = "Confusion Matrix for Random Forest")

confmat3 <- shots_test %>% 
  conf_mat(truth = is_goal, predicted_goal_xgb) %>%
  pluck(1) %>%
  as_tibble() %>%
  ggplot(aes(Prediction, Truth, alpha = n)) +
  geom_tile(show.legend = FALSE) +
  geom_text(aes(label = n), colour = "white", alpha = 1, size = 8)+
  labs(title = "Confusion Matrix for Extreme Gradient Booster")

# Plot all confusion matrices

grid.arrange(confmat1, confmat2, confmat3, ncol=3)

# Plot shots and whether they were or were predicted to be goals 

plot1 <- ggplot(shots_test, aes(x=y_width, y = x_origin_meters,colour=is_goal)) + 
  geom_point() +
  labs(x = "Byline", y = "Meters from byline", title = "Test Sample",subtitle = "Blue dot indicates an actual goal")+
  theme(legend.position = "top")+ 
  scale_x_continuous(breaks=seq(0, 76.25, 38.125))

plot2 <- ggplot(shots_test, aes(x=y_width, y = x_origin_meters, colour = predicted_goal_logreg))+ 
  geom_point()+
  labs(x = "Byline", y = "Meters from byline",title = "Predicted goals, Logistic Regression",subtitle = "Blue dot indicates a predicted goal")+
  theme(legend.position = "top")+ 
  scale_x_continuous(breaks=seq(0, 76.25, 38.125))

plot3 <- ggplot(shots_test, aes(x=y_width, y = x_origin_meters, colour = predicted_goal_rf))+ 
  geom_point()+
  labs(x = "Byline", y = "Meters from byline", title = "Predicted goals, Random Forest",subtitle = "Blue dot indicates a predicted goal")+
  theme(legend.position = "top")+ 
  scale_x_continuous(breaks=seq(0, 76.25, 38.125))

plot4 <- ggplot(shots_test, aes(x=y_width, y = x_origin_meters, colour = predicted_goal_xgb))+ 
  geom_point()+
  labs(x = "Byline", y = "Meters from byline", title = "Predicted goals, Extreme Gradient Booster",subtitle = "Blue dot indicates a predicted goal")+
  theme(legend.position = "top")+ 
  scale_x_continuous(breaks=seq(0, 76.25, 38.125))

grid.arrange(plot1, plot2, plot3, plot4, ncol=2)

#xg_scaled
plot5 <- ggplot(shots_test, aes(x=y_width, y = x_origin_meters, colour = xG_logreg))+ 
  geom_point()+
  labs(x = "Byline", y = "Meters from byline",title = "Value of xG, Logistic Regression")+
  scale_colour_gradient2(low = "white", midpoint = .5, mid = "blue", high = "darkblue")+
  scale_x_continuous(breaks=seq(0, 76.25, 38.125))+
  theme_dark()

plot6 <- ggplot(shots_test, aes(x=y_width, y = x_origin_meters, colour = xG_rf))+ 
  geom_point()+
  labs(x = "Byline", y = "Meters from byline", title = "Value of xG, Random Forest")+
  scale_colour_gradient2(low = "white", midpoint = .5, mid = "blue", high = "darkblue")+
  theme_dark()+ 
  scale_x_continuous(breaks=seq(0, 76.25, 38.125))

plot7 <- ggplot(shots_test, aes(x=y_width, y = x_origin_meters, colour = xG_xgb))+ 
  geom_point()+
  labs(x = "Byline", y = "Meters from byline", title = "Value of xG, Extreme Gradient Booster")+
  scale_colour_gradient2(low = "white", midpoint = .5, mid = "blue", high = "darkblue")+
  theme_dark()+ 
  scale_x_continuous(breaks=seq(0, 76.25, 38.125))


grid.arrange(plot5, plot6, plot7, ncol=3)

# Plot shot distribution

shots %>% ggplot(aes(x = y_width, y = x_origin_meters)) + 
  geom_bin2d(bins = 70) +
  scale_fill_continuous(type = "viridis") +
  theme_bw() + 
  labs(x = "Byline", y = "Meters from byline", title = "Distribution of all shots")
