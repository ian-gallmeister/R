library(DataComputing)
data("NCHS")
help("NCHS")
#ggplot(data = NCHS)
NCHS %>% ggplot(aes(x = age, y = height)) + geom_point(alpha = 0.01)
NCHS %>% ggplot(aes(x = age, y = height)) + geom_point(alpha = 0.01) + geom_smooth(se = FALSE, method = lm)
NCHS %>% ggplot(aes(x = age, y = height)) + geom_point(alpha = 0.01) + geom_smooth(se = FALSE, method = loess)
NCHS %>% sample_n(size = 50) %>% ggplot(aes(x = age, y = height)) + geom_point(alpha = 0.1) + geom_smooth(method = loess)
NCHS %>% sample_n(size = 5000) %>% ggplot(aes(color = sex, x = age, y = height)) + geom_point(alpha = 0.1) + geom_smooth(method = loess)
#Polynomial differentiable with continuous output an infinite number of times
#Granted, goes to zero pretty quickly
#C^3 smooth means third derivative but not fourth is continuous
#C^\infty favored
#Position -> velocity -> acceleration -> jerk -> who gives a shit?
#Interpolation really important today -- digital sound is recorded as discrete points, so want 
#   interpolated function with continuous 3rd derivative so speaker arm moves continuously and 
#   doesn't break the damn thing.
#Sink function best for sound.  Could use linear or combined quadratics etc..., but not so good