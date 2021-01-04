# Подключим необходимые библиотеки
# Для манипулирования данными будем использовать библиотеку dplyr
# Хороший курс по dplyr https://www.datacamp.com/courses/data-manipulation-with-dplyr-in-r
if (!require(pacman)) {
  install.packages("pacman")
  devtools::install_github("trinker/pacman")
  library(pacman)
}

p_load(dplyr, tidyr, ggplot2)

# Объявим тестовые данные
sample_data <- 
  data.frame(
    menu_position = 1:4,
    menu_price = c(300,300,330,370,100,300,400,500),
    order = c(rep("1",4), rep("2",4))
  )

# Посчитаем описательные статистики для каждого заказа
sample_data %>%
  group_by(bar) %>%
  summarize(
    .n = n_distinct(menu_position), # количество уникальных позиций в меню
    .mu = mean(menu_price), # мат. ожидание
    .var = var(menu_price), # дисперсия
    .sd = sd(menu_price) # стандартное отклонение
  )

# Далее посчитаем коэффициент вариации для каждого бара
# Коэф. вариации позволяет оценить вариативность в процентах
# Принято считать, что метрика с вариацией > 30% считается сильно шумной 
# 30% - это скорее условность, но тем не менее, видно, что Вороношная имеет вариацию больше
sample_data %>%
  group_by(bar) %>%
  summarize(
    coef_variation = sd(menu_price) / mean(menu_price) * 100
  )

# Демонстрация на другом примере: временной ряд
n <- 30
sample_data_2 <- 
  data.frame(
    date = rep(seq.Date(from = as.Date('2019-07-01'), to = as.Date('2019-07-30'), by = 'days'),2),
    group = c(rep("organic",n),rep("cpc",n)),
    value = c(rnorm(n, mean = 10, sd = 1),rnorm(n, mean = 10, sd = 2))
  )

sample_data_2 %>% 
  ggplot(aes(x = date, y = value, group = group, color = group)) +
  geom_line()

sample_data_2 %>% 
  group_by(group) %>%
  summarize(
    coef_variation = sd(value) / mean(value) * 100
  )
