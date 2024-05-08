library(tidyverse)

plants <- tibble(none = c(4.8, 4.8, 3.94, 4.4,4.5,4.6),
                 nutrients1  = c( 10.1, 9.7, 9.8, 9.9, 9.3, 10.1),
                 nutrients2 = c(14.8, 14.6, 14.8, 14, 13.8, 14.6))

plants |> 
  pivot_longer(cols = c(none, nutrients1,nutrients2),
               names_to = "nutrients",
               values_to = "height") |> 
  ggplot(aes(x = nutrients, y = height)) +
  geom_boxplot()


longplants <- plants |> 
  pivot_longer(cols = c(none, nutrients1,nutrients2),
                             names_to = "nutrients",
                             values_to = "height")

plant_model <- aov(height ~ nutrients, data = longplants)
summary(plant_model)






crudechicks <- tibble(year = c("2000", "2001", "2002", "2003",
                               "2004", "2005", "2006", "2007",
                               "2008", "2009"),
                      chicken = c(54.2, 54, 56.8, 57.5, 59.3, 60.5, 60.9,
                                  59.9, 58.7, 56),
                      crude = c(3311, 3405, 3336, 3521, 3674, 3670, 3685,
                                3656, 3571, 3307))


cor.test(crudechicks$chicken, crudechicks$crude, method = "spearman")


# Week 2


starwars |> 
  filter(species == "Human") |> 
  ggplot(aes(x = height, y = mass, colour = sex)) +
  geom_point() +
  theme_classic() +
  scale_x_continuous(limits = c(0,250)) +
  scale_y_continuous(limits = c(0,150)) +
  scale_colour_brewer(palette = "Accent", name = "Sex") +
  theme(legend.position = "bottom") + 
  labs(x = "Height (cm)",
       y = "Weight (kg)",
       title = "Height and Weight of Human Characters in Star Wars by Sex")


starwars |> 
  filter(species == "Human") |> 
  mutate(BMI = case_when((mass/(height^2)) > 25 ~ "High BMI",
                         (mass/(height^2)) > 18 ~ "Healthy BMI",
                         TRUE ~ "Low BMI")) |> 
  ggplot(aes(x = height, y = mass, colour = sex)) +
  geom_point() +
  theme_classic() +
  scale_x_continuous(limits = c(0,250)) +
  scale_y_continuous(limits = c(0,150)) +
  scale_colour_brewer(palette = "Accent", name = "Sex") +
  theme(legend.position = "bottom") + 
  labs(x = "Height (cm)",
       y = "Weight (kg)",
       title = "Height and Weight of Human Characters in Star Wars by Sex") +
  geom_smooth(method = lm, se = FALSE)



starwars |> 
  filter(species == "Human") |> 
  ggplot(aes(x = height, y = mass, colour = sex)) +
  geom_point() +
  theme_classic() +
  scale_x_continuous(limits = c(0,250)) +
  scale_y_continuous(limits = c(0,150)) +
  scale_colour_brewer(palette = "Accent", name = "Sex") +
  theme(legend.position = "bottom") + 
  labs(x = "Height (cm)",
       y = "Weight (kg)",
       title = "Height and Weight of Human Characters in Star Wars by Sex",
       caption = "My height and weight as reference") +
  geom_smooth(method = lm, se = FALSE) +
  geom_vline(xintercept = 157.48) +
  geom_hline(yintercept = 72)


starwars |> 
  filter(species == "Human") |> 
  mutate(BMI = case_when((mass/(height^2)) > 25 ~ "High BMI",
                           (mass/(height^2)) > 18 ~ "Healthy BMI",
                           TRUE ~ "Low BMI")) |> 
  ggplot(aes(x = height, y = mass, colour = sex, shape = BMI)) +
  geom_point() +
  theme_classic() +
  scale_x_continuous(limits = c(0,250)) +
  scale_y_continuous(limits = c(0,150)) +
  scale_colour_brewer(palette = "Accent", name = "Sex") +
  theme(legend.position = "bottom") + 
  labs(x = "Height (cm)",
       y = "Weight (kg)",
       title = "Height and Weight of Human Characters in Star Wars by Sex") +
  geom_smooth(method = lm, se = FALSE)



starwars |> 
  filter(species == "Human",
         sex == "male") |>
  ggplot(aes(x = height)) +
  geom_histogram(binwidth = 3, fill = "#bb9cd1") +
  theme_classic() +
  labs(x = "Height (cm)",
       y = "Count",
       title = "Histogram of height of male human Star Wars characters")


starwars |> 
  filter(species == "Human",
         sex == "male") |>
  ggplot(aes(x = height)) +
  geom_histogram(binwidth = 8, fill = "#bb9cd1") +
  theme_classic() +
  labs(x = "Height (cm)",
       y = "Count",
       title = "Histogram of height of male human Star Wars characters")


starwars |> 
  filter(species == "Human",
         sex == "male") |>
  ggplot(aes(x = height)) +
  geom_density(fill = "#bb9cd1") +
  theme_classic() +
  labs(x = "Height (cm)",
       y = "Density",
       title = "Density plot of height of male human Star Wars characters")



starwars |> 
  filter(species == "Human") |>
  ggplot(aes(y = height, x = sex, colour = sex)) +
  geom_boxplot() +
  theme_classic() +
  scale_colour_brewer(palette = "Accent", name = "Sex") +
  labs(y = "Height (cm)",
       x = "Sex",
       title = "Boxplot of height by sex of human characters in Star Wars") +
  theme(legend.position ="none")

hums <- starwars |> 
  filter(species == "Human")

humsm <- lm(height ~ sex, data = hums)
summary(humsm)
report::report(humsm)


starwars |> 
  filter(species == "Human") |> 
  group_by(sex) |> 
  summarise(ht = mean(height, na.rm = TRUE),
            sd = sd(height, na.rm = TRUE)) |> 
  ggplot(aes(x = sex, y = ht, fill = sex)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = ht-sd, ymax = ht+sd), width = 0.2)+
  scale_fill_brewer(palette = "Accent") +
  labs(y = "Mean Height (cm)",
       x = "Sex",
       title = "Mean height by sex of human characters in Star Wars") +
  theme_classic() +
  theme(legend.position = "none")



starwars |> 
  filter(species == "Human") |> 
  group_by(sex) |> 
  summarise(ht = mean(height, na.rm = TRUE),
            sd = sd(height, na.rm = TRUE)) |> 
  ggplot(aes(x = sex, y = ht, fill = sex)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = ht-sd, ymax = ht+sd), width = 0.2)+
  scale_fill_brewer(palette = "Accent") +
  labs(y = "Mean Height (cm)",
       x = "Sex",
       title = "Mean height by sex of human characters in Star Wars") +
  theme_classic() +
  theme(legend.position = "none") + 
  coord_flip()


library(likert)

dat <- tibble (grp = c("Dislikes Star Wars","Likes Star Wars","Dislikes Star Wars","Likes Star Wars","Dislikes Star Wars","Likes Star Wars"),
               `How helpful was this lecture?` = c("No help", "No help", "No help", "Helpful", NA, "Helpful"),
               `How helpful was this course?` = c("Unsure", "No help", "Helpful", "Unsure", "Helpful", "Helpful")) |> 
  mutate_at (.vars = vars(`How helpful was this course?`:`How helpful was this lecture?`), 
             .funs = funs(factor(., levels = c("No help",
                                               "Unsure",
                                               "Helpful"))))   |>  
  as.data.frame()

dlik <- likert (items = dat[,2:3], grouping = dat[,1])
plot(dlik)


library(ggstatsplot)
starwars |> 
  mutate(spec = fct_lump_n(species, 1),
         eye = fct_lump_n(eye_color,1)) |> 
  select(spec, eye) |> 
  ggbarstats(x = eye, y = spec) +
  labs(x = "Species (Human vs Other)",
       y = "Percentage") +
  scale_fill_brewer(palette = "Set3", name = "Eye Colour (Brown vs other)") +
  theme_classic()


starwars |> 
  mutate(spec = fct_lump_n(species, 1),
         eye = fct_lump_n(eye_color,1)) |> 
  select(spec, eye) |> 
  ggbarstats(x = eye, y = spec) +
  labs(x = "Species (Human vs Other)",
       y = "Percentage") +
  scale_fill_brewer(palette = "Set3", name = "Eye Colour (Brown vs other)") +
  theme_light()


library(vcd)


startbl <-  starwars |> 
  mutate(Species = fct_lump_n(species, 2),
         EyeColour = fct_lump_n(eye_color,2)) 
startbl <-  structable(Species ~ EyeColour, startbl)
mosaic(startbl, highlighting = EyeColour)

mosaic(~ Species + EyeColour, data = startbl, highlighting = "Species",
       highlighting_fill = c("#A6CEE3", "#B2DF8A", "#FB9A99"))


mosaic(~ Species + EyeColour, data = startbl,shade = TRUE, legend = TRUE)





starwars |> 
  mutate(species = fct_lump_n(species, 4)) |> 
  group_by(species) |> 
  filter(!is.na(species)) |> 
  tally() |> 
  ggplot(aes(x = "", fill = species, y = n)) +
  geom_bar(stat = "identity", width = 1) +
  theme_void() +
  coord_polar("y", start = 0)


starwars |> 
  mutate(species = fct_lump_n(species, 4)) |> 
  group_by(species) |> 
  filter(!is.na(species)) |> 
  tally() |> 
  ggplot(aes(x = species, fill = species, y = n)) +
  geom_bar(stat = "identity") +
  theme_classic() +
  labs(x = "Species", y = "Count") +
  theme(legend.position = "none")



library(easystats)
library(BayesFactor)

starcorr <- correlationBF(starwars$height, starwars$mass)
describe_posterior(starcorr)
bayesfactor_models(starcorr)

plot(bayesfactor_models(starcorr)) +
  scale_fill_pizza() +
  labs(title = "Bayes Factor 'Pizza Plot' for A Bayesian correlation between height and weight for Star Wars characters")




library (tidytext)
library (wordcloud)
library (textstem)
library (janeaustenr) 

austen <- austen_books() %>%
  group_by(book) %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]",
                                                 ignore_case = TRUE)))) %>%
  ungroup() %>%
  unnest_tokens(word, text) %>%
  mutate (lemma = (lemmatize_strings(word))) %>%
  anti_join(stop_words)


austen %>%
  count (lemma) %>%
  with (wordcloud(words = lemma, freq = n, max.words = 200, random.order = FALSE, rot.per = 0,
                  colors = brewer.pal(12, "Paired"), use.r.layout = FALSE))
starwars |> 
  count(homeworld) |> 
  with(wordcloud(words = homeworld, freq = n, min.freq=1, random.order = FALSE, rot.per = 0,
                 colors = brewer.pal(6, "Accent"), use.r.layout = FALSE))

starwars |> 
  count(homeworld) |> 
  ggplot(aes(x = reorder(homeworld, desc(n)), y = n, fill = as.factor(n))) +
  geom_bar(stat = "identity") +
  theme_classic() +
  labs(x = "Homeworld", y = "Count") +
  theme(legend.position = "none") +
  scale_fill_brewer(palette = "Accent") +
  coord_flip()








starwars |> 
  mutate(species = fct_lump_n(species,2)) |> 
  filter(!is.na(species)) |> 
  ggplot(aes(x = species)) +
  geom_point(aes(y = height, colour = species), position = position_jitter(width = .13), size = 1, alpha = 0.6) +
  see::geom_violinhalf(aes(y = height, alpha= 0.3, fill = species), linetype = "dashed", position = position_nudge(x = .2)) +
  geom_boxplot(aes(y = height, alpha = 0.3, colour = species), position = position_nudge(x = -.1), width = 0.1, outlier.shape = NA) +
  theme_classic() +
  labs(x = "Species", y = "Height (cm)") +
  theme(legend.position = "none") +
  coord_flip()


starwars |>
  select(height, mass, birth_year) |> 
  ggcorrmat()

starwars |>
  mutate(col = fct_lump_n(species, 2)) |> 
  ggplot(aes(x = birth_year, y = mass, size = height, colour = col)) +
  geom_point() +
  scale_size(range = c(.1, 24), name="Height") +
  theme_classic() +
  scale_x_continuous(limits = c(0,250)) +
  scale_y_continuous(limits = c(0,300)) +
  scale_colour_brewer(palette = "Accent", name = "Species") +
  theme(legend.position = "bottom") + 
  labs(x = "Birth  Year (Before Battle of Yavin)",
       y = "Weight (kg)",
       title = "Weight by Age of Characters in Star Wars") 



starwars |> 
  mutate(species = fct_lump_n(species,2)) |> 
  filter(!is.na(species)) |> 
  ggplot(aes(x = species, y = height, fill = species)) +
  geom_violin() +
  theme_classic() +
  labs(x = "Species", y = "Height (cm)") +
  theme(legend.position = "none")  +
  scale_fill_brewer(palette = "Accent")



heifers <- tibble(heifers = c(211.3, 200.4, 220.1, 200.8, 222.0, 209.3, 
             195.8, 220.4, 226.2, 218.7, 193.7, 209.7))

find_mode <- function(x) {
  ux <- unique(x)
  tab <- tabulate(match(x, ux))
  ux[tab == max(tab)]
}

heifers |> 
  ggplot(aes(x = heifers)) +
  geom_density(fill = "#bb9cd1") +
  theme_classic() +
  labs(x = "Heifer Weight (kg)",
       y = "Density")


heifers |> 
  ggplot(aes(x = heifers)) +
  geom_density(fill = "#bb9cd1") +
  geom_vline(aes(xintercept = 210.7)) +
  theme_classic() +
  labs(x = "Heifer Weight (kg)",
       y = "Density")



heifers |> 
  ggplot(aes(x = heifers)) +
  geom_density(fill = "#bb9cd1") +
  geom_vline(aes(xintercept = 210)) +
  theme_classic() +
  labs(x = "Heifer Weight (kg)",
       y = "Density")

heifers |> 
  summarise(mean = mean(heifers),
            median = median(heifers),
            min = min(heifers),
            max = max(heifers),
            mode = find_mode(round(heifers, 0)))

heifers |> 
  ggplot(aes(x = heifers)) +
  geom_histogram(fill = "#bb9cd1", binwidth = 1) +
  geom_vline(aes(xintercept = 220)) +
  theme_classic() +
  labs(x = "Heifer Weight (kg)",
       y = "Density")


wage <- readxl::read_excel("assets/UKWageData2023ONS.xlsx", 
                           skip = 5)
wage |> 
  ggplot(aes(x = Median)) +
  geom_density(fill = "#bb9cd1") +
  theme_classic() +
  geom_vline(aes(xintercept = 34475)) +
  labs(x = "UK Salaries (£)",
       y = "Density",
       title = "Distribution of UK Salaries",
       caption = "Data taken from ONS 2023 Median Salaries by Field, n = 329 fields")

wage |> 
  summarise(mean = mean(Median),
            median = median(Median),
            min = min(Median),
            max = max(Median),
            mode = find_mode(round(Median,0)))


wage |> 
  ggplot(aes(x = Median)) +
  geom_histogram(fill = "#bb9cd1", bins = 200) +
  geom_vline(aes(xintercept = 25000)) +
  geom_vline(aes(xintercept = 26000)) +
  geom_vline(aes(xintercept = 28216)) +
  geom_vline(aes(xintercept = 35248)) +
  theme_classic() +
  labs(x = "UK Salaries (£)",
       y = "Count")



wage |> 
  ggplot(aes(x = Median)) +
  geom_density(fill = "#bb9cd1") +
  theme_classic() +
  geom_vline(aes(xintercept = 34475), colour = "lightblue") +
  geom_vline(aes(xintercept = 31988),colour = "purple") +
  labs(x = "UK Salaries (£)",
       y = "Density",
       title = "Distribution of UK Salaries",
       caption = "Data taken from ONS 2023 Median Salaries by Field, n = 329 fields")


# Week 3

library(tidyverse)


resid <- tibble(x = c(1, 2, 3),
                y = c(3, 2, 6))

resid |> 
  ggplot(aes(x, y)) +
  geom_point(size = 4, colour = "lightblue") +
  theme_classic() +
  geom_hline(yintercept = 3.67) +
  labs(title = "Plot of 3 points, mean of y shown") +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.x = element_blank())

resid |> 
  ggplot(aes(x, y)) +
  geom_point(size = 4, colour = "lightblue") +
  theme_classic() +
  geom_hline(yintercept = 3.67) +
  geom_segment(aes(x = 1, y = 3.67, xend = 1, yend = 3)) +
  geom_segment(aes(x = 2, y = 3.67, xend = 2, yend = 2)) +
  geom_segment(aes(x = 3, y = 3.67, xend = 3, yend = 6)) +
  labs(title = "Plot of 3 points, mean of y shown, residuals shown") +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.x = element_blank())



vardat <- tibble(cat = c(13, 17, 30, 36, 11, 43, 23, 50, 19, 23),
                 dog = c(30, 31, 45, 43, 48, 50, 37, 32, 40, 44))
std.error <- function(x) sd(x)/sqrt(length(x))

vardat |> 
  pivot_longer(cols = c(cat, dog),
               names_to = "Species",
               values_to = "Score") |> 
  group_by(Species) |> 
  summarise(mean = mean(Score),
            sd = sd(Score),
            se = std.error(Score),
            var = var(Score))
vardat |> 
  mutate(dev_cat = cat-26.5,
         dev_cat2 = (cat-26.5)*(cat-26.5)) |> 
  summarise(sum = sum(dev_cat),
            sumcat2 = sum(dev_cat2),
            sum2 = sum(dev_cat)^2)


vardat |> 
  summarise(var_dogs = var(dog),
            var_cat = var(cat))

vardat |> 
  summarise(sd_dogs = sd(dog),
            sd_cat = sd(cat))


vardat |> 
  summarise(se_dogs = std.error(dog),
            se_cat = std.error(cat))


vardat |> 
  pivot_longer(cols = c(cat, dog),
               names_to = "Species",
               values_to = "Score") |> 
  ggplot(aes(x = Species)) +
  geom_point(aes(y = Score, colour = Species), position = position_jitter(width = .13), size = 1) +
  see::geom_violinhalf(aes(y = Score, fill = Species), linetype = "dashed", position = position_nudge(x = .2)) +
  geom_boxplot(aes(y = Score, alpha = 0.3, colour = Species), position = position_nudge(x = -.1), width = 0.1, outlier.shape = NA) +
  theme_classic() +
  labs(x = "Species", y = "Score") +
  theme(legend.position = "none") +
  coord_flip()



# Week 3


# Bayes vs Frequentist

library(tidyverse)
library(easystats)
library(bayestestR)
library(rstan)
library(rstanarm)



cat_weights <- tibble(avg_daily_snacks  = c(3, 2, 4, 2, 3, 1, 1, 0, 1, 0, 2, 3, 1, 2, 1, 3),
                      weight = c(3.8, 3.9, 5, 3.7,  4.1, 3.6, 3.7, 3.6, 3.8, 4.1, 4.3, 3.9, 3.7, 3.8, 3.5, 4.3),
                      environ = c("Indoor", "Indoor", "Outdoor", "Indoor",
                                  "Outdoor", "Indoor", "Outdoor", "Indoor",
                                  "Indoor", "Indoor", "Outdoor", "Indoor",
                                  "Outdoor", "Indoor", "Indoor", "Outdoor"))




cat_weights |> 
  ggplot(aes(x = avg_daily_snacks, y = weight)) +
  geom_point() +
  labs(x = "Average Daily Snacks", y = "Cat Weight") +
  theme_classic() +
  scale_y_continuous(limits = c(0,5))



model_fcat <- lm(weight ~ avg_daily_snacks, data = cat_weights)
summary(model_fcat)
report::report(model_fcat)
parameters(model_fcat) |>  View()
plot(model_parameters(model_fcat), show_intercept = TRUE)
plot(model_parameters(model_fcat))

cat_weights |> 
  ggplot(aes(x = avg_daily_snacks, y = weight)) +
  geom_point() +
  labs(x = "Average Daily Snacks", y = "Cat Weight",
       caption = "Weight ~ Average Daily Snacks shown") +
  theme_classic() +
  scale_y_continuous(limits = c(0,5)) +
  geom_abline(slope = 0.20, intercept = 3.55)


model_bcat <- stan_glm(weight ~ avg_daily_snacks, data = cat_weights)
summary(model_bcat)
describe_posterior(model_bcat) |>  View()
report::report(model_bcat)

posteriors <- get_parameters(model_bcat)

posteriors |> 
  ggplot(aes(x = avg_daily_snacks)) +
  geom_density(fill = "lightblue") +
  theme_classic() +
  labs(x = "Posterior Coefficient Estimates for Average Daily Snacks",
       y = "Density",
       caption = "Median Estimate Shown") +
  geom_vline(xintercept = 0.21, color = "darkblue", linewidth = 1)




cat_weights |> 
  ggplot(aes(x = avg_daily_snacks, y = weight, colour = environ)) +
  geom_point() +
  labs(x = "Average Daily Snacks", y = "Cat Weight",
       caption = "Weight ~ Average Daily Snacks shown") +
  theme_classic() +
  scale_y_continuous(limits = c(0,5)) +
  geom_abline(slope = 0.20, intercept = 3.55)





model_fcat2 <- lm(weight ~ avg_daily_snacks + environ, data = cat_weights)
summary(model_fcat2)
report::report(model_fcat2)
parameters(model_fcat2) |>  View()
plot(model_parameters(model_fcat2), show_intercept = TRUE)
plot(model_parameters(model_fcat2))

cat_weights |> 
  ggplot(aes(x = avg_daily_snacks, y = weight, colour = environ)) +
  geom_point() +
  labs(x = "Average Daily Snacks", y = "Cat Weight",
       caption = "Weight ~ Average Daily Snacks shown") +
  theme_classic() +
  scale_y_continuous(limits = c(0,5)) +
  geom_smooth()


model_bcat2 <- stan_glm(weight ~ avg_daily_snacks + environ, data = cat_weights)
summary(model_bcat2)
describe_posterior(model_bcat2) 

report::report(model_bcat2)

posteriors2 <- get_parameters(model_bcat2)


posteriors2 |> 
  pivot_longer(cols = c(avg_daily_snacks, environOutdoor),
               names_to = "Parameter",
               values_to="estimate") |> 
  ggplot() +
  geom_density(aes(x = estimate, fill = Parameter)) +
  theme_classic() +
  labs(x = "Posterior Coefficient Estimates",
       y = "Density") +
  facet_wrap(facets = ~Parameter, ncol = 1) +
  theme(legend.position = "none")




bayesfactor(model_bcat2)
interpret_bf(0.431)






# Week 3
# Meta analysis

sqrt(0.11)



# Effect sizes

job_dat <- tibble(job = c("vet", "vet", "vet","vet", "vet", "vet", "vet", "vet", "vet", "vet",
                          "assc", "assc", "assc", "assc", "assc", "assc", "assc", "assc", "assc", "assc"),
                  burnout = c(13, 12, 4, 16, 16, 20, 8, 10, 11, 10,
                              10, 11, 8, 7, 8, 10, 9, 11, 17, 10),
                  empathy = c(4, 5, 1, 4,3, 5, 2, 3,3,2,
                              2, 3, 3, 2, 2, 3, 3, 4, 5, 2),
                  satisfaction = c("yes", "no", "no", "no", "yes", "no", "yes", "no", "yes", "yes",
                                   "yes", "yes", "yes", "no", "yes", "yes", "yes","no", "yes", "yes"))


job_dat |> 
  ggplot(aes(x = burnout, y = empathy, shape = job, colour = satisfaction)) +
  geom_point() +
  theme_classic() +
  labs(title = "Burnout and empathy scores for vets and associated professions",
       subtitle = "Job Satisfaction shown",
       caption = "Mock data for teaching",
       x = "Burnout Score",
       y = "Empathy Score") +
  scale_shape_discrete(name = "Vet or Associated Profession") +
  scale_color_discrete(name = "Satisfied with job?") 


job_dat |> 
  group_by(job, satisfaction) |> 
  tally()


job_dat |> 
  group_by(job) |> 
  summarise(mean = mean(burnout),
            sd = sd(burnout),
            min = min(burnout),
            max = max(burnout))


job_dat |> 
  ggplot(aes(x = burnout, y = empathy)) +
  geom_point() +
  theme_classic() +
  labs(x = "Burnout Score", y = "Empathy Score") +
  facet_wrap(facets = ~ job, ncol = 1)


job_dat |> 
  ggplot(aes(x = burnout, y = empathy)) +
  geom_point() +
  theme_classic() +
  labs(x = "Burnout Score", y = "Empathy Score") 

cor(job_dat$burnout, job_dat$empathy, method = "pearson")


cor(job_dat$burnout, job_dat$empathy, method = "spearman")


cor.test(job_dat$burnout, as.numeric(as.factor(job_dat$job)))


job_dat <- job_dat |> 
  mutate(burnoutcat = case_when(burnout > 10 ~ "burnout",
                                TRUE ~ "no burnout"))

library(lsr)



job_tbl <- xtabs(~job_dat$job +  job_dat$satisfaction + job_dat$burnoutcat)
ftable(job_tbl)
chisq.test(ftable(job_tbl))
cramersV(ftable(job_tbl))

library(effsize)

cohen.d(d = job_dat$burnout, f = job_dat$job)

cohen.d(d = job_dat$burnout, f = job_dat$job, hedges.correction = TRUE)

cohen.d(d = job_dat$burnout, f = job_dat$job, hedges.correction = TRUE)



jobmod <- lm(burnout ~ empathy + satisfaction, data = job_dat)
summary(jobmod)

job_dat |> 
  ggplot(aes(x = empathy, y = burnout, colour = satisfaction)) +
  geom_point() +
  geom_smooth(aes(x = empathy, y = burnout)method = lm, se = FALSE, formula = burnout ~ empathy + satisfaction)+
  theme_classic() +
  labs(x = "Empathy Score", y = "Burnout Score")
 
job_dat |> 
  mutate(mod = predict(jobmod)) |> 
  ggplot() + 
  geom_point(aes(x = empathy, y = burnout, colour = satisfaction)) +
  geom_line(aes(x = empathy, y = mod)) +
  theme_classic() +
  facet_wrap(facets = ~ satisfaction, ncol = 1) +
  labs(x = "Empathy Score", y = "Burnout Score")






jobmod2 <- lm(burnout ~ empathy + job, data = job_dat)
summary(jobmod2)


job_dat |> 
  mutate(mod = predict(jobmod2)) |> 
  ggplot() + 
  geom_point(aes(x = empathy, y = burnout, colour = job)) +
  geom_line(aes(x = empathy, y = mod)) +
  theme_classic() +
  facet_wrap(facets = ~ job, ncol = 1) +
  labs(x = "Empathy Score", y = "Burnout Score")



# Week 4 

cat_weights <- tibble(avg_daily_snacks  = c(3, 2, 4, 2, 3, 1, 1, 0, 1, 0, 2, 3, 1, 2, 1, 3),
                      weight = c(3.8, 3.9, 5, 3.7,  4.1, 3.6, 3.7, 3.6, 3.8, 4.1, 4.3, 3.9, 3.7, 3.8, 3.5, 4.3))





cat_weights |> 
  ggplot(aes(x = avg_daily_snacks, y = weight)) +
  geom_point() +
  labs(x = "Average Daily Snacks", y = "Cat Weight") +
  theme_classic() +
  scale_y_continuous(limits = c(0,5))

cat_weights |> 
  ggplot(aes(x = avg_daily_snacks, y = weight)) +
  geom_point() +
  labs(x = "Average Daily Snacks", y = "Cat Weight") +
  theme_classic() +
  scale_y_continuous(limits = c(0,5)) + 
  geom_abline(slope = 1.998, intercept = -6.028)


cat_weights |> 
  ggplot(aes(x = avg_daily_snacks, y = weight)) +
  geom_point() +
  labs(x = "Average Daily Snacks", y = "Cat Weight") +
  theme_classic() +
  scale_y_continuous(limits = c(0,5)) +
  geom_abline(slope = 0.204, intercept = 3.55)

model_cat <- lm(avg_daily_snacks ~ weight, data = cat_weights)
summary(model_cat)
report::report(model_cat)


model_cat2 <- lm(weight ~ avg_daily_snacks, data = cat_weights)
summary(model_cat2)
report::report(model_cat2)



# Week 5


internet <- tibble(year = c(2013, 2014, 2015, 2016, 2017,	2018,	2019,	2020),
                   "16-24" = c(98.3,	98.9,	98.8,	99.2,	99.2,	99.3,	99.2,	99.5),
                   "25-34" = c(97.7,	98.3,	98.6,	98.9,	99.1,	99.2,	99.4,	99.5),
                   "35-44" = c(95.8,	96.7,	97.3,	98.2,	98.4,	98.6,	98.9,	99.1),
                   "45-54" = c(90.2,	92.3,	93.6,	94.9,	96.2,	96.8,	97.5,	97.9),
                   "55-64" = c(81.3,	84.2,	86.7,	88.3,	90.0,	91.8,	93.2,	94.6),
                   "65-74" = c(61.1,	65.5,	70.6,	74.1,	77.5,	80.2,	83.2,	85.5),
                  "75+" = c(29.1,	31.9,	33.0,	38.7,	40.5,	43.6,	46.8,	54.0)) |> 
  pivot_longer(cols = -year,
               names_to = "age",
               values_to = "perc")


internet |>
  ggplot(aes(x = year, y = perc, fill = age)) +
  geom_bar(stat = "identity", position = "dodge2") +
  theme_classic() +
  theme(legend.position = "bottom") +
  labs(x = "Year", y = "Percentage of Adults",
       caption = "UK adults who used the internet in the last 3 months (Jan-March)\nData from ONS")



# Week 7 Sample Data


library(tidyverse)
library(faux)

between <- list(parlour = c(robot = "robotic",
                            manual = "manual"))
within <- list(housing =c( "continually housed","seasonally housed"))
mu <- data.frame(robot = c(31, 30),
                  manual = c(34, 33),
                 row.names = within$housing)

mu1 <- data.frame(robot = c(18, 12),
                 manual = c(17, 11),
                 row.names = within$housing)
cows <- sim_design(within, between, n = 100, mu = mu, sd = c(4.15, 4.82, 2.12, 1.01), r =0.5,
                   empirical = TRUE, plot = TRUE)

welfare <- sim_design(within, between, n = 100, mu = mu1, sd = c(2, 2.8, 1.1, 1.05), r =0.5,
                      empirical = TRUE, plot = TRUE)

cows <- cows |> 
  pivot_longer(cols = c(`continually housed`,`seasonally housed`),
               names_to = "housing type",
               values_to = "Average Daily Yield") %>% 
  select(-id) 

welfare <- welfare |> 
  pivot_longer(cols = c(`continually housed`,`seasonally housed`),
               names_to = "housing type",
               values_to = "Welfare Score") %>% 
  select(`Welfare Score`)


cows <- cows |> 
  cbind(welfare) |> 
  mutate(`Welfare Score` = round(`Welfare Score`, 0))

cows |> 
  writexl::write_xlsx("cows.xlsx")


cows <- readxl::read_excel("cows.xlsx") |> 
  mutate(parlour = as.factor(parlour),
         `housing type` = as.factor(`housing type`))


cows |> 
  ggplot(aes(x = `Welfare Score`, y= `Average Daily Yield`, colour = `housing type`)) +
  geom_point() +
  theme_classic() +
  labs(x  = "Welfare Score", y = "Average Daily Yield",
       title = "Milk Yield versus Welfare Score",
       subtitle = "For Robotic vs Manual Parlours") +
  facet_wrap(facets = ~parlour, ncol = 1) +
  theme(legend.position = "bottom")



cows |> 
  summarise(mean_yield = mean(`Average Daily Yield`),
            sd_yield = sd(`Average Daily Yield`))


t.test(cows$`Average Daily Yield`, mu = 28)

summary(lm(cows$`Average Daily Yield` ~ 28))




diet <- tibble (before = c(5.04, 4.63, 4.04, 5.10, 5.43, 4.83, 3.45, 3.49, 5.02, 4.81),
                after = c( 4.78, 2.49, 4.46, 2.03, 5.13, 7.23, 3.50, 1.89, 3.30, 3.91))

diet |> 
  ggplot(aes(y = before)) +
  geom_boxplot() +
  geom_boxplot(aes(y = after, x =1)) +
  theme_classic() +
  scale_x_continuous(labels =c("Before Diet", "After Diet"), breaks = c(0,1)) +
  labs(x = "Diet", y = "Weight (kg)")


diet |> 
  summarise(before_mean = mean(before),
            after_mean = mean(after),
            before_sd = sd(before),
            after_sd = sd(after))


t.test(diet$before, diet$after, paired = TRUE, alternative = "two.sided")


robot_yield <- cows |> 
  filter(parlour == "robot") |> 
  select(`Average Daily Yield`)

manual_yield <- cows |> 
  filter(parlour == "manual") |> 
  select(`Average Daily Yield`)


t.test(robot_yield, manual_yield, alternative = "two.sided")


summary(lm(`Average Daily Yield` ~ parlour, data = cows))

model1 <-  aov(`Average Daily Yield` ~ parlour +  `housing type`, data = cows)
summary(model1)


model2 <- lm(`Average Daily Yield` ~ parlour +  `housing type`, data = cows)
summary(model2)



cor.test(cows$`Average Daily Yield`, cows$`Welfare Score`)


model3 <- lm(`Average Daily Yield` ~ `Welfare Score`, data = cows)
summary(model3)


model4 <- glm(`Average Daily Yield` ~ `Welfare Score` 
              + `housing type` + parlour, data = cows)
summary(model4)


binom.test(x = 3, n = 20, p = 0.18, alternative = "two.sided")


prop.test(x = 3, n = 20, p = 0.18, alternative = "two.sided")



work <- matrix(c(2, 5, 38, 35), 
               ncol=2, 
               byrow=TRUE,
               dimnames = list(c("Dysplasia", "No Dysplasia"),
                               c("Before Work Season", "After Work Season")))


mcnemar.test(work)


fisher.test(x = c(4,16), y = c(2,18))

# Calculate Odds Ratio
(4+18) / (16 + 2)



gsd <- matrix(c(16, 12, 84, 86), 
              ncol=2, 
              byrow=TRUE,
              dimnames = list(c("Dysplasia", "No Dysplasia"),
                              c("Inbred", "Less Inbred")))


chisq.test(gsd)

library(vcd)
assocstats(gsd)




dysp <- tibble(dysplasia = c(1, 1, 1, 1, 1, 1, 1,
                             0, 0, 0, 0, 0, 0, 0,
                             0, 0, 0, 0, 0, 0, 0),
               inflammation =c(0.91, 0.79, 1.40, 0.71, 1.01, 0.77, 0.85,
                               0.42, 1.02, 0.31, 0.05, 1.17, 0.04, 0.36, 
                               0.12, 0.02, 0.05, 0.42, 0.92, 0.72,  1.05)) 

logit <- glm(dysplasia ~ inflammation, data = dysp, family = "binomial")
summary(logit)

library(easystats)
report(logit)
parameters(logit)
exp(cbind(OddsRatio = coef(logit), confint(logit)))


residuals <- tibble(resids = resid(model4))

residuals |> 
  ggplot(aes(x = resids)) +
  geom_density()+
  theme_classic()


describe_distribution(residuals)










examples <- tibble (x.perfect = c(1,2,3,4,5,6,7,8,9,10), 
                    y.perfect = c(1,2,3,4,5,6,7,8,9,10), 
                    x.realistic = c(1,2,3,4,4,6,8,8,9,11), 
                    y.realistic = c(2, 2, 4, 5, 5, 5, 7, 8, 9, 9))


examples |> 
  ggplot (aes(x = x.perfect, y = y.perfect))+
  geom_point (shape = 8) +
  scale_y_continuous(limits =c(0,10)) +
  scale_x_continuous(limits = c(0,10)) +
  labs (title = "100% of the variation in y is explained by x",
        x = "A Perfect Explanatory Variable",
        y = "A Perfect Response Variable") +
  theme_classic()



examples |> 
  ggplot (aes(x = x.realistic, y = y.realistic))+
  geom_point () +
  scale_y_continuous(limits =c(0,10)) +
  scale_x_continuous(limits = c(0,10)) +
  labs (title = "How much of the variation in y is explained by x?",
        x = "A Realistic Explanatory Variable",
        y = "A Realistic Response Variable") +
  theme_classic()

lm(formula = y.perfect ~ x.perfect, data = examples)

lm(formula = y.realistic ~ x.realistic, data = examples)



unicorns <- tibble (NoRadio = c(150, 130, 121, 90, 98, 100, 98, 100, 113, 111),
                 RadioMusic = c(112, 127, 132, 150, 112, 128, 110, 120, 98, 107),
                 RadioDiscussion = c(75, 98, 88, 83, 83, 77, 75, 84, 93, 99)) |> 
  pivot_longer(cols = c(NoRadio:RadioDiscussion),
               names_to = "Radio",
               values_to = "DustYield")


unicorns |> 
  ggplot (aes (x = Radio, y = DustYield)) + 
  geom_point(aes(shape = Radio, colour = Radio),  position = position_jitter(width = .13)) +
  labs (y = "Dust Yield (Kg)", x = "Radio Condition") +
  scale_color_brewer(palette = "Accent") +
  scale_y_continuous(limits = c(0, 200)) +
  theme_classic () +
  theme(legend.position = "none") 



unicorns |>
  filter(Radio == "NoRadio") |> 
  mutate(UnicornNo = c(1,2,3,4,5,6,7,8,9,10)) |> 
  # The mutate function adds a new variable just to plot this one specific chart
  # And then we pipe it directly into ggplot, so we're not changing the unicorns data
  # Remember you can check this with `View(unicorns)`, you'll see 'UnicornNo' doesn't exist.
  ggplot (aes (x = UnicornNo, y = DustYield)) + 
  geom_point() +
  labs (y = "Dust Yield (Kg)", x = "Unicorn No") +
  scale_y_continuous(limits = c(0, 200)) +
  scale_x_continuous(breaks = c(0, 2, 4, 6, 8, 10)) + 
  theme_classic ()


unicorns |>
  group_by(Radio) |>
  filter(Radio == "NoRadio") |>
  summarise(mean = mean(DustYield))


unicorns |>
  filter(Radio == "NoRadio") |> 
  mutate(UnicornNo = c(1,2,3,4,5,6,7,8,9,10)) |> 
  # The mutate function adds a new variable just to plot this one specific chart
  # And then we pipe it directly into ggplot, so we're not changing the unicorns data
  # Remember you can check this with `View(unicorns)`, you'll see 'UnicornNo' doesn't exist.
  ggplot (aes (x = UnicornNo, y = DustYield)) + 
  geom_point() +
  geom_hline(yintercept = 111.1)+
  labs (y = "Dust Yield (Kg)", x = "Unicorn No") +
  scale_y_continuous(limits = c(0, 200)) +
  scale_x_continuous(breaks = c(0, 2, 4, 6, 8, 10)) + 
  theme_classic ()


unicorns |> 
  filter(Radio == "NoRadio") |> 
  mutate(Diff = DustYield-111.1)

unicorns |> 
  filter(Radio == "NoRadio") |> 
  mutate(Diff = DustYield-111.1) |> 
  summarise(`Sum of Differences` = sum(Diff))

unicorns |> 
  group_by(Radio) |> 
  summarise (Variance = var(DustYield))



Sce1 <- tibble (Condition1 = c(99,100,101,99,100,101,99,101,100,101),
                Condition2 = c(120,121,122,120,121,122,120,123,121,120),
                Condition3 = c(83,84,85,85,84,83,83,84,85,86)) |>
  pivot_longer (cols = c(Condition1:Condition3), names_to = "Condition", values_to = "DustYield") |>
  mutate (Count = c(1:30)) 

Sce2 <-tibble (Condition1 = c(84,86,123,95,87,110,99,95,121,121),
               Condition2 = c(83,115,85,85, 110,105,84,115,101,100),
               Condition3 = c(84,122,80,80,101,83,83,99, 120,120)) |>
  pivot_longer (cols = c(Condition1:Condition3), names_to = "Condition", values_to = "DustYield") |>
  mutate (Count = c(1:30)) 


# In the code above I've gathered the data into a tidy format 


ImaginaryScenario1 <- Sce1 |>
  ggplot (aes (x = Count, y = DustYield)) + 
  geom_point(aes(shape = Condition, colour = Condition)) +
  labs (y = "Dust Yield (Kg)", x = "Unicorn ID Number") +
  scale_y_continuous(limits = c(0, 200)) +
  theme_classic ()

# I have also made this chart an object because we're going to update it
# It's quicker to do this as an object
# You can compare how we update this chart with how we update the ones above.

ImaginaryScenario1


experiment <- Sce2 |> 
  ggplot (aes (x = Count, y = DustYield)) + 
  geom_point(aes(shape = Condition, colour = Condition)) +
  labs (y = "Dust Yield (Kg)", x = "Unicorn ID Number") +
  scale_y_continuous(limits = c(0, 200)) +
  theme_classic ()

experiment

ImaginaryScenario1 +
  geom_hline(yintercept = mean(Sce1$DustYield))

experiment +
  geom_hline(yintercept = mean(Sce2$DustYield))


ImaginaryScenario1 +
  geom_hline(yintercept = mean(Sce1$DustYield)) +
  geom_segment(aes(x =1, y = 100.1, xend =30, yend = 100.1, color = "red")) +
  geom_segment(aes(x = 1, y = 121.0, xend = 30, yend = 121.0, color = "green")) +
  geom_segment (aes(x = 1, y = 84.2, xend = 30, yend = 84.2, color = "blue")) +
  theme (legend.position = "none")


experiment +
  geom_hline(yintercept = mean(Sce1$DustYield)) +
  geom_segment(aes(x =1, y = 102, xend =30, yend = 102, color = "red")) +
  geom_segment(aes(x = 1, y = 98.3, xend = 30, yend = 98.3, color = "green")) +
  geom_segment (aes(x = 1, y = 97.2, xend = 30, yend = 97.2, color = "blue")) +
  theme (legend.position = "none")



Sce2 |>
  group_by(Condition) |>
  summarise(mean = mean (DustYield))




MFY <- unicorns |>
  mutate ("Y" = DustYield) |>
  mutate ("M" = mean(Y)) |>
  # If we now ask R to group the data, it will calculate the mean per group:
  group_by(Radio) |>
  mutate ("F" = mean(Y)) |>
  # Remember to ungroup after!
  ungroup() 



MFY <- MFY |>
  mutate (MY = (Y-M),
          MF = (F-M),
          FY = (Y - F))


MFY <- MFY |>
  mutate (MY2 = (MY*MY),
          MF2 = (MF*MF),
          FY2 = (FY*FY))


MFY |>
  summarise(SumSquareMY = sum(MY2),
            SumSquareMF = sum(MF2),
            SumSquareFY = sum(FY2))



MFY |>
  summarise(SumSquareMY = sum(MY2),
            SumSquareMF = sum(MF2),
            SumSquareFY = sum(FY2),
            MeanSquareMY = sum(MY2)/29,
            MeanSquareMF = sum(MF2)/2,
            MeanSquareFY = sum(FY2)/27) 


ANOVA <- aov(DustYield ~ Radio, data = unicorns)
summary(ANOVA)
