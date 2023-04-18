# 0. setup ----------------------------------------------------------------

# clear env and set time zone
rm(list=ls()) 

# set wd
path <- dirname(rstudioapi::getSourceEditorContext()$path)[1]
setwd(path)
setwd("../")

# load packages
library(readxl)
library(tidyverse)
library(xtable)

# functions
se_round <- function(se_r) {
  se <- paste0("(", sprintf("%.3f", round(se_r,3)), ")")
  return(se)
}

mean_round <- function(x) {
  r_mean <- sprintf("%.3f", round(x,3))
  return(r_mean)
}

# 1. Descriptives -------------------------------------------------

rtemp <- read_excel("results.xlsx", sheet = "descriptives") 

# Fig. 1(a): education
temp <- rtemp %>% 
  filter(gender == "male" | gender == "female") %>%
  filter(sample %in% c("LF_unrestricted","pop_edu")) %>%
  select(sample:stat, school_years_s,school_years_p,school_years_m,max_edu_p,max_edu_gpo) %>% 
  pivot_longer(cols = c(school_years_s,school_years_p,school_years_m, max_edu_p,max_edu_gpo), 
               names_to = "rel", values_to = "val") %>% 
  pivot_wider(names_from = stat, values_from = val) %>% 
  filter(sample == "pop_edu" | (rel!="school_years_p" & rel!="school_years_m")) %>% 
  filter(sample == "LF_unrestricted" | (str_sub(rel,1,3)!="max")) %>% 
  filter(!(rel == "school_years_m" & gender == "male")) %>% 
  filter(!(rel == "school_years_p" & gender == "female")) %>% 
  mutate(sample = recode(sample, "LF_unrestricted" = "Latent F.", "pop_edu" = "Education"),
         period = case_when(rel == "school_years_s" ~ "t",
                            rel == "max_edu_gpo" ~ "t-2",
                            TRUE ~ "t-1"))

temp %>% 
  ggplot(aes(x = sample, fill = gender)) +
  geom_boxplot(
    aes(ymin = p10, lower = p25, middle = p50, upper = p75, ymax = p90),
    stat = "identity"
  ) +
  facet_grid(~period) +
  scale_y_continuous(name = "Years of Education \n") +
  scale_x_discrete(name = "") +
  scale_fill_manual(labels=c("Female","male"), values=c("gray75", "gray35")) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    axis.title = element_text(size = 22),
    axis.text = element_text(size = 20),
    legend.text = element_text(size = 20),
    strip.text.x = element_text(size = 20)
  )

ggsave("desc_edu_alternative.eps",
       device=cairo_ps, height = 10, width = 10*1.618)

# Fig. 1(b): income  
temp <- rtemp %>% 
  filter(gender != "both") %>%
  filter(sample == "pop_ginc") %>%
  select(sample:stat, ginc_3y_s,ginc_3y_m,ginc_3y_p) %>% 
  pivot_longer(cols = c(ginc_3y_s, ginc_3y_p, ginc_3y_m), names_to = "rel", values_to = "val") %>% 
  pivot_wider(names_from = stat, values_from = val) %>% 
  filter(!(rel == "ginc_3y_p" & gender == "female")) %>% 
  filter(!(rel == "ginc_3y_m" & gender == "male")) %>% 
  mutate(period = case_when(rel == "ginc_3y_s" ~ "t",
                            rel == "ginc_3y_p" ~ "t-1",
                            rel == "ginc_3y_m" ~ "t-1"),
         sample = "Income"
  )

temp %>%  
  mutate(rel = recode(rel, ginc_3y_s = "Offspring", ginc_3y_m = "Mother", ginc_3y_p = "Father")) %>% 
  mutate(rel = factor(rel, levels = c("Offspring", "Mother", "Father"))) %>% 
  ggplot(aes(x = sample, fill = gender)) +
  geom_boxplot(
    aes(ymin = p10, lower = p25, middle = p50, upper = p75, ymax = p90),
    stat = "identity"
  ) +
  facet_grid(~period) +
  scale_y_continuous(name = "Gross Income \n", breaks = seq(0, 90000, 10000)) +
  scale_x_discrete(name = "") +
  scale_fill_manual(labels=c("Female","male"), values=c("gray75", "gray35")) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    axis.title = element_text(size = 22),
    axis.text = element_text(size = 20),
    legend.text = element_text(size = 20),
    strip.text.x = element_text(size = 20)
  )
ggsave("desc_ginc_alternative.eps",
       device=cairo_ps, height = 10, width = 10*1.618)


# Fig. A.1: yob alternative
temp <- rtemp %>% 
  filter(gender != "both") %>%
  filter(sample %in% c("LF_unrestricted","pop_edu","pop_ginc")) %>%
  mutate(sample = recode(sample, "LF_unrestricted" = "Latent F.", "pop_ginc" = "Income", "pop_edu" = "Education")) %>% 
  select(sample:yob_m, yob_gp_avg) %>% 
  pivot_longer(cols = c(yob_s, yob_p, yob_m, yob_gp_avg), names_to = "rel", values_to = "val") %>% 
  pivot_wider(names_from = stat, values_from = val) %>% 
  mutate(period = case_when(rel == "yob_s" ~ "t",
                            rel == "yob_m" ~ "t-1",
                            rel == "yob_p" ~ "t-1",
                            rel == "yob_gp_avg" ~ "t-2")) %>% 
  filter(!(rel == "yob_p" & gender == "female")) %>% 
  filter(!(rel == "yob_m" & gender == "male"))


temp %>%
  ggplot(aes(x = sample, fill = gender)) +
  geom_boxplot(
    aes(ymin = p10, lower = p25, middle = p50, upper = p75, ymax = p90),
    stat = "identity"
  ) +
  facet_grid(~period) +
  scale_y_continuous(name = "Year \n", breaks = c(1920,1930,1940,1950,1960,1970,1980,1989)) +
  scale_x_discrete(name = "") +
  scale_fill_manual(labels=c("Female","male"), values=c("gray75", "gray35")) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    axis.title = element_text(size = 22),
    axis.text = element_text(size = 20),
    legend.text = element_text(size = 20),
    strip.text.x = element_text(size = 20)
  )

ggsave("desc_yob_alternative.eps",
       device=cairo_ps, height = 10, width = 10*1.618)

# Tab A.2(a): additional descriptives - yob

temp <- rtemp %>% 
  filter(gender != "both") %>%
  filter(sample %in% c("LF_unrestricted","pop_edu","pop_ginc")) %>%
  mutate(sample = recode(sample, "LF_unrestricted" = "Latent F.", "pop_ginc" = "Income", "pop_edu" = "Education")) %>% 
  select(sample:yob_m, yob_gp_avg) %>% 
  pivot_longer(cols = c(yob_s, yob_p, yob_m, yob_gp_avg), names_to = "rel", values_to = "val") %>% 
  pivot_wider(names_from = stat, values_from = val) %>% 
  mutate(period = case_when(rel == "yob_s" ~ "t",
                            rel == "yob_m" ~ "t$_{-1}$",
                            rel == "yob_p" ~ "t$_{-1}$",
                            rel == "yob_gp_avg" ~ "t$_{-2}$"),
         mean = mean_round(mean),
         sd = se_round(sd)
  ) %>% 
  filter(!(rel == "yob_p" & gender == "female")) %>% 
  filter(!(rel == "yob_m" & gender == "male")) %>% 
  filter(sample == "Latent F." | rel != "yob_gp_avg") %>% 
  select(-c(p10,p25,p50,p75,p90,rel)) %>% 
  rename(dim = num) %>% 
  relocate(dim, .after = last_col()) %>% 
  rename(Sample = sample, Gender = gender, Mean = mean, SD = sd, Period = period, N = dim) %>% 
  mutate(Gender = if_else(Gender == "male", "Male", "Female")) %>% 
  arrange(Sample, Period)


print(xtable(temp, type = "latex", digits=c(0,0,0,3,3,0,0)), 
      file = "desc_yob.tex", 
      include.rownames = F, floating = F, booktabs = T, sanitize.text.function = function(x){x})

# Tab A.2(b): additional descriptives - education
tempedu <- rtemp %>% 
  filter(gender == "male" | gender == "female") %>%
  filter(sample == "pop_edu") %>%
  select(sample:stat, school_years_s,school_years_p, school_years_m) %>% 
  pivot_longer(cols = c(school_years_s, school_years_p, school_years_m), 
               names_to = "rel", values_to = "val") %>% 
  pivot_wider(names_from = stat, values_from = val) %>% 
  mutate(period = case_when(rel == "school_years_s" ~ "t",
                            rel == "school_years_m" ~ "t$_{-1}$",
                            rel == "school_years_p" ~ "t$_{-1}$"),
         sample = recode(sample, "LF_unrestricted" = "Latent F.", "pop_edu" = "Education")) %>% 
  filter(!(rel == "school_years_p" & gender == "female")) %>% 
  filter(!(rel == "school_years_m" & gender == "male")) %>% 
  mutate(mean = mean_round(mean),
         sd = se_round(sd)) %>% 
  select(-c(p10,p25,p50,p75,p90,rel)) %>% 
  relocate(num, .after = last_col()) %>% 
  rename(Sample = sample, Gender = gender, Mean = mean, SD = sd, Period = period, N = num) %>% 
  mutate(Gender = if_else(Gender == "male", "Male", "Female")) %>% 
  arrange(Sample, Period)

tempedulf <- rtemp %>% 
  filter(gender == "male" | gender == "female") %>%
  filter(sample == "LF_unrestricted") %>% 
  select(sample:stat, school_years_s,max_edu_p, max_edu_gpo) %>% 
  pivot_longer(cols = c(school_years_s, max_edu_p, max_edu_gpo), 
               names_to = "rel", values_to = "val") %>% 
  pivot_wider(names_from = stat, values_from = val) %>% 
  mutate(period = case_when(rel == "school_years_s" ~ "t",
                            rel == "max_edu_p" ~ "t$_{-1}$",
                            rel == "max_edu_gpo" ~ "t$_{-2}$"),
         sample = recode(sample, "LF_unrestricted" = "Latent F.", "pop_edu" = "Education")) %>% 
  mutate(mean = mean_round(mean),
         sd = se_round(sd)) %>% 
  select(-c(p10,p25,p50,p75,p90,rel)) %>% 
  relocate(num, .after = last_col()) %>% 
  rename(Sample = sample, Gender = gender, Mean = mean, SD = sd, Period = period, N = num) %>% 
  mutate(Gender = if_else(Gender == "male", "Male", "Female")) %>% 
  arrange(Sample, Period)

temp <- tempedu %>% 
  bind_rows(tempedulf)

print(xtable(temp, type = "latex", digits=c(0,0,0,3,3,0,0)), 
      file = "desc_education.tex", 
      include.rownames = F, floating = F, booktabs = T, sanitize.text.function = function(x){x})

# Tab A.2(c): additional descriptives - income 
temp <- rtemp %>% 
  filter(gender != "both") %>%
  filter(sample == "pop_ginc") %>%
  select(sample:stat, ginc_3y_s,ginc_3y_p,ginc_3y_m) %>% 
  pivot_longer(cols = c(ginc_3y_s,ginc_3y_p,ginc_3y_m), names_to = "rel", values_to = "val") %>% 
  pivot_wider(names_from = stat, values_from = val) %>% 
  filter(!(rel == "ginc_3y_p" & gender == "female")) %>% 
  filter(!(rel == "ginc_3y_m" & gender == "male")) %>% 
  mutate(period = case_when(rel == "ginc_3y_s" ~ "t",
                            rel == "ginc_3y_p" ~ "t$_{-1}$",
                            rel == "ginc_3y_m" ~ "t$_{-1}$"),
         sample = "Income",
         mean = mean_round(mean),
         sd = se_round(sd)
  ) %>% 
  select(-c(p10,p25,p50,p75,p90,rel)) %>% 
  rename(dim = num) %>% 
  relocate(dim, .after = last_col()) %>% 
  rename(Sample = sample, Gender = gender, Mean = mean, SD = sd, Period = period, N = dim) %>% 
  mutate(Gender = if_else(Gender == "male", "Male", "Female")) %>% 
  arrange(Sample, Period)



print(xtable(temp, type = "latex", digits=c(0,0,0,3,3,0,0)), 
      file = "desc_ginc.tex", 
      include.rownames = F, floating = F, booktabs = T, sanitize.text.function = function(x){x})

# Fig. A.2: education average
temp <- rtemp %>% 
  filter(str_detect(sample, "desc_yob")) %>% 
  select(sample, gender, mean_school, dim) %>% 
  separate(sample, into = c("tt", "Year"), sep = 8) %>% 
  mutate(Year = as.numeric(Year)) %>% 
  select(-tt)

temp %>% ggplot(aes(x = Year, y = mean_school, color = gender, group = gender)) +
  geom_line(aes(linetype=gender), size = 1.2) +
  scale_linetype_manual("Gender", values=c("solid", "dashed", "dotted")) +
  scale_color_manual("Gender", values=c("grey0","grey30","grey60")) +
  scale_x_continuous(name = "\n Year", breaks = c(1920,1930,1940,1950,1960,1970,1980,1989)) +
  scale_y_continuous(name = "Average school years \n") +
  theme_bw() +
  theme(
    axis.text = element_text(size = 20),
    axis.title = element_text(size = 22),
    legend.position='bottom',
    legend.key.width = unit(2, 'cm'),
    legend.key.size = unit(2, 'cm'), 
    legend.title = element_text(size=22), 
    legend.text = element_text(size=18)
  ) 

ggsave("school_years_pop_average.eps",
       device=cairo_ps, height = 10, width = 10*1.618)


# 2. Results: aggregates --------------------------------------------------
# Tab. 1: correlations

temp_corr  <- rtemp %>%
  filter(byyear == 0 & param == "corr") %>%
  mutate(ci_low = mean - (se * 1.96),
         ci_up = mean + (se * 1.96)
  ) %>%
  select(-c(byyear, year)) %>%
  arrange(param, type) %>%
  mutate(se = se_round(se),
         mean = mean_round(mean),
         ci_low = mean_round(ci_low),
         ci_up = mean_round(ci_up)
  ) %>%
  select(c(param, type, mean, se, ci_low, ci_up, num)) %>%
  filter(type %in% c("ginc3y_mat", "ginc3y_pat", "edu_mat_dau", "edu_pat_son"))

temp_latent  <- rtemp %>%
  filter(byyear == 0 & param != "corr") %>%
  mutate(ci_low = mean - (se * 1.96),
         ci_up = mean + (se * 1.96)
  ) %>%
  arrange(param, type) %>%
  mutate(se = se_round(se),
         mean = mean_round(mean),
         ci_low = mean_round(ci_low),
         ci_up = mean_round(ci_up)
  ) %>%
  select(c(param, type, mean, se, ci_low, ci_up, num)) %>%
  filter(type == "dau_lf" | type == "son_lf")

temp <- rbind(temp_corr, temp_latent) %>%
  mutate(sample = case_when(grepl("edu", type) ~ "Education",
                            grepl("ginc", type) ~ "Income",
                            grepl("lf", type) ~ "Latent F."),
         gender = case_when(grepl(c("dau|mat"), type) ~ "Female",
                            grepl(c("son|pat"), type) ~ "Male")
  ) %>%
  select(sample, gender, param, mean, se, num) %>%
  rename(Sample = sample, Gender = gender, Parameter = param, Estimates = mean, S.E. = se, N = num) %>%
  arrange(Sample, Parameter, Gender) %>%
  mutate(Parameter = case_when(Parameter == "corr" ~ "IGC",
                               Parameter == "lambda" ~ "$\\lambda$",
                               Parameter == "rho" ~ "$\\rho$"
  ))


print(xtable(temp, type = "latex", digits=c(0,0,0,0,3,3,0)),
      file = "overall_latent_edu_ginc.tex",
      include.rownames = F, floating = F, booktabs = T, sanitize.text.function = function(x){x})

rm(temp_corr, temp_latent)

# Tab. 2: education trends

temp  <-  rtemp %>% 
  mutate(ci_low = mean - (se * 1.96),
         ci_up = mean + (se * 1.96)
  ) %>% 
  filter(type == "edutrends_pat" | type == "edutrends_mat") %>% 
  select(-c(byyear, year, ))

# Tab. 2(a): Father - correlation
to_tab <- temp %>%
  filter(type == "edutrends_pat" & str_detect(param, "corr")) %>%
  mutate(param = as.factor(param)) %>% 
  select(mean:years_included, param) %>% 
  mutate(param = recode(param,'corr_both'="Both",
                        'corr_only offspring'="Son",
                        'corr_only parent'="Father")) %>% 
  pivot_wider(names_from = "param", values_from = c(mean, se)) %>% 
  select(years_included, mean_Son, se_Son, mean_Father, se_Father, mean_Both, se_Both, num) %>% 
  mutate(mean_Son = mean_round(mean_Son),
         mean_Father = mean_round(mean_Father),
         mean_Both = mean_round(mean_Both),
         se_Son = se_round(se_Son),
         se_Father = se_round(se_Father),
         se_Both = se_round(se_Both),
  ) 

print(xtable(to_tab, type = "latex", digits=c(0,0,3,3,3,3,3,3,0)), 
      file = "edutrends_pat_corr.tex", 
      include.rownames = F, floating = F, booktabs = T, sanitize.text.function = function(x){x})

# Tab. 2(b): Mother - correlation
to_tab <- temp %>%
  filter(type == "edutrends_mat" & str_detect(param, "corr")) %>%
  mutate(param = as.factor(param)) %>% 
  select(mean:years_included, param) %>% 
  mutate(param = recode(param,'corr_both'="Both",
                        'corr_only offspring'="Daughter",
                        'corr_only parent'="Mother")) %>% 
  pivot_wider(names_from = "param", values_from = c(mean, se)) %>% 
  select(years_included, mean_Daughter, se_Daughter, mean_Mother, se_Mother, mean_Both, se_Both, num) %>% 
  mutate(mean_Daughter = mean_round(mean_Daughter),
         mean_Mother = mean_round(mean_Mother),
         mean_Both = mean_round(mean_Both),
         se_Daughter = se_round(se_Daughter),
         se_Mother = se_round(se_Mother),
         se_Both = se_round(se_Both),
  ) 

print(xtable(to_tab, type = "latex", digits=c(0,0,3,3,3,3,3,3,0)), 
      file = "edutrends_mat_corr.tex", 
      include.rownames = F, floating = F, booktabs = T, sanitize.text.function = function(x){x})


# Tab. A.3: correlations - fullmat & fullpat
temp  <- rtemp %>%
  filter(byyear == 0 & param != "corr") %>%
  mutate(ci_low = mean - (se * 1.96),
         ci_up = mean + (se * 1.96)
  ) %>%
  arrange(param, type) %>%
  mutate(se = se_round(se),
         mean = mean_round(mean),
         ci_low = mean_round(ci_low),
         ci_up = mean_round(ci_up)
  ) %>%
  select(c(param, type, mean, se, ci_low, ci_up, num)) %>%
  filter(type == "dau_lf_full_mat_pat" | type == "son_lf_full_mat_pat")

temp <- rbind(temp_corr, temp_latent) %>%
  mutate(sample = "Latent F.",
         gender = case_when(grepl("dau", type) ~ "Female",
                            grepl("son", type) ~ "Male")
  ) %>%
  select(sample, gender, param, mean, se, num) %>%
  rename(Sample = sample, Gender = gender, Parameter = param, Estimates = mean, S.E. = se, N = num) %>%
  arrange(Sample, Parameter, Gender) %>%
  mutate(Parameter = case_when(Parameter == "lambda" ~ "$\\lambda$",
                               Parameter == "rho" ~ "$\\rho$"
  ))


print(xtable(temp, type = "latex", digits=c(0,0,0,0,3,3,0)),
      file = "overall_latent_full_mat_pat.tex",
      include.rownames = F, floating = F, booktabs = T, sanitize.text.function = function(x){x})


# Tab. A.4: correlations - other income definitions
tokeep <-c("ginc3y_mat","ginc3y_pat","ginc5y_mat","ginc5y_pat",
           "rinc3y_mat","rinc3y_pat","rinc5y_mat","rinc5y_pat",
           "full_ginc3y_mat","full_ginc3y_pat","full_ginc5y_mat","full_ginc5y_pat")

temp  <- rtemp %>%
  filter(byyear == 0 & param == "corr") %>%
  mutate(ci_low = mean - (se * 1.96),
         ci_up = mean + (se * 1.96)
  ) %>%
  select(-c(byyear, year)) %>%
  arrange(param, type) %>%
  mutate(se = se_round(se),
         mean = mean_round(mean),
         ci_low = mean_round(ci_low),
         ci_up = mean_round(ci_up)
  ) %>%
  select(c(param, type, mean, se, ci_low, ci_up, num)) %>%
  filter(type %in% tokeep) %>%
  mutate(sample = case_when(str_sub(type,1,6)=="ginc3y" ~ "Income 3 years",
                            str_sub(type,1,6)=="ginc5y" ~ "Income 5 years",
                            str_sub(type,1,6)=="rinc3y" ~ "Real Income 3 years",
                            str_sub(type,1,6)=="rinc5y" ~ "Real Income 5 years",
                            str_sub(type,1,11)=="full_ginc3y" ~ "Income (12m) 3 years",
                            str_sub(type,1,11)=="full_ginc5y" ~ "Income (12m) 5 years"),
         gender = case_when(grepl(c("dau|mat"), type) ~ "Female",
                            grepl(c("son|pat"), type) ~ "Male"),
         tosort = case_when(str_sub(type,1,6)=="ginc3y" ~ 1,
                            str_sub(type,1,6)=="ginc5y" ~ 2,
                            str_sub(type,1,6)=="rinc3y" ~ 3,
                            str_sub(type,1,6)=="rinc5y" ~ 4,
                            str_sub(type,1,11)=="full_ginc3y" ~ 5,
                            str_sub(type,1,11)=="full_ginc5y" ~ 6)
  ) %>%
  select(tosort,sample, gender, mean, se, num) %>%
  rename(Sample = sample, Gender = gender, Estimates = mean, S.E. = se, N = num) %>%
  arrange(tosort, Gender) %>%
  select(-tosort)

print(xtable(temp, type = "latex", digits=c(0,0,0,3,3,0)),
      file = "different_income_definitions.tex",
      include.rownames = F, floating = F, booktabs = T, sanitize.text.function = function(x){x})



# 3. results: by year -----------------------------------------------------
# read_excel("output_2021_short/results_short.xlsx", sheet = "c_results") %>% 
rtemp <- read_excel("results.xlsx", sheet = "results")

temp  <-  rtemp %>% 
  filter(byyear == 1) %>% 
  mutate(ci_low = mean - (se * 1.96),
         ci_up = mean + (se * 1.96)
  ) %>% 
  select(-c(byyear, se))

# 1.1 Figures
# Fig. 2(a) -- education
temp %>% 
  filter(type == "mat_edu" | type == "pat_edu") %>% 
  mutate(rel = if_else(type == "mat_edu", "Mother-to-daughter", "Father-to-son")) %>% 
  mutate(rel = factor(rel, levels = c("Mother-to-daughter","Father-to-son"))) %>% 
  ggplot(aes(x = year, y = mean)) +
  geom_line() +
  geom_ribbon(aes(ymin = ci_low, ymax = ci_up), alpha = 0.1) +
  facet_grid(~rel) +
  scale_y_continuous(name = "Correlation coef. \n", breaks = seq(0, 0.45, by = 0.05)) +
  scale_x_continuous(name = "\n Year of Birth", breaks = c(1950,1960,1970,1980,1989)) +
  theme_bw() +
  theme(
    axis.text = element_text(size = 20),
    axis.title = element_text(size = 22),
    strip.text = element_text(size = 20)
  )

ggsave("by_year_education.eps", device=cairo_ps, height = 10, width = 10*1.618)

# Fig. 2(b) -- income 
temp %>% 
  filter(type == "mat_ginc" | type == "pat_ginc") %>% 
  mutate(rel = if_else(type == "mat_ginc", "Mother-to-daughter", "Father-to-son")) %>% 
  mutate(rel = factor(rel, levels = c("Mother-to-daughter","Father-to-son"))) %>% 
  ggplot(aes(x = year, y = mean)) +
  geom_line() +
  geom_ribbon(aes(ymin = ci_low, ymax = ci_up), alpha = 0.1) +
  facet_grid(~rel) +
  scale_y_continuous(name = "Correlation coef. \n", breaks = seq(0, 0.30, by = 0.025)) +
  scale_x_continuous(name = "\n Year of Birth", breaks = c(1970,1975,1980,1985,1989)) +
  theme_bw() +
  theme(
    axis.text = element_text(size = 20),
    axis.title = element_text(size = 22),
    strip.text = element_text(size = 20)
  )

ggsave("by_year_ginc.eps", device=cairo_ps, height = 10, width = 10*1.618)


# Fig. 3 -- latent factor: lambda and rho
temp %>% 
  filter(year >= 1975) %>%
  filter(type == "dau_lf" | type == "son_lf") %>% 
  mutate(rel = if_else(type == "dau_lf", "Female offspring", "Male offspring"),
         param = if_else(param == "rho", "Rho", "Lambda")
  ) %>% 
  ggplot(aes(x = year, y = mean)) +
  geom_line() +
  geom_ribbon(aes(ymin = ci_low, ymax = ci_up), alpha = 0.1) +
  facet_grid(param~rel, scales = "free_y") +
  scale_y_continuous(name = "", breaks = seq(0.2, 1, by = 0.1)) +
  scale_x_continuous(name = "\n Year of Birth", breaks = c(1975,1980,1985,1989)) +
  theme_bw() +
  theme(
    axis.text = element_text(size = 20),
    axis.title = element_text(size = 22),
    strip.text = element_text(size = 20)
  )

ggsave("by_year_lambda_rho.eps",device=cairo_ps, height = 10, width = 10*1.618)

# 1.2 Tables

# Tab. A.5 -- education
temp  <-  rtemp %>%
  filter(byyear == 1) %>%
  select(-c(byyear, param)) %>%
  filter(type == "mat_edu" | type == "pat_edu") %>%
  mutate(type = if_else(type == "mat_edu", "maternal", "paternal")) %>%
  pivot_wider(names_from = type, values_from = c(mean, se, num)) %>%
  mutate(se_maternal = se_round(se_maternal),
         se_paternal = se_round(se_paternal),
         mean_maternal = sprintf("%.3f", round(mean_maternal,3)),
         mean_paternal = sprintf("%.3f", round(mean_paternal,3))
  ) %>%
  select(year, mean_maternal, se_maternal, num_maternal,mean_paternal, se_paternal, num_paternal)

print(xtable(temp, type = "latex", digits=c(0, 0,3,3,0,3,3,0)),
      file = "by_year_education.tex", include.rownames = F)

# Tab. A.6 -- income
temp  <-  rtemp %>%
  filter(byyear == 1) %>%
  select(-c(byyear, param)) %>%
  filter(type == "mat_ginc" | type == "pat_ginc") %>%
  mutate(type = if_else(type == "mat_ginc", "maternal", "paternal")) %>%
  pivot_wider(names_from = c(type), values_from = c(mean, se, num)) %>%
  mutate(se_maternal = se_round(se_maternal),
         se_paternal = se_round(se_paternal),
         mean_maternal = sprintf("%.3f", round(mean_maternal,3)),
         mean_paternal = sprintf("%.3f", round(mean_paternal,3))
  ) %>%
  select(year, mean_maternal, se_maternal, num_maternal, mean_paternal, se_paternal, num_paternal)

print(xtable(temp, type = "latex", digits=c(0, 0,3,3,0,3,3,0)),
      file = "by_year_ginc.tex", include.rownames = F)

# Tab. A.7(a) -- latent factor females
temp  <-  rtemp %>% #females
  filter(byyear == 1) %>%
  filter(type == "dau_lf") %>%
  filter(year >= 1975) %>%
  select(-c(byyear, type)) %>%
  pivot_wider(names_from = c(param), values_from = c(mean, se, num)) %>%
  mutate(across(starts_with("se_"), se_round),
         across(starts_with("mean_"),  mean_round),
  ) %>%
  select(year, mean_lambda, se_lambda, mean_rho, se_rho, num_lambda) %>%
  rename(num = num_lambda)

print(xtable(temp, type = "latex", digits=c(0,0,3,3,3,3,0)),
      file = "by_year_lambda_rho_female.tex", include.rownames = F)

# Tab. A.7(b) -- latent factor males
temp  <-  rtemp %>% #males
  filter(byyear == 1) %>%
  filter(type == "son_lf") %>%
  filter(year >= 1975) %>%
  select(-c(byyear, type)) %>%
  pivot_wider(names_from = c(param), values_from = c(mean, se, num)) %>%
  mutate(across(starts_with("se_"), se_round),
         across(starts_with("mean_"),  mean_round),
  ) %>%
  select(year, mean_lambda, se_lambda, mean_rho, se_rho, num_lambda) %>%
  rename(num = num_lambda)

print(xtable(temp, type = "latex", digits=c(0,0,3,3,3,3,0)),
      file = "by_year_lambda_rho_male.tex", include.rownames = F)

