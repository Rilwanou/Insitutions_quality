data <- read_excel("C:/Users/RiL/Desktop/Prog/Qualité_institutions/data/processed/data.xlsx")
data <- as.data.frame(data)
View(data)

data <- data %>%
  rename(
    former_colony = `Former colonies`,
    log_gdp_ppp_1995 = `Log GDP per capita(PPP) in 1995`,
    avg_prot_risk = `Average protection against expropriation risk 1985-1995`,
    mortality_est = `Main mortality estimate`,
    output_worker_1995 = `1995`,
    output_worker_2019 = `2019`
  )

data = data %>% mutate(
  mortality_est = as.numeric(mortality_est),
  Latitude = as.numeric(Latitude)
)

data$log_mortality = log(data$mortality_est)

# Ajout PIB 2019
gdp_2019 <- c(
  14084.19953, 7533.511854, 23535.01275, 60574.6271, 34541.70749, 6025.087349,
  9091.927689, 15741.4785, 2233.261555, 4195.670175, 50498.97311, 25824.64653,
  16091.49905, 5766.754924, 22949.52866, 5947.924089, 19743.40045, 12540.32512,
  13608.49971, 9666.802079, 2274.214139, 16124.86386, 2371.492041, 5952.403657,
  11079.62731, 3143.646883, 13388.70608, 3220.942957, 5776.103688, 61221.41954,
  7181.522654, 12115.70207, 10117.56417, 4710.50374, 1652.49342, 29495.40352,
  2437.558036, 50327.68455, 21095.7137, 8024.405273, 45163.69643, 5819.315185,
  1389.164255, 5525.443784, 5206.907165, 33229.49603, 14117.16509, 13408.35324,
  3728.234533, 1623.650984, 105542.4136, 13706.71382, 14112.9525, 4123.92334,
  2947.126953, 2273.815278, 26641.38916, 12314.08594, 2444.248287, 25548.06338,
  65548.07078, 7046.0, 11190.15319, 1206.798113
)
data = cbind(data, gdp_2019)
data$log_gdp_ppp_2019 <- log(data$gdp_2019) 

data <- data %>%
  mutate(
    log_output_worker_2019 = log(output_worker_2019), 
    log_output_worker_1995 = log(output_worker_1995),
    norm_latitude = abs(Latitude) / 90
  )

View(data)

data$`asia_dummy` <- ifelse(data$former_colony %in% c("India", "Bangladesh", "Malaysia", "Sri Lanka", "Pakistan", "Vietnam", "Indonesia", "Singapore", "Hong Kong"), 1, 0)
data$`africa_dummy` <- ifelse(data$former_colony %in% c("Algeria", "Angola", "Egypt", "Kenya", "Nigeria", "Ethiopia", "Ghana", "South Africa", "Sudan", "Tanzania", "Uganda", "Zaire", "Morocco", "Tunisia", "Senegal", "Mali", "Burkina Faso", "Cameroon", "Côte d'Ivoire", "Gabon", "Guinea", "Sierra Leone", "Togo", "Gambia", "Madagascar", "Niger", "Congo (Brazzaville)"), 1, 0)
data$`neo_europes_dummy` <- ifelse(data$former_colony %in% c("USA", "Canada", "Australia", "New Zealand"), 1, 0)
data$`other_regions_dummy` <- ifelse(data$former_colony %in% c("Australia", "New Zealand", "Malta"), 1, 0)

# Sous-échantillons
base_sample_without_neo_europes <- subset(data, neo_europes_dummy == 0)
base_sample_without_africa <- subset(data, africa_dummy == 0)


# exécution des modèles

# (Table 4, Panel C)
ols_c1 <- lm(log_gdp_ppp_1995 ~ avg_prot_risk, data = data)
ols_c2 <- lm(log_gdp_ppp_1995 ~ avg_prot_risk + norm_latitude, data = data)
ols_c3 <- lm(log_gdp_ppp_1995 ~ avg_prot_risk, data = base_sample_without_neo_europes)
ols_c4 <- lm(log_gdp_ppp_1995 ~ avg_prot_risk + norm_latitude, data = base_sample_without_neo_europes)
ols_c5 <- lm(log_gdp_ppp_1995 ~ avg_prot_risk, data = base_sample_without_africa)
ols_c6 <- lm(log_gdp_ppp_1995 ~ avg_prot_risk + norm_latitude, data = base_sample_without_africa)
ols_c7 <- lm(log_gdp_ppp_1995 ~ avg_prot_risk + asia_dummy + africa_dummy + other_regions_dummy, data = data)
ols_c8 <- lm(log_gdp_ppp_1995 ~ avg_prot_risk + asia_dummy + africa_dummy + other_regions_dummy + norm_latitude, data = data)
ols_c9 <- lm(log_output_worker_1995 ~ avg_prot_risk, data = data)

# (Table 4, Panel B)
fs_b1 <- lm(avg_prot_risk ~ log_mortality, data = data)
fs_b2 <- lm(avg_prot_risk ~ log_mortality + norm_latitude, data = data)
fs_b3 <- lm(avg_prot_risk ~ log_mortality, data = base_sample_without_neo_europes)
fs_b4 <- lm(avg_prot_risk ~ log_mortality + norm_latitude, data = base_sample_without_neo_europes)
fs_b5 <- lm(avg_prot_risk ~ log_mortality, data = base_sample_without_africa)
fs_b6 <- lm(avg_prot_risk ~ log_mortality + norm_latitude, data = base_sample_without_africa)
fs_b7 <- lm(avg_prot_risk ~ log_mortality + asia_dummy + africa_dummy + other_regions_dummy, data = data)
fs_b8 <- lm(avg_prot_risk ~ log_mortality + asia_dummy + africa_dummy + other_regions_dummy + norm_latitude, data = data)
fs_b9 <- lm(avg_prot_risk ~ log_mortality, data = data)

# (Table 4, Panel A)
iv_a1 <- ivreg(log_gdp_ppp_1995 ~ avg_prot_risk | log_mortality, data = data)
iv_a2 <- ivreg(log_gdp_ppp_1995 ~ avg_prot_risk + norm_latitude | log_mortality + norm_latitude, data = data)
iv_a3 <- ivreg(log_gdp_ppp_1995 ~ avg_prot_risk | log_mortality, data = base_sample_without_neo_europes)
iv_a4 <- ivreg(log_gdp_ppp_1995 ~ avg_prot_risk + norm_latitude | log_mortality + norm_latitude, data = base_sample_without_neo_europes)
iv_a5 <- ivreg(log_gdp_ppp_1995 ~ avg_prot_risk | log_mortality, data = base_sample_without_africa)
iv_a6 <- ivreg(log_gdp_ppp_1995 ~ avg_prot_risk + norm_latitude | log_mortality + norm_latitude, data = base_sample_without_africa)
iv_a7 <- ivreg(log_gdp_ppp_1995 ~ avg_prot_risk + asia_dummy + africa_dummy + other_regions_dummy | log_mortality + asia_dummy + africa_dummy + other_regions_dummy, data = data)
iv_a8 <- ivreg(log_gdp_ppp_1995 ~ avg_prot_risk + norm_latitude + asia_dummy + africa_dummy + other_regions_dummy | log_mortality + norm_latitude + asia_dummy + africa_dummy + other_regions_dummy, data = data)
iv_a9 <- ivreg(log_output_worker_1995 ~ avg_prot_risk | log_mortality, data = data)

# OLS 2019
ols_2019_1 <- lm(log_gdp_ppp_2019 ~ avg_prot_risk, data = data)
ols_2019_2 <- lm(log_gdp_ppp_2019 ~ avg_prot_risk + norm_latitude, data = data)
ols_2019_3 <- lm(log_gdp_ppp_2019 ~ avg_prot_risk , data = base_sample_without_neo_europes)
ols_2019_4 <- lm(log_gdp_ppp_2019 ~ avg_prot_risk + norm_latitude, data = base_sample_without_neo_europes)
ols_2019_5 <- lm(log_gdp_ppp_2019 ~ avg_prot_risk, data = base_sample_without_africa)
ols_2019_6 <- lm(log_gdp_ppp_2019 ~ avg_prot_risk + norm_latitude, data = base_sample_without_africa)
ols_2019_7 <- lm(log_gdp_ppp_2019 ~ avg_prot_risk + asia_dummy + africa_dummy + other_regions_dummy, data = data)
ols_2019_8 <- lm(log_gdp_ppp_2019 ~ avg_prot_risk + asia_dummy + africa_dummy + other_regions_dummy + norm_latitude , data = data)
ols_2019_9 <- lm(log_output_worker_2019 ~ avg_prot_risk, data = data)

# IV 2SLS 2019
iv_2019_1 <- ivreg(log_gdp_ppp_2019 ~ avg_prot_risk | log_mortality, data = data)
iv_2019_2 <- ivreg(log_gdp_ppp_2019 ~ avg_prot_risk + norm_latitude | log_mortality + norm_latitude, data = data)
iv_2019_3 <- ivreg(log_gdp_ppp_2019 ~ avg_prot_risk | log_mortality, data = base_sample_without_neo_europes)
iv_2019_4 <- ivreg(log_gdp_ppp_2019 ~ avg_prot_risk + norm_latitude | log_mortality + norm_latitude, data = base_sample_without_neo_europes)
iv_2019_5 <- ivreg(log_gdp_ppp_2019 ~ avg_prot_risk | log_mortality, data = base_sample_without_africa)
iv_2019_6 <- ivreg(log_gdp_ppp_2019 ~ avg_prot_risk + norm_latitude | log_mortality + norm_latitude, data = base_sample_without_africa)
iv_2019_7 <- ivreg(log_gdp_ppp_2019 ~ avg_prot_risk + asia_dummy + africa_dummy + other_regions_dummy | log_mortality + asia_dummy + africa_dummy + other_regions_dummy, data = data)
iv_2019_8 <- ivreg(log_gdp_ppp_2019 ~ avg_prot_risk + norm_latitude + asia_dummy + africa_dummy + other_regions_dummy | log_mortality + norm_latitude + asia_dummy + africa_dummy + other_regions_dummy, data = data)
iv_2019_9 <- ivreg(log_output_worker_2019 ~ avg_prot_risk | log_mortality, data = data)

