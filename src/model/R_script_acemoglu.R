# Installer les packages nécessaires
install.packages("readxl")
install.packages("dplyr")

# Charger les bibliothèques
library(readxl)
library(dplyr)

# Importation des données
data <- read_excel("Capture.xlsx")

# Aperçu des données
head(data)

# Convertir les colonnes en numérique

data$`Main mortality estimate` <- as.numeric(data$`Main mortality estimate`)
data$Latitude <- as.numeric(data$Latitude)

# Transformer en data.frame
data <- as.data.frame(data)

# Liste exhaustive des pays
liste_pays <- unique(data$`Former colonies`)
print(liste_pays)

gdp_2019 <- c(
  14084.19953, 7533.511854, 23535.01275, 60574.6271, 34541.70749, 6025.087349,
  9091.927689, 15741.4785, 2233.261555, 4195.670175, 50498.97311, 25824.64653,
  16091.49905, 5766.754924, 22949.52866, 5947.924089, 19743.40045, 12540.32512,
  13608.49971, 9666.802079, 2274.214139, 16126.86386, 2371.492041, 5952.403657,
  11079.62731, 3143.646883, 13388.70608, 3220.942957, 5776.103688, 61221.41954,
  7181.522654, 12115.70207, 10117.56417, 4710.50374, 1652.49342, 29495.40352,
  2437.558036, 50327.68455, 21095.7137, 8024.405273, 45163.69643, 5819.315185,
  1389.164255, 5525.443784, 5206.907165, 33229.49603, 14117.16509, 13408.35324,
  3728.234533, 1623.650984, 105542.4136, 13706.71382, 14112.9525, 4123.92334,
  2947.126953, 2273.815278, 26641.38916, 12314.08594, 2444.248287, 25548.06338,
  65548.07078, 7046.0, 11190.15319, 1206.798113
)

#ajout de la colonne gdp_2019
data = cbind(data, gdp_2019)
data$gdp_2019 <- log(gdp_2019)

# Création des variables nécessaires
data <- data %>%
  mutate(
    
    log_gdp_2019 = log(`2019`), 
    log_gdp_1995 = log(`1995`),
    norm_latitude = Latitude / 90,  # Latitude divisée par 90
  )

# Vérifier les transformations
summary(data)
data

# Chargement des données
# data <- read.csv("votre_fichier.csv")  # Chargez votre dataset ici

# Création des colonnes dummy
data$`asia_dummy` <- ifelse(data$`Former colonies` %in% c(
  "India", "Bangladesh", "Malaysia", "Sri Lanka", "Pakistan", "Vietnam", "Indonesia", "Singapore", "Hong Kong"
), 1, 0)

data$`africa_dummy` <- ifelse(data$`Former colonies`%in% c(
  "Algeria", "Angola", "Egypt", "Kenya", "Nigeria", "Ethiopia", "Ghana", "South Africa",
  "Sudan", "Tanzania", "Uganda", "Zaire", "Morocco", "Tunisia", "Senegal", "Mali", "Burkina Faso",
  "Cameroon", "Côte d'Ivoire", "Gabon", "Guinea", "Sierra Leone", "Togo", "Gambia", "Madagascar", "Niger", "Congo (Brazzaville)"
), 1, 0)

data$`neo_europes_dummy` <- ifelse(data$`Former colonies` %in% c(
  "USA", "Canada", "Australia", "New Zealand"
), 1, 0)

data$`america_dummy` <- ifelse(data$`Former colonies` %in% c(
  "Bahamas","USA", "Canada","Argentina", "Brazil", "Bolivia", "Chile", "Colombia", "Costa Rica", "Dominican Republic",
  "Ecuador", "El Salvador", "Guatemala", "Guyana", "Haiti", "Honduras", "Jamaica", "Mexico",
  "Nicaragua", "Panama", "Paraguay", "Peru", "Trinidad and Tobago", "Uruguay", "Venezuela"
), 1, 0)

data$`other_regions_dummy` <- ifelse(data$`Former colonies` %in% c(
  "Australia", "New Zealand", "Malta"
), 1, 0)

# Vérification des résultats
head(data)

# Transformer les données en data.frame si nécessaire
data <- as.data.frame(data)

# Afficher les résumés des dummies
summary(data[, c("asia_dummy", "africa_dummy", "neo_europes_dummy", "america_dummy", "other_regions_dummy")])


# Base sample (tous les pays)
base_sample <- data
# Base sample without Neo-Europes
base_sample_without_neo_europes <- subset(data, neo_europes_dummy == 0)

# Base sample without Africa
base_sample_without_africa <- subset(data, africa_dummy == 0)

# Base sample without Latin America
#base_sample_without_latin_america <- subset(data, latin_america_dummy == 0)

# Base sample without Asia
base_sample_without_asia <- subset(data, asia_dummy == 0)

# Base sample without Other Regions
base_sample_without_other_regions <- subset(data, other_regions_dummy == 0)

# Vérification des tailles de chaque échantillon
cat("Base sample:", nrow(base_sample), "\n")
cat("Without Neo-Europes:", nrow(base_sample_without_neo_europes), "\n")
cat("Without Africa:", nrow(base_sample_without_africa), "\n")
#cat("Without Latin America:", nrow(base_sample_without_latin_america), "\n")
cat("Without Asia:", nrow(base_sample_without_asia), "\n")
cat("Without Other Regions:", nrow(base_sample_without_other_regions), "\n")



#install.packages("AER")
library(AER)

data$`Main mortality estimate` = log(data$`Main mortality estimate`)
colnames(data)[colnames(data) == "Main mortality estimate"] <- "Log European settler mortality"
colnames(base_sample_without_neo_europes)[colnames(base_sample_without_neo_europes) == "Main mortality estimate"] <- "Log European settler mortality"
colnames(base_sample_without_africa)[colnames(base_sample_without_africa) == "Main mortality estimate"] <- "Log European settler mortality"
base_sample_without_neo_europes$`Log European settler mortality` = log(base_sample_without_neo_europes$`Log European settler mortality`)
base_sample_without_africa$`Log European settler mortality` = log(base_sample_without_africa$`Log European settler mortality`)
head(base_sample_without_africa)

View(data)
head(data)


# Log GDP per capita(PPP) in 1995 is our dependant variable in 1995
# gdp_2019 is our dependant variable for 2019 regressions
# log_gdp_2019 is the log ouput per workers in 2019

















#regression OLS for GDP 1995
#base simple
m_OLS_a <- lm(`Log GDP per capita(PPP) in 1995` ~ `Average protection against expropriation risk 1985-1995`, data = data)
summary(m_OLS_a)

m_OLS_b <- lm(`Log GDP per capita(PPP) in 1995`~ `Average protection against expropriation risk 1985-1995` + `norm_latitude`, data = data)
summary(m_OLS_b)
# Base sample without Neo-Europes
m_OLS_c <- lm(`Log GDP per capita(PPP) in 1995` ~ `Average protection against expropriation risk 1985-1995`, data = base_sample_without_neo_europes)
summary(m_OLS_c)

m_OLS_d <- lm(`Log GDP per capita(PPP) in 1995` ~ `Average protection against expropriation risk 1985-1995` + `norm_latitude`, data = base_sample_without_neo_europes)
summary(m_OLS_d)

# Base sample without Africa
m_OLS_e <- lm(`Log GDP per capita(PPP) in 1995` ~ `Average protection against expropriation risk 1985-1995`, data = base_sample_without_africa)
summary(m_OLS_e)

m_OLS_d <- lm(`Log GDP per capita(PPP) in 1995` ~ `Average protection against expropriation risk 1985-1995` + `norm_latitude`, data = base_sample_without_africa)
summary(m_OLS_d)

# Base simple with dummies
m_OLS_e <- lm(`Log GDP per capita(PPP) in 1995` ~ `Average protection against expropriation risk 1985-1995` + `asia_dummy` + `africa_dummy` + `other_regions_dummy`, data = data)
summary(m_OLS_e)

m_OLS_e <- lm(`Log GDP per capita(PPP) in 1995` ~ `Average protection against expropriation risk 1985-1995` + `asia_dummy` + `africa_dummy` + `other_regions_dummy` + `norm_latitude`, data = data)
summary(m_OLS_e)



#regression first stage for APER - Régression de la variable instrumentale sur la variable endogène
#base sample
first_stage_a <- lm(`Average protection against expropriation risk 1985-1995` ~ 
                    `Log European settler mortality`, data = data)
summary(first_stage_a)

first_stage_b <- lm(`Average protection against expropriation risk 1985-1995` ~ 
                    `Log European settler mortality` + `norm_latitude`, data = data)
summary(first_stage_b)

#base_sample_without_neo_europes
first_stage_c <- lm(`Average protection against expropriation risk 1985-1995` ~ 
                    `Log European settler mortality`, data = base_sample_without_neo_europes)
summary(first_stage_c)

first_stage_d <- lm(`Average protection against expropriation risk 1985-1995` ~ 
                      `Log European settler mortality` + `norm_latitude`, data = base_sample_without_neo_europes)
summary(first_stage_d)

#base_sample_without_africa
first_stage_e <- lm(`Average protection against expropriation risk 1985-1995` ~ 
                    `Log European settler mortality`, data = base_sample_without_africa)
summary(first_stage_e)

first_stage_f <- lm(`Average protection against expropriation risk 1985-1995` ~ 
                    `Log European settler mortality` + `norm_latitude`, data = base_sample_without_africa)
summary(first_stage_f)

#base_sample with dummies
first_stage_g <- lm(`Average protection against expropriation risk 1985-1995` ~ 
                      `Log European settler mortality` + `asia_dummy` + `africa_dummy` + `other_regions_dummy`, data = data)
summary(first_stage_g)

first_stage_h <- lm(`Average protection against expropriation risk 1985-1995` ~ 
                    `Log European settler mortality` + `asia_dummy` + `africa_dummy` + `other_regions_dummy` + `norm_latitude`, data = data)
summary(first_stage_h)

#regression two stage for GDP 2019



#base_sample_without_neo_europes
model_2sls <- ivreg(`Log GDP per capita(PPP) in 1995` ~ `Average protection against expropriation risk 1985-1995`| 
                      `Log European settler mortality`, data = base_sample_without_neo_europes)
summary(model_2sls)

model_2sls <- ivreg(`Log GDP per capita(PPP) in 1995` ~ `Average protection against expropriation risk 1985-1995`+ norm_latitude | 
                      `Log European settler mortality` + norm_latitude, 
                    data = base_sample_without_neo_europes)
summary(model_2sls)

#base_sample_without_africa
model_2sls <- ivreg(`Log GDP per capita(PPP) in 1995` ~ `Average protection against expropriation risk 1985-1995` | 
                      `Log European settler mortality`, 
                    data = base_sample_without_africa)
summary(model_2sls)

model_2sls <- ivreg(`Log GDP per capita(PPP) in 1995` ~ `Average protection against expropriation risk 1985-1995`+ norm_latitude | 
                      `Log European settler mortality` + norm_latitude, 
                    data = base_sample_without_africa)
summary(model_2sls)
#base_sample

model_2sls <- ivreg(`Log GDP per capita(PPP) in 1995` ~ `Average protection against expropriation risk 1985-1995` | 
                      `Log European settler mortality`, 
                    data = data)
summary(model_2sls)

model_2sls <- ivreg(`Log GDP per capita(PPP) in 1995` ~ `Average protection against expropriation risk 1985-1995`+ norm_latitude | 
                      `Log European settler mortality` + norm_latitude, 
                    data = data)
summary(model_2sls)

model_2sls <- ivreg(`Log GDP per capita(PPP) in 1995` ~ `Average protection against expropriation risk 1985-1995` + asia_dummy + africa_dummy + other_regions_dummy | 
                      `Log European settler mortality` + asia_dummy + africa_dummy + other_regions_dummy, 
                    data = data)
summary(model_2sls)

model_2sls <- ivreg(`Log GDP per capita(PPP) in 1995` ~ `Average protection against expropriation risk 1985-1995` + norm_latitude + asia_dummy + africa_dummy + other_regions_dummy | 
                      `Log European settler mortality` + norm_latitude + asia_dummy + africa_dummy + other_regions_dummy, 
                    data = data)
summary(model_2sls)

model_2sls <- ivreg(log_gdp_1995 ~ `Average protection against expropriation risk 1985-1995`| 
                      `Log European settler mortality`, 
                    data = data)
summary(model_2sls)






#regression OLS for GDP 2019
#base_sample
OLS_a <- lm(`gdp_2019` ~ 
              `Average protection against expropriation risk 1985-1995`, data = data)
summary(OLS_a)
OLS_b <- lm(`gdp_2019` ~ 
                    `Average protection against expropriation risk 1985-1995` + `norm_latitude`, data = data)
summary(OLS_b)
#base_sample_without_neo_europes
OLS_c <- lm(`gdp_2019` ~ 
                    `Average protection against expropriation risk 1985-1995` , data = base_sample_without_neo_europes)
summary(OLS_c)
OLS_d <- lm(`gdp_2019` ~ 
                    `Average protection against expropriation risk 1985-1995` + `norm_latitude`, data = base_sample_without_neo_europes)
summary(OLS_d)
#base_sample_without_africa
OLS_e <- lm(`gdp_2019` ~ 
              `Average protection against expropriation risk 1985-1995`, data = base_sample_without_africa)
summary(OLS_e)
OLS_f <- lm(`gdp_2019` ~ 
        `Average protection against expropriation risk 1985-1995` + `norm_latitude`, data = base_sample_without_africa)
summary(OLS_f)

#base_sample with dummies
OLS_g <- lm(`gdp_2019` ~ 
        `Average protection against expropriation risk 1985-1995` + `asia_dummy` + `africa_dummy` + `other_regions_dummy`, data = data)
summary(OLS_g)

OLS_h <- lm(`gdp_2019` ~ 
              `Average protection against expropriation risk 1985-1995` + `asia_dummy` + `africa_dummy` + `other_regions_dummy` + `norm_latitude` , data = data)
summary(OLS_h)

#base sample for log output for workers

model_ols <- lm(log_gdp_2019 ~ `Average protection against expropriation risk 1985-1995`, data = base_sample)
summary(model_ols)



#we have the same first stage regression for 1995

head(data)


#TWO STAGE REGRESSION FOR GDP 2019
# Second Stage - Modèle 2SLS
#base sample
model_2sls <- ivreg(gdp_2019 ~ `Average protection against expropriation risk 1985-1995`| 
                      `Log European settler mortality`,
                    data = data)
summary(model_2sls)

model_2sls <- ivreg(gdp_2019 ~ `Average protection against expropriation risk 1985-1995` + norm_latitude| 
                      `Log European settler mortality` + norm_latitude, 
                    data = data)
summary(model_2sls)

#base_sample_without_neo_europes
model_2sls <- ivreg(gdp_2019 ~ `Average protection against expropriation risk 1985-1995`| 
                      `Log European settler mortality`, 
                    data = base_sample_without_neo_europes)
summary(model_2sls)

model_2sls <- ivreg(gdp_2019 ~ `Average protection against expropriation risk 1985-1995`+ norm_latitude | 
                      `Log European settler mortality` + norm_latitude, 
                    data = base_sample_without_neo_europes)
summary(model_2sls)

#base_sample_without_africa
model_2sls <- ivreg(gdp_2019 ~ `Average protection against expropriation risk 1985-1995` | 
                      `Log European settler mortality`, 
                    data = base_sample_without_africa)
summary(model_2sls)

model_2sls <- ivreg(gdp_2019 ~ `Average protection against expropriation risk 1985-1995`+ norm_latitude | 
                      `Log European settler mortality` + norm_latitude, 
                    data = base_sample_without_africa)
summary(model_2sls)

#base sample with dummies
model_2sls <- ivreg(gdp_2019 ~ `Average protection against expropriation risk 1985-1995` + asia_dummy + africa_dummy + other_regions_dummy | 
                      `Log European settler mortality` + asia_dummy + africa_dummy + other_regions_dummy, 
                    data = base_sample)
summary(model_2sls)

model_2sls <- ivreg(gdp_2019 ~ `Average protection against expropriation risk 1985-1995` + norm_latitude + asia_dummy + africa_dummy + other_regions_dummy | 
                      `Log European settler mortality` + norm_latitude + asia_dummy + africa_dummy + other_regions_dummy, 
                    data = base_sample)
summary(model_2sls)
#two stage for log output per capita

model_2sls <- ivreg(log_gdp_2019 ~ `Average protection against expropriation risk 1985-1995`| 
                      `Log European settler mortality`, 
                    data = data)
summary(model_2sls)









