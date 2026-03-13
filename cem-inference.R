library(cem)
library(MatchIt)
library(tidyverse)
library(estimatr)

# assuming the data is loaded in as juris_comp (jurisdictional level data)

binary_comp <- juris_comp %>%
  mutate(q9c18 = ifelse(q9c18 == 1, 1, 0)) %>%
  select(q9c18, GEOID, WRLURI18, B01003_001E,
         B25001_001E,B25017_001E,B25018_001E,
         B25034_001E,B25035_001E,B25041_001E,
         B25056_001E,B25058_001E,B01001_001E,
         B09002_001E,B11007_001E,B19001_001E,
         B23025_003E,B19013_001E,B25077_001E,
         S1501_C02_015E) %>%
  na.omit()

t.test(B25001_001E ~ q9c18, data = binary_comp) #reject null, clearly different.

# population, B01003
# wrluri (maybe other wrluri?)
# education, S1501_C02_015, income, B19013
# home values, B25077; households over 65, B11007
# unemployment levels, B23025_003 B25077_001E +
m_out <- matchit(q9c18 ~ WRLURI18 + B01003_001E + 
                  B11007_001E + 
                  B23025_003E + B19013_001E +
                   S1501_C02_015E,
                 data = binary_comp, 
                 k2k = T, method = "cem", 
                 cutpoints = 8
                 ) #cutpoints matter, these are the bins
#distance = "logit"k2k = T, 

summary(m_out)
plot(summary(m_out), abs = T)


m_data <- match.data(m_out) #covariates

summary(m_data)

m_ate <- lm_robust(B25001_001E ~ q9c18, 
                   data = m_data,
                   weights = m_data$weights,
                   se_type = "HC0")
summary(m_ate)

# check for sparsity
# 
# sum(m_data$weights > 0)/nrow(binary_comp)
# 
# table(m_out$subclass)
# sum(m_data$subclass!="NULL") / nrow(binary_comp)
# 
# sum(1/m_data$weights^2)

# am I tripping or does it work like this? check with home values B25001_001E +
m_out_price <- matchit(q9c18 ~ WRLURI18 + B01003_001E + 
                   B11007_001E + 
                   B23025_003E + B19013_001E +
                    S1501_C02_015E,
                 data = binary_comp, 
                 method = "cem", 
                 k2k = T, cutpoints = 8
) #cutpoints matter, these are the bins
#distance = "logit"

summary(m_out_price)
plot(summary(m_out_price), abs = T)


m_data_price <- match.data(m_out_price) #covariates

summary(m_data_price)

m_ate_price <- lm_robust(B25077_001E ~ q9c18, 
                   data = m_data_price,
                   weights = m_data_price$weights,
                   se_type = "HC0")
summary(m_ate_price)



# filter out the WRLURI < 0 for the below avg regulation
less_reg <- binary_comp %>%
  filter(WRLURI18 < 0)

# B25077_001E +
m_out_less <- matchit(q9c18 ~ WRLURI18 + B01003_001E + 
                   B11007_001E + 
                   B23025_003E + B19013_001E +
                    S1501_C02_015E,
                 data = less_reg, 
                 method = "cem", 
                 k2k = T, cutpoints = 8
) #cutpoints matter, these are the bins
#distance = "logit"

summary(m_out_less)
plot(summary(m_out_less), abs = T)


m_data_less <- match.data(m_out_less) #covariates

summary(m_data_less)

m_ate_less <- lm_robust(B25001_001E ~ q9c18, 
                   data = m_data_less,
                   weights = m_data_less$weights,
                   se_type = "HC0")
summary(m_ate_less)
plot(summary(m_ate_less))

# price for below avg reg
m_out_price_less <- matchit(q9c18 ~ WRLURI18 + B01003_001E + 
                         B11007_001E + 
                         B23025_003E + B19013_001E +
                         S1501_C02_015E,
                       data = less_reg, 
                       method = "cem", 
                       k2k = T, cutpoints = 8
) 

summary(m_out_price_less)
plot(summary(m_out_price_less), abs = T)


m_data_price_less <- match.data(m_out_price_less) #covariates

summary(m_data_price)

m_ate_price_less <- lm_robust(B25077_001E ~ q9c18, 
                         data = m_data_price_less,
                         weights = m_data_price_less$weights,
                         se_type = "HC0")
summary(m_ate_price_less)



