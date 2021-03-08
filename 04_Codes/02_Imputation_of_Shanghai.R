# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  Servier CHC 2
# Purpose:      Imputation of Shanghai
# programmer:   Zhe Liu
# Date:         2021-02-26
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


##---- Shanghai data ----
## pack info
chpa.info <- read.xlsx('02_Inputs/ims_chpa_to20Q4.xlsx', cols = 1:21, startRow = 4) %>%  
  distinct(corp = Corp_Desc, type = MNF_TYPE, atc3 = ATC3_Code, atc4 = ATC4_Code,  
           molecule = Molecule_Desc, product = Prd_desc, pack = Pck_Desc,  
           packid = Pack_ID)

sh.info <- chpa.info %>% 
  distinct(prodid = stri_sub(packid, 1, 5), 
           atc3, 
           molecule)

## Shanghai
raw.sh1 <- read.xlsx('02_Inputs/data/上海_2017.xlsx')
raw.sh2 <- read.xlsx('02_Inputs/data/上海_2018.xlsx')

raw.sh <- bind_rows(raw.sh1, raw.sh2) %>% 
  mutate(quarter_m = stri_sub(Date, 5, 6)) %>% 
  distinct(year = stri_sub(Date, 1, 4), 
           quarter = ifelse(quarter_m %in% c('01', '02', '03'), 
                            stri_paste(year, 'Q1'), 
                            ifelse(quarter_m %in% c('04', '05', '06'), 
                                   stri_paste(year, 'Q2'), 
                                   ifelse(quarter_m %in% c('07', '08', '09'), 
                                          stri_paste(year, 'Q3'), 
                                          ifelse(quarter_m %in% c('10', '11', '12'), 
                                                 stri_paste(year, 'Q4'), 
                                                 year)))), 
           date = as.character(Date), 
           province = '上海', 
           city = '上海', 
           pchc = PCHC, 
           packid = stri_pad_left(pfc, 7, 0), 
           price = value / unit, 
           units = unit, 
           sales = value) %>% 
  mutate(pchc = case_when(pchc == 'PCHC06729' ~ 'PCHC06728', 
                          pchc == 'PCHC06622' ~ 'PCHC06620', 
                          pchc == 'PCHC06645' ~ 'PCHC06644', 
                          pchc == 'PCHC06722' ~ 'PCHC06721', 
                          pchc == 'PCHC06840' ~ 'PCHC06839', 
                          TRUE ~ pchc), 
         prodid = stri_sub(packid, 1, 5)) %>% 
  left_join(pchc.mapping4, by = c('province', 'city', 'pchc')) %>% 
  filter(pchc != '#N/A') %>% 
  left_join(sh.info, by = 'prodid') %>% 
  left_join(market.def, by = c('atc3', 'molecule')) %>% 
  filter(!is.na(market)) %>% 
  mutate(packid = if_else(stri_sub(packid, 1, 5) == '47775', 
                          stri_paste('58906', stri_sub(packid, 6, 7)), 
                          packid), 
         packid = if_else(stri_sub(packid, 1, 5) == '06470', 
                          stri_paste('64895', stri_sub(packid, 6, 7)), 
                          packid)) %>% 
  filter(units > 0, sales > 0) %>% 
  select(year, date, quarter, province, city, district, pchc, market, packid, units, sales)

## Beijing
sh.bj1 <- filter(raw.total, province == '北京')
sh.bj2 <- read_feather('02_Inputs/Beijing_CHC_2017Q1_2020Q3.feather')


##---- Growth ----
## model growth
sh.knn <- read.xlsx('02_Inputs/Shanghai_KNN_Result.xlsx')

sh.model.growth <- bind_rows(sh.bj1, sh.bj2) %>% 
  distinct(year, quarter, date, province, city, district, pchc, packid, sales, units) %>% 
  group_by(knn_pchc = pchc, packid, year, date, quarter) %>% 
  summarise(sales = sum(sales, na.rm = TRUE), 
            units = sum(units, na.rm = TRUE)) %>% 
  ungroup() %>% 
  inner_join(sh.knn, by = 'knn_pchc') %>% 
  group_by(pchc, packid, year, date, quarter) %>% 
  summarise(sales = sum(sales * knn_weight, na.rm = TRUE), 
            units = sum(units * knn_weight, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(q = stri_sub(quarter, 5, 6), 
         month = stri_sub(date, 5, 6)) %>% 
  pivot_wider(id_cols = c(pchc, packid, q, month), 
              names_from = year, 
              values_from = c(sales, units), 
              values_fill = 0) %>% 
  mutate(year = '2017', 
         quarter = stri_paste(year, q), 
         date = stri_paste(year, month), 
         growth_sales = sales_2020 / sales_2017, 
         growth_units = units_2020 / units_2017) %>% 
  filter(quarter == '2017Q4', 
         !is.na(growth_sales), !is.infinite(growth_sales), 
         !is.na(growth_units), !is.infinite(growth_units)) %>% 
  select(year, quarter, date, pchc, packid, growth_sales, growth_units)

## CHPA growth
chpa.format <- read.xlsx('02_Inputs/ims_chpa_to20Q4_fmt.xlsx')

chpa.growth <- chpa.format %>% 
  mutate(growth_sales_sup = `2020Q4_RENMINBI` / `2017Q4_RENMINBI`, 
         growth_units_sup = `2020Q4_UNIT` / `2017Q4_UNIT`, 
         year = '2017', 
         quarter = '2017Q4') %>% 
  filter(!is.na(growth_sales_sup), !is.infinite(growth_sales_sup), 
         !is.na(growth_units_sup), !is.infinite(growth_units_sup)) %>% 
  select(year, quarter, packid = Pack_ID, growth_sales_sup, growth_units_sup)


##---- Prediction ----
## predict 2020Q4
set.seed(2020)
sh.predict.sales <- raw.sh %>% 
  filter(quarter %in% c('2017Q4')) %>% 
  left_join(sh.model.growth, by = c('year', 'quarter', 'date', 'pchc', 'packid')) %>% 
  left_join(chpa.growth, by = c('year', 'quarter', 'packid')) %>% 
  mutate(factor = runif(n(), -0.05, 0.05), 
         growth_sales = if_else(is.na(growth_sales), growth_sales_sup + factor, growth_sales), 
         growth_sales = if_else(growth_sales < 0, 0, growth_sales), 
         growth_sales = if_else(is.na(growth_sales), 1, growth_sales), 
         growth_sales = if_else(growth_sales > 3, 3, growth_sales), 
         growth_sales = if_else(growth_sales > quantile(growth_sales, 0.9), 
                                mean(growth_sales[growth_sales >= quantile(growth_sales, 0.25) & 
                                                    growth_sales <= quantile(growth_sales, 0.75)]), 
                                growth_sales), 
         growth_units = if_else(is.na(growth_units), growth_units_sup + factor, growth_units), 
         growth_units = if_else(growth_units < 0, 0, growth_units), 
         growth_units = if_else(is.na(growth_units), 1, growth_units), 
         growth_units = if_else(growth_units > 3, 3, growth_units), 
         growth_units = if_else(growth_units > quantile(growth_units, 0.9), 
                                mean(growth_units[growth_units >= quantile(growth_units, 0.25) & 
                                                    growth_units <= quantile(growth_units, 0.75)]), 
                                growth_units)) %>% 
  mutate(units_imp = units * growth_units, 
         sales_imp = sales * growth_sales, 
         year = '2020', 
         quarter = gsub('2017', '2020', quarter), 
         date = gsub('2017', '2020', date), 
         flag = 1) %>% 
  select(year, date, quarter, province, city, district, pchc, market, packid, 
         units = units_imp, sales = sales_imp, flag)


##---- Result ----
imp.sh <- sh.predict.sales %>% 
  group_by(year, date, quarter, province, city, district, pchc, market, packid, flag) %>% 
  summarise(units = sum(units, na.rm = TRUE),
            sales = sum(sales, na.rm = TRUE)) %>% 
  ungroup() %>% 
  filter(units > 0, sales > 0) %>% 
  select(year, date, quarter, province, city, district, pchc, market, packid, 
         units, sales, flag)
