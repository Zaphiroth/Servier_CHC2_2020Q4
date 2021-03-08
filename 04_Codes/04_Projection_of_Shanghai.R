# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  Servier CHC 2
# Purpose:      Projection of Shanghai
# programmer:   Zhe Liu
# Date:         2021-03-08
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

## history
servier.history <- read_xlsx('06_Deliveries/Servier_CHC2_2018Q4_2020Q3_20210114.xlsx')

## Shanghai
history.sh <- servier.history %>% 
  filter(`Period Type` == 'QTR', 
         City == '上海', 
         Date == '2020Q3') %>% 
  mutate(`Pack Code` = stri_pad_left(`Pack Code`, 7, 0))

## Beijing
sh.bj1 <- servier.history %>% 
  filter(`Period Type` == 'QTR', 
         Channel == 'CHC', 
         City == '北京', 
         Date == '2020Q3') %>% 
  mutate(`Pack Code` = stri_pad_left(`Pack Code`, 7, 0))

sh.bj2 <- servier.qtr %>% 
  filter(`Period Type` == 'QTR', 
         Channel == 'CHC', 
         City == '北京', 
         Date == '2020Q4')


##---- Growth ----
## KNN model
# sh.knn <- read.xlsx('02_Inputs/Shanghai_KNN_Result.xlsx')

## Beijing growth
bj.growth <- bind_rows(sh.bj1, sh.bj2) %>% 
  group_by(MKT, `Category II`, `Pack Code`) %>% 
  arrange(Date) %>% 
  summarise(growth_sales = lead(`Value LC`) / `Value LC`, 
            growth_units = lead(Units) / Units) %>% 
  ungroup() %>% 
  filter(!is.na(growth_sales))

## CHPA growth
chpa.format <- read.xlsx('02_Inputs/ims_chpa_to20Q4_fmt.xlsx')

chpa.growth <- chpa.format %>% 
  mutate(growth_sales_sup = `2020Q4_RENMINBI` / `2020Q3_RENMINBI`, 
         growth_units_sup = `2020Q4_UNIT` / `2020Q3_UNIT`, 
         quarter = '2020Q3') %>% 
  filter(!is.na(growth_sales_sup), !is.infinite(growth_sales_sup), 
         !is.na(growth_units_sup), !is.infinite(growth_units_sup)) %>% 
  select(quarter, packid = Pack_ID, growth_sales_sup, growth_units_sup)


##---- Result ----
sh.qtr <- history.sh %>% 
  left_join(bj.growth, by = c('MKT', 'Category II', 'Pack Code')) %>% 
  left_join(chpa.growth, by = c('Date' = 'quarter', 'Pack Code' = 'packid')) %>% 
  mutate(growth_sales = if_else(is.na(growth_sales), growth_sales_sup, growth_sales), 
         growth_units = if_else(is.na(growth_units), growth_units_sup, growth_units)) %>% 
  mutate(Quarter = 'Q4', 
         Date = '2020Q4', 
         `Value LC` = `Value LC` * growth_sales, 
         `Value LC_Raw` = `Value LC_Raw` * growth_sales, 
         Units = Units * growth_units, 
         Units_Raw = Units_Raw * growth_units, 
         `Counting Units` = `Counting Units` * growth_units, 
         `Counting Units_Raw` = `Counting Units_Raw` * growth_units) %>% 
  filter(`Value LC` > 0, Units > 0) %>% 
  select(-starts_with('growth_'))
