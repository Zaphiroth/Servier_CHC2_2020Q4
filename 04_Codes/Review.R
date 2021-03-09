# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  Servier CHC 2
# Purpose:      Review
# programmer:   Zhe Liu
# Date:         2021-03-03
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


##---- Uniform info ----
chk <- servier.delivery %>% 
  group_by(`Pack Code`) %>% 
  mutate(`Product Name` = last(`Product Name`), 
         Product = last(Product), 
         `Corporation Description` = last(`Corporation Description`), 
         Corporation_CN = last(Corporation_CN), 
         `NFCI Description` = first(na.omit(`NFCI Description`))) %>% 
  ungroup() %>% 
  distinct(`Pack Code`, MKT, `ATCIII Code`, `ATCIV Code`, `Category I`, `Category II`, 
           `Molecule composition Name`, `Product Name`, Product, Prod_CN_Name, 
           `Pack Description`, `Pack Form`, `Pack Strength`, `Pack Size`, 
           `NFCI Description`, `Corporation Description`, Corporation_CN, 
           `Manufacture Type`, `是否是原研`) %>% 
  add_count(`Pack Code`, MKT) %>% 
  filter(n > 1)

chk <- servier.delivery %>% 
  distinct(`Pack Code`, MKT, `ATCIII Code`, `ATCIV Code`, `Category I`, `Category II`, 
           `Molecule composition Name`, `Product Name`, Product, Prod_CN_Name, 
           `Pack Description`, `Pack Form`, `Pack Strength`, `Pack Size`, 
           `NFCI Description`, `Corporation Description`, Corporation_CN, 
           `Manufacture Type`, `是否是原研`, City, Date, VBP) %>% 
  add_count(`Pack Code`, MKT, City, Date) %>% 
  filter(n > 1)


##---- VBP pack ----
vbp.market <- vbp.info %>% 
  filter(sheet == 4, 
         molecule %in% c('METFORMIN', 'VILDAGLIPTIN', 'TRIMETAZIDINE', 'VALSARTAN', 'CAPTOPRIL')) %>% 
  mutate(prodid = stri_sub(packid, 1, 5))

result.pack.vbp <- servier.delivery %>% 
  distinct(city = City, molecule = `Molecule composition Name`, packid = `Pack Code`, flag_bid = 1)

result.prod.vbp <- servier.delivery %>% 
  distinct(city = City, molecule = `Molecule composition Name`, prodid = stri_sub(`Pack Code`, 1, 5), flag_prod = 1)

vbp.check <- vbp.market %>% 
  left_join(result.pack.vbp, by = c('city', 'molecule', 'packid')) %>% 
  left_join(result.prod.vbp, by = c('city', 'molecule', 'prodid')) %>% 
  mutate(`存在情况` = case_when(flag_bid == 1 & flag_prod == 1 ~ '有', 
                            is.na(flag_bid) & flag_prod == 1 ~ 'Pack没有，产品有', 
                            is.na(flag_bid) & is.na(flag_prod) ~ '没有', 
                            TRUE ~ NA_character_))

write.xlsx(vbp.check, '05_Internal_Review/Result_VBP_Check.xlsx')


##---- Check SOP -----
## CHPA
# chpa.format <- read.xlsx('05_Internal_Review/ims_chpa_to20Q4_fmt.xlsx')

servier.chpa <- chpa.format %>% 
  pivot_longer(cols = c(ends_with('UNIT'), ends_with('SU'), ends_with('RENMINBI')), 
               names_to = 'quarter', 
               values_to = 'value') %>% 
  separate(quarter, c('quarter', 'measure'), sep = '_') %>% 
  pivot_wider(id_cols = c(Pack_ID, ATC3_Code, Molecule_Desc, Prd_desc, 
                          Pck_Desc, Corp_Desc, quarter), 
              names_from = measure, 
              values_from = value) %>% 
  left_join(market.def, by = c('ATC3_Code' = 'atc3', 'Molecule_Desc' = 'molecule')) %>% 
  filter(!is.na(market), 
         UNIT > 0, RENMINBI > 0, 
         stri_sub(quarter, 1, 4) %in% c('2018', '2019', '2020')) %>% 
  select(Pack_ID, Date = quarter, ATC3 = ATC3_Code, MKT = market, 
         Molecule_Desc, Prod_Desc = Prd_desc, Pck_Desc, Corp_Desc, 
         Units = UNIT, DosageUnits = SU, Sales = RENMINBI) %>% 
  filter(!(ATC3 == 'V03B' & !(Prod_Desc %in% kTCM)))

write.xlsx(servier.chpa, '05_Internal_Review/Servier_CHC2_2018Q4_2020Q4_CHPA.xlsx')

## price
price.check <- servier.delivery %>% 
  filter(`Period Type` == 'QTR') %>% 
  mutate(price = round(`Value LC` / Units)) %>% 
  group_by(Channel, City, MKT, `Molecule composition Name`, `Product Name`, `Pack Code`) %>% 
  mutate(Sales = sum(`Value LC`, na.rm = TRUE)) %>% 
  ungroup() %>% 
  arrange(Channel, City, MKT, -Sales, Date) %>% 
  distinct(Channel, Date, City, MKT, `Molecule composition Name`, `Product Name`, `Pack Code`, price) %>% 
  pivot_wider(id_cols = c(Channel, City, MKT, `Molecule composition Name`, `Product Name`, `Pack Code`), 
              names_from = Date, 
              values_from = price)

write.xlsx(price.check, '05_Internal_Review/Servier_CHC2_2018Q1_2020Q4_Price_Check.xlsx')


