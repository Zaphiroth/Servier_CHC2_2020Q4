# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  Servier CHC 2
# Purpose:      Check
# programmer:   Zhe Liu
# Date:         2021-03-03
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


##---- Uniform info ----
chk <- servier.history %>% 
  mutate(`Pack Code` = stri_pad_left(`Pack Code`, 7, 0)) %>% 
  bind_rows(servier.qtr) %>% 
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

chk <- servier.result %>% 
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

result.pack.vbp <- servier.result %>% 
  distinct(city = City, molecule = `Molecule composition Name`, packid = `Pack Code`, flag_bid = 1)

result.prod.vbp <- servier.result %>% 
  distinct(city = City, molecule = `Molecule composition Name`, prodid = stri_sub(`Pack Code`, 1, 5), flag_prod = 1)

vbp.check <- vbp.market %>% 
  left_join(result.pack.vbp, by = c('city', 'molecule', 'packid')) %>% 
  left_join(result.prod.vbp, by = c('city', 'molecule', 'prodid')) %>% 
  mutate(`存在情况` = case_when(flag_bid == 1 & flag_prod == 1 ~ '有', 
                            is.na(flag_bid) & flag_prod == 1 ~ 'Pack没有，产品有', 
                            is.na(flag_bid) & is.na(flag_prod) ~ '没有', 
                            TRUE ~ NA_character_))

write.xlsx(vbp.check, '05_Internal_Review/Result_VBP_Check.xlsx')

