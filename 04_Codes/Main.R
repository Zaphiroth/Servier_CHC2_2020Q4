# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  Servier CHC 2
# Purpose:      Main
# programmer:   Zhe Liu
# Date:         2021-03-01
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


##---- Total sample ----
imp.total <- raw.total %>% 
  mutate(flag = 0) %>% 
  bind_rows(imp.fj)

write_feather(imp.total, '03_Outputs/Servier_CHC2_Imp.feather')


##---- Universe info ----
## target city
kTargetCity <- c('北京', '上海', '杭州', '南京')

## PCHC
pchc.universe <- read.xlsx("02_Inputs/2020_PCHC_Universe更新维护.xlsx", 
                           sheet = "2020 CHC universe", cols = 1:19)

pchc.universe.m <- pchc.universe %>% 
  distinct(province = gsub('省|市', '', `省`), 
           city = gsub('市', '', `地级市`), 
           district = `区[县/县级市]`, 
           pchc = `新版PCHC_Code`, 
           est = `其中：西药药品收入（千元）`) %>% 
  filter(est > 0) %>% 
  group_by(pchc) %>% 
  summarise(province = first(na.omit(province)), 
            city = first(na.omit(city)), 
            district = first(na.omit(district)), 
            est = sum(est, na.rm = TRUE)) %>% 
  ungroup()

hospital.universe <- bind_rows(imp.total, pchc.universe.m) %>% 
  group_by(pchc) %>% 
  summarise(province = first(province),
            city = first(na.omit(city)),
            district = first(na.omit(district)), 
            est = first(na.omit(est))) %>% 
  ungroup() %>% 
  filter(!is.na(province), !is.na(city), !is.na(district), !is.na(est)) %>% 
  mutate(flag_sample = if_else(pchc %in% unique(imp.total$pchc), 1, 0))

## city tier
city.tier <- read.xlsx("02_Inputs/pchc_city_tier.xlsx") %>% 
  group_by(city) %>% 
  mutate(tier = if_else(is.na(city_tier), first(na.omit(city_tier)), city_tier)) %>% 
  ungroup() %>% 
  mutate(tier = if_else(is.na(tier), 5, tier)) %>% 
  distinct(city, tier)


##---- Run Project ----
source('04_Codes/functions/ProjectSample.R', encoding = 'UTF-8')
source('04_Codes/functions/ProjectNation.R', encoding = 'UTF-8')
source('04_Codes/functions/UpdatePrice.R', encoding = 'UTF-8')

proj.sample <- ProjectSample(raw.total = imp.total, 
                             pchc.universe = hospital.universe)

proj.nation <- ProjectNation(proj.sample.total = proj.sample, 
                             pchc.universe = hospital.universe, 
                             city.tier = city.tier)

proj.cs <- proj.nation %>% 
  # mutate(sales = if_else(province == '上海', sales * 0.5, sales), 
  #        units = if_else(province == '上海', units * 0.5, units)) %>% 
  mutate(channel = 'CHC') %>% 
  bind_rows(bj.chs)

proj.price <- UpdatePrice(proj.nation = proj.cs, 
                          raw.total = imp.total)

write_feather(proj.price, '03_Outputs/Servier_CHC2_Proj.feather')


##---- Format info ----
## CHPA
chpa.info <- read.xlsx('02_Inputs/ims_chpa_to20Q4.xlsx', cols = 1:21, startRow = 4) %>%  
  distinct(corp = Corp_Desc, type = MNF_TYPE, atc3 = ATC3_Code, atc4 = ATC4_Code,  
           molecule = Molecule_Desc, product = Prd_desc, pack = Pck_Desc,  
           packid = Pack_ID)

master.info <- read.xlsx('02_Inputs/Product standardization master data-A-S-1211.xlsx') %>% 
  mutate(PACK_ID = gsub('禁用', '', PACK_ID)) %>% 
  distinct(corp = CORP_NAME_EN, type = MNF_TYPE, atc3 = ATC3_CODE, atc4 = ATC4_CODE, 
           atc3_cn = ATC3, molecule = MOLE_NAME_EN, molecule_cn = MOLE_NAME_CH, 
           product = PROD_DESC, product_cn = PROD_NAME_CH, pack = PCK_DESC, 
           route = NFC1_NAME_CH, packid = PACK_ID)

corp.info <- read.xlsx('02_Inputs/MANU.xlsx')

prod.cn <- read.xlsx('02_Inputs/Product standardization master data-A-S-0106_updated.xlsx') %>% 
  distinct(PACK_ID, PROD_NAME_CH, NFC1_NAME_CH)

sup.info <- read_xlsx('02_Inputs/Unmatched_SKU_M.xlsx') %>% 
  distinct(packid = `Pack Code`, product_cn = Prod_CN_Name, route = `NFCI Description`)

std.info <- bind_rows(chpa.info, master.info, sup.info) %>% 
  left_join(corp.info, by = c('corp' = 'name')) %>% 
  group_by(atc3) %>% 
  mutate(atc3_cn = first(na.omit(atc3_cn))) %>% 
  ungroup() %>% 
  group_by(molecule) %>% 
  mutate(molecule_cn = first(na.omit(molecule_cn))) %>% 
  ungroup() %>% 
  group_by(product) %>% 
  mutate(product_cn = first(na.omit(product_cn))) %>% 
  ungroup() %>% 
  group_by(packid) %>% 
  summarise(corp = first(na.omit(corp)), 
            corp_cn = first(na.omit(namec)), 
            type = first(na.omit(type)), 
            atc3 = first(na.omit(atc3)), 
            atc4 = first(na.omit(atc4)), 
            atc3_cn = first(na.omit(atc3_cn)), 
            molecule = first(na.omit(molecule)), 
            molecule_cn =first(na.omit(molecule_cn)), 
            product = first(na.omit(product)), 
            product_cn = first(na.omit(product_cn)), 
            pack = first(na.omit(pack)), 
            route = first(na.omit(route))) %>% 
  ungroup() %>% 
  left_join(prod.cn, by = c('packid' = 'PACK_ID')) %>% 
  mutate(product_cn = if_else(is.na(product_cn), PROD_NAME_CH, product_cn), 
         route = if_else(is.na(route), NFC1_NAME_CH, route)) %>% 
  select(-PROD_NAME_CH, -NFC1_NAME_CH)

## VBP info
vbp.info.list <- map(1:4, 
                     function(x) {
                       print(x)
                       read.xlsx('02_Inputs/带量采购中标结果汇总（1-3批）_20201222.xlsx', 
                                 sheet = x) %>% 
                         mutate(`中标时间` = as.character(as.Date(as.numeric(`中标时间`), origin = '1899-12-30')), 
                                `执行日期` = as.character(as.Date(as.numeric(`执行日期`), origin = '1899-12-30')), 
                                sheet = x)
                     })

vbp.nat <- bind_rows(vbp.info.list) %>% 
  mutate(PACK_ID = stri_pad_left(as.numeric(PACK_ID), 7, 0)) %>% 
  filter(!is.na(PACK_ID)) %>% 
  mutate(city = 'National') %>% 
  distinct(city, molecule_cn = `通用名`, packid = PACK_ID, sheet)

vbp.city <- bind_rows(vbp.info.list) %>% 
  mutate(PACK_ID = stri_pad_left(as.numeric(PACK_ID), 7, 0)) %>% 
  filter(!is.na(PACK_ID)) %>% 
  filter(`省份` %in% c('北京', '上海', '浙江', '江苏')) %>% 
  mutate(`城市` = case_when(`省份` == '北京' ~ '北京', 
                          `省份` == '上海' ~ '上海', 
                          `省份` == '浙江' ~ '杭州', 
                          `省份` == '江苏' ~ '南京', 
                          TRUE ~ `城市`)) %>% 
  distinct(city = `城市`, molecule_cn = `通用名`, packid = PACK_ID, sheet)

vbp.info <- bind_rows(vbp.nat, vbp.city) %>% 
  mutate(`是否是中标品种` = '中标产品', 
         prodid = stri_sub(packid, 1, 5)) %>% 
  left_join(distinct(chpa.info, 
                     prodid = stri_sub(packid, 1, 5), 
                     molecule), 
            by = 'prodid') %>% 
  filter(!is.na(molecule)) %>% 
  distinct(city, molecule, packid, `是否是中标品种`, sheet)

vbp.mole <- vbp.info %>% 
  mutate(`是否进入带量采购` = case_when(sheet == 1 ~ '4+7', 
                                sheet == 2 ~ '扩围', 
                                sheet == 3 ~ '第二批', 
                                sheet == 4 ~ '第三批', 
                                TRUE ~ NA_character_), 
         `是否进入带量采购` = if_else(`是否进入带量采购` == '扩围' & city == 'National', 
                              '4+7', `是否进入带量采购`)) %>% 
  filter(molecule != 'TRIMETAZIDINE') %>% 
  distinct(city, molecule, `是否进入带量采购`)

## city EN
city.en <- read.xlsx("02_Inputs/CityEN.xlsx")

## TCM
kTCM <- c('PU JI              S4I', 
          'GANG TAI           SYO', 
          'FU ZHI QING        GGS', 
          'MUSK HEMORRHOIDS   HMA', 
          'SHEXIANG ZHICHUAN  HMA', 
          'JIN XUAN ZHI KE    HMA', 
          'FU FANG JING JIE   RGZ', 
          'ZHI JI             G.Z', 
          'NIUHUANG ZHIQING   HJD', 
          'JIUHUA ZHICHUANG   JRJ', 
          'HUAIYU QINGRE ZHIX HMA', 
          'XIAO ZHI           LZF')


##---- Run format ----
## format
source('04_Codes/functions/FormatServier.R', encoding = 'UTF-8')

servier.qtr <- FormatServier(proj.price = proj.price, 
                             std.info = std.info, 
                             vbp.info = vbp.info, 
                             vbp.mole = vbp.mole, 
                             city.en = city.en, 
                             target.city = kTargetCity, 
                             tcm = kTCM)

## history
servier.history <- read_xlsx('06_Deliveries/Servier_CHC2_2018Q4_2020Q3_20210114.xlsx')

## Shanghai
source('04_Codes/functions/ProjectShanghai.R', encoding = 'UTF-8')

chpa.format <- read.xlsx('02_Inputs/ims_chpa_to20Q4_fmt.xlsx')

sh.qtr <- ProjectShanghai(servier.qtr = servier.qtr, 
                          servier.history = servier.history, 
                          chpa.format = chpa.format)

## TCM Others
growth.others <- servier.history %>% 
  filter(`Period Type` == 'QTR', 
         Date %in% c('2020Q2', '2020Q3'), 
         `Molecule composition Name` == 'TCM Others') %>% 
  mutate(`Pack Code` = stri_pad_left(`Pack Code`, 7, 0)) %>% 
  group_by(Channel, City, `Category II`, `Molecule composition Name`) %>% 
  arrange(Date) %>% 
  summarise(growth_sales = lead(`Value LC`) / `Value LC`, 
            growth_units = lead(Units) / Units) %>% 
  ungroup() %>% 
  filter(!is.na(growth_sales))

others.qtr <- servier.history %>% 
  filter(`Period Type` == 'QTR', 
         Date == '2020Q3', 
         `Molecule composition Name` == 'TCM Others') %>% 
  mutate(`Pack Code` = stri_pad_left(`Pack Code`, 7, 0)) %>% 
  left_join(growth.others, by = c('Channel', 'City', 'Category II', 
                                  'Molecule composition Name')) %>% 
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

## QTR
servier.qtr <- servier.qtr %>% 
  filter(City != '上海') %>% 
  bind_rows(sh.qtr, others.qtr)

## MAT
servier.mat <- servier.history %>% 
  filter(`Period Type` == 'QTR', 
         Date %in% c('2020Q3', '2020Q2', '2020Q1')) %>% 
  mutate(`Pack Code` = stri_pad_left(`Pack Code`, 7, 0)) %>% 
  bind_rows(servier.qtr) %>% 
  group_by(City, `Pack Code`) %>% 
  mutate(`Product Name` = last(`Product Name`), 
         Product = last(Product), 
         `Corporation Description` = last(`Corporation Description`), 
         Corporation_CN = last(Corporation_CN), 
         `NFCI Description` = first(na.omit(`NFCI Description`)), 
         VBP = last(na.omit(VBP))) %>% 
  ungroup() %>% 
  group_by(Channel, City_EN, City, `Period Type` = 'MAT', Year = '2020', 
           Quarter = 'Q4', Date = '2020Q4', `ATCIII Code`, `ATCIV Code`, MKT, 
           `Category I`, `Category II`, `Molecule composition Name`, `Product Name`, 
           Product, Prod_CN_Name, `Pack Code`, `Pack Description`, `Pack Form`, 
           `Pack Strength`, `Pack Size`, `NFCI Description`, `Corporation Description`, 
           Corporation_CN, `Manufacture Type`, VBP, `是否是中标品种`, `是否是原研`) %>% 
  summarise(`Value LC` = sum(`Value LC`, na.rm = TRUE), 
            Units = sum(Units, na.rm = TRUE), 
            `Counting Units` = sum(`Counting Units`, na.rm = TRUE), 
            `Value LC_Raw` = sum(`Value LC_Raw`, na.rm = TRUE), 
            Units_Raw = sum(Units_Raw, na.rm = TRUE), 
            `Counting Units_Raw` = sum(`Counting Units_Raw`, na.rm = TRUE)) %>% 
  ungroup()

## result
servier.delivery <- servier.history %>% 
  mutate(`Pack Code` = stri_pad_left(`Pack Code`, 7, 0)) %>% 
  bind_rows(servier.qtr, servier.mat) %>% 
  group_by(`Pack Code`) %>% 
  mutate(`Product Name` = last(`Product Name`), 
         Product = last(Product), 
         Prod_CN_Name = last(na.omit(Prod_CN_Name)), 
         `Corporation Description` = last(`Corporation Description`), 
         Corporation_CN = last(Corporation_CN), 
         `NFCI Description` = first(na.omit(`NFCI Description`))) %>% 
  ungroup() %>% 
  select(Channel, City_EN, City, `Period Type`, Year, Quarter, Date, `ATCIII Code`, 
         `ATCIV Code`, MKT, `Category I`, `Category II`, `Molecule composition Name`, 
         `Product Name`, Product, Prod_CN_Name, `Pack Code`, `Pack Description`, 
         `Pack Form`, `Pack Strength`, `Pack Size`, `NFCI Description`, 
         `Corporation Description`, Corporation_CN, `Manufacture Type`, VBP, 
         `是否是中标品种`, `是否是原研`, `Value LC`, Units, `Counting Units`, 
         `Value LC_Raw`, Units_Raw, `Counting Units_Raw`) %>% 
  arrange(Channel, desc(`Period Type`), Date, City_EN, MKT, `Pack Code`)

write.xlsx(servier.delivery, '03_Outputs/Servier_CHC2_2018Q4_2020Q4.xlsx')
