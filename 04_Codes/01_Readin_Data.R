# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  Servier CHC 2
# Purpose:      Readin
# programmer:   Zhe Liu
# Date:         2021-02-25
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


##---- Universe info ----
## PCHC info
pchc.mapping <- read.xlsx('02_Inputs/Universe_PCHCCode_20210225.xlsx', sheet = 'PCHC')

pchc.mapping1 <- pchc.mapping %>% 
  filter(!is.na(`单位名称`), !is.na(PCHC_Code)) %>% 
  group_by(province = `省`, city = `地级市`, district = `区[县/县级市】`, hospital = `单位名称`) %>% 
  summarise(pchc = first(PCHC_Code)) %>% 
  ungroup()

pchc.mapping2 <- pchc.mapping %>% 
  filter(!is.na(ZS_Servier.name), !is.na(PCHC_Code)) %>% 
  group_by(province = `省`, city = `地级市`, district = `区[县/县级市】`, hospital = `ZS_Servier.name`) %>% 
  summarise(pchc = first(PCHC_Code)) %>% 
  ungroup()

pchc.mapping3 <- bind_rows(pchc.mapping1, pchc.mapping2) %>% 
  distinct(province, city, district, hospital, pchc)

pchc.mapping4 <- pchc.mapping3 %>% 
  group_by(pchc) %>% 
  summarise(province = first(na.omit(province)),
            city = first(na.omit(city)),
            district = first(na.omit(district))) %>% 
  ungroup()

## market definition
market.def <- read_xlsx('02_Inputs/市场分子式明细_chk_20201127.xlsx') %>% 
  filter(!(ATCIII.Code %in% c('A10C', 'A10D'))) %>% 
  distinct(atc3 = ATCIII.Code, molecule = Molecule.Composition.Name, market = TC)


##---- Raw data ----
## Servier
raw.servier <- read_csv('02_Inputs/data/Servier_Pfizer_ahbjjssd20Q4_zj20Q3Q4_packid_moleinfo.csv', 
                        locale = locale(encoding = 'GB18030'))

raw.data <- raw.servier %>% 
  filter(Project == 'Servier', 
         Quarter == '2020Q4', 
         !(Province %in% c('北京市'))) %>% 
  distinct(year = as.character(Year), 
           quarter = Quarter, 
           date = gsub('/', '', Month), 
           province = gsub('省|市', '', Province), 
           city = if_else(City == '市辖区', '北京', gsub('市', '', City)), 
           district = County, 
           hospital = Hospital_Name, 
           atc3 = stri_sub(ATC4_Code, 1, 4), 
           molecule = Molecule_Desc, 
           packid = stri_pad_left(packcode, 7, 0), 
           units = if_else(is.na(Volume), Value / Price, Volume), 
           sales = Value) %>% 
  left_join(pchc.mapping3, by = c('province', 'city', 'district', 'hospital')) %>% 
  filter(!is.na(pchc)) %>% 
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

## Fujian
raw.fj1 <- read_csv('02_Inputs/data/Servier_Pfizer_fj20Q3Q4_packid_moleinfo.csv', 
                    locale = locale(encoding = 'GB18030'))

raw.fj <- raw.fj1 %>% 
  filter(Project == 'Servier', 
         Quarter == '2020Q4', 
         !is.na(packcode)) %>% 
  distinct(year = as.character(Year), 
           quarter = Quarter, 
           date = as.character(Month), 
           province = gsub('省|市', '', Province), 
           city = if_else(City == '市辖区', '北京', gsub('市', '', City)), 
           district = County, 
           hospital = Hospital_Name, 
           atc3 = stri_sub(ATC4_Code, 1, 4), 
           molecule = Molecule_Desc, 
           packid = stri_pad_left(packcode, 7, 0), 
           units = if_else(is.na(Volume), Value / Price, Volume), 
           sales = Value) %>% 
  left_join(pchc.mapping3, by = c('province', 'city', 'district', 'hospital')) %>% 
  filter(!is.na(pchc)) %>% 
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

## Guangzhou
raw.gz1 <- read.xlsx('02_Inputs/data/gz_广东省_2020Q4_packid_moleinfo.xlsx')

raw.gz <- raw.gz1 %>% 
  distinct(year = as.character(Year), 
           quarter = Quarter, 
           date = as.character(Month), 
           province = '广东', 
           city = '广州', 
           hospital = Hospital_Name, 
           atc3 = stri_sub(ATC4_Code, 1, 4), 
           molecule = Molecule_Desc, 
           packid = stri_pad_left(packcode, 7, 0), 
           price = Price, 
           units = if_else(is.na(Volume), Value / Price, Volume), 
           sales = Value) %>% 
  left_join(pchc.mapping3, by = c('province', 'city', 'hospital')) %>% 
  filter(!is.na(pchc)) %>% 
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

## Venous
raw.venous1 <- read_csv('02_Inputs/data/Servier_ahbjjssdzj_17181920_packid_moleinfo.csv', 
                        locale = locale(encoding = 'GB18030'))

raw.venous <- raw.venous1 %>% 
  filter(Quarter == '2020Q4', 
         !is.na(packcode), 
         !(Province %in% c('北京市'))) %>% 
  distinct(year = as.character(Year), 
           quarter = Quarter, 
           date = gsub('/', '', Month), 
           province = gsub('省|市', '', Province), 
           city = if_else(City == '市辖区', '北京', gsub('市', '', City)), 
           district = County, 
           hospital = Hospital_Name, 
           atc3 = stri_sub(ATC4_Code, 1, 4), 
           molecule = Molecule_Desc, 
           packid = stri_pad_left(packcode, 7, 0), 
           units = if_else(is.na(Volume), Value / Price, Volume), 
           sales = Value) %>% 
  left_join(pchc.mapping3, by = c('province', 'city', 'district', 'hospital')) %>% 
  filter(!is.na(pchc)) %>% 
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
raw.bj1 <- read_csv('02_Inputs/data/【法柏】20Q4北京交付表修-2103_Formatted_packid_moleinfo.csv', 
                    locale = locale(encoding = 'GB18030'))

raw.bj <- raw.bj1 %>% 
  filter(Project %in% c('Servier', 'Servier\\痔疮静脉市场'), 
         Quarter == '2020Q4') %>% 
  distinct(year = as.character(Year), 
           quarter = Quarter, 
           date = gsub('/', '', Month), 
           province = gsub('省|市', '', Province), 
           city = if_else(City == '市辖区', '北京', gsub('市', '', City)), 
           district = County, 
           hospital = Hospital_Name, 
           atc3 = stri_sub(ATC4_Code, 1, 4), 
           molecule = Molecule_Desc, 
           packid = stri_pad_left(packcode, 7, 0), 
           units = if_else(is.na(Volume), Value / Price, Volume), 
           sales = Value) %>% 
  left_join(pchc.mapping3, by = c('province', 'city', 'district', 'hospital')) %>% 
  filter(!is.na(pchc)) %>% 
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

## check
# chk <- raw.data %>% 
#   filter(is.na(pchc), 
#          grepl('中心', hospital), 
#          grepl('社区', hospital), 
#          !grepl('卫生院|卫生室|卫生站|服务站|社区站|医院', hospital)) %>% 
#   distinct(province, city, district, hospital) %>% 
#   arrange(province, city, district, hospital)


##---- Raw total ----
raw.total <- bind_rows(raw.data, raw.fj, raw.venous, raw.bj, raw.gz) %>% 
  group_by(pchc) %>% 
  mutate(province = first(na.omit(province)), 
         city = first(na.omit(city)), 
         district = first(na.omit(district))) %>% 
  ungroup() %>% 
  group_by(year, date, quarter, province, city, district, pchc, market, packid) %>% 
  summarise(units = sum(units, na.rm = TRUE), 
            sales = sum(sales, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(flag = 0)

write_feather(raw.total, '03_Outputs/Servier_CHC2_Raw.feather')
