# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  CHC Functionalize
# Purpose:      Format Servier
# programmer:   Zhe Liu
# Date:         2020-12-16
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


FormatServier <- function(proj.price, 
                          std.info, 
                          vbp.info, 
                          vbp.mole, 
                          city.en, 
                          target.city, 
                          tcm) {
  
  ##---- City ----
  servier.city <- proj.price %>% 
    group_by(channel, year, quarter, province, city, market, packid) %>% 
    summarise(sales = sum(sales, na.rm = TRUE), 
              units = sum(units, na.rm = TRUE)) %>% 
    ungroup() %>% 
    left_join(city.en, by = 'city') %>% 
    left_join(std.info, by = 'packid') %>% 
    left_join(vbp.info, by = c('city', 'molecule', 'packid')) %>% 
    mutate(`是否是原研` = if_else(type != 'L', '原研', '仿制'), 
           `Manufacture Type` = if_else(type != 'L', 'MNC', 'Local')) %>% 
    mutate(first_num_position = stri_locate_first(pack, regex = '\\d')[,1], 
           last_space_position = stri_locate_last(pack, regex = '\\s')[,1], 
           `Pack Form` = str_squish(substr(pack, 1, first_num_position - 1)), 
           `Pack Form` = if_else(nchar(`Pack Form`) == 0, NA_character_, `Pack Form`), 
           `Pack Strength` = str_squish(substr(pack, first_num_position, 
                                      last_space_position - 1)), 
           `Pack Strength` = if_else(nchar(`Pack Strength`) == 0, NA_character_, `Pack Strength`), 
           `Pack Size` = as.integer(str_squish(substr(pack, last_space_position, 
                                                   nchar(pack)))), 
           `Pack Size` = if_else(nchar(`Pack Size`) == 0, NA_integer_, `Pack Size`)) %>% 
    mutate(
      `Category I` = case_when(
        market == 'HTN' & atc3 == 'C09A' ~ 'RAASi Plain', 
        market == 'HTN' & atc3 == 'C09C' ~ 'RAASi Plain', 
        market == 'HTN' & atc3 == 'C09B' ~ 'RAASi FDC', 
        market == 'HTN' & atc3 == 'C09D' ~ 'RAASi FDC', 
        market == 'HTN' & atc3 == 'C02A' ~ 'ANTI-HTN', 
        market == 'HTN' & atc3 == 'C02B' ~ 'ANTI-HTN', 
        market == 'HTN' & atc3 == 'C02C' ~ 'ANTI-HTN', 
        market == 'HTN' & atc3 == 'C03A' ~ 'DIURETICS', 
        market == 'HTN' & atc3 == 'C07A' ~ 'BB', 
        market == 'HTN' & atc3 == 'C08A' ~ 'CCB', 
        market == 'HTN' & atc3 == 'C08B' ~ 'CCB', 
        market == 'Diabetes' & atc3 == 'A10H' ~ 'SULPHONYLUREA', 
        market == 'Diabetes' & atc3 == 'A10J' ~ 'BIGUANIDE', 
        market == 'Diabetes' & atc3 == 'A10K' ~ 'GLITAZONE', 
        market == 'Diabetes' & atc3 == 'A10L' ~ 'AG Is', 
        market == 'Diabetes' & atc3 == 'A10M' ~ 'METAGLINIDE', 
        market == 'Diabetes' & atc3 == 'A10N' ~ 'DPP-IV', 
        market == 'Diabetes' & atc3 == 'A10P' ~ 'SGLT2', 
        market == 'Diabetes' & atc3 == 'A10S' ~ 'GLP-1', 
        market == 'Diabetes' & atc3 == 'A10X' ~ 'OTHERS', 
        market == 'IHD' & atc3 == 'C07A' ~ 'BB', 
        market == 'IHD' & atc3 == 'C08A' ~ 'CCB', 
        market == 'IHD' & atc3 == 'C08B' ~ 'CCB', 
        market == 'IHD' & atc3 == 'C01E' ~ 'NITRITES', 
        market == 'IHD' & atc3 == 'C01D' & molecule == 'TRIMETAZIDINE' ~ 'TMZ', 
        market == 'IHD' & atc3 == 'C01D' & molecule != 'TRIMETAZIDINE'~ 'OTHERS', 
        market == 'Venous Disease' & atc3 == 'C05A' ~ 'HD Topical', 
        market == 'Venous Disease' & atc3 == 'C05B' ~ 'CVD Topical', 
        market == 'Venous Disease' & atc3 == 'C05C' ~ 'VAD Oral', 
        market == 'Venous Disease' & stri_sub(atc3, 1, 3) == 'V03' ~ 'TCM', 
        TRUE ~ NA_character_
      ), 
      `Category II` = case_when(
        atc3 == 'C09A' ~ 'ACEi PLAIN', 
        atc3 == 'C09C' ~ 'ARB PLAIN', 
        atc4 %in% c('C09B3', 'C09D3') ~ 'A+C FDC', 
        atc4 %in% c('C09B1', 'C09D1') ~ 'A+D FDC', 
        TRUE ~ NA_character_
      ), 
      `Category II` = if_else(market == 'Diabetes', `Category I`, `Category II`), 
      `Category I` = case_when(market == 'Diabetes' ~ 'OAD&GLP-1', 
                               `Category I` == 'ANTI-HTN' ~ 'Others', 
                               `Category I` == 'DIURETICS' ~ 'DU', 
                               `Category I` == 'RAASi Plain' ~ 'RAASi PLAIN', 
                               `Category I` == 'NITRITES' ~ 'LAN', 
                               TRUE ~ `Category I`), 
      `Category II` = case_when(`Category II` == 'A+C FDC' ~ 'A+C', 
                                `Category II` == 'A+D FDC' ~ 'A+D', 
                                `Category II` == 'ACEi PLAIN' ~ 'ACEI', 
                                `Category II` == 'ARB PLAIN' ~ 'ARB', 
                                `Category II` == 'AG Is' ~ 'AGI', 
                                `Category II` == 'BIGUANIDE' ~ 'MET', 
                                `Category II` == 'DPP-IV' ~ 'DPP-4', 
                                `Category II` == 'GLITAZONE' ~ 'TZD', 
                                `Category II` == 'METAGLINIDE' ~ 'GLINIDE', 
                                `Category II` == 'SULPHONYLUREA' ~ 'SU', 
                                TRUE ~ `Category II`), 
      `Category II` = case_when(market == 'Venous Disease' & `Category I` == 'TCM' & route == '口服固体' ~ 'TCM Oral', 
                                market == 'Venous Disease' & `Category I` == 'TCM' & route == '外用制剂' ~ 'TCM Topical', 
                                market == 'Venous Disease' ~ NA_character_, 
                                TRUE ~ `Category II`), 
      `Category II` = if_else(is.na(`Category II`) & market %in% c('HTN', 'Venous Disease'), 
                              `Category I`, `Category II`)
    ) %>% 
    mutate(
      sales_adj = case_when(
        market == 'HTN' & stri_sub(atc3, 1, 3) == 'C07' ~ sales * 0.75, 
        market == 'IHD' & stri_sub(atc3, 1, 3) == 'C07' ~ sales * 0.25, 
        market == 'HTN' & stri_sub(atc3, 1, 3) == 'C08' ~ sales * 0.9, 
        market == 'IHD' & stri_sub(atc3, 1, 3) == 'C08' ~ sales * 0.1, 
        TRUE ~ sales
      ), 
      units_adj = case_when(
        market == 'HTN' & stri_sub(atc3, 1, 3) == 'C07' ~ units * 0.75, 
        market == 'IHD' & stri_sub(atc3, 1, 3) == 'C07' ~ units * 0.25, 
        market == 'HTN' & stri_sub(atc3, 1, 3) == 'C08' ~ units * 0.9, 
        market == 'IHD' & stri_sub(atc3, 1, 3) == 'C08' ~ units * 0.1, 
        TRUE ~ units
      )
    ) %>% 
    mutate(sales_adj = round(sales_adj, 2), 
           units_adj = round(units_adj), 
           dosageunits_adj = round(`Pack Size` * units_adj), 
           sales = round(sales, 2), 
           units = round(units), 
           dosageunits = round(`Pack Size` * units)) %>% 
    mutate(`Period Type` = 'QTR', 
           q = stri_sub(quarter, 5, 6), 
           p = trimws(stri_sub(product, 1, -4))) %>% 
    select(Channel = channel, 
           City_EN = city_en, 
           City = city, 
           `Period Type`, 
           Year = year, 
           Quarter = q, 
           Date = quarter, 
           `ATCIII Code` = atc3, 
           `ATCIV Code` = atc4, 
           MKT = market, 
           `Category I`, 
           `Category II`, 
           `Molecule composition Name` = molecule, 
           `Product Name` = product, 
           Product = p, 
           Prod_CN_Name = product_cn, 
           `Pack Code` = packid, 
           `Pack Description` = pack, 
           `Pack Form`, 
           `Pack Strength`, 
           `Pack Size`, 
           `NFCI Description` = route, 
           `Corporation Description` = corp, 
           Corporation_CN = corp_cn, 
           `Manufacture Type`, 
           `是否是中标品种`, 
           `是否是原研`, 
           `Value LC` = sales_adj, 
           Units = units_adj, 
           `Counting Units` = dosageunits_adj, 
           `Value LC_Raw` = sales, 
           Units_Raw = units, 
           `Counting Units_Raw` = dosageunits) %>% 
    arrange(Channel, Date, City, MKT, `Pack Code`)
  
  ##---- National ----
  servier.qtr <- servier.city %>% 
    filter(Channel == 'CHC') %>% 
    group_by(Channel, `Period Type`, Year, Quarter, Date, `ATCIII Code`, `ATCIV Code`, 
             MKT, `Category I`, `Category II`, `Molecule composition Name`, 
             `Product Name`, Product, Prod_CN_Name, `Pack Code`, `Pack Description`, 
             `Pack Form`, `Pack Strength`, `Pack Size`, `NFCI Description`, 
             `Corporation Description`, Corporation_CN) %>% 
    summarise(City_EN = 'National', 
              City = 'National', 
              `Manufacture Type` = first(na.omit(`Manufacture Type`)), 
              `是否是中标品种` = first(na.omit(`是否是中标品种`)), 
              `是否是原研` = first(na.omit(`是否是原研`)), 
              `Value LC` = sum(`Value LC`, na.rm = TRUE), 
              Units = sum(Units, na.rm = TRUE), 
              `Counting Units` = sum(`Counting Units`, na.rm = TRUE), 
              `Value LC_Raw` = sum(`Value LC_Raw`, na.rm = TRUE), 
              Units_Raw = sum(Units_Raw, na.rm = TRUE), 
              `Counting Units_Raw` = sum(`Counting Units_Raw`, na.rm = TRUE)) %>% 
    ungroup() %>% 
    bind_rows(servier.city) %>% 
    left_join(vbp.mole, by = c('City' = 'city', 'Molecule composition Name' = 'molecule')) %>% 
    mutate(VBP = case_when(`Molecule composition Name` == 'TRIMETAZIDINE' & `NFCI Description` == '口服固体长效' ~ '第二批', 
                           `Molecule composition Name` == 'TRIMETAZIDINE' & `NFCI Description` == '口服固体' ~ '第三批', 
                           TRUE ~ `是否进入带量采购`)) %>% 
    filter(!(`ATCIII Code` == 'V03B' & !(`Product Name` %in% tcm))) %>% 
    filter(City %in% c('National', target.city)) %>% 
    filter(`Value LC` > 0, Units > 0, `Counting Units` > 0, 
           !is.infinite(`Value LC`), !is.infinite(Units), !is.infinite(`Counting Units`)) %>% 
    select(Channel, City_EN, City, `Period Type`, Year, Quarter, Date, `ATCIII Code`, 
           `ATCIV Code`, MKT, `Category I`, `Category II`, `Molecule composition Name`, 
           `Product Name`, Product, Prod_CN_Name, `Pack Code`, `Pack Description`, 
           `Pack Form`, `Pack Strength`, `Pack Size`, `NFCI Description`, 
           `Corporation Description`, Corporation_CN, `Manufacture Type`, VBP, 
           `是否是中标品种`, `是否是原研`, `Value LC`, Units, `Counting Units`, 
           `Value LC_Raw`, Units_Raw, `Counting Units_Raw`)
  
  return(servier.qtr)
}
