# Impact analysis of the zero-fare program on public transport
# Author: Rafael de Acypreste
# Advisor: Profa. Thais Carvalho Valadares Rodrigues

# Data importation and initial data manipulation



  # Function to load packages ----
load_packages_fftp <- function(){ 
  
  # Load the necessary libraries 
library(readxl)       # To read Excel files
library(dplyr)        # To manipulate data
library(readr)        # To read CSV files
library(tidyr)        # To manipulate data
library(purrr)        # To manipulate data
library(sidrar)       # To access IBGE data
library(stringr)      # To manipulate strings
library(writexl)      # To write Excel files
library(basedosdados) # To access the Brazilian government data
  
  
  # Non-CRAN packages
if(!require("roadtrafficdeaths")){
  devtools::install_github("pabsantos/roadtrafficdeaths")
}
library(roadtrafficdeaths) # To access the road traffic deaths data in Brazil


options(scipen = 999)      # To avoid scientific notation

source("src/functions.r")  # To load the utility functions

}



load_packages_fftp()

#___________________________________________________#
#    Data importation codes 
#___________________________________________________#




#___________________________________________________#
#  IPCA Índice de Preços ao Consumidor Amplo (CPI) ---- 
# Dec 2020 = 100
#___________________________________________________#


# info_sidra(1737)

ipca_table <- file.path("data", "ipca.csv")

if(!file.exists(ipca_table)){
  
  # Customized periods 
  periods <- c(paste0(2000:2021, "12"), "202005")
  
  ipca  <-     
    get_sidra(x        = 1737,    # IPCA table (Brazilian CPI analogous - consumer price index) 
              period   = periods,
              variable = 2266,    # 1993 = 100
              header   = TRUE,
              format   = 3)
  
  ipca |> 
    write_csv(ipca_table)
  
  
}


ipca <- read_csv(ipca_table)

value_ref_052020 <- ipca |>
  filter(Mês == "maio 2020")  |>     # May 2020 reference value
  pull(Valor)

value_ref_122020 <- ipca |> 
  filter(Mês == "dezembro 2020") |>  # December 2020 reference value
  pull(Valor)

  # Adjusting the IPCA values to the reference value of December 2020 = 1
tbl_cpi_202012 <- 
  ipca |> 
  filter(Mês != "maio 2020") |> 
  select(year = Mês,
         "IPCA_12_2020" = Valor) |> 
  mutate(IPCA_12_2020 = IPCA_12_2020 / value_ref_122020,
         year = str_extract(year, "\\d{4}"))  |> 
  type_convert()


rm(ipca)


#___________________________________________________#
#    Zero-fare program starting points ----
#___________________________________________________#

  # Accessed in: Santini, Daniel, 2024, "Brazilian municipalities with full
  # Fare-Free Public Transport policies - updated March 2024",
  # https://doi.org/10.7910/DVN/Z927PD, Harvard Dataverse, V1
tbl_zero_fare <- 
    read_xlsx("data/zero-fare-program-starting-points.xlsx",
              sheet = "vigentes",
              col_types = c("text", "guess",
                            "numeric", "guess",
                            "guess", "date",   # To correctly import the date
                            "guess", "guess",
                            "guess", "guess",
                            "guess", "guess")) 

 # Name adjustments
zero_fare_start <- 
    tbl_zero_fare |> 
    select("ibge_code"    = `Código IBGE`,
           "start_year"   = `Início`,
           "start_date"   = `Início data`) |> 
    mutate(start_date = as.Date(start_date, format = "%d/%m/%Y"),
           fare_free = "Yes") 

total_fftp_adoption_by_year <-
zero_fare_start |>
  count(start_year) |>
  arrange(desc(start_year))



sum(total_fftp_adoption_by_year$n)

total_fftp_adoption_by_year |> 
  filter(start_year >= 2003, start_year <= 2019) |> 
  summarise(total = sum(n))


 # If the program started in the last quarter of the year, we consider the next year
 # If relevant
adjust_fftp_start <- FALSE

if (adjust_fftp_start){
  
  zero_fare_start <-
    zero_fare_start |>
    mutate(start_year = ifelse(as.numeric(str_sub(start_date, 6, 7)) >= 10,
                               start_year + 1, # If 4th quarter, the program starts
                               start_year))    # we consider the next year
  
    
}

  ## Always treated municipalities ----
  # They should be excluded from the analysis        
tbl_always_treated <-
  zero_fare_start |>
  filter(start_year < 2003) |>      # Before the period of interest (tax data available)
  select(ibge_code, start_year) |>
  mutate(always_treated = "Remove")

#___________________________________________________#
#   Municipalities data ----
#___________________________________________________#


#___________________________________________________#
##   Population (estimations) ----
#___________________________________________________#


# info_sidra(6579)

pop_table <- file.path("data", "pop_estimations.csv")

if(!file.exists(pop_table)){

    # Customized periods due to lack of data for some years
periods <- list(as.character(2003:2006),
                as.character(2008:2009),
                as.character(2011:2013),
                as.character(2014:2021))

pop_estimations <- 
    periods |> 
    map_dfr(\(y) get_sidra(x      = 6579, 
                           period = y,
                           geo    = "City", # Municipalities
                           header = TRUE,
                           format = 3))     # Return codes and names of the
                                            # geographic level and descriptors' names


#___________________________________________________#
##   Population (census and official countings) ----
#___________________________________________________#


 # 2007 counting poulation
# info_sidra(793)
pop_2007 <- 
  get_sidra(x      = 793,
            geo    = "City",
            header = TRUE,
            format = 3) 
    

 # 2010 census
# info_sidra(608)
pop_2010 <- 
  get_sidra(x         = 608,
            variable  = 93,      # Resident population
            geo       = "City",
            classific = c("c1"),
            category  = list(0), # Totals only
            header    = TRUE,
            format    = 3) 
    



pop_estimations  |> 
 bind_rows(pop_2007) |> 
 bind_rows(select(pop_2010, -`Situação do domicílio`, -Sexo)) |> 
    arrange(`Ano`, `Município (Código)`) |> 
    write_csv("data/pop_estimations.csv")


}

    # Read the CSV file (changing names)
pop_estimations <- 
    read_csv(pop_table) |> 
    select("ibge_code"      = `Município (Código)`,
           "year"           = Ano,
           "pop_estimation" = Valor,
           "municipality"   = `Município`) 




# Unbalanced data (excluding incomplete data)
pop_estimations |>
  count(year)

tbl_incomplete_pop <-
  pop_estimations |>
  count(ibge_code) |>
  arrange(n) |>
  filter(n < 19)  |> 
  mutate(incomplete_pop = "Remove") 

  # Excluding incomplete data
pop_estimations <-
  pop_estimations |>
  left_join(tbl_incomplete_pop, by = "ibge_code") |>
  filter(is.na(incomplete_pop))  |>
  select(-n, -incomplete_pop) |> 
  mutate(ibge_6_code = as.numeric(substr(ibge_code, 1, 6))) 


rm(tbl_incomplete_pop)



#___________________________________________________#
##   Population (DATASUS) suposedly more reliable ----
#___________________________________________________#


pop_datasus_2000_21 <- read_delim(
  "data/pop-datasus-2000-21.csv",
  delim         = ";",
  escape_double = FALSE,
  locale        = locale(encoding = "ISO-8859-1"),
  trim_ws       = TRUE,
  skip          = 3
) |>
  drop_na() |>                    # Drop empty rows (notes)
  filter(Município != "Total") |> # Exclude totals
  separate(
    `Município`,
    into  = c("ibge_6_code", "municipality_datasus"),
    sep   = " ",
    extra = "merge"
  ) |>
  pivot_longer(cols     = `2000`:`2021`,
               names_to = "year",
               values_to = "pop_datasus") |>
  type_convert()


  # Joining the two sources of population data
pop_estimations <- 
pop_estimations |> 
  left_join(pop_datasus_2000_21, by = join_by("ibge_6_code", "year")) |> 
  select(-municipality_datasus) |> 
  select(ibge_code, municipality, year, pop_estimation, pop_datasus, ibge_6_code) 


#___________________________________________________#
###   Municipality GDP ---- 
#___________________________________________________#



# info_sidra(5938)

gdp_table <- file.path("data", "gdp_municipality.csv")

if(!file.exists(gdp_table)){

    # Customized periods due to download limitations (50,000 per request)
periods <- list(as.character(2003:2006),
                as.character(2007:2010),
                as.character(2011:2014),
                as.character(2015:2018),
                as.character(2019:2021))

gdp_municipality  <- 
    periods |> 
    map_dfr(\(y) get_sidra(x        = 5938, 
                           period   = y,
                           geo      = "City",
                           variable = c(37,   # Current GDP
                                        497), # Participation in the State's GDP 
                           header   = TRUE,
                           format   = 3)) 

gdp_municipality |> 
    arrange(Ano, `Município (Código)`) |>  # Sort by year and municipality
    write_csv("data/gdp_municipality.csv")

}

    # Read the CSV file
gdp_municipality  <- 
    read_csv(gdp_table) |> 
    select("ibge_code"      = `Município (Código)`,
           "year"           = Ano,
           `Variável`,
            Valor)  |> 
    pivot_wider(names_from  = `Variável`, # Two distinct variables in the same column
                values_from = Valor) |>
    rename("gdp_current"             = `Produto Interno Bruto a preços correntes`,
           "gdp_state_participation" = `Participação do produto interno bruto a preços correntes no produto interno bruto a preços correntes da unidade da federação`) 


  #### GDP deflation to Dec 2020 ----
gdp_municipality <-
  gdp_municipality |>
  left_join(tbl_cpi_202012, by = "year")  |>
  mutate(gdp_current = gdp_current / IPCA_12_2020) |> 
  select(-IPCA_12_2020)



#___________________________________________________#
###   Territorial area ---- 
#___________________________________________________#



territorial_area_table <- file.path("data", "2010-Areas_MU_UF_RE_BR.xls")

if(!file.exists(territorial_area_table)){
  

  # Specify the URL of the Excel file
  url <- "https://geoftp.ibge.gov.br/organizacao_do_territorio/estrutura_territorial/areas_territoriais/2010/Areas_MU_UF_RE_BR.xls"
  
  # Create the full path to the file (2010 base territorial areas)
  file_path <- file.path("data", "2010-Areas_MU_UF_RE_BR.xls")
  
  # Download the file directly to the data folder
  download.file(url, file_path, mode = "wb")
  
}

  # Read the Excel file
territorial_area_2010  <-
  read_xls(territorial_area_table, sheet = "MUNICÍPIO") |>
  select("ibge_code"             = GEOCODIGO,
         "territorial_area_2010" = AR_MUN_2010) |>
  type_convert()

    



#___________________________________________________#
###   Tax collection ----

# Source: 

# NOTA TÉCNICA
# Estimativas anuais da arrecadação tributária e das receitas totais dos municípios 
# brasileiros entre 2003 e 2019
# In: IPEA, Carta de Conjuntura | 48 | 3˚ trimestre de 2020

# Available at: https://www.ipea.gov.br/cartadeconjuntura/index.php/2020/07/estimativas-anuais-da-arrecadacao-tributaria-e-das-receitas-totais-dos-municipios-brasileiros-entre-2003-e-2019/
# Accessed on: 2024-04-23

# Database is deflated by the IPCA (Índice de Preços ao Consumidor Amplo) to May 2020 values

#___________________________________________________#

if(!file.exists(file.path("data", "200730_base_receitas_municipais.xlsx"))){
    
# Specify the URL of the Excel file
url <- "http://www.ipea.gov.br/cartadeconjuntura/wp-content/uploads/2020/07/200730_base_receitas_municipais.xlsx"

# Create the full path to the file
file_path <- file.path("data", "200730_base_receitas_municipais.xlsx")

# Download the file directly to the data folder
download.file(url, file_path, mode = "wb")

}

#### Total tax collection ----

  # Read the Excel file
tbl_total_tax_collection  <-
  read_xlsx("data/200730_base_receitas_municipais.xlsx",
            sheet = "Receitas Totais") |>
  select(-UF, -`Município`) |>
  rename("ibge_code"    = Cod.IBGE, 
         "pop_2019"     = POP_2019) |> # Population in 2019
  pivot_longer(
    cols      = -c(ibge_code, pop_2019),
    names_to  = "year",
    values_to = "total_tax_collection"
  ) |>
  type_convert()

##### Total tax deflation to Dec 2020 ----
def_index <- value_ref_052020 / value_ref_122020 # Deflation index

total_tax_collection  <-
  tbl_total_tax_collection |>
  mutate(total_tax_collection = total_tax_collection / def_index)


rm(tbl_total_tax_collection)

#### ISS collection ----

    # Read the Excel file
tbl_iss_collection <-  
    read_xlsx("data/200730_base_receitas_municipais.xlsx",
              sheet = "ISS") |>
  select(-UF, -`Município`) |>
  rename("ibge_code"    = Cod.IBGE, 
         "pop_2019"     = POP_2019) |>
  pivot_longer(
    cols      = -c(ibge_code, pop_2019),
    names_to  = "year",
    values_to = "iss_collection"
  ) |>
  select(-pop_2019) |>
  type_convert() 


##### ISS (tax on services) deflation to Dec 2020 ----

iss_collection  <-
  tbl_iss_collection |> 
  mutate(iss_collection = iss_collection / def_index)


rm(tbl_iss_collection)








#___________________________________________________#
###   Employment information ---- 
#___________________________________________________#

# All data are from RAIS (Relação Anual de Informações Sociais) database
# 

 #### Dictionary download ----
rais_dictionary_path <- file.path("data", "rais_dictionary.csv")

if(!file.exists(rais_dictionary_path)){
  

  # Define your Google Cloud project
  set_billing_id("dados-rais-tarifa-zero")
  
  # Import data directly from the BigQuery database
  query <- bdplyr("br_me_rais.dicionario")
  rais_dictionary <- bd_collect(query)
  
  # To save the dictionary
  write_csv(rais_dictionary,
            rais_dictionary_path)
  
    
  
}
  


  # To consult if necessary
rais_dictionary <- read_csv(rais_dictionary_path) 


 #### Total employment download ----
rais_total_employment_path <- file.path("data", "rais_total_employment.csv")



if(!file.exists(rais_total_employment_path)){
  
  # Define your Google Cloud project
set_billing_id("dados-rais-tarifa-zero")

  # Query to download the total employment data
bare_query <- "SELECT ano, id_municipio, SUM(vinculo_ativo_3112)
FROM basedosdados.br_me_rais.microdados_vinculos
WHERE ano between 2003 and 2022
GROUP BY ano, id_municipio"
  
    
  # Import data directly from the BigQuery database and save as .csv
basedosdados::download(query = bare_query,
                       path  = rais_total_employment_path)
  

}


rais_employment <- read_csv(rais_total_employment_path) |> 
  rename("ibge_code"        = id_municipio,
         "year"             = ano,
         "total_employment" = f0_) # Total employment in 31th December of each year



  #### Mean wage download ----

rais_dec_wage_path <- file.path("data", "rais_dec_wage_excluded_zeros.csv")


if(!file.exists(rais_dec_wage_path)){
  
  # Define your Google Cloud project
set_billing_id("projeto-rafael-acypreste")
  
  # Query to download the mean wage data
bare_query_wage <- "SELECT ano, id_municipio, AVG(valor_remuneracao_dezembro)
FROM basedosdados.br_me_rais.microdados_vinculos
WHERE ano BETWEEN 2000 AND 2022 AND valor_remuneracao_dezembro > 0
GROUP BY ano, id_municipio"
  
  # Import data directly from the BigQuery database and save as .csv
basedosdados::download(query = bare_query_wage,
                       path  = rais_dec_wage_path)

}



rais_wage <- read_csv(rais_dec_wage_path) |> 
  rename("ibge_code" = id_municipio,
         "year"      = ano,
         "mean_wage" = f0_) # Mean wage in 31th December of each year


##### Deflation to Dec 2020 ----

rais_wage <-
  rais_wage |>
  left_join(tbl_cpi_202012, by = "year")  |>
  mutate(mean_wage = mean_wage / IPCA_12_2020) |> 
  select(-IPCA_12_2020)




#___________________________________________________#
###   Ministry of Transportation ---- 
#___________________________________________________#

# There is no tidy database. Crafted from raw files downloaded
# from the Ministry of Transportation website

if(!file.exists(file.path("data", "fleet_municipalities_2003_2021.csv"))){
  
  # Dowloading the raw files (caution to the 2015 year, which is in .rar format)
  


  # Directory to store the zip files
  if (!dir.exists("data/raw_fleet")) {
    dir.create("data/raw_fleet")
  }
  
  # Directory to store the unzipped files
  if (!dir.exists("data/fleet")) {
    dir.create("data/fleet")
  }
  
  
  
  
#### 2003 to 2008 (zip files) ----


# Downloading the zip files

for(year in 2003:2008) {
  # Specify the URL of the zip file
  url <-
    file.path(
      "https://www.gov.br/transportes/pt-br/assuntos/transito/arquivos-senatran/estatisticas/renavam",
      year,
      paste0("frota", year, ".zip")
    )
  
  # Create the full path to the file
  file_path <- file.path("data", "raw_fleet", paste0(year, ".zip"))
  
  # Download the file directly to the data folder
  download.file(url, file_path, mode = "wb")
  
}



### 2009 to 2012 (zip files) ----

for(year in 2009:2012){
  
  # Specify the URL of the zip file
  url <- file.path("https://www.gov.br/transportes/pt-br/assuntos/transito/arquivos-senatran/estatisticas/renavam",
                   year,
                   paste0("frota_", year, ".zip"))
  

  # Create the full path to the file
  file_path <- file.path("data", "raw_fleet", paste0(year, ".zip"))
  
  # Download the file directly to the data folder
  download.file(url, file_path, mode = "wb")
  
}





# Download Urls from 2013 to 2021 (unstructured paths)
url <-
  c(
    "https://www.gov.br/transportes/pt-br/assuntos/transito/arquivos-senatran/estatisticas/renavam/2013/julho/frotamunic-jul-2013.zip",
    "https://www.gov.br/transportes/pt-br/assuntos/transito/arquivos-senatran/estatisticas/renavam/2014/julho/frotamunicjul-2014.xlsx",
    "https://www.gov.br/transportes/pt-br/assuntos/transito/arquivos-senatran/estatisticas/renavam/2015/julho/frota_por_municipio_e_tipo-jul_15.rar",
    "https://www.gov.br/transportes/pt-br/assuntos/transito/arquivos-senatran/estatisticas/renavam/2016/julho/frota_por_municipio_e_tipo-jul_16.xlsx",
    "https://www.gov.br/transportes/pt-br/assuntos/transito/arquivos-senatran/estatisticas/renavam/2017/julho/frota_munic_julho_2017.xls",
    "https://www.gov.br/transportes/pt-br/assuntos/transito/arquivos-senatran/estatisticas/renavam/2018/julho/frota_munic_julho_2018.xls",
    "https://www.gov.br/transportes/pt-br/assuntos/transito/arquivos-senatran/estatisticas/renavam/2019/julho/frota_munic_modelo_julho_2019.xls",
    "https://www.gov.br/transportes/pt-br/assuntos/transito/arquivos-senatran/estatisticas/renavam/2020/julho/frota_munic_modelo_julho_2020.xls",
    "https://www.gov.br/transportes/pt-br/assuntos/transito/arquivos-senatran/estatisticas/renavam/2021/julho/frota_munic_modelo_julho_2021.xls"
  )

  # Specify the path and extension of the files
file_path <- c(
  file.path("data", "raw_fleet","2013.zip"),
  file.path("data", "raw_fleet", "2014.xlsx"),
  file.path("data", "raw_fleet", "2015.rar"),
  file.path("data", "raw_fleet", "2016.xlsx"),
  file.path("data", "raw_fleet", paste0(2017:2021, ".xls"))
)

  # Download the files directly to the data folder
walk2(url, file_path, \(x, y) download.file(
  url      = x,
  destfile =  y,
  mode     = "wb"
))



### 2013 (unzip files) ----
unzip(file.path("data", "raw_fleet", "2013.zip"), exdir = "data/raw_fleet")

# renaming file
file.rename(file.path("data", "raw_fleet", "Frota Munic.JUL.2013.xls"),
            file.path("data", "raw_fleet", "2013.xls"))




#### 2003 to get colnames (importing to R) ----
#   (despite the difference in writing, the variables are in the same order) 

table_fleet_2003 <-
  unzip_fleet_2003_2009(
    year_tbl           = 2003,
    zip_specific       = "Frota Tipo-Munic 2003.zip",
    zip_specific_table = "Frota_Mun_Jul_03.xls",
    sheet              = 1,
    skip               = 1
  )

colnames <- colnames(table_fleet_2003)[-length(colnames(table_fleet_2003))] # Removing the year column name

#### 2004 to 2009 (aggretating tables) ----

years <- c(2004, 2005, 2006, 2007, 2008, 2009)

zip_specific <- c(
  "Frota Munic 012004 Internet.zip",
  "Frota Tipo Munic 2005.zip",
  "Frota Tipo Munic 2006.zip",
  "Frota Tipo Munic 2007.zip",
  "Frota Tipo Munic 2008.zip",
  "Frota Munic 2009.zip"
)

zip_specific_table <- c(
  "Frota Munic 072004 Internet.xls",
  "Frota Munic 072005 Internet.xls",
  "Frota Munic Jul2006 Internet.xls",
  "Frota Munic Jul2007 Internet.xls",
  "Frota Munic Jul2008.xls",
  "Frota Munic Jul2009.xls"
)

sheet <- list(1, 1, "JUL_2006", "JUL_2007", "JUL_2008", "JUL_2009")

skip <- rep(3, 6)


# Aggregating by row
table_fleet_2004_2009 <-
  pmap_dfr(
    list(years, zip_specific, zip_specific_table, sheet, skip),
    # inputs
    \(x1, x2, x3, x4, x5) unzip_fleet_2003_2009(
      year_tbl           = x1,
      zip_specific       = x2,
      zip_specific_table = x3,
      sheet              = x4,
      skip               = x5,
      colnames = colnames
    )
  )



#### 2010 to 2012 (aggretating tables) ----

years <- 2010:2012

zip_specific_table <- c(
  "Frota_Municipios/Frota Munic Jul2010.xls",
  "Frota Munic. 2011/Frota Munic. JUL 2011.xls",
  "Frota Munic. 2012/Frota Munic. JUL.2012.xls"
)

sheet <- list("JUL_2010", "JUL_2011", "JUL_2010") # There is an error in 2012 sheet name

skip <- c(3, 4, 4)



# Aggregating by row
table_fleet_2010_2012 <-
  pmap_dfr(
    list(years, zip_specific_table, sheet, skip),
    # inputs
    \(x1, x2, x3, x4) unzip_fleet_2010_2012(
      year_tbl           = x1,
      zip_specific_table = x2,
      sheet              = x3,
      skip               = x4,
      colnames           = colnames
    )
  )



#### 2013 and 2017-21 (aggretating tables) ----

years_to_read <- c(2013, 2017:2021)

file_paths <-
  file.path("data", "fleet", paste0(years_to_read, ".xls"))


sheet <-
  c("JUL_2013",
    "JULHO_2017",
    "MAIO_2017",  # Despite the wrong month, the file is probably from July 2018
    "JULHO_2019",
    "Julho 2020",
    "JULHO_2021")

# Aggregating by row
table_fleet_2013_to_2021 <-
  pmap_dfr(
    list(file_paths, sheet, years_to_read),
    \(x, y, z) read_xls(
      path      = x,
      sheet     = y,
      skip      = 4,
      col_names = colnames
    ) |>
      mutate(year = z)
  )



# Reorginize the folder structure
new_path <- file.path("data",
                      "fleet",
                      paste0("Frota_", years_to_read, ".xls"))

file.rename(file_paths, new_path)



#### 2014, 2016 (aggregating tables) ----


years_to_read <- c(2014, 2016)

file_paths <- 
  file.path("data", "raw_fleet", paste0(years_to_read, ".xlsx"))

sheet <-
  c("JUL_2014",
    "JUL_2016")

# Aggregating by row
table_fleet_2014_2016 <-
  pmap_dfr(
    list(file_paths, sheet, years_to_read),
    \(x, y, z) read_xlsx(
      path      = x,
      sheet     = y,
      skip      = 4,
      col_names = colnames
    ) |>
      mutate(year = z)
  )


# Reorginize the folder structure
new_path <- file.path("data",
                      "fleet",
                      paste0("Frota_", years_to_read, ".xls"))

file.rename(file_paths, new_path)

#### 2015 (aggregating tables) ----

# The .rar format demanded manual intervention

years_to_read <- c(2015)

file_paths <-
  file.path("data", "Frota_por_Municipio_e_Tipo-JUL_15.xls")

sheet <-
  c("JUL_2015")

table_fleet_2015 <- read_xls(file_paths,
                             sheet     = sheet,
                             skip      = 4,
                             col_names = colnames) |>
  mutate(year = years_to_read)



#___________________________________________________#
###   Adjusting fleet tables ---- 
#___________________________________________________#

table_fleet_raw <- 
  table_fleet_2003 |>
  bind_rows(table_fleet_2004_2009) |>
  bind_rows(table_fleet_2010_2012) |>
  bind_rows(table_fleet_2013_to_2021) |>
  bind_rows(table_fleet_2014_2016) |>
  bind_rows(table_fleet_2015) |>
  arrange(UF, MUNICÍPIO, year) 


table_fleet_raw |> 
  write_csv("data/fleet_municipalities_2003_2021.csv")



  # Removing the unecessary tables
rm(
  table_fleet_2003,
  table_fleet_2004_2009,
  table_fleet_2010_2012,
  table_fleet_2013_to_2021,
  table_fleet_2014_2016,
  table_fleet_2015
)

}



#### Importing the raw fleet table ----

  # Read the CSV file (adapting names)
table_fleet_raw <- 
  read_csv("data/fleet_municipalities_2003_2021.csv") |>
  rename("municipality"    = MUNICÍPIO,
         "federation_unit" = UF) |>
  mutate(municipality = str_to_lower(municipality, locale = "pt")) |> 
  filter(!is.na(municipality))





#### IBGE codes ----
tbl_ibge_codes <-
  pop_estimations |>
  select(municipality, ibge_code) |>
  distinct() |>
  separate(municipality,
           into = c("municipality", "federation_unit"),
           sep  = " - ") |>
  mutate(
    municipality = str_to_lower(municipality, locale = "pt"),
    municipality = replace_special_char(municipality)
  )


#### Adjusting the fleet table ----
tbl_fleet <-
  table_fleet_raw |>
  mutate(
    municipality = str_replace(
      municipality,
      # Adjusting the municipality name
      "(picarras|balneario de picarras)",
      # that adopted the zero-fare program
      "balneario picarras"
    ),
    municipality = str_replace(municipality, "piui", "piumhi")
  ) |> # but has a different name in the IBGE database
  left_join(tbl_ibge_codes, join_by("municipality", "federation_unit")) |>
  filter(!is.na(ibge_code)) |>
  select(-federation_unit, -municipality)






#___________________________________________________#
####   Removing the unecessary fleet raw files ----  
#___________________________________________________#

folder_path <- file.path("data", "raw_fleet")

unlink(folder_path, recursive = TRUE)


#___________________________________________________#
###   Traffic mortality (Ministry of Health) ---- 
#___________________________________________________#


# rdeaths loaded from the roadtrafficdeaths package
rtdeaths_2003_2022 <-
  rtdeaths |>
  filter(ano_ocorrencia >= 2003) |>
  count(ano_ocorrencia, cod_municipio_ocor) |>
  rename(year           = ano_ocorrencia,
         ibge_code      = cod_municipio_ocor,
         traffic_deaths = n) |>
  type_convert()


# Complete missing combinations of year and ibge_code with zeros for traffic_deaths
rtdeaths_2003_2022  <-
  rtdeaths_2003_2022 |>
  complete(year, ibge_code, fill = list(traffic_deaths = 0))


#___________________________________________________#
#    Final database (Joining tables) ---- 
#___________________________________________________#


## Excluding always treated municipalities ----
pop_estimations <- 
  pop_estimations |>
  left_join(tbl_always_treated, by = "ibge_code")  |> 
  filter(is.na(always_treated)) |> 
  select(-start_year, -always_treated)

## Base table and fare-free program starting points ----
tbl_fare_free <-
  pop_estimations |>
  left_join(zero_fare_start, by = "ibge_code") |>
  arrange(ibge_code, year) |>
  mutate(
    fare_free = ifelse(year >= start_year & fare_free == "Yes", 1, 0),
    fare_free = replace_na(fare_free, 0)  # In the previous, NA are not evaluated
  )  


## IBGE six digits code column ----
tbl_fare_free <-
  tbl_fare_free |>
  # mutate(ibge_6_code = as.numeric(substr(ibge_code, 1, 6))) |>
  separate(municipality,
           into = c("municipality", "federation_unit"),
           sep  = " - ")

## Joining all tables ----
tbl_fare_free <-  
  tbl_fare_free |>
  left_join(gdp_municipality,
            by = c("ibge_code", "year")) |>
  left_join(territorial_area_2010,
            by = "ibge_code") |>
  left_join(rais_employment,
            by = c("ibge_code", "year")) |>
  left_join(rais_wage,
            by = c("ibge_code", "year")) |>
  left_join(iss_collection,
            by = join_by("ibge_6_code" == "ibge_code", "year")) |>
  left_join(total_tax_collection,
            by = join_by("ibge_6_code" == "ibge_code", "year")) |>
  left_join(tbl_fleet,
            by = c("ibge_code", "year")) |>
  left_join(rtdeaths_2003_2022,
            by = join_by("ibge_6_code" == "ibge_code", "year")) 




# Removing the unecessary tables

remove_unecessary <- FALSE

if(remove_unecessary) {
  rm(
    gdp_municipality,
    iss_collection,
    pop_estimations,
    tbl_cpi_202012,
    rtdeaths_2003_2022,
    tbl_always_treated,
    tbl_fleet,
    table_fleet_raw,
    tbl_zero_fare,
    tbl_ibge_codes,
    territorial_area_2010,
    total_fftp_adoption_by_year,
    total_tax_collection,
    zero_fare_start
  )
}


## Exporting the final database ----
tbl_fare_free |>
  write_csv("data/fare_free_municipalities_2003_2021.csv")


