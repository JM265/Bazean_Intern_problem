setwd("/Users/majunting/Desktop/Bazean_Intern")
options(digits=20)
options(scipen=999)
prod=read.csv("EagleFord_Production.csv",header=T)
well=read.csv("EagleFord_Well_Meta.csv",header=T)
prod.api=read.csv("prod_api.csv",header=T)
prod$api=prod.api$api
prod_top12_sum=sqldf("SELECT api, SUM(volume_oil_formation_bbls) AS oil_12_sum, SUM(volume_gas_formation_mcf) AS gas_12_sum FROM prod WHERE ind<13 GROUP BY api")
library("sqldf")
#prod=read.csv.sql("EagleFord_Production.csv")
#well=read.csv.sql("EagleFord_Well_Meta.csv",header=T)
#sqldf("SELECT COUNT(DISTINCT api) FROM prod")
#sqldf("SELECT COUNT(DISTINCT api) FROM well")
comp_oil_gas=sqldf("SELECT operator_name, SUM(cum_12_gas) AS sum_12_gas, SUM(cum_12_oil) AS sum_12_oil, SUM(cum_6_gas) AS sum_6_gas, SUM(cum_6_oil) AS sum_6_oil FROM well GROUP BY operator_name")
comp_oil_gas_nan=sqldf("SELECT COUNT(*) FROM comp_oil_gas WHERE sum_12_gas IS NULL OR sum_12_oil IS NULL OR sum_6_gas IS NULL OR sum_6_oil IS NULL")

#try to find well production from production file where sum 0 in WELL file
gas_zero_name=sqldf("SELECT operator_name FROM comp_oil_gas WHERE sum_12_gas =0")
gas_zero_api=sqldf("SELECT api, operator_name FROM well WHERE operator_name IN 
                   (SELECT operator_name FROM gas_zero_name) ORDER BY operator_name")
#prod_api=sqldf("SELECT DISTINCT api AS api1 FROM prod")
#comman=sqldf("SELECT COUNT(*) FROM (SELECT DISTINCT api AS api1 FROM prod) INNER JOIN well ON api1=well.api")
comp_gas_zero=sqldf("SELECT gas_12_sum, gas_zero_api.api, operator_name FROM prod_top12_sum JOIN  gas_zero_api ON gas_zero_api.api=prod_top12_sum.api")
gas_zero_sum=sqldf("SELECT operator_name, SUM(gas_12_sum) AS cum_12_gas FROM comp_gas_zero GROUP BY operator_name")

oil_zero_name=sqldf("SELECT operator_name FROM comp_oil_gas WHERE sum_12_oil =0")
oil_zero_api=sqldf("SELECT api, operator_name FROM well WHERE operator_name IN 
                   (SELECT operator_name FROM oil_zero_name) ORDER BY operator_name")
comp_oil_zero=sqldf("SELECT oil_12_sum, oil_zero_api.api, operator_name FROM prod_top12_sum JOIN  oil_zero_api ON oil_zero_api.api=prod_top12_sum.api")
oil_zero_sum=sqldf("SELECT operator_name, SUM(oil_12_sum) AS cum_12_oil FROM comp_oil_zero GROUP BY operator_name")


#Based on their history, which is most likely to be successful
max12_gas_company=sqldf("SELECT operator_name, sum_12_gas FROM comp_oil_gas ORDER BY sum_12_gas DESC LIMIT 1")
max12_oil_company=sqldf("SELECT operator_name, sum_12_oil FROM comp_oil_gas ORDER BY sum_12_oil DESC LIMIT 1")
#max6_gas_company=sqldf("SELECT operator_name, sum_6_gas FROM comp_oil_gas ORDER BY sum_6_gas DESC LIMIT 1")
#max6_oil_company=sqldf("SELECT operator_name, sum_6_oil FROM comp_oil_gas ORDER BY sum_6_oil DESC LIMIT 1")
max12_gas_company
max12_oil_company
#max6_gas_company
#max6_oil_company



#Who is the best company by size?
#compute each company's size
company_size=sqldf("SELECT operator_name, COUNT(DISTINCT api) as well_num FROM well GROUP BY operator_name ORDER BY COUNT(DISTINCT api) DESC ")
company_size[1:3,]
#compute each company's initial production
company_ip_sum=sqldf("SELECT operator_name, SUM(ipmo_oil) AS ipmo_oil_sum, SUM(ipmo_gas) AS ipmo_gas_sum FROM well GROUP BY operator_name")
#sqldf("SELECT operator_name, ipmo_oil_sum FROM company_ip ORDER BY ipmo_oil_sum DESC LIMIT 1")
#sqldf("SELECT operator_name, ipmo_gas_sum FROM company_ip ORDER BY ipmo_gas_sum DESC LIMIT 1")
#combine size and ip
company_size_ip=sqldf("SELECT company_size.operator_name, well_num, ipmo_oil_sum, ipmo_gas_sum FROM company_size JOIN company_ip_sum ON company_size.operator_name=company_ip_sum.operator_name")

library("binr")
well_num_bin=bins(size_prod1$well_num,target.bins = 8, minpts = 10)
well_num_bin$binct
oil_gmax1=sqldf("SELECT operator_name, MAX(ipmo_oil_sum),well_num FROM company_size_ip WHERE well_num=1")
oil_gmax2=sqldf("SELECT operator_name, MAX(ipmo_oil_sum),well_num FROM company_size_ip WHERE well_num=2")
oil_gmax3=sqldf("SELECT operator_name, MAX(ipmo_oil_sum),well_num FROM company_size_ip WHERE 3<=well_num<=4")
oil_gmax4=sqldf("SELECT operator_name, MAX(ipmo_oil_sum),well_num FROM company_size_ip WHERE 5<=well_num<=7")
oil_gmax5=sqldf("SELECT operator_name, MAX(ipmo_oil_sum),well_num FROM company_size_ip WHERE 8<=well_num<=12")
oil_gmax6=sqldf("SELECT operator_name, MAX(ipmo_oil_sum),well_num FROM company_size_ip WHERE 13<=well_num<=21")
oil_gmax7=sqldf("SELECT operator_name, MAX(ipmo_oil_sum),well_num FROM company_size_ip WHERE 22<=well_num<=44")
oil_gmax8=sqldf("SELECT operator_name, MAX(ipmo_oil_sum),well_num FROM company_size_ip WHERE 45<=well_num")

gas_gmax1=sqldf("SELECT operator_name, MAX(ipmo_gas_sum),well_num FROM company_size_ip WHERE well_num=1")
gas_gmax2=sqldf("SELECT operator_name, MAX(ipmo_gas_sum),well_num FROM company_size_ip WHERE well_num=2")
gas_gmax3=sqldf("SELECT operator_name, MAX(ipmo_gas_sum),well_num FROM company_size_ip WHERE 3<=well_num<=4")
gas_gmax4=sqldf("SELECT operator_name, MAX(ipmo_gas_sum),well_num FROM company_size_ip WHERE 5<=well_num<=7")
gas_gmax5=sqldf("SELECT operator_name, MAX(ipmo_gas_sum),well_num FROM company_size_ip WHERE 8<=well_num<=12")
gas_gmax6=sqldf("SELECT operator_name, MAX(ipmo_gas_sum),well_num FROM company_size_ip WHERE 13<=well_num<=21")
gas_gmax7=sqldf("SELECT operator_name, MAX(ipmo_gas_sum),well_num FROM company_size_ip WHERE 22<=well_num<=44")
gas_gmax8=sqldf("SELECT operator_name, MAX(ipmo_gas_sum),well_num FROM company_size_ip WHERE 45<=well_num")

gas_size_max=sqldf("SELECT operator_name, MAX(ipmo_gas_sum) as max_gas, well_num FROM company_size_ip GROUP BY well_num")
oil_size_max=sqldf("SELECT operator_name, MAX(ipmo_oil_sum) as max_oil, well_num FROM company_size_ip GROUP BY well_num")

#find outliers
#delete the first row which without operator_name
company_size_ip=company_size_ip[-1,]

ip_oil=company_size_ip$ipmo_oil_sum
ip_gas=company_size_ip$ipmo_gas_sum
well_n=company_size_ip$well_num
fit_oil=lm(ip_oil~well_n)
fit_gas=lm(ip_gas~well_n)
library(car)
influencePlot(fit_gas)
influencePlot(fit_oil)

#Who is most likly to succeed in the future when drilling wells?

#Who has the best performing production where more production is better?
dec_well=sqldf("SELECT operator_name, SUM(first_year_decline) AS dec_sum FROM well GROUP BY operator_name ORDER BY dec_sum DESC")
