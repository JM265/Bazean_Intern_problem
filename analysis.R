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
sqldf("SELECT COUNT(DISTINCT api) FROM prod")
sqldf("SELECT COUNT(DISTINCT api) FROM well")
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
company_size=company_size[-1,]
#compute each company's initial production
company_ip_sum=sqldf("SELECT operator_name, SUM(ipmo_oil) AS ipmo_oil_sum, SUM(ipmo_gas) AS ipmo_gas_sum FROM well GROUP BY operator_name")
#sqldf("SELECT operator_name, ipmo_oil_sum FROM company_ip ORDER BY ipmo_oil_sum DESC LIMIT 1")
#sqldf("SELECT operator_name, ipmo_gas_sum FROM company_ip ORDER BY ipmo_gas_sum DESC LIMIT 1")
#combine size and ip
company_size_ip=sqldf("SELECT company_size.operator_name, well_num, ipmo_oil_sum, ipmo_gas_sum FROM company_size JOIN company_ip_sum ON company_size.operator_name=company_ip_sum.operator_name")

library("binr")
well_num_bin=bins(company_size$well_num,target.bins = 8, minpts = 10)
well_num_bin$binct
oil_gmax1=sqldf("SELECT operator_name, MAX(ipmo_oil_sum),well_num FROM company_size_ip WHERE well_num=1")
oil_gmax2=sqldf("SELECT operator_name, MAX(ipmo_oil_sum),well_num FROM company_size_ip WHERE well_num=2")
oil_gmax3=sqldf("SELECT operator_name, MAX(ipmo_oil_sum),well_num FROM company_size_ip WHERE well_num=3")
oil_gmax4=sqldf("SELECT operator_name, MAX(ipmo_oil_sum),well_num FROM company_size_ip WHERE 4<=well_num AND well_num<=5")
oil_gmax5=sqldf("SELECT operator_name, MAX(ipmo_oil_sum),well_num FROM company_size_ip WHERE 6<=well_num AND well_num<=8")
oil_gmax6=sqldf("SELECT operator_name, MAX(ipmo_oil_sum),well_num FROM company_size_ip WHERE 9<=well_num AND well_num<=14")
oil_gmax7=sqldf("SELECT operator_name, MAX(ipmo_oil_sum),well_num FROM company_size_ip WHERE 15<=well_num AND well_num<=32")
oil_gmax8=sqldf("SELECT operator_name, MAX(ipmo_oil_sum),well_num FROM company_size_ip WHERE 33<=well_num")

oil_gmax1
oil_gmax2
oil_gmax3
oil_gmax4
oil_gmax5
oil_gmax6
oil_gmax7
oil_gmax8

gas_gmax1=sqldf("SELECT operator_name, MAX(ipmo_gas_sum),well_num FROM company_size_ip WHERE well_num=1")
gas_gmax2=sqldf("SELECT operator_name, MAX(ipmo_gas_sum),well_num FROM company_size_ip WHERE well_num=2")
gas_gmax3=sqldf("SELECT operator_name, MAX(ipmo_gas_sum),well_num FROM company_size_ip WHERE well_num=3")
gas_gmax4=sqldf("SELECT operator_name, MAX(ipmo_gas_sum),well_num FROM company_size_ip WHERE 4<=well_num AND well_num<=5")
gas_gmax5=sqldf("SELECT operator_name, MAX(ipmo_gas_sum),well_num FROM company_size_ip WHERE 6<=well_num AND well_num<=8")
gas_gmax6=sqldf("SELECT operator_name, MAX(ipmo_gas_sum),well_num FROM company_size_ip WHERE 9<=well_num AND well_num<=14")
gas_gmax7=sqldf("SELECT operator_name, MAX(ipmo_gas_sum),well_num FROM company_size_ip WHERE 15<=well_num AND well_num<=32")
gas_gmax8=sqldf("SELECT operator_name, MAX(ipmo_gas_sum),well_num FROM company_size_ip WHERE well_num>=33")

gas_gmax1
gas_gmax2
gas_gmax3
gas_gmax4
gas_gmax5
gas_gmax6
gas_gmax7
gas_gmax8


oil_size1=sqldf("SELECT operator_name, ipmo_oil_sum,well_num FROM company_size_ip WHERE well_num=1")
oil_size2=sqldf("SELECT operator_name, ipmo_oil_sum,well_num FROM company_size_ip WHERE well_num=2")
oil_size3=sqldf("SELECT operator_name, ipmo_oil_sum,well_num FROM company_size_ip WHERE well_num=3")
oil_size4=sqldf("SELECT operator_name, ipmo_oil_sum,well_num FROM company_size_ip WHERE 4<=well_num AND well_num<=5")
oil_size5=sqldf("SELECT operator_name, ipmo_oil_sum,well_num FROM company_size_ip WHERE 6<=well_num AND well_num<=8")
oil_size6=sqldf("SELECT operator_name, ipmo_oil_sum,well_num FROM company_size_ip WHERE 9<=well_num AND well_num<=14")
oil_size7=sqldf("SELECT operator_name, ipmo_oil_sum,well_num FROM company_size_ip WHERE 15<=well_num AND well_num<=32")
oil_size8=sqldf("SELECT operator_name, ipmo_oil_sum,well_num FROM company_size_ip WHERE 33<=well_num")

par(mfrow=c(2,2))
boxplot(oil_size1$ipmo_oil_sum)
boxplot(oil_size2$ipmo_oil_sum)
boxplot(oil_size3$ipmo_oil_sum)
boxplot(oil_size4$ipmo_oil_sum)
boxplot(oil_size5$ipmo_oil_sum)
boxplot(oil_size6$ipmo_oil_sum)
boxplot(oil_size7$ipmo_oil_sum)
boxplot(oil_size8$ipmo_oil_sum)

boxplot(subset(oil_size1$ipmo_oil_sum,oil_size1$ipmo_oil_sum<100))
boxplot(subset(oil_size2$ipmo_oil_sum,oil_size2$ipmo_oil_sum<100))
boxplot(subset(oil_size3$ipmo_oil_sum,oil_size3$ipmo_oil_sum<100))
boxplot(subset(oil_size4$ipmo_oil_sum,oil_size4$ipmo_oil_sum<300))
boxplot(subset(oil_size5$ipmo_oil_sum,oil_size5$ipmo_oil_sum<600))
boxplot(subset(oil_size6$ipmo_oil_sum,oil_size6$ipmo_oil_sum<1000))
boxplot(subset(oil_size7$ipmo_oil_sum,oil_size7$ipmo_oil_sum<5000))
boxplot(subset(oil_size8$ipmo_oil_sum,oil_size8$ipmo_oil_sum<10000))




gas_size_max=sqldf("SELECT operator_name, MAX(ipmo_gas_sum) as max_gas, well_num FROM company_size_ip GROUP BY well_num")
oil_size_max=sqldf("SELECT operator_name, MAX(ipmo_oil_sum) as max_oil, well_num FROM company_size_ip GROUP BY well_num")
write.table(oil_size_max,"/Users/majunting/Desktop/Bazean_Intern/oil_size.txt",col.names=TRUE)
#find outliers
#delete the first row which without operator_name
#company_size_ip=company_size_ip[-1,]

ip_oil=company_size_ip$ipmo_oil_sum
ip_gas=company_size_ip$ipmo_gas_sum
well_n=company_size_ip$well_num
fit_oil=lm(ip_oil~well_n)
fit_gas=lm(ip_gas~well_n)
library(outliers)
outlier(well_n)
library(car)
influencePlot(fit_gas)
influencePlot(fit_oil)
inf=lm.influence(fit_gas)
summary(well_n)
#Who is most likely to succeed in the future when drilling wells?
non_zero_gas=sqldf("SELECT * FROM comp_oil_gas WHERE sum_6_gas!=0  ")
non_zero_oil=sqldf("SELECT * FROM comp_oil_gas WHERE sum_6_oil!=0  ")
sum_26_gas=non_zero_gas$sum_12_gas-non_zero_gas$sum_6_gas
sum_26_oil=non_zero_oil$sum_12_oil-non_zero_oil$sum_6_oil
inc_rate_oil=sum_26_oil/non_zero_oil$sum_6_oil
inc_rate_gas=sum_26_gas/non_zero_gas$sum_6_gas
fut_oil=inc_rate_oil*sum_26_oil
fut_gas=inc_rate_gas*sum_26_gas
non_zero_oil$operator_name[which(fut_oil==max(fut_oil))]
non_zero_gas$operator_name[which(fut_gas==max(fut_gas))]
non_zero_gas[which(inc_rate_gas==max(inc_rate_gas)),]


#Who has the best performing production where more production is better?
dec_well_oil=sqldf("SELECT operator_name, ip_date, SUM(first_year_decline) AS dec_sum, SUM(cum_12_gas) AS sum_12_gas, SUM(cum_12_oil) AS sum_12_oil FROM well GROUP BY operator_name ORDER BY dec_sum ASC, sum_12_oil DESC")
dec_well_gas=sqldf("SELECT operator_name, ip_date, SUM(first_year_decline) AS dec_sum, SUM(cum_12_gas) AS sum_12_gas, SUM(cum_12_oil) AS sum_12_oil FROM well GROUP BY operator_name ORDER BY dec_sum ASC, sum_12_gas DESC")
dec_well_oil[1:10,]
dec_well_gas[1:10,]
#New wells performing better than old wells?
#two sample t-test
well_decline=well$first_year_decline
well_decline[is.na(well_decline)]=0
well$first_year_decline=well_decline
well$ip_date=as.Date(well$ip_date,"%m/%d/%y")
well$ip_date=as.Date(well$ip_date,"%m/%d/%y")
date_decline=sqldf("SELECT ip_date, first_year_decline, cum_12_gas, cum_12_oil FROM well WHERE ip_date IS NOT NULL ORDER BY ip_date DESC")
#date_decline[1:10,]
date_decline$ip_date=as.numeric(format(date_decline$ip_date,"%Y"))

old_well=sqldf("SELECT ip_date, first_year_decline,cum_12_gas, cum_12_oil FROM date_decline WHERE ip_date < 2004")
new_well=sqldf("SELECT ip_date, first_year_decline,cum_12_gas, cum_12_oil FROM date_decline WHERE ip_date> 2004")
old_num=sqldf("SELECT COUNT(*) FROM old_well")
new_num=sqldf("SELECT COUNT(*) FROM new_well")
old_dec=old_well$first_year_decline
new_dec=new_well$first_year_decline
old_oil=old_well$cum_12_oil
new_oil=new_well$cum_12_oil
old_gas=old_well$cum_12_gas
new_gas=new_well$cum_12_gas
t.test(old_dec,new_dec,alternative = c("greater"),var.equal=F)
t.test(old_oil,new_oil,alternative = c("less"),var.equal=F)
t.test(old_gas,new_gas,alternative = c("less"),var.equal=F)



