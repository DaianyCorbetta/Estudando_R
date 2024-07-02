#
#1. Identificar o banco de dados CASchools dentro do pacote “AER” do software R.------#
#
#
install.packages("AER")
library(AER)
#carrega os dados, 
data("CASchools")
#exibe todo o conjunto de dados
CASchools

#----------------------------------------------------------------------------
#
#. Aprender sobre o conjunto de dados:
  #descreva esse conjunto — identificando o assunto, 
  #quais são as variáveis exibidas e 
  #qual é o tipo dessas variáveis (se são numéricas, ou categóricas, ou binárias etc.).

help("CASchools")
names(CASchools) 

dim(CASchools)
head(CASchools)
CASchools[1:3,] #lista primeiras 3 linhas do banco
str(CASchools)

CASchools$district = as.factor(CASchools$district) #mudar tipo variável
str(CASchools)

CASchools$school = as.factor(CASchools$school)
str(CASchools)

#-----------DEFININDO AS VARIÁVEIS -----------------#
county_data <- CASchools$county
students_data <- CASchools$students
math_data <- CASchools$math
read_data <- CASchools$read
income_data <- CASchools$income
lunch_data <- CASchools$lunch


#----------------MEDIDAS DESCRITIVAS----------------
summary(CASchools)

#PARA FACTOR
county_data <- CASchools$county
table(county_data)
county_data
prop.table(county_data)
nlevels(county_data)#numero de fatores

#---------------
descritivas = function(x)list( #não rodou
  Média=mean(x),
  Mediana=median(x),
  Max.Min=range(x),
  Amplitude=max("students_data") - min("students_data"),
  Variância=var(x),
  DesvioPadrão=sd(x), 
  CoeficienteVariação=sd(x)/mean(x)*100, 
  Quantis=quantile(x)
)
descritivas("students_data")

#-----------------GRAFICOS------------------------------#
plot(income_data)
plot(students_data)
hist(students_data)
boxplot(students_data)
plot(math_data)
hist(math_data)
boxplot(math_data)
prop.table(student_data)

library(stats) # Para correlação
library(ggplot2) # Para visualização

qplot(math_data,income_data)
qplot(math_data,lunch_data, data = CASchools)

