#=========================================================================#
#                         
#                         MODELAGEM ESTATÍSTICA                           #
#                        AULA 1       
#
#-------------------------------------------------------------------------#
#                                                                         #                                                   
#-------------------------------------------------------------------------#

#------------------------------------------------------------------------

# limpar variaveis
rm(list=ls())

#------------------------------------------------------------------------

# Definir diretório onde está o arquivo de dados.
setwd("F:\\UniCesumar\\HOME 2022\\Modelagem Estat?stica\\Aulas\\Aula 1")


data = read.csv(file ='PISA2015a.csv', 
                header = T, 
                fileEncoding="UTF-8-BOM",
                sep = ";", 
                na.strings = '..')

head(data) #listar 6 primeiras linhas do código


str(data)


names(data) #variáveis

data$Country     = as.factor(data$Country) #trocar tipo da variável
data$Series.Code = as.factor(data$Series.Code)


str(data)
#------------------------------------------------------------------------


#------------------------------------------------------------------------
install.packages("tidyverse")
library(tidyverse)



df <- data[1:1161, c(1, 4, 7)] %>%  
  spread(key=Series.Code, value=YR2015)  %>%  
  rename(MAT = LO.PISA.MAT,                        
            MAT.F = LO.PISA.MAT.FE,
            MAT.M = LO.PISA.MAT.MA,
            LEIT = LO.PISA.REA,
            LEIT.F = LO.PISA.REA.FE,
            LEIT.M = LO.PISA.REA.MA,
            CIENCIA = LO.PISA.SCI,
            CIENCIA.F = LO.PISA.SCI.FE,
            CIENCIA.M = LO.PISA.SCI.MA
) %>%
  drop_na()


head(df)

view(df)

#------------------------------------------------------------------------
#------------------------------------------------------------------------
# Medidas Descritivas

attach(df)

descritivas = function(x)list(
  Média=mean(x),
  Mediana=median(x),
  Max.Min=range(x),
  Amplitude=max(x) - min(x),
  Variância=var(x),
  DesvioPadr?o=sd(x),
  CoeficienteVariação=sd(x)/mean(x)*100,
  Quantis=quantile(x)
)


descritivas(MAT)

#------------------------------------------------------------------------

#------------------------------------------------------------------------
# Visualisar os dados!

install.packages("ggplot2")
library("ggplot2")

#Rank de Matem?tica por Pais

x11()
ggplot(data=df,aes(x=reorder(Country,MAT),y=MAT)) + 
  geom_bar(stat ='identity',aes(fill=MAT))+
  coord_flip() + 
  theme_grey() + 
  scale_fill_gradient(name="N?vel Pontua??o")+
  labs(title = 'Rank de Pa?ses por pontua??o em Matem?tica',
       y='Pontua??o',x='Pa?ses')+ 
  geom_hline(yintercept = mean(df$MAT),size = 1, color = 'blue')

#Rank de Matem?tica por Pais

x11()
ggplot(data=df,aes(x=reorder(Country,LEIT),y=LEIT)) + 
  geom_bar(stat ='identity',aes(fill=LEIT))+
  coord_flip() + 
  theme_grey() + 
  scale_fill_gradient(name="N?vel Pontua??o")+
  labs(title = 'Rank de Pa?ses por pontua??o em Leitura',
       y='Pontua??o',x='Pa?ses')+ 
  geom_hline(yintercept = mean(df$LEIT),size = 1, color = 'blue')

#------------------------------------------------------------------------
# BOX PLOT

# O BoxPlot precisa de d?as vari?veis

df2 = df[,c(1,3,4,6,7,9,10)] %>%   # Colunas relevantes 
  pivot_longer(c(2,3,4,5,6,7),names_to = 'Score')
view(df2) 



x11()
ggplot(data = df2, aes(x=Score,y=value, color=Score)) + 
  geom_boxplot()+
  scale_color_brewer(palette="Dark2") + 
  geom_jitter(shape=16, position=position_jitter(0.2))+
  labs(title = 'Maior pontua??o Masculino x Feminino',
       y='Pontua??o',x='Tipo do Teste')

#------------------------------------------------------------------------
# Calcular a porcentagem de diferen?a entre Masculino e Feminino

df = df %>% mutate(MAT.Diff = ((MAT.M - MAT.F)/MAT.F)*100,
                   LEIT.Diff = ((LEIT.M - LEIT.F)/LEIT.F)*100,
                   CIENCIA.Diff = ((CIENCIA.M - CIENCIA.F)/CIENCIA.F)*100,
                   Total.Score = MAT + LEIT + CIENCIA,
                   Avg.Diff = (MAT.Diff+LEIT.Diff+CIENCIA.Diff)/3
)
view(df)


##### MATHS SCORE #####
x11()
ggplot(data=df, aes(x=reorder(Country, MAT.Diff), y=MAT.Diff)) +
  geom_bar(stat = "identity", aes(fill=MAT.Diff)) +
  coord_flip() +
  theme_light() +
  geom_hline(yintercept = mean(df$MAT.Diff), size=1, color="black") +
  scale_fill_gradient(name="% N?vel de Diferen?a") +
  labs(title="Masculino x Feminino em Matem?tica", x="", 
       y="% diferen?a para Feminino")


##### Reading SCORE #####
x11()
ggplot(data=df, aes(x=reorder(Country, LEIT.Diff), y=LEIT.Diff)) +
  geom_bar(stat = "identity", aes(fill=LEIT.Diff)) +
  coord_flip() +
  theme_light() +
  geom_hline(yintercept = mean(df$LEIT.Diff), size=1, color="black") +
  scale_fill_gradient(name="% N?vel de Diferen?a") +
  labs(title="Masculino x Feminino em Leitura", x="", 
       y="% diferen?a para Feminino")






#------------------------------------------------------------------------
#------------------------------------------------------------------------
install.packages("tidyverse")
library("tidyverse")

# Banco de Dados diamonds
data(diamonds)


head(diamonds)

summary(diamonds)

attach(diamonds)

descritivas = function(x)list(
  M?dia=mean(x),
  Mediana=median(x),
  Max.Min=range(x),
  Amplitude=max(x) - min(x),
  Vari?ncia=var(x),
  DesvioPadr?o=sd(x),
  CoeficienteVaria??o=sd(x)/mean(x)*100,
  Quantis=quantile(x)
)

descritivas(carat)
descritivas(price)

#HISTOGRAMA - Pre?o
x11()
ggplot(data=diamonds, aes(x=price)) +
  geom_histogram(fill="steelblue", color="black", binwidth = 2000) +
  ggtitle("Histogram of Price Values")



x11()
ggplot(data=diamonds, aes(x=carat)) +
  geom_histogram(fill="steelblue", color="black", binwidth = 0.05) +
  ggtitle("Histogram of Price Values")


#DISPERS?O CARAT x Pre?o
x11()
ggplot(data=diamonds, aes(x=carat, y=price, color=cut)) + 
  geom_point()


#BOXPLOT PRE?O, separado pelo CORTE
x11()
ggplot(data=diamonds, aes(x=cut, y=price)) + 
  geom_boxplot(fill="steelblue")


# HISTOGRAMA POR CORTE
ggplot(data = diamonds, aes(price,fill= cut))+
  scale_fill_brewer(type = 'qual')+
  geom_histogram(bins = 100)+
  scale_x_log10()+
  facet_wrap(~color, ncol=3)
