# Base de dados -----------------------------------------------------------
insurance <- read.csv('insurance.csv'); insurance

# histogramas -------------------------------------------------------------

(mfrow=c(1, 1))

par(mfrow=c(1, 4))

hist (
  insurance[, 'children'],
  main="Filhos",
  xlab='Filhos',
  ylab='Frequência Absoluta',
  col='red'
)

hist (
  insurance[, 'age'],
  main="Idade",
  xlab='Idade',
  ylab='Frequência Absoluta',
  col='blue'
)

hist (
  insurance[, 'bmi'],
  main="Imc",
  xlab='Imc',
  ylab='Frequência Absoluta',
  col='orange'
)

hist (
  insurance[, 'charges'],
  main="Valor Seguro",
  xlab='Valor Seguro',
  ylab='Frequência Absoluta',
  col='purple'
)

par(mfrow=c(1, 1))


# barplots ----------------------------------------------------------------

# age

age_faixa_tot <- insurance[insurance$age,]; age_faixa_tot
age_faixa1 <- insurance[insurance$age < 30, ]; age_faixa1
age_faixa2 <- insurance[insurance$age >= 30 & insurance$age < 40,]; age_faixa2
age_faixa3 <- insurance[insurance$age >= 40 & insurance$age < 50, ]; age_faixa3
age_faixa4 <- insurance[insurance$age >= 50, ]; age_faixa4

age_p1 <- (nrow(age_faixa1)/nrow(age_faixa_tot)) * 100; age_p1
age_p2 <- (nrow(age_faixa2)/nrow(age_faixa_tot)) * 100; age_p2
age_p3 <- (nrow(age_faixa3)/nrow(age_faixa_tot)) * 100; age_p3
age_p4 <- (nrow(age_faixa4)/nrow(age_faixa_tot)) * 100; age_p4

age_p_tot <- c(age_p1, age_p2, age_p3, age_p4); age_p_tot

barplot(age_p_tot, main='Percentual por Idade', xlab='Faixas',
        ylab='Percentual', col=c('lightgreen', 'lightblue', 'lightyellow', 'tomato'),
        legend.text = c('Menor que 30', 'Maior ou igual a 30 e menor que 40',
                        'Maior ou igual a 40 e menor que 50', 'Maior ou igual a 50'))

# bmi

bmi_faixa_tot <- insurance[insurance$bmi,]; bmi_faixa_tot
bmi_faixa1 <- insurance[insurance$bmi < 18.5, ]; bmi_faixa1
bmi_faixa2 <- insurance[insurance$bmi >= 18.5 & insurance$bmi < 24.9, ]; bmi_faixa2
bmi_faixa3 <- insurance[insurance$bmi >= 25 & insurance$bmi < 29.9, ]; bmi_faixa3
bmi_faixa4 <- insurance[insurance$bmi >= 30 & insurance$bmi < 39.9, ]; bmi_faixa4
bmi_faixa5 <- insurance[insurance$bmi >= 40, ]; bmi_faixa5

bmi_p1 <- (nrow(bmi_faixa1)/nrow(bmi_faixa_tot)) * 100; bmi_p1
bmi_p2 <- (nrow(bmi_faixa2)/nrow(bmi_faixa_tot)) * 100; bmi_p2
bmi_p3 <- (nrow(bmi_faixa3)/nrow(bmi_faixa_tot)) * 100; bmi_p3
bmi_p4 <- (nrow(bmi_faixa4)/nrow(bmi_faixa_tot)) * 100; bmi_p4
bmi_p5 <- (nrow(bmi_faixa5)/nrow(bmi_faixa_tot)) * 100; bmi_p5

bmi_p_tot <- c(bmi_p1, bmi_p2, bmi_p3, bmi_p4, bmi_p5); bmi_p_tot

barplot(bmi_p_tot, main='Percentual por Bmi', xlab='Faixas',
        ylab='Percentual', col=c('tomato', 'lightyellow', 'violet', 'lightblue', 'lightgreen'),
        legend.text = c('Magreza', 'Normal', 'Sobrepeso', 'Obesidade', 'Obesidade grave'))

# sex

sex_faixa_tot <- insurance[insurance$sex,]; sex_faixa_tot
sex_faixa1 <- insurance[insurance$sex == 'male', ]; sex_faixa1
sex_faixa2 <- insurance[insurance$sex == 'female', ]; sex_faixa2

sex_p1 <- (nrow(sex_faixa1)/nrow(sex_faixa_tot)) * 100; sex_p1
sex_p2 <- (nrow(sex_faixa2)/nrow(sex_faixa_tot)) * 100; sex_p2

sex_p_tot <- c(sex_p1, sex_p2); sex_p_tot

barplot(sex_p_tot, main='Percentual por Sexo', xlab='Faixas',
        ylab='Percentual', col=c('tomato', 'lightyellow'),
        legend.text = c('Masculino', 'Feminino'))

# smoker

smoker_faixa_tot <- insurance[insurance$smoker,]; smoker_faixa_tot
smoker_faixa1 <- insurance[insurance$smoker == 'yes', ]; smoker_faixa1
smoker_faixa2 <- insurance[insurance$smoker == 'no', ]; smoker_faixa2

smoker_p1 <- (nrow(smoker_faixa1)/nrow(smoker_faixa_tot)) * 100; smoker_p1
smoker_p2 <- (nrow(smoker_faixa2)/nrow(smoker_faixa_tot)) * 100; smoker_p2

smoker_p_tot <- c(smoker_p1, smoker_p2); smoker_p_tot

barplot(smoker_p_tot, main='Percentual por Fumantes', xlab='Faixas',
        ylab='Percentual', col=c('lightblue', 'lightgreen'),
        legend.text = c('Sim', 'Não'))

# children

children_faixa_tot <- insurance[insurance$children,]; children_faixa_tot
children_faixa1 <- insurance[insurance$children == 0, ]; children_faixa1
children_faixa2 <- insurance[insurance$children == 1, ]; children_faixa2
children_faixa3 <- insurance[insurance$children == 2, ]; children_faixa3
children_faixa4 <- insurance[insurance$children == 3, ]; children_faixa4
children_faixa5 <- insurance[insurance$children >= 4, ]; children_faixa5

children_p1 <- (nrow(children_faixa1)/nrow(children_faixa_tot)) * 100; children_p1
children_p2 <- (nrow(children_faixa2)/nrow(children_faixa_tot)) * 100; children_p2
children_p3 <- (nrow(children_faixa3)/nrow(children_faixa_tot)) * 100; children_p3
children_p4 <- (nrow(children_faixa4)/nrow(children_faixa_tot)) * 100; children_p4
children_p5 <- (nrow(children_faixa5)/nrow(children_faixa_tot)) * 100; children_p5

children_p_tot <- c(children_p1, children_p2, children_p3, children_p4, children_p5); children_p_tot

barplot(children_p_tot, main='Percentual por Filhos', xlab='Faixas',
        ylab='Percentual', col=c('tomato', 'lightyellow', 'violet', 'lightblue', 'lightgreen'),
        legend.text = c('0 Filhos', '1 Filhos', '2 Filhos', '3 Filhos', '4 ou mais filhos'))

# region

region_faixa_tot <- insurance[insurance$region,]; region_faixa_tot
region_faixa1 <- insurance[insurance$region == "southwest", ]; region_faixa1
region_faixa2 <- insurance[insurance$region == "southeast", ]; region_faixa2
region_faixa3 <- insurance[insurance$region == "northwest", ]; region_faixa3
region_faixa4 <- insurance[insurance$region == "northeast", ]; region_faixa4

region_p1 <- (nrow(region_faixa1)/nrow(region_faixa_tot)) * 100; region_p1
region_p2 <- (nrow(region_faixa2)/nrow(region_faixa_tot)) * 100; region_p2
region_p3 <- (nrow(region_faixa3)/nrow(region_faixa_tot)) * 100; region_p3
region_p4 <- (nrow(region_faixa4)/nrow(region_faixa_tot)) * 100; region_p4

region_p_tot <- c(region_p1, region_p2, region_p3, region_p4); region_p_tot

barplot(region_p_tot, main='Percentual por Região', xlab='Faixas',
        ylab='Percentual', col=c('tomato', 'lightyellow', 'violet', 'lightblue'),
        legend.text = c('southwest', 'southeast', 'northwest', 'northeast'))

# charges


charges_faixa_tot <- insurance[insurance$charges,]; charges_faixa_tot
charges_faixa1 <- insurance[insurance$charges < 2000, ]; charges_faixa1
charges_faixa2 <- insurance[insurance$charges >= 2000 & insurance$charges < 4000, ]; charges_faixa2
charges_faixa3 <- insurance[insurance$charges >= 4000 & insurance$charges < 8000, ]; charges_faixa3
charges_faixa4 <- insurance[insurance$charges >= 8000 & insurance$charges < 12000, ]; charges_faixa4
charges_faixa5 <- insurance[insurance$charges >= 12000, ]; charges_faixa5

charges_p1 <- (nrow(charges_faixa1)/nrow(charges_faixa_tot)) * 100; charges_p1
charges_p2 <- (nrow(charges_faixa2)/nrow(charges_faixa_tot)) * 100; charges_p2
charges_p3 <- (nrow(charges_faixa3)/nrow(charges_faixa_tot)) * 100; charges_p3
charges_p4 <- (nrow(charges_faixa4)/nrow(charges_faixa_tot)) * 100; charges_p4
charges_p5 <- (nrow(charges_faixa5)/nrow(charges_faixa_tot)) * 100; charges_p5

charges_p_tot <- c(charges_p1, charges_p2, charges_p3, charges_p4, charges_p5); charges_p_tot

barplot(charges_p_tot, main='Percentual por Charges', xlab='Faixas',
        ylab='Percentual', col=c('tomato', 'lightyellow', 'violet', 'lightblue', 'lightgreen'),
        legend.text = c('Menor que 2000', 'Entre 2000 e 4000', 
                        'Entre 4000 e 8000', 'Entre 8000 e 12000', 'Maior ou igual 12000'))


# boxplots ----------------------------------------------------------------

(mfrow=c(1, 1))

# region

par(mfrow=c(1, 4))

boxplot(
  insurance[insurance$region=='northwest', 'charges'],
  main="northwest",
  ylab='charges',
  col='lightblue'
)

boxplot(
  insurance[insurance$region=='southwest', 'charges'],
  main="southwest",
  ylab='charges',
  col='orange'
)

boxplot(
  insurance[insurance$region=='northeast', 'charges'],
  main="northeast",
  ylab='charges',
  col='lightgreen'
)

boxplot(
  insurance[insurance$region=='southeast', 'charges'],
  main="southeast",
  ylab='charges',
  col='lightyellow'
)

par(mfrow=c(1, 1))

# smoker 

par(mfrow=c(1, 2))

boxplot(
  insurance[insurance$smoker=='yes', 'charges'],
  main="sim",
  ylab='charges',
  col='violet'
)

boxplot(
  insurance[insurance$smoker=='no', 'charges'],
  main="não",
  ylab='charges',
  col='blue'
)

par(mfrow=c(1, 1))

# sex

par(mfrow=c(1, 2))

boxplot(
  insurance[insurance$sex=='male', 'charges'],
  main="masculino",
  ylab='charges',
  col='purple'
)

boxplot(
  insurance[insurance$sex=='female', 'charges'],
  main="feminino",
  ylab='charges',
  col='yellow'
)

par(mfrow=c(1, 1))

# age

median_age <- median(insurance$age)

par(mfrow=c(1, 2))

boxplot(
  insurance[insurance$age<median_age, 'charges'],
  main="idade < 39 (mediana)",
  ylab='charges',
  col='lightgray'
)

boxplot(
  insurance[insurance$age>=median_age, 'charges'],
  main="idade >= 39 (mediana)",
  ylab='charges',
  col='darkgray'
)

par(mfrow=c(1, 1))

# bmi

median_bmi <- median(insurance$bmi)

par(mfrow=c(1, 2))

boxplot(
  insurance[insurance$bmi<median_bmi, 'charges'],
  main="bmi < mediana (30.4)",
  ylab='charges',
  col='lightgreen'
)

boxplot(
  insurance[insurance$age>=median_bmi, 'charges'],
  main="bmi >= mediana (30.4)",
  ylab='charges',
  col='darkgreen'
)

par(mfrow=c(1, 1))

