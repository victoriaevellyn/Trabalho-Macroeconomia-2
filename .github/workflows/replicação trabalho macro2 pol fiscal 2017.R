####TRABALHO FINAL####       Victoria Evellyn C. M. Sousa  (15/0151110) 

#A frequencia dos dados é mensal para o periodo entre 1/2013 a 11/2017

#Instalando os Pacotes
install.packages("readxl")
install.packages("mFilter")
install.packages("vars")
install.packages("Metrics")

#Importando
library("readxl")
base = read_excel("C:\\Users\\Victória\\Documents\\Macro 2\\basetrab.xlsx", sheet = "Plan1", range='$b$1:$g$60')

basets <- ts(base, start=c(2001,1), end=c(2005,11), frequency=12)

ERP1 <- basets[,1]
ERP2 <- basets[,2]
ERP3 <- basets[,3]

EDLSP1 <- basets[,4]
EDLSP2 <- basets[,5]
EDLSP3 <- basets[,6]

plot(EDLSP1, main =' Expectativa dívida',ylab ='-',xlab = 'Tempo ', col='red', lty=1, type= 'l')
lines(EDLSP2, col= "BLUE")
lines(EDLSP3, col= "BLACK")


plot(ERP1, main =' Expectativa resultado primário',ylab ='-',xlab = 'Tempo ', col='red', lty=1, type= 'l')
lines(ERP2, col= "BLUE")
lines(ERP3, col= "BLACK")


dERP3 <- ERP3 -ERP2
# Criando a Base de Dados
data <- cbind(dERP3, EDLSP3)
library(vars)

# VAR livre 3 eq com Constante 
varLIVRE <- VAR(data, p=1, type=c("const"))
summary(varLIVRE)

#RMSE com constante
library(Metrics)
rmse(data[2:nrow(data),], fitted.values(varLIVRE))

# Funcao Impulso-Resposta das expectativas da dívida em VAR livre com constante
irfEDLSP3 <- irf(varLIVRE, impulse = c("EDLSP3"), response = c("EDLSP3"), n.ahead=48)
plot(irfEDLSP3)
irfEDLSP3

# Funcao Impulso-Resposta das expectativas do resultado primário na expectativa da dívida em VAR livre com constante
irfEDLSP3dERP3  <- irf(varLIVRE, impulse = c("dERP3"), response = c("EDLSP3"), n.ahead=48)
plot(irfEDLSP3dERP3)
irfEDLSP3dERP3


# Funcao Impulso-Resposta das expectativas da dívida nas expectativas do resultado primário em VAR livre com constante
irfdERP3EDLSP3  <- irf(varLIVRE, impulse = c("EDLSP3"), response = c("dERP3"), n.ahead=48)
plot(irfdERP3EDLSP3)
irfdERP3EDLSP3

# Funcao Impulso-Resposta das expectativas do resultado primário em VAR livre com constante
irfdERP3 <- irf(varLIVRE, impulse = c("dERP3"), response = c("dERP3"), n.ahead=48)
plot(irfdERP3)
irfdERP3

