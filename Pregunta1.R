
x <- c(506, 508, 499, 503, 504, 510, 497, 512, 514, 505, 493, 496, 506, 502, 509, 496)

# APARTADO A), intervalo de confianda para mu conociendo sigma

xbar <- mean(x)
sigma <- sqrt(25)
n <- length(x)
z005 <- qnorm(0.1/2, lower.tail=FALSE)

IC <- c(xbar-z005*sigma/(sqrt(n)),xbar+z005*sigma/(sqrt(n))); IC

# podemos utilizar esta libreria porque nos dan todos los resultados 
# del experimento, EN CASO CONTRARIO, LO DEBERÍAMOS HACER COMO ANTES

# install.packages("BSDA")
library(BDSA)
z.test(x, sigma.x = sigma, conf.level = 0.9) # nos da el intervalo de confianza


# APARTADO NUEVO, queremos probar la hipotesis de que las cajas tienen un 
# peso diferente a 500g
# H0: mu=500, con un nivel de confianza del 90%
# H1: mu != 500

# criterio de la región de rechazo

zc <- qnorm((1-0.9)/2, lower.tail=FALSE)
mu0 <- 500
zobs <- (xbar-mu0)/(sigma/(sqrt(n)))
zobs > zc # como es true, significa que mu0 está en la región de rechazo, 
# por tanto, rechazamos h0

z.test(x, sigma.x = sigma, conf.level = 0.9, mu=mu0) 
# forma rápida porque tenemos los resultados del experimento

# criterio del p-valor

pvalor <- 2*pnorm(zobs, lower.tail=FALSE); pvalor
# como el p-valor es menor a alpha 

# también tenemos el criterio de los IC, ya que si mu0 está dentro del 
# IC podemos aceptar H0, en caso contrario, la rechazamos

# hasta ahora mirábamos el contraste bilateral, ya que queríamos estudiar 
# la hipotesis de que las cajas tuviesen un peso diferente a 500g, pero si 
# mirásemos el contraste lateral a la derecha, el z.test sería:
z.test(x, sigma.x = sigma, conf.level = 0.9, mu=mu0, alternative='greater')

# ahora, para el contraste lateral a la izquierda, el z.test sería:
z.test(x, sigma.x = sigma, conf.level = 0.9, mu=mu0, alternative='less')


# APARTADO B)

# SEMIAMPLITUD DEL INTERVALO=m=2/2=1, ya que la longitud total es 2, 
# por tanto el error ha de ser menor a 1, y por lo tanto, según la fórmula 
# del intervalo de confianza el error es z(alpha/2)*sigma/sqrt(n). así pues,
# igualamos 1 a esa fórmula del error y sacamos n

n <- (sigma*qnorm(0.05/2, lower.tail=FALSE))^2; n 
# redondeamos a 97 que sería el mínimo número entero necesario de muestras


# APARTADO C)

t <- qt(0.01/2, df=n-1, lower.tail=FALSE); t
s <- sd(x); s
IC_sinsigma <- c(xbar-t*s/sqrt(n), xbar+t*s/sqrt(n)); IC_sinsigma
t.test(x, conf.level = 0.99) # lo mismo que el z.test pero ahora con la t, 
# porque se distribuye como una t de student

t.test(x, conf.level = 0.99, mu=500) # para estudiar la hipotesis de los 500g

# APARTADO D)
chi1 <- qchisq(0.05/2, df=n-1, lower.tail=FALSE)
chi2 <- qchisq(1-0.05/2, df=n-1, lower.tail=FALSE)
IC_var <- c((n-1)*var(x)/chi1, (n-1)*var(x)/chi2); IC_var
sqrt(IC_var)
