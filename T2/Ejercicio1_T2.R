
# Datos -------------------------------------------------------------------

# Crear el dataset en R
Proteina_Alta_1 = c(11.44, 10.12, 10.59, 11.55, 9.90, 12.29, 10.88, 9.57)
Proteina_Alta_2 = c(11.18, 9.78, 10.64, 11.39, 9.85, 12.45, 11.30, 9.74)
Proteina_Baja_1 = c(11.22, 9.54, 9.98, 10.67, 10.06, 12.10, 11.26, 9.44)
Proteina_Baja_2 = c(1.00, 9.42, 10.08, 10.87, 10.21, 11.89, 10.83, 9.61)

alta = c(rbind(Proteina_Alta_1, Proteina_Alta_2))
bajo = c(rbind(Proteina_Baja_1, Proteina_Baja_2))

alta_c <- rep("Alt", length(alta))

bajo_c <- rep("Bajo", length(bajo))



df <- data.frame(valor = c(alta, bajo), 
              tratamiento = c(alta_c, bajo_c))

m_dca <- aov(formula = valor ~ tratamiento, data = df)

anova(m_dca)


# Ejercicio_2 -------------------------------------------------------------

# Datos de especies observadas por día en cada bosque
RC <- c(4, 2, 5, 2, 2, 1, 2, 3, 2, 4)
BSN <- c(10, 10, 12, 11, 10, 12, 14, 12, 14, 11)
EP <- c(1, 1, 2, 3, 1, 1, NA, 2, 2, 3)  # NA representa el valor faltante
BSA <- c(8, 9, 9, 5, 7, 8, 7, 4, 12, 9)

n <- length(RC)

rc_ <- rep("RC", n)
bsn_ <- rep("BSN", n)
ep_ <- rep("EP", n)
bsa_ <- rep("BSA", n)

df_2 <- data.frame(datos = c(RC, BSN, EP, BSA),
           trat = c(rc_, bsn_, ep_, bsa_))

m_dca_2 <- aov(formula = datos ~ trat, data = df_2)

shapiro.test(m_dca_2$residuals)

anova(m_dca_2)

# Varianza constante de los erres

library(lmtest)

bptest(m_dca_2)
