x1 = rnorm(20,2.2,1)
x2 = rnorm(25,2.35,1)
x3 = rnorm(18,3.5,1)
x4 = rnorm(19,4,1)

datos = data.frame(valor = c(x1,x2,x3,x4),trat=rep(c(1,2,3,4),c(20,25,18,19)))

model = aov(valor~trat,data=datos)

resumen_bonferroni=pairwise.t.test(datos$valor,datos$trat,p.adjust.method = "bonferroni")

LSD.test(model,trt="trat",p.adj = "bonferroni",console=T)
