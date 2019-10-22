debugSource("Aspirador.R")
debugSource("Estado.R")
debugSource("Aspirador.R")
debugSource("buscaDesinformada.R")
debugSource("buscaInformada.R")

inicial <- Aspirador(desc = c(Q1 = 0, Q2 = 1,Q3 = 1, Q4 = 1, A = 1))

objetivo <- Aspirador()
objetivo$desc <- c(Q1 = 0, Q2 = 0,Q3 = 0, Q4 = 0, A = 1)

cat("====\tBusca em Largura\t====\n")
print(unlist(buscaEmLargura(inicial, objetivo)))

cat("====\tBusca em Profundidade\t=====\n")
print(buscaEmProfundidade(inicial, objetivo))

cat("====\tBusca de Custo Uniforme\t=====\n")
print(buscaCustoUniforme(inicial, objetivo))

cat("====\tBusca Best-First (Gulosa)\t=====\n")
print(buscaBestFirst(inicial, objetivo, "Gulosa"))
 
cat("====\tBusca Best-First (A*)\t=====\n")
print(buscaBestFirst(inicial, objetivo, "AEstrela"))