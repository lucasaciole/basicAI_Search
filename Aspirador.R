source("Estado.R")

## Classe e métodos para o problema do Mundo do Aspirador
Aspirador <- function(desc = NULL, pai = NULL){
  
  e <- environment()
  
  assign("desc", desc, envir = e)
  assign("pai", pai, envir = e)
  assign("g", 0, envir = e)
  assign("h", Inf, envir = e)
  assign("f", Inf, envir = e)
  
  class(e) <- c("Aspirador", "Estado")
  
  return(e)
}

## Sobrecarregando o operador "==" para comparação entre estados
Ops.Aspirador = function(obj1,obj2){
  if(.Generic == "=="){
    return(all(obj1$desc == obj2$desc))
  }
}

## Sobrecarga da função genérica "print" do R
print.Aspirador <- function(obj) {
  cat("(Q1 Q2 Q3 Q4 A): (", obj$desc, ")\n")
  cat("G(n): ", obj$g, "\n")
  cat("H(n): ", obj$h, "\n")
  cat("F(n): ", obj$f, "\n")
}

## Sobrecarga da função genérica "heuristica", definida por Estado.R
heuristica.Aspirador <- function(atual){
  
  if(is.null(atual$desc))
    return(Inf)
  ## k = quadrados sujos
  k = sum(desc[1:4])
  
  ## h(obj) = 2 * k || 2 k + 1
  if(as.numeric(desc[as.numeric(desc[5])]) == 1)
    return(2*k)
  
  return(2*k + 1)
}

## Definição de um método de custo para o problema do Aspirador
custo <- function(obj, ops){
  UseMethod("custo")
}

custo.default  <- function(obj, ops){
  print("Funcao Generica. Defina a função de custo para o seu problema!\n")
  return(NULL)
}
              
custo.Aspirador <- function(noh, ops){
  if (all(noh$desc - ops[[1]] == noh$pai$desc)) {
    return(noh$pai$g + 2)
    
  } else if (all(noh$desc - ops[[2]] == noh$pai$desc)) {
    return(noh$pai$g + 1)
  } else {
    return(noh$pai$g + 3)
  }
}

geraFilhos.Aspirador <- function(obj) {
  
  filhos <- list()
  
  filhosDesc <- list()
  
  desc <- obj$desc
  
  quadradoAtual <- as.numeric(desc[5])
  
  ## gera filhos usando todos os operadores  
  if(quadradoAtual == 1){
    
    operadores <- list(c(-1,0,0,0,0), c(0,0,0,0,1), c(0,0,0,0,2))
    
    filhosDesc <- lapply(operadores, function(op) desc+op)
    
  } else if(quadradoAtual == 2) {
    
    operadores <- list(c(0,-1,0,0,0), c(0,0,0,0,-1), c(0,0,0,0,2))
    
    filhosDesc <- lapply(operadores, function(op) desc+op)
  } else if(quadradoAtual == 3) {
    
    operadores <- list(c(0,0,-1,0,0), c(0,0,0,0,1), c(0,0,0,0,-2))
    
    filhosDesc <- lapply(operadores, function(op) desc+op)
  } else {
    
    operadores <- list(c(0,0,0,-1,0), c(0,0,0,0,-1), c(0,0,0,0-2))
    
    filhosDesc <- lapply(operadores, function(op) desc+op)
  }
  
  incompativeis <- sapply(1:length(filhosDesc),
                          function(i) {
                            fDesc <- filhosDesc[[i]]
                            if((any(fDesc[1:4] > 1)) ||     ##    #Quadrados > 1 OU
                               (any(fDesc[1:4] < 0)) ||     ##    #Quadrados < 0 OU
                               (any(fDesc[5] < 0))   ||     ##    #Aspirador não esta em nenhum quadrado
                               (any(fDesc[5] > 4)))         ##    #Aspirador não esta em nenhum quadrado
                              i ## é incompatível: retorna índice
                            else
                              0 ## senão é compatível
                          })
  
  ## Se houver incompativeis
  if (!all(incompativeis == 0)) {
    ## mantém no vetor apenas os que são incompatíveis
    incompativeis <- incompativeis[incompativeis != 0]
    
    ## remove estados filhos incompatíveis
    filhosDesc <- filhosDesc[-incompativeis]
  }
  ## gera os objetos Aspirador para os filhos
  for(filhoDesc in filhosDesc){
    filho <- Aspirador(desc = filhoDesc, pai = obj)
    filho$h <- heuristica(filho)
    filho$g <- custo(filho, operadores)
    filhos <- c(filhos, list(filho))
  }
  
  return(filhos)
}