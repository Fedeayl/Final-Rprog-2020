# Función para asignar un vector con el nombre de los departamentos según el orden de la Corte Electoral
Vector_Departamentos <- function() {
  V <- c(
    "Montevideo",
    "Canelones",
    "Maldonado",
    "Rocha",
    "Treinta y Tres",
    "Cerro Largo",
    "Rivera",
    "Artigas",
    "Salto",
    "Paysandu",
    "Rio Negro",
    "Soriano",
    "Colonia",
    "San Jose",
    "Flores",
    "Florida",
    "Durazno",
    "Lavalleja",
    "Tacuarembo"
  )
  V
  
}

# Función para asignar un vector con el nombre de los partidos según el orden de la Corte Electoral
Vector_Partidos <- function() {
  V <- c(
    "Partido Frente Amplio",
    "Partido Nacional",
    "Partido Colorado",
    "Partido Independiente",
    "Partido Asamblea Popular",
    "Partido de los Trabajadores",
    "Partido Ecologista Radical Intransigente",
    "Partido de la Gente",
    "Partido Verde Animalista",
    "Partido Digital",
    "Partido Cabildo Abierto"
  )
  V
  
}


# Función de resultados generales, por departamento, partido, circuito, hoja.
FUN_Votos <- function(data,
                      Departamento = 1:19,
                      Partido = Vector_Partidos(),
                      Circuito = FALSE,
                      Hoja = FALSE,
                      As.Arg = FALSE) {
  
  #Mensajes de error - argumentos inadecuados
  stopifnot(is.logical(Circuito))
  stopifnot(is.logical(Hoja))
  stopifnot(is.logical(As.Arg))
  
  depto <- Vector_Departamentos()
  if (is.numeric(Departamento) == FALSE){
  
    for (i in length(Departamento)){
      if (Departamento[i] %in% depto ==FALSE) stop("Ingrese un departamento válido (sin tildes)")
    } 
    
  }
  
  partidos <- Vector_Partidos()
  if (is.numeric(Partido) == FALSE){
    for (i in length(Partido)){
      if (Partido[i] %in% partidos ==FALSE) {
        stop(paste("Ingrese un partido válido:", paste(partidos, collapse=", "), sep= " "))
        } 
    }
    
  }
  
  # Definición de los niveles de agregación
  
  if (Circuito == FALSE & Hoja == FALSE) {
    A <- aggregate (CNT_VOTOS ~ DEPTO + LEMA, data, sum)
    
  } else if (Circuito == TRUE & Hoja == FALSE) {
    A <- aggregate (CNT_VOTOS ~ DEPTO + LEMA + CIRCUITO, data, sum)
    
  } else if (Circuito == FALSE & Hoja == TRUE) {
    A <- aggregate (CNT_VOTOS ~ DEPTO + LEMA + HOJA, data, sum)
    
  } else {
    A <- aggregate (CNT_VOTOS ~ DEPTO + CIRCUITO + LEMA + HOJA, data, sum)
  }
  
  l <- list()
  
  depto <- Vector_Departamentos()
  
  if (is.numeric(Departamento) == FALSE &
      any(Departamento %in% depto)) {
    Departamento <- which(depto %in% Departamento)
  }  # Traduce el nombre del depto en el indice en el vector
  
  
  for (i in Departamento) {
    l[[i]] <- A[A$DEPTO == depto[i], ]
    #Crea una lista con una entrada por deparatamento
  }
  
  L <- list()
  for (i in Departamento) {
    L[[i]] <-  l[[i]][order(l[[i]]$CNT_VOTOS, decreasing = T), ]
    # Ordena los datos en cada entrada de la lista (cada departamento)
    
  }
  
  
  if (As.Arg == FALSE) {
    L <-  rlist::list.rbind(L)
    OUT <- L
      
    OUT <- OUT[OUT$LEMA %in% Partido, ] # Subset por partido
    
    return(OUT)
    
  } else{
    return(L) # Si As.Arg = TRUE devuelve una lista
    
  }
  
}


# Función que devuelve el partido ganador y el márgen de victoria por departamento
Winner <- function(data){
  
      win <- function(data){
    L <- FUN_Votos(data, As.Arg = TRUE) # Utiliza la función base para estructurar los datos
    v <- character()
    depto <- Vector_Departamentos()
    
    # Extrae el ganador por departamento
    for(i in 1:length(L)){
      v[i] <-  L[[i]][c(2,3)][1,1]
    }
    
    # Arma un Df con el nombre del Depto y el partido más votado
    V <- as.data.frame(cbind(depto[1:length(v)],v)) 
    
    names(V) <- c("Depto", "Ganador")
    
    
    return(V)
    
  }
      winfor <- function(data){
    L <- FUN_Votos(data, As.Arg = TRUE) # Utiliza la función base para estructurar los datos
    v <- numeric()
    depto <- Vector_Departamentos()
    
    
    # Aprovechando el Df ordeando, extraer el primer y el segundo valor de cada entrada
    # Luego calcula el porcentaje, con dos decimales
    for(i in 1:length(L)){
      v[i] <-round(100*(L[[i]]$CNT_VOTOS[1]-L[[i]]$CNT_VOTOS[2])/
                     sum(L[[i]]$CNT_VOTOS),2)
    }
    V <- as.data.frame(cbind(depto[1:length(v)], v))
    names(V) <- c("Depto", "Margen")
    return(V)
    
  }
        
      w1 <- win(data)
      w2 <- winfor(data)
      
      return(merge(w1, w2))
        
}


# Función para obtener las hojas más votadas
Hoja_win <- function(data, 
                     Departamento = Vector_Departamentos(), 
                     Partido = c("Partido Frente Amplio", "Partido Nacional", "Partido Colorado")){
  
  A <- FUN_Votos(Base, Departamento, Partido, Hoja = TRUE)
  A <- split(A, A$DEPTO)
  
  l <- list()
  
  if (length(Partido) != 1){
    A <- lapply(A, function(A) split(A, A$LEMA))
    A <- unlist(A, recursive=FALSE)
    
    for (i in 1:(length(Departamento)*length(Partido))){
      l[[i]]<-A[[i]][1,]
    }
    
    
  } else { 
    
    for (i in 1:(length(Departamento))){
      l[[i]]<-A[[i]][1,] }
  }
  
  OUT <-  rlist::list.rbind(l)
  return(OUT)
}



# Función que recupera los circuitos en los que el partido que resultó en nesimo (nth) lugar
Circ_nth <- function(data, Departamento = Vector_Departamentos(), 
                      Partido = c("Partido Frente Amplio", "Partido Nacional", 
                                  "Partido Colorado", "Partido Cabildo Abierto"), 
                      nth=1){
  
  A <- FUN_Votos(Base, Departamento, Circuito = TRUE)
  A <- split(A, A$DEPTO)
  A <- lapply(A, function(A) split(A, A$CIRCUITO))
  A <- unlist(A, recursive=FALSE)
  
  l <- list()
  
  for (i in 1:length(A)){
    l[[i]]<-A[[i]][nth, ]
    
  }
  
  OUT <-  rlist::list.rbind(l)
  OUT <- OUT[OUT$LEMA %in% Partido, ]
  return(OUT)
}




#Estadisticos de resumen de la distribución

Estadisticos <- function(x){
  
  stopifnot(is.numeric(x))
  
      v <- c(min(x), 
             round(mean(x, na.rm = T),2), 
             median(x, na.rm = T), 
             max(x), 
             round(sd(x, na.rm = T),2),
             length(x))
     
       vc <- c("Mínimo", "Media", "Mediana", "Máximo", "D.Estandar", "N")
      
       V <- as.data.frame(cbind(vc, v))
       names(V) <- c("Estadístico", "Valor")
      
      V
}




