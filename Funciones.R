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

# Función de resultados generales, por departamento, partido, circuito, hoja.
FUN_Votos <- function(data,
                      Departamento = 1:19,
                      Partido =
                        c("Partido Frente Amplio",
                          "Partido Nacional",
                          "Partido Colorado",
                          "Partido Cabildo Abierto",
                          "Partido Independiente",
                          "Partido Asamblea Popular",
                          "Partido de los Trabajadores",
                          "Partido Ecologista Radical Intransigente",
                          "Partido de la Gente",
                          "Partido Verde Animalista",
                          "Partido Digital"),
                      Circuito = FALSE,
                      Hoja = FALSE,
                      As.Arg = FALSE) {
  # La función debe devolver un Df con la lista de circuitos y el partido ganador
  # Como argumento de entrada, además de la base, debe aceptar un vector con los nombres de los departamentos
  # y también uno con el nombre del partido
  # Por defecto, devuelve todos los departamentos y los cuatro partidos principales
  
  stopifnot(is.logical(Circuito))
  stopifnot(is.logical(Hoja))
  
  
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
    Departamento <- which(Departamento == depto)
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
    
    if (length(Partido) == 1) {
      OUT <- OUT[OUT$LEMA == Partido, ]
    } else if (length(Partido) == 11) {
      OUT <- OUT
    } else{
      stop("Introduzca un solo partido")
    }
    
    return(OUT)
    
  } else{
    return(L)
    
  }
  
}


# Función para identificar el ganador por departamento
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

# Función para determinar el margen de victoria
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

# Función de resultados que combina las funciones win y winfor
Winner <- function(data){
        w1 <- win(data)
        w2 <- winfor(data)

        return(merge(w1, w2))
        
}


# Función para obtener las hojas más votadas
Hoja_win <- function(data, Departamento=1:19){
        # La función debe devolver una base con departamento, partido, lista más votada y votos obtenidos
        # Como argumento de entrada, además de la base, debe aceptar un vector con los nombres de los departamentos
        # Por defecto, devuelve todos los departamentos
        require("rlist") # No logré una alternativa que no requiera este paquete
        
        A <- aggregate (CNT_VOTOS ~ DEPTO + LEMA + HOJA, data, sum)
        l <- list()
        depto <- Vector_Departamentos()
        
        if(is.numeric(Departamento) ==FALSE & any(Departamento %in% depto)){ 
                Departamento <- which(Departamento == depto)
        }  # Traduce el nombre del depto en el indice en el vector
        
        
        for (i in Departamento){
                l[[i]] <- A[A$DEPTO == depto[i],] 
                #Crea una lista con una entrada por deparatamento
        }
        
        L <- list()
        for (i in Departamento){
                L[[i]] <-  l[[i]][order(l[[i]]$CNT_VOTOS, decreasing = T),] 
                # Ordena los datos en cada entrada de la lista (cada departamento)
                
        }
        
        D <- list()
        for (i in Departamento){
                D[[i]] <- rbind(L[[i]][L[[i]]$LEMA=="Partido Frente Amplio",][1,c(1:4)],
                                L[[i]][L[[i]]$LEMA=="Partido Nacional",][1,c(1:4)],
                                L[[i]][L[[i]]$LEMA=="Partido Colorado",][1,c(1:4)])
        } # Obtiene la lista más votada de cada lema requerido para el departamento i
        
        D <- rlist::list.rbind(D) # Transforma la lista en un Df
        rownames(D) <- NULL
        names(D) <- c("Departamento", "Partido", "Hoja", "Votos")
        return(D)
        
        
}


# Función que recupera los circuitos en los que el partido que resultó en nesimo lugar
Circ_nth <- function(data, Departamento=1:19, Partido = 
                       c("Partido Frente Amplio", "Partido Nacional", 
                         "Partido Colorado", "Partido Cabildo Abierto"), nth=1){
  # La función debe devolver un Df con la lista de circuitos y el partido ganador
  # Como argumento de entrada, además de la base, debe aceptar un vector con los nombres de los departamentos
  # y también uno con el nombre del partido
  # nth determina la posición en el ranking buscada
  # Por defecto, devuelve todos los departamentos y los cuatro partidos principales
  
  A <- aggregate (CNT_VOTOS ~ DEPTO + LEMA + CIRCUITO, data, sum)
  l <- list()
  depto <- Vector_Departamentos()
  
  if(is.numeric(Departamento) ==FALSE & any(Departamento %in% depto)){ 
    Departamento <- which(Departamento == depto)
  }  # Traduce el nombre del depto en el indice en el vector
  
  
  for (i in Departamento){
    l[[i]] <- A[A$DEPTO == depto[i],] 
    #Crea una lista con una entrada por deparatamento
  }
  
  L <- list()
  for (i in Departamento){
    L[[i]] <-  l[[i]][order(l[[i]]$CNT_VOTOS, decreasing = T),] 
    # Ordena los datos en cada entrada de la lista (cada departamento)
    
  }
  # Subdivide cada entrada de la lista por nro de circuito
  L <- lapply(L, function(L) split(L, L$CIRCUITO))
  L <- unlist(L, recursive=FALSE) #Para que quede una lista de un solo nivel
  
  X <- list()
  for (i in 1:length(L)) {
    X[[i]]<-L[[i]][nth,]
  }
  #Extrae la fila nro N de cada entrada de la lista
  
  OUT <-  rlist::list.rbind(X) # Reestructura los datos en un dataframe
  OUT <- OUT[OUT$LEMA==Partido,] # Filtra por Partido
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




