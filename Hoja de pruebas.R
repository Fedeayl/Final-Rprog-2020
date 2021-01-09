Circ_nth2 <- function(data, Departamento=1:19, Partido = 
                            c("Partido Frente Amplio", "Partido Nacional", 
                              "Partido Colorado", "Partido Cabildo Abierto"), nth=1){
        
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
        
        empate <- function(d=L){
                v <- vector()
                for (i in 1:length(d)){
                        v[i] <-  duplicated(d[[i]][, 4][nth+1])
                }
                emp <- which(v[i] == TRUE)
                emp
        } #Función para cubrir la posibilidad de existencia de empates en el nth lugar

        X <- list()
        Xe <- list()
        emp <- empate(L)
        
        for (i in 1:length(L)) {
                X[[i]] <- L[[i]][nth,]   #Extrae la fila nro N de cada entrada de la lista
                
                if(L[[i]] %in% emp == TRUE){
                        Xe[[i]] <- L[[i]][nth+1, ]
                } 
        }

        OUT <- c(X, Xe)
        OUT <-  rlist::list.rbind(OUT) # Reestructura los datos en un dataframe
        OUT <- OUT[OUT$LEMA==Partido,] # Filtra por Partido
        return(OUT)
}

AA <- Circ_nth2(Base, Partido="Partido Nacional", nth=2)

dim(AA)
dim(Circ_nth(Base, Partido = "Partido Nacional", nth=2))
names(AA)
rep <- repetido(AA)
which(rep ==TRUE)
duplicated(AA[[1000]][, 4])[3] == TRUE

AA[[1]] %in% which(rep != TRUE)

AA[[1000]][, 4][3]

AA[[5976]]

AA[[1]][, 4][3] %in% which(empate(AA)==TRUE)
unique(Base$LEMA)











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






Hoja_win <- function(data, Departamento=1:19){
        
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



