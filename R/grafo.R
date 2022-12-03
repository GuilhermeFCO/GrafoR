#' grafo
#'
#' @param nVertices
#' @param arestas
#'
#' @return
#' @export
#'
#' @examples
grafo <- function(nVertices, arestas) {
	novoGrafo <- list()

	for (i in 1:nVertices) {
		novoGrafo <- append(novoGrafo, list(list()))
	}
	names(novoGrafo) <- c(1:nVertices)

	for (i in 1:length(arestas)) {
		aresta <-	stringr::str_split(arestas[i], pattern = " ")[[1]] %>% as.numeric()

		novoGrafo[[aresta[1]]][[length(novoGrafo[[aresta[1]]])+1]] <- list(vertice=aresta[2], peso=aresta[3])

		novoGrafo[[aresta[2]]][[length(novoGrafo[[aresta[2]]])+1]] <- list(vertice=aresta[1], peso=aresta[3])
	}

	attr(novoGrafo, "class") <- "grafo"
	return(novoGrafo)
}

#' printGrafo
#'
#' @param x
#' @param pesos
#'
#' @return
#' @export
#'
#' @examples
printGrafo <- function(x, pesos = FALSE) {
	cat("\n\n")
	if (!pesos) {
		for (i in 1:length(x)) {
			cat(paste0("[", names(x)[i], "] -> "))

			for (j in 1:length(x[[i]])) {
				cat(paste0(x[[i]][[j]]$vertice, " -> "))
			}

			cat("≡\n\n")
		}
	} else {
		for (i in 1:length(x)) {
			cat(paste0("[", names(x)[i], "] -> "))

			for (j in 1:length(x[[i]])) {
				cat(paste0(x[[i]][[j]]$vertice, " (", x[[i]][[j]]$peso, ")", " -> "))
			}

			cat("≡\n\n")
		}
	}
}

#' grafoDeArquivo
#'
#' @param path
#'
#' @return
#' @export
#'
#' @examples
grafoDeArquivo <- function(path) {
	arquivo <- readLines(path)

	return(grafo(arquivo[1] %>% as.numeric(), arquivo[2:length(arquivo)]))
}

#' grafoToFile
#'
#' @param x
#' @param path
#'
#' @return
#' @export
#'
#' @examples
grafoToFile <- function(x, path) {
	cat(ordem(x), sep = "\n", file = path, append = FALSE)
	matriz <- matrizAdjacencia(x)[[2]]
	for (i in 1:nrow(matriz)) {
		for (j in i:ncol(matriz)) {
			if (matriz[i, j] != 0) {
				cat(paste0(i, " ", j, " ", matriz[i, j]), sep = "\n", append = TRUE, file = path)
			}
		}
	}
}

#' grafoFromJSON
#'
#' @param path
#' @param pathToSave
#'
#' @return
#' @export
#'
#' @examples
grafoFromJSON <- function(path, pathToSave) {
	x <- jsonlite::read_json(path)
	vertices <- c()
	for (i in 1:x$data$edges$length) {
		aux <- x$data$edges$`_data`[[i]]
		vertices <- c(vertices, paste0(aux$from, " ", aux$to, " ", aux$label))
	}
	x <- grafo(x$data$nodes$length, vertices)
	grafoToFile(x, pathToSave)
}

#' grafoToJSON
#'
#' @param path
#' @param pathToSave
#'
#' @return
#' @export
#'
#' @examples
grafoToJSON <- function(path, pathToSave) {
	x <- grafoDeArquivo(path)
	aux <- get("json")
	aux$data$nodes$length <- ordem(x)
	aux$data$edges$length <- tamanho(x)

	for (i in 1:ordem(x)) {
		aux$data$nodes$`_data`[[i]] <- list("id" = i, "label" = i)
	}
	names(aux$data$nodes$`_data`) <- 1:ordem(x)
	matriz <- matrizAdjacencia(x)
	count <- 1

	for (i in 1:nrow(matriz)) {
		for (j in i:nrow(matriz)) {
			if (matriz[i, j] == 1) {
				for (k in 1:length(x[[i]])) {
					if (x[[i]][[k]]$vertice == j) {
						aux$data$edges$`_data`[[count]] <- list("from" = i, "to" = j, "id" = count, "color" = list(), "label" = x[[i]][[k]]$peso)
						break
					}
				}
				count = count + 1
			}
		}
	}
	names(aux$data$edges$`_data`) <- 1:tamanho(x)

	aux <- jsonlite::toJSON(aux, auto_unbox = TRUE)
	write(aux, pathToSave)
}
