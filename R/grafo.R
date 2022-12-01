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
#' @param caminho
#'
#' @return
#' @export
#'
#' @examples
grafoDeArquivo <- function(caminho) {
	arquivo <- readLines(caminho)

	return(grafo(arquivo[1] %>% as.numeric(), arquivo[2:length(arquivo)]))
}
