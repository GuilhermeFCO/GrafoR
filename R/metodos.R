#' ordem
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
ordem <- function(x) {
	return(length(x))
}

#' Title
#'
#' @param x
#'
#' @return
#@export
#'
#' @examples
matrizAdjacencia <- function(x) {
	matriz <- matrix(0, nrow = ordem(x), ncol = ordem(x))

	for (i in 1:length(x)) {
		if (length(x[[i]])) {
			for (j in 1:length(x[[i]])) {
				matriz[i, x[[i]][[j]]$vertice] <- 1
			}
		}
	}

	return(matriz)
}

#' tamanho
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
tamanho <- function(x) {
	matriz <- matrizAdjacencia(x)

	soma = 0

	for (i in 1:nrow(matriz)) {
		for (j in i:ncol(matriz)) {
			soma = soma + matriz[i, j]
		}
	}

	return(soma)
}

#' vizinhos
#'
#' @param x
#' @param vertice
#'
#' @return
#' @export
#'
#' @examples
vizinhos <- function(x, vertice) {
	verticesVizinhos <- c()

	if (length(x[[vertice]])) {
		for (i in 1:length(x[[vertice]])) {
			verticesVizinhos <- c(verticesVizinhos, x[[vertice]][[i]]$vertice)
		}
	}

	return(verticesVizinhos)
}

#' grau
#'
#' @param x
#' @param vertice
#'
#' @return
#' @export
#'
#' @examples
grau <- function(x, vertice) {
	return(length(x[[vertice]]))
}

#' sequenciaGraus
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
sequenciaGraus <- function(x) {
	graus <- c()

	for (i in 1:length(x)) {
		graus <- c(graus, length(x[[i]]))
	}

	graus <- sort(graus, decreasing = TRUE)

	return(graus)
}
