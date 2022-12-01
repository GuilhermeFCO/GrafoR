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

#' menorCaminho
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
menorCaminho <- function(x) {
	n <- length(x)
	L <- matrix(NA, nrow = n, ncol = n)
	R <- matrix(NA, nrow = n, ncol = n)

	for (i in 1:n) {
		for (j in 1:n) {
			if (i == j) L[i, j] <- 0
			else {
				flag = FALSE
				for (k in 1:length(x[[i]])) {
					if (x[[i]][[k]]$vertice == j) {
						flag = TRUE
						break
					}
				}
				if (flag) L[i, j] <- x[[i]][[k]]$peso
				else L[i, j] <- Inf
			}
			if (is.infinite(L[i, j])) R[i, j] <- 0
			else R[i, j] <- i
		}
	}

	for (k in 1:n) {
		for (i in 1:n) {
			for (j in 1:n) {
				if (L[i, j] > L[i, k] + L[k, j]) {
					L[i, j] <- L[i, k] + L[k, j]
					R[i, j] <- R[k, j]
				}
			}
		}
	}
	colnames(L) <- c(1:n)
	rownames(L) <- c(1:n)

	colnames(R) <- c(1:n)
	rownames(R) <- c(1:n)

	return(list(L, R))
}

#' excentricidade
#'
#' @param x
#' @param vertice
#'
#' @return
#' @export
#'
#' @examples
excentricidade <- function(x, vertice) {
	L <- menorCaminho(x)[[1]]
	vertices <- colnames(L)[L[vertice, ] == max(L[vertice, ])]
	distancias <- L[vertice, L[vertice, ] == max(L[vertice, ])]
	return(list(vertices, distancias))
}

#' raio
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
raio <- function(x) {
	L <- menorCaminho(x)[[1]]

	for (i in 1:nrow(L)) {
		aux <- excentricidade(x, i)
		vertices <- aux[[1]]
		distancias <- aux[[2]]

		if (i == 1) {
			min <- as.integer(distancias[1])
			minVertice <- paste0(i, " - ", vertices)
		} else {
			if (distancias[1] < min) {
				min <- as.integer(distancias[1])
				minVertice <- paste0(i, " - ", vertices)
			}
		}
	}

	return(list(min, minVertice))
}

#' diametro
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
diametro <- function(x) {
	L <- menorCaminho(x)[[1]]

	for (i in 1:nrow(L)) {
		aux <- excentricidade(x, i)
		vertices <- aux[[1]]
		distancias <- aux[[2]]

		if (i == 1) {
			max <- as.integer(distancias[1])
			maxVertice <- paste0(i, " - ", vertices)
		} else {
			if (distancias[1] > max) {
				max <- as.integer(distancias[1])
				maxVertice <- paste0(i, " - ", vertices)
			}
		}
	}

	return(list(max, maxVertice))
}

centro <- function(x) {

}
