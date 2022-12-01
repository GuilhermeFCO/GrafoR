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

#' distanciaCaminhoMinimo
#'
#' @param x
#' @param vertice
#' @param verticeDestino
#'
#' @return
#' @export
#'
#' @examples
distanciaCaminhoMinimo <- function(x, vertice, verticeDestino) {
	aux <- menorCaminho(x)
	L <- aux[[1]]
	R <- aux[[2]]
	caminho <- c(verticeDestino)
	atual <- verticeDestino
	while (atual != vertice) {
		caminho <- c(caminho, R[vertice, atual])
		atual <- R[vertice, atual]
	}
	caminho <- rev(caminho)
	return(list(L[vertice, verticeDestino], caminho))
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
	distancia <- L[vertice, L[vertice, ] == max(L[vertice, ])][1]
	return(list(vertices, distancia))
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
	for (i in 1:length(x)) {
		aux <- excentricidade(x, i)
		vertices <- aux[[1]]
		distancia <- aux[[2]]

		if (i == 1) {
			min <- as.integer(distancia)
			minVertice <- i
			destinoVertices <- vertices
			count = 2
		} else {
			if (distancia < min) {
				min <- as.integer(distancia)
				minVertice <- list(list(i, vertices))
				count <- 2
			} else if (distancia == min) {
				minVertice[[count]] <- list(i, vertices)
				count = count + 1
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
	for (i in 1:length(x)) {
		aux <- excentricidade(x, i)
		vertices <- aux[[1]]
		distancia <- aux[[2]]

		if (i == 1) {
			max <- as.integer(distancia)
			maxVertice <- list(list(i, vertices))
			count = 2
		} else {
			if (distancia > max) {
				max <- as.integer(distancia)
				maxVertice <- list(list(i, vertices))
				count = 2
			} else if (distancia == max) {
				maxVertice[[count]] <- list(i, vertices)
				count = count + 1
			}
		}
	}

	return(list(max, maxVertice))
}

#' centro
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
centro <- function(x) {
	vertices <- raio(x)[[2]]
	verticesCentro <- c()
	for (i in 1:length(vertices)) {
		verticesCentro <- c(verticesCentro, vertices[[i]][[1]])
	}
	return(verticesCentro)
}

#' buscaProfundidade
#'
#' @param x
#' @param vertice
#' @param visitados
#' @param arestasOut
#' @param matriz
#'
#' @return
#' @export
#'
#' @examples
buscaProfundidade <- function(x, vertice, visitados = c(), arestasOut = c(), matriz = NULL) {
	visitados <- c(visitados, vertice)

	if (is.null(matriz)) {
		matriz <- matrix(0, nrow = length(x), ncol = length(x))
	}

	for (v in vizinhos(x, vertice)) {
		if (!(v %in% visitados)) {
			matriz[vertice, v] <- 1
			matriz[v, vertice] <- 1
			aux <- buscaProfundidade(x, v, visitados, arestasOut, matriz)
			visitados <- aux[[1]]
			matriz <- aux[[2]]
			arestasOut <- aux[[3]]
		} else {
			if (matriz[vertice, v] == 0) {
				if (vertice < v) {
					arestasOut <- c(arestasOut, paste0(vertice, " - ", v))
				} else {
					arestasOut <- c(arestasOut, paste0(v, " - ", vertice))
				}
				matriz[vertice, v] <- 1
				matriz[v, vertice] <- 1
			}
		}
	}

	return(list(visitados, matriz, arestasOut))
}

#' centralidade
#'
#' @param x
#' @param vertice
#'
#' @return
#' @export
#'
#' @examples
centralidade <- function(x, vertice) {
	sum = 0
	for (i in (1:length(x))[-vertice]) {
		sum = sum + distanciaCaminhoMinimo(x, vertice, i)[[1]]
	}
	return((length(x)-1)/sum)
}
