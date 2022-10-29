ordem <- function(x) {
	return(length(x))
}

matrizIncidencia <- function(x) {
	matriz <- matrix(0, nrow = ordem(x), ncol = ordem(x))

	for (i in 1:length())
}

tamanho <- function(x) {
	soma = 0

	for (i in 1:length(x)) {
		soma = soma + length(x[[i]])
	}

	return(soma)
}
