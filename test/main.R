remotes::install_github("GuilhermeFCO/grafoR")
library(grafoR)

x <- grafo(
	6,
	c(
		"1 2 1",
		"1 3 3",
		"2 3 1",
		"2 4 3",
		"2 5 2",
		"3 4 2",
		"4 5 3",
		"4 6 2",
		"5 6 3"
	)
)

y <- grafoDeArquivo("../../TP1-Guilherme-3398/exemplo")

grafoR::ordem(y)
grafoR::tamanho(y)
grafoR::printGrafo(y)
grafoR::printGrafo(y, pesos = TRUE)
grafoR::vizinhos(y, 5)
grafoR::grau(x, 2)
grafoR::sequenciaGraus(y)

