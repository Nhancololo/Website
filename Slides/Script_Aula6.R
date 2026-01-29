'---------------------------------------------------------------------
                        Script Dados da área
---------------------------------------------------------------------'


'--------------------------------------------------------------------
              Limpeza do seu ambiente da trabalho
---------------------------------------------------------------------'
rm(list = ls)
gc()


'-------------------------------------------------------------------
              Rode os pacotes (onde vem if ... e depois 
        Ignore o que segue qdo estiver usando seus dados
--------------------------------------------------------------------'
if (!require("pacman")) install.packages("pacman")
pacman::p_load(sf, spdep, ggplot2, patchwork, dplyr, geodata)

# Baixar dados dos EUA (Nível 1 = Estados)
usa_sf <- tryCatch({
  # Tenta baixar direto
  usa_vect <- geodata::gadm(country = "USA", level = 1, path = tempdir(), version="latest")
  sf::st_as_sf(usa_vect)
}, error = function(e) {
  message("Erro ao baixar dados, gadm está com problemas, baixa direto no site: https://gadm.org/maps.html.")
})

# FILTRAR apenas Utah, Colorado, Arizona, New Mexico
four_corners <- usa_sf %>% 
  filter(NAME_1 %in% c("Utah", "Colorado", "Arizona", "New Mexico")) %>%
  st_make_valid()

# Extrair centroides para o grafo
coords <- suppressWarnings(st_coordinates(st_centroid(four_corners))) #suppressWarnings() era para tirar
coords_df <- as.data.frame(coords)


theme_comp <- theme_void() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "bottom")+
  theme(legedn.title=element_text(hjust=0.5))

'------------------------ Comece aqui ------------------------------------------'


'-----------------------------------------------------------------------------------
                      Tipo Rook (Torre)
------------------------------------------------------------------------------------'
nb_rook <- poly2nb(four_corners, queen = FALSE)
nb_lines_rook <- nb2lines(nb_rook, coords = coords, as_sf = TRUE)
st_crs(nb_lines_rook) <- st_crs(four_corners)

p_rook <- ggplot() +
  geom_sf(data = four_corners, fill = "white", color = "gray20", linewidth = 0.5) +
  geom_sf(data = nb_lines_rook, aes(color = "Conexão (Rook)"), linewidth = 1.2) +
  geom_point(data = coords_df, aes(X, Y), size = 3) +
  geom_sf_text(data = four_corners, aes(label = NAME_1), size = 3, nudge_y = -0.5, nudge_x=1) +
  scale_color_manual(values = "firebrick", name = "") +
  labs(title = "Critério Torre (Rook)", 
       subtitle = "Utah e New Mexico; Colorado e Arizona\n śo tem 1 ponto em comum") +
  theme_comp


'---------------------------------------------------------------------------------
                            Tipo Queen
----------------------------------------------------------------------------------'
#identificar quais polígonos são vizinhos e constroir lista de vizinhos
nb_queen <- poly2nb(four_corners, queen = TRUE)

#criar segmentos de reta ligando os centroides das áreas vizinhas
nb_lines_queen <- nb2lines(nb_queen, coords = coords, as_sf = TRUE)
st_crs(nb_lines_queen) <- st_crs(four_corners) # atribuir a nb_lines_queen CRS igual do four_corners


p_queen <- ggplot() +
  geom_sf(data = four_corners, fill = "white", color = "gray20", linewidth = 0.5) +
  geom_sf(data = nb_lines_queen, aes(color = "Conexão (Queen)"), linewidth = 1.2) +
  geom_point(data = coords_df, aes(X, Y), size = 3) +
  geom_sf_text(data = four_corners, aes(label = NAME_1), size = 3, nudge_y = -0.5, nudge_x=1) +
  scale_color_manual(values = "steelblue", name = "") +
  labs(title = "Critério Rainha (Queen)", 
       subtitle = "Utah, Colorado, Arizona, New Mexico\n tem 1 ponto em comum") +
  theme_comp

'-----------------------------------------------------------------------------------
                      Vizinhança por distancia
------------------------------------------------------------------------------------'

if (!require("pacman")) install.packages("pacman")
pacman::p_load(sf, spdep, ggplot2, geobr, dplyr, patchwork)

# Baixar mapa municipal de Mato Grosso (MT)
mt_sf <- read_municipality(code_muni = "MT", year = 2020, showProgress = FALSE)

coords_mt <- suppressWarnings(st_coordinates(st_centroid(mt_sf)))

theme_map <- theme_void() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
        plot.subtitle = element_text(hjust = 0.5, size = 14))

'----------------------------------------------------------------------------------
                                      k-vizinhos
-----------------------------------------------------------------------------------'

# Calcular os k=4 vizinhos mais próximos
k <- 4
knn_nb <- knearneigh(coords_mt, k = k)
nb_knn <- knn2nb(knn_nb)

# Converter para linhas espaciais para plotar
lines_knn <- nb2lines(nb_knn, coords = coords_mt, as_sf = TRUE)
st_crs(lines_knn) <- st_crs(mt_sf)

ggplot() +
  geom_sf(data = mt_sf, fill = "gray95", color = "gray80") +
  geom_sf(data = lines_knn, color = "purple", linewidth = 0.5, alpha = 0.6) +
  geom_point(data = as.data.frame(coords_mt), aes(X, Y), size = 0.8) +
  labs(title = paste0("k-Vizinhos Mais Próximos (k=", k, ")"),
       subtitle = "Cada município conecta-se aos 4 centroides mais próximos") +
  theme_map


'-----------------------------------------------------------------------------------------
                                Limiar
-----------------------------------------------------------------------------------------'

# Para precisão, vamos projetar para SIRGAS 2000 / Brazil Polyconic (EPSG 5880) para usar metros.
mt_proj <- st_transform(mt_sf, 5880)
coords_proj <- st_coordinates(st_centroid(mt_proj))

# Definir raio de 120 km (120000 metros)
dist_nb <- dnearneigh(coords_proj, 0, 120000)

# Converter para linhas
lines_dist <- nb2lines(dist_nb, coords = coords_proj, as_sf = TRUE)
st_crs(lines_dist) <- st_crs(mt_proj)

ggplot() +
  geom_sf(data = mt_proj, fill = "gray95", color = "gray80") +
  geom_sf(data = lines_dist, color = "darkorange", linewidth = 0.5, alpha = 0.6) +
  geom_point(data = as.data.frame(coords_proj), aes(X, Y), size = 0.8) +
  labs(title = "Limiar de Distância Fixa (120 km)",
       subtitle = "Conexões apenas se d < 120km (Note as ilhas isoladas)") +
  theme_map


'--------------------------------------------------------------------------
            Vizinhança por inverso da distancia (Decaimento)
---------------------------------------------------------------------------'

# Identificar Cuiabá
id_cuiaba <- which(mt_sf$name_muni == "Cuiabá")

# Calcular distâncias de Cuiabá para TODOS os outros municípios
nb_all <- dnearneigh(coords_proj, 0, 900000) # Raio grande para pegar quase todo estado
dists <- nbdists(nb_all, coords_proj)

# Calcular Pesos (Inverso da Distância: 1/d)
weights_list <- lapply(dists, function(x) 1/(x/1000)) # /1000 para km

# Preparar dados apenas para Cuiabá para visualização
vizinhos_cuiaba <- nb_all[[id_cuiaba]]
pesos_cuiaba <- weights_list[[id_cuiaba]]

# Criar linhas saindo de Cuiabá

lines_cuiaba <- vector("list", length(vizinhos_cuiaba))

for(i in seq_along(vizinhos_cuiaba)) {
  dest_idx <- vizinhos_cuiaba[i]
  lines_cuiaba[[i]] <- st_linestring(rbind(coords_proj[id_cuiaba,], coords_proj[dest_idx,]))
}

sf_decay <- st_sf(peso = pesos_cuiaba, geometry = st_sfc(lines_cuiaba), crs = 5880)

ggplot() +
  geom_sf(data = mt_proj, fill = "gray95", color = "white") +
  geom_sf(data = sf_decay, aes(color = peso, linewidth = peso), alpha = 0.8) +
  geom_point(aes(x=coords_proj[id_cuiaba,1], y=coords_proj[id_cuiaba,2]), color="red", size=3) +
  scale_color_viridis_c(option = "magma", name = "Peso (1/d)") +
  scale_linewidth(range = c(0.1, 2), guide = "none") +
  labs(title = "Decaimento por Distância (Foco: Cuiabá)",
       subtitle = "A espessura e cor indicam a força da influência") +
  theme_map


'--------------------------------------------------------------------------------
                            Distancia social
---------------------------------------------------------------------------------'

# Carregar mapa do Brasil (Estados)
br_states <- read_state(year = 2020, showProgress = FALSE)

# Coordenadas aproximadas das cidades de interesse
# (Sorriso-MT, Santos-SP, Paranaguá-PR)

cidades_df <- data.frame(
  cidade = c("Sorriso (MT)", "Porto de Santos (SP)", "Porto de Paranaguá (PR)"),
  lat = c(-12.5427, -23.9618, -25.5205),
  lon = c(-55.7211, -46.3322, -48.5095),
  tipo = c("Origem", "Destino", "Destino")
)

cidades_sf <- st_as_sf(cidades_df, coords = c("lon", "lat"), crs = 4326)

# Criar conexões (Arcos)
sorriso_coords <- subset(cidades_df, cidade == "Sorriso (MT)")

destinos <- subset(cidades_df, tipo == "Destino")

conexoes <- lapply(1:nrow(destinos), function(i) {
  st_linestring(rbind(
    c(sorriso_coords$lon, sorriso_coords$lat),
    c(destinos$lon[i], destinos$lat[i])
  ))
})

conexoes_sf <- st_sf(geometry = st_sfc(conexoes), crs = 4326)

ggplot() +
  geom_sf(data = br_states, fill = "gray95", color = "white") +
  # Destacar Estados envolvidos
  geom_sf(data = subset(br_states, abbrev_state %in% c("MT", "SP", "PR")), 
          fill = "gray85", color = "white") +
  
  # Linhas de Fluxo (Curvas para indicar movimento/distância)
  geom_curve(data = data.frame(x1 = sorriso_coords$lon, y1 = sorriso_coords$lat,
                               x2 = destinos$lon, y2 = destinos$lat),
             aes(x = x1, y = y1, xend = x2, yend = y2),
             color = "darkgreen", size = 1, curvature = 0.2, 
             arrow = arrow(length = unit(0.03, "npc"))) +
  
  geom_point(data = cidades_df, aes(x = lon, y = lat, color = tipo), size = 3) +
  scale_color_manual(values = c("red", "blue")) +
  
  geom_text(data = cidades_df, aes(x = lon, y = lat, label = cidade), 
            vjust = -1, fontface = "bold", size = 3, nudge_x=8, nudge_y=-2) + #usei nudge pra mover legenda
  labs(title = "Vizinhança Econômica (Fluxo de Commodities)",
       subtitle = "A conexão funcional supera a proximidade geográfica") +
  theme_void() +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))


'-----------------------------------------------------------------------------------------
                    Normalização da matriz de vizinhança
------------------------------------------------------------------------------------------'

if (!require("pacman")) install.packages("pacman")
pacman::p_load(sf, spdep, geobr, dplyr)

# Carregar mapa do estado de Sergipe 
se_sf <- read_municipality(code_muni = "SE", year = 2020, showProgress = FALSE)

# Criar vizinhança (Queen)
nb <- poly2nb(se_sf, queen = TRUE)

# Criar Matriz Binária (0 e 1)
# Necessária para os cálculos manuais de Coluna, Espectral e CAR
W_binaria <- nb2mat(nb, style = "B", zero.policy = TRUE)

paste("Dimensão da Matriz W:", nrow(W_binaria), "x", ncol(W_binaria))

'-------------------------------------------------------------------------------
              Por linha, style = "W"
-------------------------------------------------------------------------------'

lw_row <- nb2listw(nb, style = "W", zero.policy = TRUE)

# Extrair a matriz de pesos para verificação
W_row <- listw2mat(lw_row)

# A soma dos pesos de cada linha deve ser 1 (para quem tem vizinhos)
soma_linhas <- rowSums(W_row)
print(head(soma_linhas)) # Deve mostrar 1, 1, 1... (por baixo), 

'-------------------------------------------------------------------------------
                  Por coluna (Ops. farás manualmente)
-------------------------------------------------------------------------------'

# Calcular a soma de cada coluna da matriz binária
col_somas <- colSums(W_binaria)
# Proteção contra divisão por zero (caso haja ilhas)
col_somas[col_somas == 0] <- 1 

# Dividir cada elemento pela soma da sua coluna
# A função sweep aplica a operação na MARGIN=2 (colunas)
W_col <- sweep(W_binaria, MARGIN = 2, STATS = col_somas, FUN = "/")

# A soma da primeira coluna deve ser 1
paste("Soma da Coluna 1:", sum(W_col[,1])) 

'--------------------------------------------------------------------------------
                              Espectral
---------------------------------------------------------------------------------'

# Calcular autovalores da matriz binária
autovalores <- eigen(W_binaria, only.values = TRUE)$values

# Encontrar o maior autovalor absoluto (Raio Espectral)
lambda_max <- max(abs(autovalores))

# Normalizar a matriz
W_spec <- W_binaria / lambda_max

# O maior autovalor da nova matriz deve ser 1
print(paste("Novo Lambda Max:", max(abs(eigen(W_spec, only.values=TRUE)$values))))


'----------------------------------------------------------------------------------
                   EDA (Analise exploratoria dos dados)
---------------------------------------------------------------------------------'

'............................................................
              Índice de Moran Global
............................................................'

if (!require("pacman")) install.packages("pacman")
pacman::p_load(sf, spdep, ggplot2, patchwork, dplyr, geobr)

#Carregar dados: Malha de Minas Gerais (MG)
mg_sf <- read_municipality(code_muni = "MG", year = 2020, showProgress = FALSE)
#simulando dados

coords <- st_coordinates(st_centroid(mg_sf))
set.seed(123)
mg_sf$indicador <- (-coords[,2]) * 10 + rnorm(nrow(mg_sf), mean = 0, sd = 15)

# Definir Vizinhança e Pesos
# Vizinhança Queen
nb <- poly2nb(mg_sf, queen = TRUE)

# normalizar por linha (style W)
lw <- nb2listw(nb, style = "W", zero.policy = TRUE) #recomendo usar sempre zero.policy = TRUE

# Cálculo do Índice de Moran
# A) Teste Rápido para variavel de interresse "indicador"
moran_analitico <- moran.test(mg_sf$indicador, listw=lw, randomisation = TRUE)
print(moran_analitico)

# B) Teste Monte Carlo (Robusto)
# Simula 999 permutações aleatórias
moran_mc <- moran.mc(mg_sf$indicador, listw=lw, nsim = 999)

print(moran_mc)



'............................................................
                 Índice de Gray
............................................................'

pacman::p_load(ggspatial)
#Cálculo do Índice C de Geary

# A) Teste Analítico
geary_analitico <- geary.test(mg_sf$indicador, listw=lw, randomisation = TRUE)

# B) Teste Monte Carlo
set.seed(123)
geary_mc <- geary.mc(mg_sf$indicador, listw=lw, nsim = 999)

print(geary_analitico)
print(geary_mc)


# Calculamos o Geary Local (localC) para ver onde vizinhos diferem muito.
# Valores altos no mapa indicam vizinhos muito diferentes (outliers locais).
mg_sf$geary_local <- localC(mg_sf$indicador, listw=lw)

# Mapa da Variável Original
p1 <- ggplot(mg_sf) +
  geom_sf(aes(fill = indicador), color = NA) +
  scale_fill_viridis_c(option = "magma", name = "Valor") +
  labs(title = "A. Variável Original", subtitle = "Padrão Norte-Sul") +
  theme_void()+ 
  annotation_scale(location = "bl", width_hint = 0.3, bar_cols = c("black", "white")) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering)

p2 <- ggplot(mg_sf) +
  geom_sf(aes(fill = geary_local), color = NA) +
  scale_fill_viridis_c(option = "cividis", name = "Local C") +
  labs(title = "B. Dissimilaridade Local (Geary)", 
       subtitle = "Áreas claras = Vizinhos muito diferentes") +
  theme_void()+ 
  annotation_scale(location = "bl", width_hint = 0.3, bar_cols = c("black", "white")) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering)

p1 + p2

'-------------------------------------------------------------------------------------------
                              Estatisticas locais
---------------------------------------------------------------------------------------------'

'............................................................
                 Moran Local
............................................................'

pacman::p_load(tidyverse,sf,spdep,geobr,patchwork, ggtext, ggspatial)  


# Cálculo do Moran Local
loc_m <- localmoran(mg_sf$indicador, listw=lw)

# Preparar dados para plotagem
mg_sf$z_score <- as.numeric(scale(mg_sf$indicador)) 
mg_sf$lag_z   <- lag.listw(lw, mg_sf$z_score)    
mg_sf$p_value <- loc_m[, 5] # P-valor do teste local

# Classificação dos Quadrantes
sig_level <- 0.05

mg_sf <- mg_sf %>% 
  mutate(quadrante = case_when(
    p_value > sig_level ~ "NS",
    z_score > 0 & lag_z > 0 ~ "HH",
    z_score < 0 & lag_z < 0 ~ "LL",
    z_score > 0 & lag_z < 0 ~ "HL",
    z_score < 0 & lag_z > 0 ~ "LH"
  )) %>%
  mutate(quadrante = factor(quadrante, 
                            levels = c("HH", "LL", "HL", "LH", "NS"),
                            labels = c("Alto-Alto (HH)", "Baixo-Baixo (LL)", 
                                       "Alto-Baixo (HL)", "Baixo-Alto (LH)", 
                                       "Não Significativo")))

# Cores
cores_lisa <- c(
  "Alto-Alto (HH)" = "#FF0000",    
  "Baixo-Baixo (LL)" = "#0000FF", 
  "Alto-Baixo (HL)" = "#FFA500",  
  "Baixo-Alto (LH)" = "#87CEFA",  
  "Não Significativo" = "#eeeeee"
)

# Mapa
g_map <- ggplot(mg_sf) +
  geom_sf(aes(fill = quadrante), color = "black", size = 0.05) +
  scale_fill_manual(values = cores_lisa) +
  theme_void() +
  labs(title = "A. LISA",
       subtitle = "Identificação de regimes locais (p < 0.05)", 
       fill="Legenda") +
  theme(plot.title = element_text(size = 12, face = "bold"))+
  
  annotation_scale(
    location = "bl",           
    width_hint = 0.3,          
    bar_cols = c("black", "white"), 
    text_family = "sans"       
  ) +
  
  annotation_north_arrow(
    location = "tl",           
    which_north = "true",      
    pad_x = unit(0.2, "in"),   
    pad_y = unit(0.2, "in"),   
    style = north_arrow_fancy_orienteering 
  )

#
g_scatter <- ggplot(mg_sf, aes(x = z_score, y = lag_z)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_point(aes(color = quadrante), size = 1.5, alpha = 0.6) +
  # Linha de regressão (Moran Global)
  geom_smooth(method = "lm", se = FALSE, color = "black", size = 0.8) +
  # Anotações dos Quadrantes
  annotate("text", x = 2, y = 2, label = "HH", color = "red", fontface="bold") +
  annotate("text", x = -2, y = -2, label = "LL", color = "blue", fontface="bold") +
  annotate("text", x = 2, y = -1, label = "HL", color = "orange", fontface="bold") +
  annotate("text", x = -2, y = 1, label = "LH", color = "#87CEFA", fontface="bold") +
  scale_color_manual(values = cores_lisa) +
  labs(title = "B. Diagrama de Dispersão",
       subtitle = paste("I de Moran Global:", round(moran.test(mg_sf$indicador, lw)$estimate[1], 3)),
       x = "Valor Padronizado (y)",
       y = "Defasagem Espacial (Wy)") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(size = 12, face = "bold"))


g_map + g_scatter


'............................................................
                 Getis-Ord
............................................................'

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, sf, spdep, geobr, ggspatial)

#Cálculo do Getis-Ord Gi*
# A função retorna os Z-scores (desvios padrão)
gi_star <- localG(mg_sf$indicador, listw=lw)

# Adicionar ao mapa
mg_sf$gi_zscore <- as.numeric(gi_star)

# Classificação para o Mapa (Níveis de Confiança)
# Baseado na distribuição Normal Padrão
mg_sf$classificacao <- case_when(
  mg_sf$gi_zscore >= 1.96  ~ "Hot Spot (95%)",
  mg_sf$gi_zscore <= -1.96 ~ "Cold Spot (95%)",
  TRUE ~ "Não Significativo"
)

cores_gi <- c(
  "Hot Spot (95%)" = "#d7191c",
  "Cold Spot (95%)" = "#2c7bb6",
  "Não Significativo" = "gray90"
)

ggplot(mg_sf) +
  geom_sf(aes(fill = classificacao), color = "white", size = 0.05) +
  scale_fill_manual(values = cores_gi, name = "Intensidade (Gi*)") +
  theme_void() +
  labs(title = "Análise de Hot Spots (Getis-Ord Gi*)",
       subtitle = "Identificação de aglomerados de alta e baixa intensidade") +
  theme(plot.title = element_text(size = 14, face = "bold"),
        legend.position = "right") +
  
  annotation_scale(
    location = "bl",           
    width_hint = 0.3,          
    bar_cols = c("black", "white"), 
    text_family = "sans"       
  ) +
  
  annotation_north_arrow(
    location = "tl",           
    which_north = "true",      
    pad_x = unit(0.2, "in"),   
    pad_y = unit(0.2, "in"),   
    style = north_arrow_fancy_orienteering 
  )


'----------------------------------------------------------------------------------
                                Riscos e suavização
----------------------------------------------------------------------------------'

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, sf, spdep, geobr, patchwork, viridis, ggspatial)

mg_sf <- read_municipality(code_muni = "MG", year = 2020, showProgress = FALSE)

set.seed(999)

populacao_simulada <- floor(rlnorm(nrow(mg_sf), meanlog = 9, sdlog = 1.2))
coords <- st_coordinates(st_centroid(mg_sf))
padrao_norte_sul <- (coords[,2] - min(coords[,2])) / (max(coords[,2]) - min(coords[,2]))

risco_real <- 0.0005 * (1 + (padrao_norte_sul * 2)) # Risco varia de 0.05% a 0.15%
casos_simulados <- rpois(nrow(mg_sf), lambda = populacao_simulada * risco_real)

mg_dados <- mg_sf %>%
  mutate(
    populacao = populacao_simulada,  
    casos = casos_simulados        
  )

# Cálculo das Taxas 

# Passo 1: Calcular Taxa Bruta (Incidência por 10.000 habitantes)

mg_dados <- mg_dados %>%
  mutate(taxa_bruta = (casos / populacao) * 10000)

# Passo 2: Suavização Bayesiana Empírica Local 

nb <- poly2nb(mg_dados, queen = TRUE)

# A função EBlocal precisa dos CASOS (ri) e da POPULAÇÃO (ni)

eb_resultado <- EBlocal(ri = mg_dados$casos, 
                        ni = mg_dados$populacao, 
                        nb = nb, 
                        zero.policy = TRUE)

# Adicionamos a taxa suavizada ao mapa (multiplicando por 10k para ficar na mesma escala)
mg_dados$taxa_suavizada <- eb_resultado$est * 10000


# Definir limites iguais para garantir que as cores representem os mesmos valores
escala_limites <- range(c(mg_dados$taxa_bruta, mg_dados$taxa_suavizada), na.rm = TRUE)


p1 <- ggplot(mg_dados) +
  geom_sf(aes(fill = taxa_bruta), color = NA) +
  scale_fill_viridis_c(option = "turbo", limits = escala_limites, name = "Taxa/10k") +
  theme_void() +
  labs(title = "A. Taxa Bruta (Dados Observados)",
       subtitle = "Ruído excessivo em municípios pequenos") +
  theme(plot.title = element_text(size = 12, face = "bold"))+
  
  annotation_scale(
    location = "bl",           
    width_hint = 0.3,          
    bar_cols = c("black", "white"), 
    text_family = "sans"       
  ) +
  
  annotation_north_arrow(
    location = "tl",           
    which_north = "true",      
    pad_x = unit(0.2, "in"),   
    pad_y = unit(0.2, "in"),   
    style = north_arrow_fancy_orienteering 
  )

p2 <- ggplot(mg_dados) +
  geom_sf(aes(fill = taxa_suavizada), color = NA) +
  scale_fill_viridis_c(option = "turbo", limits = escala_limites, name = "Taxa/10k") +
  theme_void() +
  labs(title = "B. Taxa Suavizada (Empirical Bayes)",
       subtitle = "Padrão espacial real recuperado") +
  theme(plot.title = element_text(size = 12, face = "bold")) +
  
  annotation_scale(
    location = "bl",           
    width_hint = 0.3,          
    bar_cols = c("black", "white"), 
    text_family = "sans"       
  ) +
  
  annotation_north_arrow(
    location = "tl",           
    which_north = "true",      
    pad_x = unit(0.2, "in"),   
    pad_y = unit(0.2, "in"),   
    style = north_arrow_fancy_orienteering 
  )

p1 + p2



'-----------------------------------------------------------------------------------------
                  É dependencia ou variavel omitida? (Outro EDA)
------------------------------------------------------------------------------------------'

if (!require("pacman")) install.packages("pacman")
pacman::p_load(sf, spdep, ggplot2, patchwork, viridis)


if (!exists("mg_dados")) {
  mg_dados <- read_municipality(code_muni = "MG", year = 2020, showProgress = FALSE)
  coords <- st_coordinates(st_centroid(mg_dados))
  mg_dados$taxa_bruta <- (-coords[,2] * 10) + rnorm(nrow(mg_dados), 0, 5)
}

# Criar uma variável explicativa X aleatória (sem padrão espacial)
set.seed(123)
mg_dados$variavel_x <- rnorm(nrow(mg_dados))

#Ajuste do Modelo de Regressão Linear (OLS)
modelo_ols <- lm(taxa_bruta ~ variavel_x, data = mg_dados)

# Extrair os resíduos do modelo
mg_dados$residuos <- residuals(modelo_ols)

#Teste I de Moran nos Resíduos
# Necessário recriar a lista de pesos (W)
nb <- poly2nb(mg_dados, queen = TRUE)
lw <- nb2listw(nb, style = "W", zero.policy = TRUE)

# Função para resíduos de regressão (lm.morantest)
moran_residuos <- lm.morantest(modelo_ols, lw, alternative = "two.sided")

print(moran_residuos)

p1 <- ggplot(mg_dados) +
  geom_sf(aes(fill = residuos), color = NA) +
  scale_fill_distiller(palette = "RdBu", name = "Resíduos") +
  labs(title = "A. Mapa dos Resíduos OLS", 
       subtitle = "Padrão visível (não aleatório)") +
  
  annotation_scale(
    location = "bl",           
    width_hint = 0.3,          
    bar_cols = c("black", "white"), 
    text_family = "sans"       
  ) +
  
  annotation_north_arrow(
    location = "tl",           
    which_north = "true",      
    pad_x = unit(0.2, "in"),   
    pad_y = unit(0.2, "in"),   
    style = north_arrow_fancy_orienteering 
  )+
  theme_void()

#
mg_dados$residuos_z <- scale(mg_dados$residuos)
mg_dados$lag_residuos <- lag.listw(lw, mg_dados$residuos_z)

p2 <- ggplot(mg_dados, aes(x = residuos_z, y = lag_residuos)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  geom_hline(yintercept = 0, linetype="dashed") +
  geom_vline(xintercept = 0, linetype="dashed") +
  labs(title = "B. Moran dos Resíduos",
       subtitle = paste("I de Moran =", round(moran_residuos$statistic, 3)),
       x = "Resíduos (Z)", y = "Lag Espacial dos Resíduos") +
  theme_bw()

p1 + p2


'------------------------------------------------------------------------------------
                Tem dependencia? Se sim vá até a Modelagem
-------------------------------------------------------------------------------------'


'............................................................
                 Modelo CAR
............................................................'

if (!require("pacman")) install.packages("pacman")
pacman::p_load(spatialreg, spdep, modelsummary,knitr, kableExtra, texreg,ggplot2, patchwork, sf)

#Preparação dos Dados
if (!exists("mg_dados")) {
  mg_dados <- geobr::read_municipality(code_muni = "MG", year = 2020, showProgress = FALSE)
  coords <- st_coordinates(st_centroid(mg_dados))
  set.seed(123)
  mg_dados$taxa_bruta <- (-coords[,2] * 10) + rnorm(nrow(mg_dados), 0, 5)
  mg_dados$variavel_x <- rnorm(nrow(mg_dados))
}

# Matriz de Pesos Espaciais
nb <- poly2nb(mg_dados, queen = TRUE)
lw <- nb2listw(nb, style = "W", zero.policy = TRUE)

#Ajuste dos Modelos
# Regressão linear classica 
mod_ols <- lm(taxa_bruta ~ variavel_x, data = mg_dados)

# Modelo CAR (Incorpora dependência espacial condicional)
# family = "CAR" ajusta via Máxima Verossimilhança
mod_car <- spautolm(taxa_bruta ~ variavel_x, 
                    data = mg_dados, 
                    listw = lw, 
                    family = "CAR")

# Função auxiliar para formatar valor
format_coef <- function(est, se, pval) {
  stars <- case_when(pval < 0.001 ~ "***", pval < 0.01 ~ "**", pval < 0.05 ~ "*", TRUE ~ "")
  paste0(format(round(est, 3), nsmall=3), " (", format(round(se, 3), nsmall=3), ")", stars)
}


sum_ols <- summary(mod_ols)
coef_ols <- sum_ols$coefficients
res_ols <- c(
  format_coef(coef_ols[1,1], coef_ols[1,2], coef_ols[1,4]), # Intercepto
  format_coef(coef_ols[2,1], coef_ols[2,2], coef_ols[2,4]), # Variavel X
  "-",                                                      # Lambda (Não existe no OLS)
  round(AIC(mod_ols), 1)                                    # AIC
)


sum_car <- summary(mod_car)
coef_car <- sum_car$Coef
# Lambda (parametro espacial) e seu SE
lambda_val <- mod_car$lambda
lambda_se  <- mod_car$lambda.se
# Teste Z para o Lambda (aproximado)
lambda_p   <- 2 * (1 - pnorm(abs(lambda_val / lambda_se)))

res_car <- c(
  format_coef(coef_car[1,1], coef_car[1,2], coef_car[1,4]), # Intercepto
  format_coef(coef_car[2,1], coef_car[2,2], coef_car[2,4]), # Variavel X
  format_coef(lambda_val, lambda_se, lambda_p),             # Lambda
  round(AIC(mod_car), 1)                                    # AIC
)

#
tabela_final <- data.frame(
  Parametro = c("Intercepto", "Variável X", "Lambda (Espacial)", "AIC"),
  OLS = res_ols,
  CAR = res_car
)

#Gerar Tabela (HTML/LaTeX)
kbl(tabela_final, 
    #format = "latex", 
    booktabs = TRUE, 
    align = "lcc", 
    caption = NULL) %>% 
  kable_styling(latex_options = c("HOLD_position"), 
                full_width = FALSE, 
                position = "center") %>%
  add_header_above(c(" " = 1, "Modelos" = 2)) %>%
  footnote(general = "* p<0.05; ** p<0.01; *** p<0.001. Valores em parênteses são erros-padrão.")



# Diagnóstico dos Resíduos
mg_dados$resid_ols <- residuals(mod_ols)
mg_dados$resid_car <- residuals(mod_car)

# Teste de Moran
moran_car <- moran.test(mg_dados$resid_car, listw=lw)
print(paste("I de Moran (Resíduos CAR):", round(moran_car$estimate[1], 3), 
            "| p-valor:", round(moran_car$p.value, 3)))

p1 <- ggplot(mg_dados) +
  geom_sf(aes(fill = resid_ols), color = NA) +
  scale_fill_distiller(palette = "RdBu", name = "Resíduos") +
  labs(title = "A. Resíduos OLS", subtitle = "Dependência visível") +
  
  annotation_scale(
    location = "bl",           
    width_hint = 0.3,          
    bar_cols = c("black", "white"), 
    text_family = "sans"       
  ) +
  
  annotation_north_arrow(
    location = "tl",           
    which_north = "true",      
    pad_x = unit(0.2, "in"),   
    pad_y = unit(0.2, "in"),   
    style = north_arrow_fancy_orienteering 
  )+
  theme_void()

p2 <- ggplot(mg_dados) +
  geom_sf(aes(fill = resid_car), color = NA) +
  scale_fill_distiller(palette = "RdBu", name = "Resíduos") +
  labs(title = "B. Resíduos CAR", subtitle = "Padrão removido") +
  
  annotation_scale(
    location = "bl",           
    width_hint = 0.3,          
    bar_cols = c("black", "white"), 
    text_family = "sans"       
  ) +
  
  annotation_north_arrow(
    location = "tl",           
    which_north = "true",      
    pad_x = unit(0.2, "in"),   
    pad_y = unit(0.2, "in"),   
    style = north_arrow_fancy_orienteering 
  )+
  theme_void()

p1 + p2


'............................................................
                 Modelo ICAR
............................................................'

if (!require("pacman")) install.packages("pacman")

# CARBayes é o pacote padrão para modelagem Bayesiana de áreas no CRAN
pacman::p_load(CARBayes, spdep, sf, ggplot2, patchwork, coda, gt)

if (!exists("mg_dados")) {
  mg_dados <- geobr::read_municipality(code_muni = "MG", year = 2020, showProgress = FALSE)
  coords <- st_coordinates(st_centroid(mg_dados))
  set.seed(123)
  mg_dados$taxa_bruta <- (-coords[,2] * 10) + rnorm(nrow(mg_dados), 0, 5)
  mg_dados$variavel_x <- rnorm(nrow(mg_dados))
}

#Matriz de Vizinhança Binária (W)
# O pacote CARBayes exige uma matriz binária (0 e 1), não normalizada.
nb <- poly2nb(mg_dados, queen = TRUE)
W_binaria <- nb2mat(nb, style = "B", zero.policy = TRUE)

#Ajuste do Modelo ICAR (Bayesiano)

set.seed(123)
modelo_icar <- S.CARleroux(formula = taxa_bruta ~ variavel_x, 
                           data = mg_dados, 
                           family = "gaussian", #terias que ver a distr dos seus dados para decidir aqui
                           W = W_binaria, 
                           burnin = 2000,   # Descarta as primeiras 2000 iterações (aquecimento)
                           n.sample = 10000, # Total de amostras MCMC
                           thin = 10,        # Salva a cada 10 para reduzir autocorrelação
                           rho = 1,          # RHO = 1 define o ICAR
                           verbose = FALSE)


#Extrair os resultados
# O objeto mod_icar$summary.results contém médias e quantis
summ <- as.data.frame(modelo_icar$summary.results)


params_interesse <- c("(Intercept)", "variavel_x", "tau2", "nu2")
summ_filt <- summ[rownames(summ) %in% params_interesse, ]

# Função para formatar: "Média [IC 2.5%; IC 97.5%]"
format_bayes <- function(mean, lower, upper) {
  paste0(format(round(mean, 3), nsmall=3), " [", 
         format(round(lower, 3), nsmall=3), "; ", 
         format(round(upper, 3), nsmall=3), "]")
}

tabela_icar <- data.frame(
  Parametro = rownames(summ_filt),
  Estimativa = mapply(format_bayes, summ_filt$Mean, summ_filt$`2.5%`, summ_filt$`97.5%`)
)

# Renomear
is_html_output <- knitr::is_html_output()
label_tau2 <- if(is_html_output) "&tau;<sup>2</sup>" else "$\\tau^2$"
label_nu2  <- if(is_html_output) "&nu;<sup>2</sup>"  else "$\\nu^2$"

tabela_icar$Parametro <- recode(
  tabela_icar$Parametro,
  "(Intercept)" = "Intercepto",
  "variavel_x"  = "Variável X",
  "tau2"        = paste("Variância Espacial", label_tau2),
  "nu2"         = paste("Variância do Erro", label_nu2)
)


kbl(tabela_icar, 
    #format = "latex",
    booktabs = TRUE, 
    align = "lc", 
    caption = NULL, 
    escape = FALSE) %>%
  kable_styling(latex_options = c("HOLD_position", "striped"), 
                full_width = FALSE, 
                position = "center") %>%
  footnote(general = "Estimativas: Média a posteriori [IC 95%].") 


'............................................................
                 Resíduo
............................................................'

# O modelo estima um efeito aleatório (phi) para cada município.
mg_dados$efeito_icar <- apply(modelo_icar$samples$phi, 2, mean)

#
ggplot(mg_dados) +
  geom_sf(aes(fill = efeito_icar), color = NA) +
  scale_fill_distiller(palette = "RdBu", name = expression("Efeito Espacial" ~Phi)) +
  labs(title = "Componente Espacial ICAR", 
       subtitle = "Padrão latente recuperado (Suavizado)") +
  theme_void() +
  theme(plot.title = element_text(face = "bold"))+
  
  annotation_scale(
    location = "bl",           
    width_hint = 0.3,          
    bar_cols = c("black", "white"), 
    text_family = "sans"       
  ) +
  
  annotation_north_arrow(
    location = "tl",           
    which_north = "true",      
    pad_x = unit(0.2, "in"),   
    pad_y = unit(0.2, "in"),   
    style = north_arrow_fancy_orienteering 
  )


'............................................................
                 Modelo BYM2
............................................................'

# Instalação do INLA 
options(timeout = 600)

if (!requireNamespace("INLA", quietly = TRUE)) {
  install.packages("INLA", repos=c(getOption("repos"), INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)
}


# Veja como instalar INLA no link:  https://www.r-inla.org/download-install
# veja também browseVignettes("INLA") ou system.file("doc", package = "INLA") e veja a documentacao
# Veja também https://github.com/hrue/r-inla/tree/devel/rinla/vignettes

if (!require("pacman")) install.packages("pacman")
pacman::p_load(INLA, spdep, sf, dplyr, knitr, kableExtra, ggplot2, patchwork)


# Preparação dos Dados
if (!exists("mg_dados")) {
  mg_dados <- geobr::read_municipality(code_muni = "MG", year = 2020, showProgress = FALSE)
  coords <- st_coordinates(st_centroid(mg_dados))
  set.seed(123)
  mg_dados$taxa_bruta <- (-coords[,2] * 10) + rnorm(nrow(mg_dados), 0, 5)
  mg_dados$variavel_x <- rnorm(nrow(mg_dados))
}

# Criar um identificador numérico sequencial para as áreas (exigência do INLA)
mg_dados$ID_AREA <- 1:nrow(mg_dados)

#Matriz de Vizinhança e Grafo
nb <- poly2nb(mg_dados, queen = TRUE)

plot(st_geometry(mg_dados), border = "lightgrey", main = "Estrutura de Vizinhança (Grafo)")
plot(nb, coords, add = TRUE, col = "red", pch = 19, cex = 0.6, lwd = 0.5)


# Converter para formato de grafo do INLA
nb2INLA("mg_graph.adj", nb)
g <- inla.read.graph("mg_graph.adj")


#Definição dos PC Priors (Penalised Complexity)
# Prior para a Precisão (Tau): Prob(desvio padrão > 1) = 0.01 , vc pode usar outros
#A probabilidade do Desvio Padrão (σ) ser maior que 1 é de apenas 1% (0.01).

# Prior para Mistura (Phi): Prob(phi < 0.5) = 0.5 (neutro), note que Phi = 1 (tudo é espacial), Phi=0 (tudo é aleatório).
#Você está dizendo ao modelo: "Eu não sei se o fenômeno é mais espacial ou 
# mais aleatório, então vou deixar 50% de chance para cada lado"

hyper_pc <- list(
  prec = list(prior = "pc.prec", param = c(1, 0.01)),
  phi  = list(prior = "pc", param = c(0.5, 0.5))
)

#Ajuste do Modelo BYM2

formula_bym2 <- taxa_bruta ~ variavel_x + 
  f(ID_AREA, model = "bym2", graph = g, scale.model = TRUE, 
    hyper = hyper_pc)

modelo_inla <- inla(formula_bym2, 
                    data = mg_dados, 
                    family = "gaussian", 
                    control.predictor = list(compute = TRUE), 
                    control.compute = list(dic = TRUE, waic = TRUE))

# Função auxiliar de formatação "Média [IC 95%]"
fmt <- function(m, l, u) {
  paste0(format(round(m, 3), nsmall=3), " [", 
         format(round(l, 3), nsmall=3), "; ", 
         format(round(u, 3), nsmall=3), "]")
}

# Efeitos Fixos
fix <- modelo_inla$summary.fixed[, c("mean", "0.025quant", "0.975quant")]
df_fix <- data.frame(
  Parametro = rownames(fix),
  Valor = mapply(fmt, fix$mean, fix$`0.025quant`, fix$`0.975quant`)
)

# renomear nomes
df_fix$Parametro <- recode(df_fix$Parametro, 
                           "(Intercept)" = "Intercepto", 
                           "variavel_x" = "Variável X")

# Hiperparâmetros
hyp <- modelo_inla$summary.hyperpar[, c("mean", "0.025quant", "0.975quant")]
df_hyp <- data.frame(
  Parametro = rownames(hyp),
  Valor = mapply(fmt, hyp$mean, hyp$`0.025quant`, hyp$`0.975quant`)
)

is_html_output <- knitr::is_html_output()

label_taU <- if(is_html_output) "&tau;" else "$\\tau$"
label_phI <- if(is_html_output) "&phi;" else "$\\phi$"

df_hyp$Parametro <- recode(df_hyp$Parametro,
                           "Precision for the Gaussian observations" = "Precisão (Likelihood)",
                           "Precision for ID_AREA" = paste("Precisão Marginal", label_taU),
                           "Phi for ID_AREA"       = paste("Dependência Espacial", label_phI)
)

#Métricas de Ajuste
df_met <- data.frame(
  Parametro = c("DIC", "WAIC"),
  Valor = c(format(round(modelo_inla$dic$dic, 2), nsmall=2),
            format(round(modelo_inla$waic$waic, 2), nsmall=2))
)

# Unir tudo
df_final <- rbind(df_fix, df_hyp, df_met)

# Gerar Tabela
kbl(df_final, 
    # format = "latex",
    booktabs = TRUE, 
    align = "lr", 
    caption = NULL) %>% 
  kable_styling(latex_options = c("HOLD_position"), 
                full_width = FALSE, 
                position = "center") %>%
  pack_rows("Efeitos Fixos (Média [IC 95%])", 1, nrow(df_fix)) %>%
  pack_rows("Hiperparâmetros (Média [IC 95%])", nrow(df_fix) + 1, nrow(df_fix) + nrow(df_hyp)) %>%
  pack_rows("Qualidade do Ajuste", nrow(df_final) - 1, nrow(df_final)) %>%
  row_spec((nrow(df_final)-1):nrow(df_final), bold = TRUE) 



# DIAGNÓSTICO

mg_dados$efeito_bym <- modelo_inla$summary.random$ID_AREA$mean[1:nrow(mg_dados)]
mg_dados$ajustado   <- modelo_inla$summary.fitted.values$mean
mg_dados$residuos   <- mg_dados$taxa_bruta - mg_dados$ajustado

#Mapa do Efeito Espacial (Risco/Nível estimado)
p1 <- ggplot(mg_dados) +
  geom_sf(aes(fill = efeito_bym), color = NA) +
  scale_fill_distiller(palette = "RdBu", name = "Efeito\nEspacial") +
  labs(title = "A. Componente Espacial (BYM2)") +
  theme_minimal()+
  
  annotation_scale(
    location = "bl",           
    width_hint = 0.3,          
    bar_cols = c("black", "white"), 
    text_family = "sans"       
  ) +
  
  annotation_north_arrow(
    location = "tl",           
    which_north = "true",      
    pad_x = unit(0.2, "in"),   
    pad_y = unit(0.2, "in"),   
    style = north_arrow_fancy_orienteering 
  )

# Mapa dos Resíduos 
p2 <- ggplot(mg_dados) +
  geom_sf(aes(fill = residuos), color = NA) +
  scale_fill_distiller(palette = "PuOr", name = "Resíduos") +
  labs(title = "B. Mapa de Resíduos") +
  theme_minimal()+
  
  annotation_scale(
    location = "bl",           
    width_hint = 0.3,          
    bar_cols = c("black", "white"), 
    text_family = "sans"       
  ) +
  
  annotation_north_arrow(
    location = "tl",           
    which_north = "true",      
    pad_x = unit(0.2, "in"),   
    pad_y = unit(0.2, "in"),   
    style = north_arrow_fancy_orienteering 
  )

#Gráfico Observado vs Esperado
cor_p <- round(cor(mg_dados$taxa_bruta, mg_dados$ajustado), 3)
p3 <- ggplot(mg_dados, aes(x = ajustado, y = taxa_bruta)) +
  geom_point(alpha = 0.3, color = "darkblue") +
  geom_abline(col = "red", linetype = "dashed") +
  labs(title = "C. Ajuste do Modelo", 
       subtitle = paste0("Correlação: ", cor_p),
       x = "Predito (INLA)", y = "Observado") +
  theme_minimal()


(p1 + p2 + p3)

# TESTE DE MORAN NOS RESÍDUOS

lw <- nb2listw(nb, style = "W")
moran_test <- moran.test(mg_dados$residuos, lw)

print(moran_test)

if(moran_test$p.value > 0.05) {
  message("SUCESSO: Resíduos são aleatórios (p > 0.05). O modelo removeu a autocorrelação.")
} else {
  message("ATENÇÃO: Ainda existe dependência espacial nos resíduos.")
}


'---------------------------------------------------------------------------------
                            Modelos espaciais ecométricos
----------------------------------------------------------------------------------'

if (!require("pacman")) install.packages("pacman")
pacman::p_load(spatialreg, spdep, sf, texreg, knitr, kableExtra, dplyr)

# Preparação dos Dados
if (!exists("mg_dados")) {
  mg_dados <- geobr::read_municipality(code_muni = "MG", year = 2020, showProgress = FALSE)
  coords <- st_coordinates(st_centroid(mg_dados))
  set.seed(123)
  mg_dados$taxa_bruta <- (-coords[,2] * 10) + rnorm(nrow(mg_dados), 0, 5)
  mg_dados$variavel_x <- rnorm(nrow(mg_dados))
}

# Matriz de Pesos Espaciais (Padronizada por linha 'W' é o padrão para SAR)
nb <- poly2nb(mg_dados, queen = TRUE)
lw <- nb2listw(nb, style = "W", zero.policy = TRUE)

# Ajuste do Modelo
# A) OLS (Referência)
mod_ols <- lm(taxa_bruta ~ variavel_x, data = mg_dados)

# SAR (Spatial Autoregressive Model) - Máxima Verossimilhança
# A função lagsarlm ajusta do modelo SAR via ML
mod_sar <- lagsarlm(taxa_bruta ~ variavel_x, 
                    data = mg_dados, 
                    listw = lw)

#
mapa_vars <- c(
  "(Intercept)" = "Intercepto",
  "variavel_x"  = "Variável X",
  "rho"         = "$\\rho$ (Dependência Espacial)"
)

#
mapa_gof <- list(
  list("raw" = "nobs", "clean" = "N", "fmt" = 0),
  list("raw" = "r.squared", "clean" = "$\\ R^2$", "fmt" = 3),
  list("raw" = "aic", "clean" = "AIC", "fmt" = 1),
  list("raw" = "logLik", "clean" = "Log Likelihood", "fmt" = 1)
)
# Gerar a Tabela Principal
modelsummary(
  list("OLS (Clássico)" = mod_ols, "SAR (Lag Espacial)" = mod_sar),
  coef_map = mapa_vars,      
  gof_map = mapa_gof,      
  estimate="{estimate} [{conf.low}, {conf.high}]",
  stars = c('*' = .05, '**' = .01, '***' = .001),
  title = NULL, 
  output = "kableExtra", 
  escape = FALSE
) %>%
  kable_styling(latex_options = c("HOLD_position"), 
                full_width = FALSE, 
                position = "center") %>%
  row_spec(5, bold = TRUE) %>% 
  as.character() %>%
  cat()

texreg(
  list(mod_ols, mod_sar),
  custom.model.names = c("OLS (Clássico)", "SAR (Lag Espacial)"),
  custom.coef.names = c("Intercepto", "Variável X", "$\\rho$ (Autocorrelação)"),
  caption = "Comparação de Modelos: OLS vs SAR",
  digits = 3,
  booktabs = TRUE, 
  dcolumn = TRUE
)


if (!require("pacman")) install.packages("pacman")
pacman::p_load(spatialreg, spdep, sf, ggplot2, dplyr, tidyr, patchwork, viridis, Matrix)

# 
if (!exists("mod_sar") || !exists("mg_dados")) {
  mg_dados <- geobr::read_municipality(code_muni = "MG", year = 2020, showProgress = FALSE)
  coords <- st_coordinates(st_centroid(mg_dados))
  set.seed(123)
  mg_dados$taxa_bruta <- (-coords[,2] * 10) + rnorm(nrow(mg_dados), 0, 5)
  mg_dados$variavel_x <- rnorm(nrow(mg_dados))
  
  nb <- poly2nb(mg_dados, queen = TRUE)
  lw <- nb2listw(nb, style = "W", zero.policy = TRUE)
  mod_sar <- lagsarlm(taxa_bruta ~ variavel_x, data = mg_dados, listw = lw)
}

#Gráfico de Impactos (Direto vs Indireto Unificado)

set.seed(123)
imp_mc_plot <- impacts(mod_sar, listw = lw, R = 1000)

if (is.null(imp_mc_plot$smat) && is.null(imp_mc_plot$res)) {
  imp_mc_plot <- impacts(mod_sar, listw = lw, R = 1000, zstats = TRUE)
}

# Dataframe Direto e Indireto
df_impactos <- data.frame(
  direct = imp_mc_plot$res$direct,
  indirect = imp_mc_plot$res$indirect
) %>%
  pivot_longer(cols = everything(), names_to = "Tipo", values_to = "Valor") %>%
  mutate(Tipo = factor(Tipo, levels = c("direct", "indirect"),
                       labels = c("Direto", "Indireto (Spillover)")))

# 
g_impactos <- ggplot(df_impactos, aes(x = Tipo, fill = Tipo, y=Valor)) +
  geom_col(width = 0.2, color = "gray30") +
  scale_fill_manual(values = c("Direto" = "#1b9e77", "Indireto (Spillover)" = "#d95f02")) +
  labs(title = "A. Impactos: Direto vs. Indireto", 
       y = "Magnitude do efeito", x = NULL) +
  theme_minimal() + 
  theme(legend.position = "none", 
        legend.title = element_blank())

# Mapa dos Valores Ajustados

mg_dados$fitted_sar <- fitted(mod_sar)

g_fit <- ggplot(mg_dados) +
  geom_sf(aes(fill = fitted_sar), color = NA) +
  scale_fill_viridis_c(option = "turbo", name = "Predito") +
  labs(title = "B. Valores Preditos (SAR)", 
       subtitle = "Padrão espacial recuperado")+
  theme_minimal() + 
  
  annotation_scale(
    location = "bl",           
    width_hint = 0.3,          
    bar_cols = c("black", "white"), 
    text_family = "sans"       
  ) +
  
  annotation_north_arrow(
    location = "tl",           
    which_north = "true",      
    pad_x = unit(0.2, "in"),   
    pad_y = unit(0.2, "in"),   
    style = north_arrow_fancy_orienteering 
  )


# Diagnóstico dos Resíduos
mg_dados$resid_sar <- residuals(mod_sar)
moran_res <- moran.test(mg_dados$resid_sar, lw)
mg_dados$resid_lag <- lag.listw(lw, mg_dados$resid_sar)

g_resid_scatter <- ggplot(mg_dados, aes(x = resid_sar, y = resid_lag)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray") +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE, color = "red", size = 0.8) +
  labs(title = "C. Scatter de Moran (Resíduos)", 
       subtitle = paste0("I de Moran: ", round(moran_res$estimate[1], 3), 
                         " (p-valor: ", round(moran_res$p.value, 3), ")"),
       x = "Resíduos", y = "Lag Espacial") +
  theme_minimal()


# 
(g_impactos | g_fit | g_resid_scatter)


'............................................................
                 Modelo SEM
............................................................'

if (!require("pacman")) install.packages("pacman")
pacman::p_load(spatialreg, spdep, sf, modelsummary, kableExtra, dplyr)

# SEM (Spatial Error Model)
# A função errorsarlm ajusta o SEM via Máxima Verossimilhança
mod_sem <- errorsarlm(taxa_bruta ~ variavel_x, data = mg_dados, listw = lw)

#Tabela 
mapa_vars <- c(
  "(Intercept)" = "Intercepto",
  "variavel_x"  = "Variável X",
  "rho"         = "$\\rho$ (Lag Espacial)",  # Parâmetro do SAR
  "lambda"      = "$\\lambda$ (Erro Espacial)" # Parâmetro do SEM
)

mapa_gof <- list(
  list("raw" = "nobs", "clean" = "N", "fmt" = 0),
  list("raw" = "r.squared", "clean" = "$R^2$", "fmt" = 3),
  list("raw" = "aic", "clean" = "AIC", "fmt" = 1),
  list("raw" = "logLik", "clean" = "Log Likelihood", "fmt" = 1)
)

#Gerar a Tabela Comparativa (OLS, SAR, SEM)
modelsummary(
  list(
    "OLS (Clássico)" = mod_ols, 
    "SAR" = mod_sar, 
    "SEM" = mod_sem
  ),
  coef_map = mapa_vars,      
  gof_map = mapa_gof,      
  estimate = "{estimate} [{conf.low}, {conf.high}]",
  statistic = NULL, 
  stars = c('*' = .05, '**' = .01, '***' = .001),
  title = NULL,     
  output = "kableExtra", 
  escape = FALSE  
) %>%
  kable_styling(latex_options = c("HOLD_position"), 
                full_width = FALSE, 
                position = "center") %>%
  row_spec(c(5, 7), bold = TRUE) %>% 
  as.character() %>%
  cat()


if (!require("pacman")) install.packages("pacman")
pacman::p_load(spatialreg, spdep, sf, ggplot2, dplyr, tidyr, patchwork, viridis, Matrix, ggspatial)


if (!exists("mod_sem") || !exists("mg_dados")) {
  mg_dados <- geobr::read_municipality(code_muni = "MG", year = 2020, showProgress = FALSE)
  coords <- st_coordinates(st_centroid(mg_dados))
  set.seed(123)
  mg_dados$taxa_bruta <- (-coords[,2] * 10) + rnorm(nrow(mg_dados), 0, 5)
  mg_dados$variavel_x <- rnorm(nrow(mg_dados))
  
  nb <- poly2nb(mg_dados, queen = TRUE)
  lw <- nb2listw(nb, style = "W", zero.policy = TRUE)
  
  mod_sem <- errorsarlm(taxa_bruta ~ variavel_x, data = mg_dados, listw = lw)
}



# Mapa dos Valores Ajustados 
mg_dados$fitted_sem <- fitted(mod_sem)

g_fit <- ggplot(mg_dados) +
  geom_sf(aes(fill = fitted_sem), color = NA) +
  scale_fill_viridis_c(option = "turbo", name = "Predito") +
  labs(title = "A. Valores Preditos (SEM)", 
       subtitle = "Padrão recuperado (Erro Corrigido)") +
  theme_minimal() + 
  annotation_scale(
    location = "bl",            
    width_hint = 0.3,           
    bar_cols = c("black", "white"), 
    text_family = "sans"        
  ) +
  annotation_north_arrow(
    location = "tl",            
    which_north = "true",       
    pad_x = unit(0.2, "in"),    
    pad_y = unit(0.2, "in"),    
    style = north_arrow_fancy_orienteering 
  )


# Diagnóstico dos Resíduos
mg_dados$resid_sem <- residuals(mod_sem)
moran_res <- moran.test(mg_dados$resid_sem, lw)
mg_dados$resid_lag_sem <- lag.listw(lw, mg_dados$resid_sem)

g_resid_scatter <- ggplot(mg_dados, aes(x = resid_sem, y = resid_lag_sem)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray") +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE, color = "red", size = 0.8) +
  labs(title = "B. Scatter de Moran (Resíduos SEM)", 
       subtitle = paste0("I de Moran: ", round(moran_res$estimate[1], 3), 
                         " (p-valor: ", round(moran_res$p.value, 3), ")"),
       x = "Resíduos", y = "Lag Espacial") +
  theme_minimal()

(g_fit | g_resid_scatter)

'............................................................
                 Modelo SLX
............................................................'

if (!require("pacman")) install.packages("pacman")
pacman::p_load(spatialreg, spdep, sf, modelsummary, kableExtra, dplyr, ggplot2, patchwork, viridis)


if (!exists("mg_dados")) {
  mg_dados <- geobr::read_municipality(code_muni = "MG", year = 2020, showProgress = FALSE)
  coords <- st_coordinates(st_centroid(mg_dados))
  set.seed(123)
  mg_dados$taxa_bruta <- (-coords[,2] * 10) + rnorm(nrow(mg_dados), 0, 5)
  mg_dados$variavel_x <- rnorm(nrow(mg_dados))
}


# SLX (Spatial Lag of X)
# lmSLX cria automaticamente as defasagens (lag.variavel_x)
mod_slx <- lmSLX(taxa_bruta ~ variavel_x, data = mg_dados, listw = lw)


mapa_vars <- c(
  "(Intercept)"    = "Intercepto",
  "variavel_x"     = "Variável X (Direto)",
  "lag.variavel_x" = "WX $\\theta$", # SLX
  "rho"            = "$\\rho$ (Lag Espacial)",       # SAR
  "lambda"         = "$\\lambda$ (Erro Espacial)"    # SEM
)

mapa_gof <- list(
  list("raw" = "nobs", "clean" = "N", "fmt" = 0),
  list("raw" = "r.squared", "clean" = "$\\ R^2$", "fmt" = 3),
  list("raw" = "aic", "clean" = "AIC", "fmt" = 1),
  list("raw" = "logLik", "clean" = "Log Likelihood", "fmt" = 1)
)

# 
modelsummary(
  list(
    "OLS" = mod_ols, 
    "SLX" = mod_slx,
    "SAR" = mod_sar, 
    "SEM" = mod_sem
  ),
  coef_map = mapa_vars,      
  gof_map = mapa_gof,      
  estimate = "{estimate} [{conf.low}, {conf.high}]",
  statistic = NULL, 
  stars = c('*' = .05, '**' = .01, '***' = .001),
  title = NULL,     
  output = "kableExtra",
  escape = FALSE 
) %>%
  kable_styling(latex_options = c("HOLD_position"), 
                full_width = FALSE, 
                position = "center") %>%
  row_spec(c(5, 7, 9), bold = TRUE) %>% 
  as.character() %>%
  cat()


# Extraindo coeficientes e intervalos de confiança do SLX
coefs <- coef(mod_slx)
cis   <- confint(mod_slx)


# Mapa dos Valores Ajustados
mg_dados$fitted_slx <- fitted(mod_slx)

g_fit <- ggplot(mg_dados) +
  geom_sf(aes(fill = fitted_slx), color = NA) +
  scale_fill_viridis_c(option = "turbo", name = "Predito") +
  labs(title = "A. Valores Preditos (SLX)", 
       subtitle = "Ajuste com defasagens de X") +
  theme_minimal() + 
  annotation_scale(location = "bl", width_hint = 0.3) +
  annotation_north_arrow(location = "tl", style = north_arrow_fancy_orienteering,
                         pad_x = unit(0.1, "in"), pad_y = unit(0.1, "in"))

# Diagnóstico dos Resíduos (Scatter de Moran)
mg_dados$resid_slx <- residuals(mod_slx)
moran_slx <- moran.test(mg_dados$resid_slx, lw)
mg_dados$resid_lag_slx <- lag.listw(lw, mg_dados$resid_slx)

g_resid_scatter <- ggplot(mg_dados, aes(x = resid_slx, y = resid_lag_slx)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray") +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE, color = "red", size = 0.8) +
  labs(title = "B. Scatter de Moran (Resíduos SLX)", 
       subtitle = paste0("I de Moran: ", round(moran_slx$estimate[1], 3), 
                         " (p-valor: ", round(moran_slx$p.value, 3), ")"),
       x = "Resíduos", y = "Lag Espacial") +
  theme_minimal()


( g_fit | g_resid_scatter)

'............................................................
                 Modelo SDM
............................................................'

if (!require("pacman")) install.packages("pacman")
pacman::p_load(spatialreg, spdep, sf, modelsummary, kableExtra, dplyr, ggplot2, patchwork, viridis, ggspatial, tidyr, Matrix)

# 1. Preparação dos Dados
if (!exists("mg_dados")) {
  mg_dados <- geobr::read_municipality(code_muni = "MG", year = 2020, showProgress = FALSE)
  coords <- st_coordinates(st_centroid(mg_dados))
  set.seed(123)
  mg_dados$taxa_bruta <- (-coords[,2] * 10) + rnorm(nrow(mg_dados), 0, 5)
  mg_dados$variavel_x <- rnorm(nrow(mg_dados))
}


# SDM (Spatial Durbin Model)
# type = "mixed" inclui lag de Y (rho) e lag de X (theta)
mod_sdm <- lagsarlm(taxa_bruta ~ variavel_x, data = mg_dados, listw = lw, type = "mixed")

#Tabela
mapa_vars <- c(
  "(Intercept)"    = "Intercepto",
  "variavel_x"     = "$\\beta$",
  "lag.variavel_x" = "WX $\\theta$", # Compartilhado por SLX e SDM
  "rho"            = "$\\rho$",       # Compartilhado por SAR e SDM
  "lambda"         = "$\\lambda$"    # Exclusivo do SEM
)

mapa_gof <- list(
  list("raw" = "nobs", "clean" = "N", "fmt" = 0),
  list("raw" = "r.squared", "clean" = "$R^2$", "fmt" = 3),
  list("raw" = "aic", "clean" = "AIC", "fmt" = 1),
  list("raw" = "logLik", "clean" = "Log Likelihood", "fmt" = 1)
)

# Tabela Unificada
modelsummary(
  list(
    "OLS" = mod_ols, 
    "SLX" = mod_slx,
    "SAR" = mod_sar, 
    "SEM" = mod_sem,
    "SDM" = mod_sdm
  ),
  coef_map = mapa_vars,      
  gof_map = mapa_gof,      
  estimate = "{estimate} [{conf.low}, {conf.high}]",
  statistic = NULL, 
  stars = c('*' = .05, '**' = .01, '***' = .001),
  title = NULL,     
  output = "kableExtra",
  escape = FALSE
) %>%
  kable_styling(latex_options = c("HOLD_position"), 
                full_width = FALSE, 
                position = "center") %>%
  row_spec(c(5, 7, 9), bold = TRUE) %>% 
  as.character() %>%
  cat()

# Mapa dos Valores Ajustados
mg_dados$fitted_sdm <- fitted(mod_sdm)

g_fit <- ggplot(mg_dados) +
  geom_sf(aes(fill = fitted_sdm), color = NA) +
  scale_fill_viridis_c(option = "turbo", name = "Predito") +
  labs(title = "A. Valores Preditos (SDM)", 
       subtitle = "Ajuste considerando Wy e WX") +
  theme_minimal() + 
  annotation_scale(location = "bl", width_hint = 0.3) +
  annotation_north_arrow(location = "tl", style = north_arrow_fancy_orienteering,
                         pad_x = unit(0.1, "in"), pad_y = unit(0.1, "in"))

#Diagnóstico dos Resíduos
mg_dados$resid_sdm <- residuals(mod_sdm)
moran_sdm <- moran.test(mg_dados$resid_sdm, lw)
mg_dados$resid_lag_sdm <- lag.listw(lw, mg_dados$resid_sdm)

g_resid_scatter <- ggplot(mg_dados, aes(x = resid_sdm, y = resid_lag_sdm)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray") +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE, color = "red", size = 0.8) +
  labs(title = "C. Scatter de Moran (Resíduos SDM)", 
       subtitle = paste0("I de Moran: ", round(moran_sdm$estimate[1], 3), 
                         " (p-valor: ", round(moran_sdm$p.value, 3), ")"),
       x = "Resíduos", y = "Lag Espacial") +
  theme_minimal()

g_fit | g_resid_scatter


'............................................................
                Modelo SDEM
............................................................'

if (!require("pacman")) install.packages("pacman")
pacman::p_load(spatialreg, spdep, sf, modelsummary, kableExtra, dplyr, ggplot2, patchwork, viridis, ggspatial, tidyr, Matrix)


if (!exists("mg_dados")) {
  mg_dados <- geobr::read_municipality(code_muni = "MG", year = 2020, showProgress = FALSE)
  coords <- st_coordinates(st_centroid(mg_dados))
  set.seed(123)
  mg_dados$taxa_bruta <- (-coords[,2] * 10) + rnorm(nrow(mg_dados), 0, 5)
  mg_dados$variavel_x <- rnorm(nrow(mg_dados))
}


# SDEM (Spatial Durbin Error Model)
# errorsarlm com Durbin=TRUE inclui WX (theta) e lambda (erro)
mod_sdem <- errorsarlm(taxa_bruta ~ variavel_x, data = mg_dados, listw = lw, Durbin = TRUE)

#Configuração da Tabela
mapa_vars <- c(
  "(Intercept)"    = "Intercepto",
  "variavel_x"     = "$\\beta$",
  "lag.variavel_x" = "WX $\\theta$",      # Theta (SLX, SDM, SDEM)
  "rho"            = "$\\rho$",      # Rho (SAR, SDM)
  "lambda"         = "$\\lambda$"     # Lambda (SEM, SDEM)
)

mapa_gof <- list(
  list("raw" = "nobs", "clean" = "N", "fmt" = 0),
  list("raw" = "r.squared", "clean" = "$R^2$", "fmt" = 3),
  list("raw" = "aic", "clean" = "AIC", "fmt" = 1),
  list("raw" = "logLik", "clean" = "Log Likelihood", "fmt" = 1)
)

# Tabela Unificada (6 Modelos)
modelsummary(
  list(
    "OLS"  = mod_ols, 
    "SLX"  = mod_slx,
    "SAR"  = mod_sar, 
    "SEM"  = mod_sem,
    "SDM"  = mod_sdm,
    "SDEM" = mod_sdem
  ),
  coef_map = mapa_vars,      
  gof_map = mapa_gof,      
  estimate = "{estimate} [{conf.low}, {conf.high}]",
  statistic = NULL, 
  stars = c('*' = .05, '**' = .01, '***' = .001),
  title = NULL,    
  output = "kableExtra", 
  escape = FALSE
) %>%
  kable_styling(latex_options = c("HOLD_position"), 
                full_width = FALSE, 
                position = "center") %>%
  row_spec(c(5, 7, 9), bold = TRUE) %>% 
  as.character() %>%
  cat()


# Mapa dos Valores Ajustados
mg_dados$fitted_sdem <- fitted(mod_sdem)

g_fit <- ggplot(mg_dados) +
  geom_sf(aes(fill = fitted_sdem), color = NA) +
  scale_fill_viridis_c(option = "turbo", name = "Predito") +
  labs(title = "A. Valores Preditos (SDEM)", 
       subtitle = "Padrão recuperado (WX + Erro)") +
  theme_minimal() + 
  annotation_scale(location = "bl", width_hint = 0.3, bar_cols = c("black", "white")) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering)

# Diagnóstico dos Resíduos
mg_dados$resid_sdem <- residuals(mod_sdem)
moran_sdem <- moran.test(mg_dados$resid_sdem, lw)
mg_dados$resid_lag_sdem <- lag.listw(lw, mg_dados$resid_sdem)

g_resid_scatter <- ggplot(mg_dados, aes(x = resid_sdem, y = resid_lag_sdem)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray") +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE, color = "red", size = 0.8) +
  labs(title = "B. Scatter de Moran (Resíduos SDEM)", 
       subtitle = paste0("I de Moran: ", round(moran_sdem$estimate[1], 3), 
                         " (p-valor: ", round(moran_sdem$p.value, 3), ")"),
       x = "Resíduos", y = "Lag Espacial") +
  theme_minimal()


(g_fit | g_resid_scatter)


'............................................................
                Modelo  SARAR
............................................................'

if (!require("pacman")) install.packages("pacman")
pacman::p_load(spatialreg, spdep, sf, modelsummary, kableExtra, dplyr, ggplot2, patchwork, viridis, ggspatial, tidyr, Matrix)

# Ajuste do SARAR (SAC)
# Estima rho e lambda simultaneamente
mod_sarar <- sacsarlm(taxa_bruta ~ variavel_x, data = mg_dados, listw = lw)

mapa_vars <- c(
  "(Intercept)"    = "Intercepto",
  "variavel_x"     = "$\\beta$",
  "lag.variavel_x" = "WX $\\theta$",      
  "rho"            = "$\\rho$",      
  "lambda"         = "$\\lambda$"     
)

mapa_gof <- list(
  list("raw" = "nobs", "clean" = "N", "fmt" = 0),
  list("raw" = "r.squared", "clean" = "$R^2$", "fmt" = 3),
  list("raw" = "aic", "clean" = "AIC", "fmt" = 1),
  list("raw" = "logLik", "clean" = "Log Likelihood", "fmt" = 1)
)

modelsummary(
  list(
    "OLS"   = mod_ols, 
    "SLX"   = mod_slx,
    "SAR"   = mod_sar, 
    "SEM"   = mod_sem,
    "SDM"   = mod_sdm,
    "SDEM"  = mod_sdem,
    "SARAR" = mod_sarar
  ),
  coef_map = mapa_vars,      
  gof_map = mapa_gof,      
  estimate = "{estimate} [{conf.low}, {conf.high}]",
  statistic = NULL, 
  stars = c('*' = .05, '**' = .01, '***' = .001),
  title = NULL,     
  output = "kableExtra", 
  escape = FALSE
) %>%
  kable_styling(latex_options = c("HOLD_position"), 
                full_width = FALSE, 
                position = "center") %>%
  row_spec(c(5, 7, 9), bold = TRUE) %>% 
  as.character() %>%
  cat()


set.seed(123)
imp_sarar <- impacts(mod_sarar, listw = lw, R = 1000)

if (is.null(imp_sarar$res)) {
  imp_sarar <- impacts(mod_sarar, listw = lw, R = 1000, zstats = TRUE)
}

df_impactos <- data.frame(
  direct = imp_sarar$res$direct,
  indirect = imp_sarar$res$indirect
) %>%
  pivot_longer(cols = everything(), names_to = "Tipo", values_to = "Valor") %>%
  mutate(Tipo = factor(Tipo, levels = c("direct", "indirect"),
                       labels = c("Direto", "Indireto (Spillover Global)")))


g_impactos <- ggplot(df_impactos, aes(x = Tipo, fill = Tipo, y=Valor)) +
  geom_col(width = 0.2, color = "gray30") +
  scale_fill_manual(values = c("Direto" = "#1b9e77", "Indireto (Spillover)" = "#d95f02")) +
  labs(title = "A. Impactos: Direto vs. Indireto", 
       y = "Magnitude do efeito", x = NULL) +
  theme_minimal() + 
  theme(legend.position = "none", 
        legend.title = element_blank())

# Mapa dos Valores Ajustados
mg_dados$fitted_sarar <- fitted(mod_sarar)

g_fit <- ggplot(mg_dados) +
  geom_sf(aes(fill = fitted_sarar), color = NA) +
  scale_fill_viridis_c(option = "turbo", name = "Predito") +
  labs(
    title = "B. Valores Preditos (SARAR)", 
    subtitle = expression("Ajuste simultâneo" ~ (rho + lambda))
  ) +
  theme_minimal() + 
  annotation_scale(location = "bl", width_hint = 0.3, bar_cols = c("black", "white")) +
  annotation_north_arrow(location = "tl", style = north_arrow_fancy_orienteering,
                         pad_x = unit(0.1, "in"), pad_y = unit(0.1, "in"))

# Diagnóstico dos Resíduos
mg_dados$resid_sarar <- residuals(mod_sarar)
moran_sarar <- moran.test(mg_dados$resid_sarar, lw)
mg_dados$resid_lag_sarar <- lag.listw(lw, mg_dados$resid_sarar)

g_resid_scatter <- ggplot(mg_dados, aes(x = resid_sarar, y = resid_lag_sarar)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray") +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE, color = "red", size = 0.8) +
  labs(title = "C. Scatter de Moran (Resíduos SARAR)", 
       subtitle = paste0("I de Moran: ", round(moran_sarar$estimate[1], 3), 
                         " (p-valor: ", round(moran_sarar$p.value, 3), ")"),
       x = "Resíduos", y = "Lag Espacial") +
  theme_minimal()


(g_impactos | g_fit | g_resid_scatter)


'............................................................
                 Modelo SMA
............................................................'

if (!require("pacman")) install.packages("pacman")
pacman::p_load(spatialreg, spdep, sf, modelsummary, kableExtra, dplyr, ggplot2, patchwork, viridis, ggspatial, tidyr, Matrix)


if (!exists("mg_dados")) {
  mg_dados <- geobr::read_municipality(code_muni = "MG", year = 2020, showProgress = FALSE)
  coords <- st_coordinates(st_centroid(mg_dados))
  set.seed(123)
  mg_dados$taxa_bruta <- (-coords[,2] * 10) + rnorm(nrow(mg_dados), 0, 5)
  mg_dados$variavel_x <- rnorm(nrow(mg_dados))
}

if (!exists("lw")) {
  nb <- poly2nb(mg_dados, queen = TRUE)
  lw <- nb2listw(nb, style = "W", zero.policy = TRUE)
}


mod_sma <- spautolm(taxa_bruta ~ variavel_x, data = mg_dados, listw = lw, family = "SMA")

sum_sma <- summary(mod_sma)

#Extrair Betas da matriz de coeficientes 
coefs_mat <- sum_sma$Coef # Matriz com Estimate, Std. Error, etc.
df_tidy_betas <- data.frame(
  term = rownames(coefs_mat),
  estimate = coefs_mat[, "Estimate"],
  std.error = coefs_mat[, "Std. Error"]
)

# Extrair Lambda
df_tidy_lambda <- data.frame(
  term = "lambda",
  estimate = mod_sma$lambda,
  std.error = mod_sma$lambda.se
)

#Unir tudo
df_tidy_sma <- rbind(df_tidy_betas, df_tidy_lambda)

#Calcular estatísticas finais
df_tidy_sma$statistic <- df_tidy_sma$estimate / df_tidy_sma$std.error
df_tidy_sma$p.value <- 2 * (1 - pnorm(abs(df_tidy_sma$statistic)))
df_tidy_sma$conf.low <- df_tidy_sma$estimate - (1.96 * df_tidy_sma$std.error)
df_tidy_sma$conf.high <- df_tidy_sma$estimate + (1.96 * df_tidy_sma$std.error)

#
df_glance_sma <- data.frame(
  nobs = length(residuals(mod_sma)),
  logLik = as.numeric(logLik(mod_sma)),
  aic = AIC(mod_sma),
  r.squared = NA
)

#
mod_sma_custom <- list(tidy = df_tidy_sma, glance = df_glance_sma)
class(mod_sma_custom) <- "modelsummary_list"

#
mapa_vars <- c(
  "(Intercept)"    = "Intercepto",
  "variavel_x"     = "$\\beta$",
  "lag.variavel_x" = "WX $\\theta$",      
  "rho"            = "$\\rho$",      
  "lambda"         = "$\\lambda$"     
)

mapa_gof <- list(
  list("raw" = "nobs", "clean" = "N", "fmt" = 0),
  list("raw" = "r.squared", "clean" = "$R^2$", "fmt" = 3),
  list("raw" = "aic", "clean" = "AIC", "fmt" = 1),
  list("raw" = "logLik", "clean" = "Log Likelihood", "fmt" = 1)
)

# Tabela Final
modelsummary(
  list(
    "OLS"   = mod_ols, 
    "SLX"   = mod_slx,
    "SAR"   = mod_sar, 
    "SEM"   = mod_sem,
    "SDM"   = mod_sdm,
    "SDEM"  = mod_sdem,
    "SARAR" = mod_sarar,
    "SMA"   = mod_sma_custom
  ),
  coef_map = mapa_vars,      
  gof_map = mapa_gof,      
  estimate = "{estimate} [{conf.low}, {conf.high}]",
  statistic = NULL, 
  stars = c('*' = .05, '**' = .01, '***' = .001),
  title = NULL,     
  output = "kableExtra", 
  escape = FALSE
) %>%
  kable_styling(latex_options = c("HOLD_position", "scale_down"),
                full_width = FALSE, 
                position = "center") %>%
  row_spec(c(5, 7, 9), bold = TRUE) %>% 
  as.character() %>%
  cat()


# Mapa
mg_dados$fitted_sma <- fitted(mod_sma)
g_fit <- ggplot(mg_dados) +
  geom_sf(aes(fill = fitted_sma), color = NA) +
  scale_fill_viridis_c(option = "turbo", name = "Predito") +
  labs(title = "A. Valores Preditos (SMA)") +
  theme_minimal() + 
  annotation_scale(location = "bl", width_hint = 0.3) +
  annotation_north_arrow(location = "tl", style = north_arrow_fancy_orienteering)

#Resíduos
mg_dados$resid_sma <- residuals(mod_sma)
moran_sma <- moran.test(mg_dados$resid_sma, lw)
mg_dados$resid_lag_sma <- lag.listw(lw, mg_dados$resid_sma)

g_resid_scatter <- ggplot(mg_dados, aes(x = resid_sma, y = resid_lag_sma)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray") +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE, color = "red", size = 0.8) +
  labs(title = "B. Scatter de Moran (Resíduos SMA)", 
       subtitle = paste0("I de Moran: ", round(moran_sma$estimate[1], 3), 
                         " (p-valor: ", round(moran_sma$p.value, 3), ")"),
       x = "Resíduos", y = "Lag Espacial") +
  theme_minimal()

( g_fit | g_resid_scatter)



'------------------------------------------------------------------------------
                    Modelos de suporte limitado
-------------------------------------------------------------------------------'


'............................................................
                 Probit
............................................................'

if (!require("pacman")) install.packages("pacman")
pacman::p_load(spatialprobit, spdep, sf, geobr, ggplot2, viridis, ggspatial, kableExtra, dplyr, Matrix, patchwork, scales)

# Preparação e Simulação (IGNORE ISTO)

if (!exists("sp_dados") || !("y_bin" %in% names(sp_dados))) {
  sp_dados <- geobr::read_municipality(code_muni = "SP", year = 2020, showProgress = FALSE)
  coords <- st_coordinates(st_centroid(sp_dados))
  
  knn <- knearneigh(coords, k = 6)
  nb_sp <- knn2nb(knn)
  lw_sp <- nb2listw(nb_sp, style = "W")
  W_mat <- as(lw_sp, "CsparseMatrix")
  
  set.seed(999)
  n <- nrow(sp_dados)
  rho_true <- 0.65
  beta_0 <- -1.0
  beta_1 <- 1.5
  
  sp_dados$x_var <- rnorm(n, 0, 1)
  
  I_n <- Matrix::Diagonal(n)
  inv_spatial <- solve(I_n - rho_true * W_mat)
  epsilon <- rnorm(n, 0, 1)
  
  y_latente <- as.vector(inv_spatial %*% (beta_0 + beta_1 * sp_dados$x_var + epsilon))
  
  # Y binário: 0 ou 1 (Corte em 0)
  sp_dados$y_bin <- ifelse(y_latente > 0, 1, 0)
  
} else {
  if (!exists("W_mat")) {
    knn <- knearneigh(st_coordinates(st_centroid(sp_dados)), k = 6)
    nb_sp <- knn2nb(knn)
    lw_sp <- nb2listw(nb_sp, style = "W")
    W_mat <- as(lw_sp, "CsparseMatrix")
  }
}

# Ajuste do Modelo PROBIT (Binário)

mod_sar_probit <- sarprobit(y_bin ~ x_var, 
                            W = W_mat, 
                            data = sp_dados, 
                            ndraw = 2000, 
                            burn.in = 500, 
                            showProgress = FALSE)

# Tabela de Resultados

all_draws1 <- as.data.frame(mod_sar_probit$B)
if (!is.null(mod_sar_probit$names)) colnames(all_draws1) <- mod_sar_probit$names
if (!any(grepl("rho", colnames(all_draws1), ignore.case = TRUE))) all_draws1$rho <- as.vector(mod_sar_probit$rho)

resumo_bayesiano1 <- data.frame(
  Parametro  = names(all_draws1),
  Estimativa = colMeans(all_draws1),
  IC_Inf     = apply(all_draws1, 2, quantile, probs = 0.025),
  IC_Sup     = apply(all_draws1, 2, quantile, probs = 0.975)
)

# Formatação da Tabela
resumo_bayesiano1$Resultado <- sprintf("%.3f [%.3f, %.3f]", 
                                       resumo_bayesiano1$Estimativa, 
                                       resumo_bayesiano1$IC_Inf, 
                                       resumo_bayesiano1$IC_Sup)


is_html_output <- knitr::is_html_output()
label_rho <- if(is_html_output) "&rho; (Dependência)" else "$\\rho$ (Dependência)"

resumo_bayesiano1$Parametro <- dplyr::case_when(
  resumo_bayesiano1$Parametro %in% c("(Intercept)", "beta_1") ~ "Intercepto",
  resumo_bayesiano1$Parametro %in% c("x_var", "beta_2") ~ "Variável X",
  grepl("rho", resumo_bayesiano1$Parametro, ignore.case = TRUE) ~ label_rho,
  TRUE ~ resumo_bayesiano1$Parametro
)

tabela_final1 <- resumo_bayesiano1 %>%
  filter(!duplicated(Parametro)) %>%
  dplyr::select(Parametro, Resultado)
rownames(tabela_final1) <- NULL


is_html_output <- knitr::is_html_output()
label_rho <- if(is_html_output) "&rho; (Dependência)" else "$\\rho$ (Dependência)"

kbl(tabela_final1, 
    #format = "latex",
    booktabs = TRUE, 
    align = "lc",  
    caption = NULL, 
    escape = FALSE) %>%
  kable_styling(latex_options = c("HOLD_position", "striped"), 
                full_width = FALSE, 
                position = "center") %>%
  row_spec(0, bold = TRUE) 


if (!require("pacman")) install.packages("pacman")
pacman::p_load(spatialprobit, spdep, sf, geobr, ggplot2, viridis, ggspatial, kableExtra, dplyr, Matrix, patchwork, scales)


#
X_mat <- model.matrix(~ x_var, data = sp_dados)

#
all_params <- colMeans(mod_sar_probit$B)
beta_vec <- all_params[1:ncol(X_mat)] 
rho_hat  <- mean(mod_sar_probit$rho)

#Predição Linear (X * Beta)
xb <- X_mat %*% beta_vec

#(I - rho*W)^-1
I_n <- Matrix::Diagonal(nrow(sp_dados))
S_inv <- solve(I_n - rho_hat * W_mat)

#y*_pred = S_inv * Xb
y_star_pred <- as.vector(S_inv %*% xb)

#
y_obs <- sp_dados$y_bin
lo <- ifelse(y_obs == 0, -Inf, 0)
hi <- ifelse(y_obs == 0, 0, Inf)

#
z_lo <- lo - y_star_pred
z_hi <- hi - y_star_pred

safe_pnorm <- function(q) pnorm(q)
safe_dnorm <- function(x) dnorm(x)

#
diff_cdf <- safe_pnorm(z_hi) - safe_pnorm(z_lo)
diff_cdf[diff_cdf < 1e-10] <- 1e-10 

#
diff_pdf <- safe_dnorm(z_lo) - safe_dnorm(z_hi) # pdf(lo) - pdf(hi)

#
# E[y* | y] = mu + sigma * (pdf_lo - pdf_hi) / (cdf_hi - cdf_lo)
sp_dados$y_latente_esperada <- y_star_pred + (diff_pdf / diff_cdf)

# u = (I - rho*W) * E[y*|y] - X*beta
A_mat <- (I_n - rho_hat * W_mat)
term_spatial_removed <- as.vector(A_mat %*% sp_dados$y_latente_esperada)

sp_dados$resid_generalized <- term_spatial_removed - as.vector(xb)

# Teste de Moran nos Resíduos Generalizados
moran_resid <- moran.test(sp_dados$resid_generalized, lw_sp)
label_moran <- paste0("Moran (Gen. Resid): ", round(moran_resid$estimate[1], 3), 
                      " (p: ", round(moran_resid$p.value, 3), ")")

# Impactos (Efeitos Marginais)
imp_probit <- impacts(mod_sar_probit)

df_imp <- data.frame(
  Tipo = factor(c("direct", "indirect", "total"), 
                levels = c("direct", "indirect", "total"),
                labels = c("Direto", "Indireto", "Total")),
  Valor = c(imp_probit$direct[, "posterior_mean"], 
            imp_probit$indirect[, "posterior_mean"], 
            imp_probit$total[, "posterior_mean"])
)

# PREDIÇÃO E CLASSIFICAÇÃO 
sp_dados$latente_predita <- as.vector(fitted(mod_sar_probit))

#Definição dos Cortes (Breaks): O vetor 'phi' contém os limites: 0 (fixo)
breaks_finais <- c(-Inf, 0, Inf)

#Classificação
sp_dados$cat_predita <- cut(sp_dados$latente_predita, 
                            breaks = breaks_finais, 
                            labels = FALSE)
sp_dados$cat_predita <- sp_dados$cat_predita-1

#
if(any(is.na(sp_dados$cat_predita))) {
  sp_dados$cat_predita_class[is.na(sp_dados$cat_predita)] <- 1
}


# Tema Customizado (Do seu exemplo)
theme_map_custom <- function() {
  list(
    theme_void(),
    theme(
      legend.position = "bottom", 
      legend.box.spacing = unit(5, "pt"),
      legend.title = element_text(size=9, face="bold"),
      plot.title = element_text(face="bold", size=12, hjust = 0),
      plot.subtitle = element_text(size=9, color="grey30")
    ),
    annotation_scale(location = "br", width_hint = 0.3),
    annotation_north_arrow(location = "tr", height = unit(1, "cm"), width = unit(1, "cm"),
                           style = north_arrow_fancy_orienteering)
  )
}

# CORES PADRONIZADAS
cor_zero <- "white"
cor_um   <- "#FDE725"

# Mapa Observado
g_obs <- ggplot(sp_dados) +
  geom_sf(aes(fill = as.factor(y_bin)), color = "black", lwd = 0.05) +
  scale_fill_manual(values = c("0" = cor_zero, "1" = cor_um), name = "Observado") +
  labs(title = "C. Observado (Binário)", subtitle = "Valor Real") +
  theme_void() + 
  theme(legend.position = "bottom", legend.box.spacing = unit(0, "pt")) +
  annotation_scale(location = "br", width_hint = 0.3) +
  annotation_north_arrow(location = "tr", height = unit(0.8, "cm"), width = unit(0.8, "cm"), 
                         style = north_arrow_fancy_orienteering)

#Mapa Predito
g_pred <- ggplot(sp_dados) +
  geom_sf(aes(fill = as.factor(cat_predita)), color = "black", lwd = 0.05) +
  scale_fill_manual(values = c("0" = cor_zero, "1" = cor_um), name = "Predito") +
  labs(title = "C. Predito pelo Modelo") +
  theme_void() + 
  theme(legend.position = "bottom", legend.box.spacing = unit(0, "pt")) +
  annotation_scale(location = "br", width_hint = 0.3) +
  annotation_north_arrow(location = "tr", height = unit(0.8, "cm"), width = unit(0.8, "cm"), 
                         style = north_arrow_fancy_orienteering)

#Gráfico de Impactos
g_imp <- ggplot(df_imp, aes(x = Tipo, y = Valor, fill = Tipo)) +
  geom_col(width = 0.5, color = "black", alpha = 0.9) +
  geom_text(aes(label = round(Valor, 3)), vjust = -0.5, fontface = "bold") +
  scale_fill_viridis_d(option = "viridis", begin = 0.3, end = 0.9) +
  labs(title = "A. Efeitos Marginais Médios", 
       y = "Mudança na Probabilidade", x = NULL) +
  theme_minimal() + theme(legend.position = "none")

#Mapa de Resíduos
g_resid <- ggplot(sp_dados) +
  geom_sf(aes(fill = resid_generalized), color = "black", lwd = 0.02) +
  scale_fill_distiller(palette = "RdBu", direction = -1,
                       name = "Resíduo") +
  labs(title = "D. Resíduos", 
       subtitle = paste0("Autocorrelação:\n", label_moran)) +
  theme_void()+
  theme(legend.box.spacing = unit(0, "pt")) +
  annotation_scale(location = "br", width_hint = 0.3) +
  annotation_north_arrow(location = "tr", height = unit(0.8, "cm"), width = unit(0.8, "cm"),
                         style = north_arrow_fancy_orienteering)

(g_obs | g_pred) / (g_imp | g_resid) + plot_layout(heights = c(1.2, 1))


'............................................................
                 Probit Ordenado
............................................................'

#
if (!require("pacman")) install.packages("pacman")
pacman::p_load(spatialprobit, spdep, sf, geobr, ggplot2, viridis, 
               kableExtra, dplyr, Matrix, patchwork, ggspatial, scales, truncnorm)

#Ignore esta parte

if (!exists("sp_dados") || !("y_ordered" %in% names(sp_dados))) {
  message("Baixando shapefile e simulando dados...")
  sp_dados <- geobr::read_municipality(code_muni = "SP", year = 2020, showProgress = FALSE)
  
  sp_dados <- sp_dados[!is.na(st_dimension(sp_dados)), ]
  coords <- st_coordinates(st_centroid(sp_dados))
  
  # Matriz de Vizinhança (k=6)
  knn <- knearneigh(coords, k = 6)
  nb_sp <- knn2nb(knn)
  lw_sp <- nb2listw(nb_sp, style = "W")
  W_mat <- as(lw_sp, "CsparseMatrix")
  
  set.seed(123) 
  n <- nrow(sp_dados)
  rho_true <- 0.60
  beta_x <- 2.0
  intercept_true <- -0.5
  
  sp_dados$x_var <- rnorm(n, 0, 1)
  
  # SAR: y* = (I - rho W)^-1 (Xb + e)
  I_n <- Matrix::Diagonal(n)
  inv_spatial <- solve(I_n - rho_true * W_mat)
  epsilon <- rnorm(n, 0, 1)
  
  xb <- intercept_true + (beta_x * sp_dados$x_var)
  y_latente <- as.vector(inv_spatial %*% (xb + epsilon))
  
  cortes_sim <- quantile(y_latente, probs = c(0.33, 0.66))
  sp_dados$y_ordered <- cut(y_latente, 
                            breaks = c(-Inf, cortes_sim, Inf), 
                            labels = FALSE)
} else {
  
  if (!exists("W_mat")) {
    knn <- knearneigh(st_coordinates(st_centroid(sp_dados)), k = 6)
    nb_sp <- knn2nb(knn)
    lw_sp <- nb2listw(nb_sp, style = "W")
    W_mat <- as(lw_sp, "CsparseMatrix")
  }
}

# AJUSTE DO MODELO (ORDERED PROBIT)
mod_sar_ordered <- sarorderedprobit(y_ordered ~ x_var, 
                                    W = W_mat, 
                                    data = sp_dados, 
                                    ndraw = 2000, 
                                    burn.in = 500, 
                                    showProgress = FALSE)

#TABELA DE RESULTADOS E IMPACTOS

all_draws <- as.data.frame(mod_sar_ordered$B)
if (!is.null(mod_sar_ordered$names)) colnames(all_draws) <- mod_sar_ordered$names
if (!any(grepl("rho", colnames(all_draws), ignore.case = TRUE))) all_draws$rho <- as.vector(mod_sar_ordered$rho)

resumo_bayesiano <- data.frame(
  Parametro  = names(all_draws),
  Estimativa = colMeans(all_draws),
  IC_Inf     = apply(all_draws, 2, quantile, probs = 0.025),
  IC_Sup     = apply(all_draws, 2, quantile, probs = 0.975)
)

# Formatação da Tabela
resumo_bayesiano$Resultado <- sprintf("%.3f [%.3f, %.3f]", 
                                      resumo_bayesiano$Estimativa, 
                                      resumo_bayesiano$IC_Inf, 
                                      resumo_bayesiano$IC_Sup)


resumo_bayesiano$Parametro <- dplyr::case_when(
  resumo_bayesiano$Parametro %in% c("(Intercept)", "beta_1") ~ "Intercepto",
  resumo_bayesiano$Parametro %in% c("x_var", "beta_2") ~ "Variável X",
  grepl("rho", resumo_bayesiano$Parametro, ignore.case = TRUE) ~ label_rho,
  TRUE ~ resumo_bayesiano$Parametro
)

tabela_final <- resumo_bayesiano %>%
  filter(!duplicated(Parametro)) %>%
  dplyr::select(Parametro, Resultado)
rownames(tabela_final) <- NULL

kbl(tabela_final, 
    #format = "latex", 
    booktabs = TRUE, 
    caption = "", 
    escape = FALSE) %>%
  kable_styling(latex_options = c("HOLD_position", "striped"), 
                full_width = FALSE, 
                position = "center") %>%
  row_spec(0, bold = TRUE) 


#
if (!require("pacman")) install.packages("pacman")
pacman::p_load(spatialprobit, spdep, sf, geobr, ggplot2, viridis, 
               kableExtra, dplyr, Matrix, patchwork, ggspatial, scales, truncnorm)


# Cálculo dos Impactos (Médios)
rho_medio <- mean(mod_sar_ordered$rho)
beta_val <- resumo_bayesiano$Estimativa[resumo_bayesiano$Parametro == "Variável X"]

impacto_total  <- beta_val / (1 - rho_medio)
impacto_direto <- beta_val 
impacto_indireto <- impacto_total - impacto_direto

df_imp <- data.frame(
  Tipo = factor(c("direct", "indirect", "total"), 
                levels = c("direct", "indirect", "total"),
                labels = c("Direto", "Indireto", "Total")),
  Valor = c(impacto_direto, impacto_indireto, impacto_total)
)



# PREDIÇÃO E CLASSIFICAÇÃO 
sp_dados$latente_predita <- as.vector(fitted(mod_sar_ordered))

#Definição dos Cortes (Breaks): O vetor 'phi' contém os limites: 0 (fixo) e o valor estimado
breaks_finais <- c(-Inf, mod_sar_ordered$phi, Inf)

#Classificação
sp_dados$cat_predita_class <- cut(sp_dados$latente_predita, 
                                  breaks = breaks_finais, 
                                  labels = FALSE)

#
if(any(is.na(sp_dados$cat_predita_class))) {
  sp_dados$cat_predita_class[is.na(sp_dados$cat_predita_class)] <- 1
}


#CÁLCULO DOS RESÍDUOS GENERALIZADOS

#Chesher, A. and Irish, M., 1987. Residual analysis in the grouped and censored normal linear model. Journal of Econometrics, 34(1-2), pp.33-61.

#Gourieroux, C., Monfort, A., Renault, E. and Trognon, A., 1987. Generalised residuals. Journal of econometrics, 34(1-2), pp.5-32.


beta_hat <- resumo_bayesiano$Estimativa[resumo_bayesiano$Parametro == "Variável X"] 
intercepto <- resumo_bayesiano$Estimativa[resumo_bayesiano$Parametro == "Intercepto"]
rho_hat <- mean(mod_sar_ordered$rho)
cuts <- c(-Inf, 0, mod_sar_ordered$phi, Inf) # Cuts: 0 é fixo no spatialprobit

#y* = (I - rho*W)^-1 * (X*beta)

X_mat <- model.matrix(~ x_var, data = sp_dados) 
betas_vec <- c(intercepto, beta_hat) # Ordem deve bater com X_mat

#X * Beta
xb <- X_mat %*% betas_vec

#(I - rho * W)^-1

I_n <- Matrix::Diagonal(nrow(sp_dados)) #I
S_inv <- solve(I_n - rho_hat * W_mat) #(I - rho * W)^-1
y_star_pred <- as.vector(S_inv %*% xb)  #(I - rho * W)^-1 *xb

#E[y* | y_obs]: mu + sigma * (pdf(a) - pdf(b)) / (cdf(b) - cdf(a))

y_obs <- as.numeric(sp_dados$y_ordered)
lo <- cuts[y_obs]     # Limite inferior da categoria observada
hi <- cuts[y_obs + 1] # Limite superior da categoria observada

z_lo <- lo - y_star_pred
z_hi <- hi - y_star_pred

safe_pnorm <- function(q) pnorm(q)
safe_dnorm <- function(x) dnorm(x)

diff_cdf <- safe_pnorm(z_hi) - safe_pnorm(z_lo)
diff_cdf[diff_cdf < 1e-10] <- 1e-10 

diff_pdf <- safe_dnorm(z_lo) - safe_dnorm(z_hi) # Note a ordem: pdf(lo) - pdf(hi)

# E[y* | y] = mu + (phi(lo) - phi(hi)) / (Phi(hi) - Phi(lo))
sp_dados$y_latente_esperada <- y_star_pred + (diff_pdf / diff_cdf)

# u = (I - rho*W) * E[y*|y] - X*beta
A_mat <- (I_n - rho_hat * W_mat)
term_spatial_removed <- as.vector(A_mat %*% sp_dados$y_latente_esperada)

sp_dados$resid_generalized <- term_spatial_removed - as.vector(xb)

#Teste de Moran
moran_resid <- moran.test(sp_dados$resid_generalized, lw_sp)

label_moran <- paste0("Moran (Gen. Resid): ", round(moran_resid$estimate[1], 3), 
                      " (p: ", round(moran_resid$p.value, 3), ")")

max_res <- max(abs(sp_dados$resid_generalized), na.rm=TRUE)

# Graficos
theme_map_custom <- function() {
  list(
    theme_void(),
    theme(
      legend.position = "bottom", 
      legend.box.spacing = unit(5, "pt"),
      legend.title = element_text(size=9, face="bold"),
      plot.title = element_text(face="bold", size=12, hjust = 0),
      plot.subtitle = element_text(size=9, color="grey30")
    ),
    annotation_scale(location = "br", width_hint = 0.3),
    annotation_north_arrow(location = "tr", height = unit(1, "cm"), width = unit(1, "cm"),
                           style = north_arrow_fancy_orienteering)
  )
}

#Observado
g_obs <- ggplot(sp_dados) +
  geom_sf(aes(fill = factor(y_ordered, levels = 1:3)), color = "white", lwd = 0.02) +
  scale_fill_viridis_d(option = "viridis", name = "Observed", drop = FALSE) +
  labs(title = "A. Dados Observados", subtitle = "Variável Dependente Real") +
  theme_map_custom()

#Predito
g_pred <- ggplot(sp_dados) +
  geom_sf(aes(fill = factor(cat_predita_class, levels = 1:3)), color = "white", lwd = 0.02) +
  scale_fill_viridis_d(option = "viridis", name = "Predicted", drop = FALSE) +
  labs(title = "B. Predição do Modelo") +
  theme_map_custom()

#Impactos
g_imp <- ggplot(df_imp, aes(x = Tipo, y = Valor, fill = Tipo)) +
  geom_col(width = 0.6, color = "black", alpha = 0.8) +
  geom_text(aes(label = round(Valor, 2)), vjust = -0.5, size=4, fontface = "bold") +
  scale_fill_viridis_d(option = "cividis", begin = 0.2, end = 0.8) +
  labs(title = "C. Decomposição de Impactos", y = "Magnitude", x = NULL) +
  theme_minimal() + 
  theme(legend.position = "none", panel.grid.minor = element_blank())

#Resíduos
max_res <- max(abs(sp_dados$resid_generalized), na.rm=TRUE)
g_resid <- ggplot(sp_dados) +
  geom_sf(aes(fill = resid_generalized), color = "white", lwd = 0.02) +
  scale_fill_distiller(palette = "RdBu", direction = -1, 
                       limits = c(-max_res, max_res),
                       name = "Resíduo") +
  labs(title = "D. Resíduos", 
       subtitle = paste0("Autocorrelação:\n", label_moran)) +
  theme_map_custom()


g_obs+g_pred+g_imp+g_resid

'............................................................
                 Tobit
............................................................'

if (!require("pacman")) install.packages("pacman")
pacman::p_load(spatialprobit, spdep, sf, geobr, ggplot2, viridis, kableExtra, dplyr, Matrix, patchwork, ggspatial, scales)

# Preparação e Simulação (DADOS TOBIT - CENSURA EM 0)

if (!exists("sp_dados") || !("y_tobit" %in% names(sp_dados))) {
  sp_dados <- geobr::read_municipality(code_muni = "SP", year = 2020, showProgress = FALSE)
  coords <- st_coordinates(st_centroid(sp_dados))
  
  knn <- knearneigh(coords, k = 6)
  nb_sp <- knn2nb(knn)
  lw_sp <- nb2listw(nb_sp, style = "W")
  W_mat <- as(lw_sp, "CsparseMatrix")
  
  set.seed(123)
  n <- nrow(sp_dados)
  rho_true <- 0.60
  beta_x <- 2.0
  sigma_true <- 1.5 
  
  sp_dados$x_var <- rnorm(n, 0, 1)
  
  I_n <- Matrix::Diagonal(n)
  inv_spatial <- solve(I_n - rho_true * W_mat)
  epsilon <- rnorm(n, 0, sigma_true)
  
  y_latente <- as.vector(inv_spatial %*% (-1 + beta_x * sp_dados$x_var + epsilon))
  
  sp_dados$y_tobit <- pmax(0, y_latente)
  
} else {
  if (!exists("W_mat")) {
    knn <- knearneigh(st_coordinates(st_centroid(sp_dados)), k = 6)
    nb_sp <- knn2nb(knn)
    lw_sp <- nb2listw(nb_sp, style = "W")
    W_mat <- as(lw_sp, "CsparseMatrix")
  }
}

# Ajuste do Modelo (SAR TOBIT)
mod_sar_tobit <- sartobit(y_tobit ~ x_var, 
                          W = W_mat, 
                          data = sp_dados, 
                          ndraw = 1000, 
                          burn.in = 200, 
                          showProgress = FALSE)

# Tabela de Resultados 

draws_beta <- as.data.frame(mod_sar_tobit$B)
if (!is.null(mod_sar_tobit$names) && length(mod_sar_tobit$names) == ncol(draws_beta)) {
  colnames(draws_beta) <- mod_sar_tobit$names
}

#
if (!is.null(mod_sar_tobit$pdraw)) {
  draws_rho <- data.frame(rho = as.vector(mod_sar_tobit$pdraw))
} else {
  draws_rho <- data.frame(rho = as.vector(mod_sar_tobit$rho))
}

#
draws_sigma <- data.frame(sigma2 = as.vector(mod_sar_tobit$sdraw))

#
if ("rho" %in% colnames(draws_beta)) {
  draws_beta <- draws_beta[, !colnames(draws_beta) %in% "rho"]
}

all_draws <- cbind(draws_beta, draws_rho, draws_sigma)

# Estatísticas
resumo_bayesiano <- data.frame(
  Parametro  = names(all_draws),
  Estimativa = colMeans(all_draws),
  IC_Inf     = apply(all_draws, 2, quantile, probs = 0.025),
  IC_Sup     = apply(all_draws, 2, quantile, probs = 0.975)
)

resumo_bayesiano$Resultado <- sprintf("%.3f [%.3f, %.3f]", 
                                      resumo_bayesiano$Estimativa, 
                                      resumo_bayesiano$IC_Inf, 
                                      resumo_bayesiano$IC_Sup)

label_sigma <- if(is_html_output) "&sigma;<sup>2</sup> (Variância)" else "$\\sigma^2$ (Variância)"
# Renomear
resumo_bayesiano$Parametro <- dplyr::case_when(
  resumo_bayesiano$Parametro %in% c("(Intercept)", "beta_1") ~ "Intercepto",
  resumo_bayesiano$Parametro %in% c("x_var", "beta_2") ~ "Variável X",
  grepl("rho", resumo_bayesiano$Parametro, ignore.case = TRUE) ~ label_rho,
  grepl("sigma", resumo_bayesiano$Parametro, ignore.case = TRUE) ~ label_sigma,
  TRUE ~ resumo_bayesiano$Parametro
)

tabela_final <- resumo_bayesiano %>% dplyr::select(Parametro, Resultado)
rownames(tabela_final) <- NULL

kbl(tabela_final, 
    # format = "latex", 
    booktabs = TRUE, 
    caption = "", 
    escape = FALSE) %>%
  kable_styling(latex_options = c("HOLD_position", "striped"), 
                full_width = FALSE, 
                position = "center") %>%
  row_spec(0, bold = TRUE) %>%
  footnote(general = "Estimativas: Média a Posteriori [Intervalo de Credibilidade 95%].") 


if (!require("pacman")) install.packages("pacman")
pacman::p_load(spatialprobit, spdep, sf, geobr, ggplot2, viridis, kableExtra, 
               dplyr, Matrix, patchwork, ggspatial, scales)


# A. Impactos (Marginais na Latente)
beta_val <- resumo_bayesiano$Estimativa[resumo_bayesiano$Parametro == "Variável X"]
rho_val  <- mean(draws_rho$rho, na.rm = TRUE)

#Cálculo
impacto_total_latente  <- beta_val / (1 - rho_val)
impacto_direto_latente <- beta_val
impacto_indireto_latente <- impacto_total_latente - impacto_direto_latente

#
df_imp <- data.frame(
  Tipo = factor(c("direct", "indirect", "total"), 
                levels = c("direct", "indirect", "total"),
                labels = c("Direto", "Indireto", "Total")),
  Valor = c(impacto_direto_latente, 
            impacto_indireto_latente, 
            impacto_total_latente)
)


# Resíduos Generalizados (Chesher & Irish, 1987)
beta_hat <- colMeans(draws_beta)
rho_hat  <- mean(draws_rho$rho)
sigma_hat <- sqrt(mean(draws_sigma$sigma2)) 

# Matrizes
X_mat <- model.matrix(~ x_var, data = sp_dados)

beta_hat <- beta_hat[colnames(draws_beta) %in% colnames(X_mat) | colnames(draws_beta) == "(Intercept)"]

xb <- X_mat %*% beta_hat

I_n <- Matrix::Diagonal(nrow(sp_dados))
S_inv <- solve(I_n - rho_hat * W_mat)

# Média Latente (Sem censura): mu = (I - rho W)^-1 Xb
y_star_mu <- as.vector(S_inv %*% xb)

# E[y* | y]: 
# Se y > 0: E = y_obs
# Se y = 0: E = mu - sigma * (pdf(z)/cdf(z)), onde z = (0 - mu)/sigma 

y_obs <- sp_dados$y_tobit
z_score <- (0 - y_star_mu) / sigma_hat
mills_ratio <- dnorm(z_score) / pnorm(z_score)

y_latente_generalized <- y_obs
censurados <- (y_obs == 0)

# E[y* | y* < 0] é mu - sigma * lambda(-z_score)
y_latente_generalized[censurados] <- y_star_mu[censurados] - (sigma_hat * mills_ratio[censurados])

# u = (I - rho W) * y_generalized - Xb
term_spatial_removed <- as.vector((I_n - rho_hat * W_mat) %*% y_latente_generalized)
sp_dados$resid_generalized <- term_spatial_removed - as.vector(xb)

# Moran
moran_resid <- moran.test(sp_dados$resid_generalized, lw_sp)
label_moran <- paste0("I de Moran: ", round(moran_resid$estimate[1], 3), 
                      " (p: ", round(moran_resid$p.value, 3), ")")

# Cores
cor_zero <- "white"
cor_um   <- "#FDE725"

# PLOTS
sp_dados$y_pred_censurado <- pmax(0, y_star_mu) 

# C. Mapa Observado 
g_obs <- ggplot(sp_dados) +
  geom_sf(aes(fill = y_tobit), color = "black", lwd = 0.05) +
  scale_fill_viridis_c(option = "magma", direction = -1, name = "Real") +
  labs(title = "C. Observado (y)", subtitle = "Valores Reais (Censurados em 0)") +
  theme_void() + 
  theme(legend.position = "bottom", legend.box.spacing = unit(0, "pt")) +
  annotation_scale(location = "br", width_hint = 0.3) +
  annotation_north_arrow(location = "tr", height = unit(0.8, "cm"), width = unit(0.8, "cm"), 
                         style = north_arrow_fancy_orienteering)

# Mapa Predito
g_pred <- ggplot(sp_dados) +
  geom_sf(aes(fill = y_pred_censurado), color = "black", lwd = 0.05) +
  scale_fill_viridis_c(option = "magma", direction = -1, name = "Predito",
                       limits = c(0, max(sp_dados$y_tobit))) +
  labs(title = "D. Predição de Valores (y)", subtitle = "Expectativa dos Valores Observáveis") +
  theme_void() + 
  theme(legend.position = "bottom", legend.box.spacing = unit(0, "pt")) +
  annotation_scale(location = "br", width_hint = 0.3) +
  annotation_north_arrow(location = "tr", height = unit(0.8, "cm"), width = unit(0.8, "cm"),
                         style = north_arrow_fancy_orienteering)

#Impactos
g_imp <- ggplot(df_imp, aes(x = Tipo, y = Valor, fill = Tipo)) +
  geom_col(width = 0.5, color = "black", alpha = 0.9) +
  geom_text(aes(label = round(Valor, 3)), vjust = -0.5, fontface = "bold") +
  scale_fill_viridis_d(option = "viridis", begin = 0.3, end = 0.9) +
  labs(title = "A. Impactos na Latente (y*)", y = "Mudança", x = NULL) +
  theme_minimal() + theme(legend.position = "none")

# Mapa de Resíduos
g_resid <- ggplot(sp_dados) +
  geom_sf(aes(fill = resid_generalized), color = "black", lwd = 0.05) +
  scale_fill_gradient2(low = "#440154", mid = "white", high = "#FDE725", midpoint = 0,
                       limits = c(-max_res, max_res), name = "Resíduo") +
  labs(title = "B. Resíduos Generalizados", subtitle = label_moran) +
  theme_void() + 
  theme(legend.position = "bottom", legend.box.spacing = unit(0, "pt")) +
  annotation_scale(location = "br", width_hint = 0.3) +
  annotation_north_arrow(location = "tr", style = north_arrow_fancy_orienteering)

(g_obs | g_pred) / (g_imp | g_resid) + plot_layout(heights = c(1.2, 1))


'--------------------------------------------------------------------------------------------
                                  Modelos locais
--------------------------------------------------------------------------------------------'

'-------------------------------------------------------------------------------------------
                                  GWR
-------------------------------------------------------------------------------------------'
if (!require("pacman")) install.packages("pacman")
pacman::p_load(GWmodel, sf, sp, spdep, ggplot2, viridis, gridExtra, dplyr,kableExtra, geobr, ggspatial)

if (!exists("mg_dados")) {
  mg_dados <- geobr::read_municipality(code_muni = "MG", year = 2020, showProgress = FALSE)
}

#
mg_proj <- st_transform(mg_dados, 5880)

set.seed(999) 
coords <- st_coordinates(st_centroid(mg_proj))
n <- nrow(mg_proj)

# Variável X1
mg_proj$X1 <- rnorm(n, 10, 2)

# Variável X2
mg_proj$X2 <- 0.3 * mg_proj$X1 + rnorm(n, 5, 1)

lat_norm <- (coords[,2] - min(coords[,2])) / (max(coords[,2]) - min(coords[,2]))
beta1_local <- 0.5 + (2.0 * lat_norm) 

lon_norm <- (coords[,1] - min(coords[,1])) / (max(coords[,1]) - min(coords[,1]))
beta2_local <- 3.0 - (1.5 * lon_norm)

# Variável Dependente Y = Intercepto + Beta1*X1 + Beta2*X2 + Erro
mg_proj$Y <- 10 + (beta1_local * mg_proj$X1) + (beta2_local * mg_proj$X2) + rnorm(n, 0, 1)





#Conversão para Objeto Spatial (Requisito do GWmodel)
mg_sp <- as(mg_proj, "Spatial")


#DIAGNÓSTICO DE COLINEARIDADE LOCAL

# Seleção da largura da banda (bandwidth)
bw_diag <- bw.gwr(Y ~ X1 + X2, data = mg_sp, approach = "AICc", 
                  kernel = "bisquare", adaptive = TRUE)

paste("Bandwidth Ótimo (k vizinhos):", bw_diag)

#
collin_diag <- gwr.collin.diagno(Y ~ X1 + X2, data = mg_sp, bw = bw_diag, 
                                 kernel = "bisquare", adaptive = TRUE)

print(summary(collin_diag$SDF$local_CN))

#Ajuste do Modelo GWR
gwr_model <- gwr.basic(Y ~ X1 + X2, 
                       data = mg_sp, 
                       bw = bw_diag, 
                       kernel = "bisquare", 
                       adaptive = TRUE, 
                       F123.test = TRUE)

#print(gwr_model)


#INFERÊNCIA  (descomente leva tempo para rodar)
#mc_test <- gwr.montecarlo(Y ~ X1 + X2, 
#                          data = mg_sp, 
#                          nsims = 99, 
#                          kernel = "bisquare", 
#                         adaptive = TRUE, 
#                         bw = bw_diag)
#print(mc_test)

gwr_adj <- gwr.t.adjust(gwr_model)


#Nota, precisamos extrair os resultados para fazer os mapas

# Extrair resultados para SF
results_sf <- st_as_sf(gwr_adj$SDF)

#Estimativa do Coeficiente Local (Beta X1)
p_beta <- ggplot(results_sf) +
  geom_sf(aes(fill = X1_t), color = "white") +
  scale_fill_viridis_c(option = "turbo", name = expression(hat(beta)[1])) +
  labs(title = "A)") +
  theme_void()+
  annotation_scale(location = "br", width_hint = 0.3)+
  annotation_north_arrow(location = "tr", height = unit(1, "cm"), width = unit(1, "cm"),
                         style = north_arrow_fancy_orienteering)

p_beta2 <- ggplot(results_sf) +
  geom_sf(aes(fill = X2_t), color = "white") +
  scale_fill_viridis_c(option = "turbo", name = expression(hat(beta)[2])) +
  labs(title = "B)") +
  theme_void()+
  annotation_scale(location = "br", width_hint = 0.3)+
  annotation_north_arrow(location = "tr", height = unit(1, "cm"), width = unit(1, "cm"),
                         style = north_arrow_fancy_orienteering)

# valor-p ajustado
p_sig <- ggplot(results_sf) +
  geom_sf(aes(fill = X2_p_by < 0.05), color = "white", size = 0.05) +
  scale_fill_manual(values = c("TRUE" = "#377eb8", "FALSE" = "gray95"), 
                    name = "Significativo\n(p-adj < 0.05)") +
  labs(title = "C)") +
  theme_void() +
  theme(legend.position = "bottom")+
  annotation_scale(location = "br", width_hint = 0.3)+
  annotation_north_arrow(location = "tr", height = unit(1, "cm"), width = unit(1, "cm"),
                         style = north_arrow_fancy_orienteering)

# Resíduos
nb <- poly2nb(results_sf, queen = TRUE)
lw <- nb2listw(nb, style = "W", zero.policy = TRUE)

results_sf$residual <- gwr_model$SDF$residual
moran_gwr <- moran.test(results_sf$residual, lw, zero.policy = TRUE)
label_moran <- paste0("I de Moran: ", round(moran_gwr$estimate[1], 3), 
                      " (p = ", round(moran_gwr$p.value, 3), ")")

p_resid <- ggplot(results_sf) +
  geom_sf(aes(fill = residual), color = "white", size = 0.05) +
  scale_fill_gradient2(low = "#d73027", mid = "white", high = "#4575b4", 
                       midpoint = 0, name = "Resíduos") +
  labs(title = "D)", 
       subtitle = label_moran) +
  theme_void() +
  annotation_scale(location = "br", width_hint = 0.3)+
  annotation_north_arrow(location = "tr", height = unit(1, "cm"), width = unit(1, "cm"),
                         style = north_arrow_fancy_orienteering)


(p_beta |p_beta2 )/(p_sig |p_resid)



'..........................................................................................
                                          MGWR
...........................................................................................'

if (!require("pacman")) install.packages("pacman")
pacman::p_load(GWmodel, sf, sp, ggplot2, viridis, gridExtra, kableExtra, dplyr, ggspatial)

#CALIBRAÇÃO DO MODELO MGWR 

mgwr_model <- gwr.multiscale(
  formula = Y ~ X1 + X2,
  data = mg_sp,
  kernel = "bisquare",
  adaptive = TRUE,         
  criterion = "dCVR",      # Critério de convergência robusto
  threshold = 1e-5,        # Tolerância para convergência
  max.iterations = 100,   
  predictor.centered = c(TRUE, TRUE) # Centralizar X1 e X2
)


#ANÁLISE DAS ESCALAS ESPACIAIS (BANDWIDTHS) 

# Extrair bandwidths do objeto
bws_mgwr <- mgwr_model$GW.arguments$bws
names(bws_mgwr) <- c("Intercepto", "X1", "X2") 

df_bws <- data.frame(
  Variavel = names(bws_mgwr),
  Bandwidth_Otimizado = as.numeric(bws_mgwr),
  N_Total = nrow(mg_sp)
) %>%
  mutate(
    Proporcao_N = round((Bandwidth_Otimizado / N_Total) * 100, 1),
    Escala_Interpretada = case_when(
      Bandwidth_Otimizado < N_Total * 0.2 ~ "Local (Heterogêneo)",
      Bandwidth_Otimizado > N_Total * 0.8 ~ "Global (Estacionário)",
      TRUE ~ "Regional (Intermediária)"
    )
  )

kbl(df_bws, 
    caption = "Escalas Espaciais Identificadas pelo MGWR",
    col.names = c("Variável", "Vizinhos (k)", "N Total", "% da Amostra", "Interpretação"),
    booktabs = TRUE) %>%
  kable_styling(latex_options = "HOLD_position", full_width = FALSE) %>%
  row_spec(0, bold = TRUE)

#COMPARAÇÃO DE AJUSTE: GWR vs MGWR
if (exists("gwr_model")) {
  aic_gwr <- gwr_model$GW.diagnostic$AICc
  aic_mgwr <- mgwr_model$GW.diagnostic$AICc 
  
  df_comp <- data.frame(
    Modelo = c("GWR", "MGWR"),
    AICc = c(aic_gwr, aic_mgwr),
    Melhoria = c("-", sprintf("%.2f", aic_gwr - aic_mgwr))
  )
  
  print(
    kbl(df_comp, caption = "Comparação de Ajuste (AICc)", booktabs = TRUE) %>%
      kable_styling(bootstrap_options = c("striped", "hover"), full_width = FALSE)
  )
}


#VISUALIZAÇÃO DOS RESULTADOS
mgwr_sf <- st_as_sf(mgwr_model$SDF)

#Coeficiente Local X1
p_beta_mgwr <- ggplot(mgwr_sf) +
  geom_sf(aes(fill = X1), color = "white") +
  scale_fill_viridis_c(option = "turbo", name = expression(hat(beta)[X1] ~ "(MGWR)")) +
  labs(title = "A)") +
  theme_void() +
  annotation_scale(location = "br", width_hint = 0.3)+
  annotation_north_arrow(location = "tr", height = unit(1, "cm"), width = unit(1, "cm"),
                         style = north_arrow_fancy_orienteering)

#Coeficiente Local X2
p_beta2_mgwr <- ggplot(mgwr_sf) +
  geom_sf(aes(fill = X2), color = "white") +
  scale_fill_viridis_c(option = "turbo", name = expression(hat(beta)[X2] ~ "(MGWR)")) +
  labs(title = "B)") +
  theme_void() +
  annotation_scale(location = "br", width_hint = 0.3)+
  annotation_north_arrow(location = "tr", height = unit(1, "cm"), width = unit(1, "cm"),
                         style = north_arrow_fancy_orienteering)

#Significância (t-value > 1.96) para X1
p_sig_mgwr <- ggplot(mgwr_sf) +
  geom_sf(aes(fill = abs(X1_TV) > 1.96), color = "white", size = 0.05) +
  scale_fill_manual(values = c("TRUE" = "#377eb8", "FALSE" = "gray95"), 
                    name = "Signif. (t > 1.96)") +
  labs(title = "C. Significância X1", 
       subtitle = "Inferência Local") +
  theme_void() +
  annotation_scale(location = "br", width_hint = 0.3)+
  annotation_north_arrow(location = "tr", height = unit(1, "cm"), width = unit(1, "cm"),
                         style = north_arrow_fancy_orienteering)

#I de Moran
nb <- spdep::poly2nb(mgwr_sf, queen = TRUE)
lw <- spdep::nb2listw(nb, style = "W", zero.policy = TRUE)

resid_col <- grep("resid", names(mgwr_sf), ignore.case = TRUE, value = TRUE)[1]
moran_mgwr <- spdep::moran.test(mgwr_sf[[resid_col]], lw, zero.policy = TRUE)

label_moran_m <- paste0("I de Moran: ", round(moran_mgwr$estimate[1], 3), 
                        " (p = ", round(moran_mgwr$p.value, 3), ")")

p_resid_mgwr <- ggplot(mgwr_sf) +
  geom_sf(aes(fill = .data[[resid_col]]), color = "white", size = 0.05) +
  scale_fill_gradient2(low = "#d73027", mid = "white", high = "#4575b4", 
                       midpoint = 0, name = "Resíduos") +
  labs(title = "D. Resíduos MGWR", 
       subtitle = label_moran_m) +
  theme_void() +
  annotation_scale(location = "br", width_hint = 0.3)+
  annotation_north_arrow(location = "tr", height = unit(1, "cm"), width = unit(1, "cm"),
                         style = north_arrow_fancy_orienteering)


grid.arrange(p_beta_mgwr, p_beta2_mgwr, p_sig_mgwr, p_resid_mgwr, ncol = 2)




'Recomendação leia capítulo 4 do material de apoio e aulas 5'




'............................................................
Nota: Com margem de erro de 10%, arrisco-me a dizer que, 
para quem atua na área da saúde, se pesquisa sobre "spatial ... 
(alguma coisa da saúde)", o que mais encontrará é a aplicação 
deste conteúdo (maior enfase no modelo BYM2), 
tanto em revistas de alto quanto de baixo impacto.

                       FIM
............................................................'







