'------------------------------------------------------------------------------
                                Aula 2
 ------------------------------------------------------------------------------'


# 0. INSTALAÇÃO E CARREGAMENTO DE PACOTES


if (!require("pacman")) installed.packages("pacman")

install.packages("")


if (!require("pacman")) install.packages("pacman")

pacman::p_load(
  sf,           # Simple Features (Pacote que mais usaremos)
  tidyverse,    # Manipulação de dados (comtém dplyr, ggplot2, etc)
  geobr,        # Para shapfiles do Brasil
  geodata,      # Para shapfiles do mundo todo incluindo Brasil
  terra,        # Dados Raster
  mapsf,        # Para visualização mais detalhada/catografia
  ggspatial,    # Para elementos de mapa para ggplot como scale bar, north arrow, etc.
  ggrepel,      # Para resolver problema de sobreposição de rótulos
  ggmapinset,   # Para destacar um lugar específico (Zoom e Insets, etc.)
  leaflet,      # Mapas Interativos via web
  units,        # Unidades de medida
  viridis,       # Paletas de cores
  tidyterra
)



'------------------------------------------------------------------------------
                                Pacote Sf
 ------------------------------------------------------------------------------'

browseVignettes("sf")

'------------------------------------------------------------------------------
                                Criação de objetos
 ------------------------------------------------------------------------------'
ponto <- st_point(c(2,3))
# Ponto
ponto <- st_point(c(2, 2))

# Linha
linha <- st_linestring()

linha1 <- rbind(c(1, 1), c(3, 3), c(4, 1))


linha <- st_linestring(linha1)

plot(linha)
# Polígono (deve fechar no mesmo ponto)

poly_coords <- rbind(c(1, 1), c(4, 1), c(4, 4), c(1, 1))

q <- list(poly_coords)

poligono <- st_polygon(list(poly_coords))
plot(poligono)



# Criar Coluna de Geometria (sfc) com CRS (WGS84 = 4326)
coluna_geo <- st_sfc(ponto, linha, poligono, crs = 4326)
plot(coluna_geo)

# Criar o Data Frame Espacial (sf)

meu_maoa <- st_sf(
  id=1:3,
  nome=c("Ponto A","Estrada B", "Terreno C"),
  geometry=coluna_geo
)

meu_mapa <- st_sf(
  id = 1:3,
  nome = c("Ponto A", "Estrada B", "Terreno C"),
  geometry = coluna_geo
)


'------------------------------------------------------------------------------
                  Leitura de Arquivos (Shapefiles, GeoJSON, etc)
 ------------------------------------------------------------------------------'

# Exemplo com dataset nativo do pacote (Carolina do Norte - EUA)

arquivo_nc <- system.file("shape/nc.shp", package = "sf")

nc <- read_sf(arquivo_nc)

glimpse(nc) # Estrutura dos dados

# Sistemas de Coordenadas (CRS)
st_crs(nc) # Verificar CRS atual (NAD27)

# Transformação de projeção (Reprojeção)

# Converter para UTM Zona 17N (Metros) - EPSG 32617
nc_utm <- st_transform(nc, crs = 32617)
st_crs(nc_utm)
# Converter para WGS84 (Lat/Long) - EPSG 4326
nc_gps <- st_transform(nc, crs = 4326)

plot(nc_utm$geometry)
'------------------------------------------------------------------------------
                 Operações Geométricas Unárias
 ------------------------------------------------------------------------------'

# Buffer (Área de influência) - 10km ao redor dos centroides
centroides <- st_centroid(nc_utm)
buffers <- st_buffer(centroides, dist = 10000)

glimpse(nc_utm)
glimpse(centroides)

nc_utm1 <- nc_utm

nc_utm1 <- st_transform(nc_utm1,crs=31982)
plot(centroides$geometry)
plot(nc_utm$geometry, add=TRUE)


# Visualização rápida
ggplot() +
  geom_sf(data = nc_utm, fill = "white") +
  geom_sf(data = buffers, fill = "red", alpha = 0.3) +
  geom_sf(data = centroides, color = "black") +
  theme_void()

'------------------------------------------------------------------------------
                 Operações Geométricas Binárias (Interseção, União)
 ------------------------------------------------------------------------------'

# Criar dois círculos de exemplo
c1 <- st_point(c(0, 0)) |> st_buffer(1) |> st_sfc() |> st_sf(id = "A")
c2 <- st_point(c(1, 0)) |> st_buffer(1) |> st_sfc() |> st_sf(id = "B")


par(mfrow=c(1,3))
plot(c2$st_sfc.st_buffer.st_point.c.1..0....1..)
plot(c1$st_sfc.st_buffer.st_point.c.0..0....1..)

# Interseção (área comum)
interseccao <- st_intersection(c1, c2)

plot(interseccao$st_sfc.st_buffer.st_point.c.0..0....1..)

'?O que conteceu?'

plot(c2$st_sfc.st_buffer.st_point.c.1..0....1..)
plot(c1$st_sfc.st_buffer.st_point.c.0..0....1.., add=TRUE)

plot(interseccao$st_sfc.st_buffer.st_point.c.0..0....1.., add=TRUE, col="black")


# Diferença (o que tem em A que não tem em B)
diferenca <- st_difference(c1, c2)


plot(c2$st_sfc.st_buffer.st_point.c.1..0....1..)
plot(c1$st_sfc.st_buffer.st_point.c.0..0....1.., add=TRUE)

plot(diferenca$st_sfc.st_buffer.st_point.c.0..0....1.., add=TRUE, col="black")


# Diferença simétrica (O que não é comum)
diferenca_sym <- st_sym_difference(c1, c2)

plot(c2$st_sfc.st_buffer.st_point.c.1..0....1..)
plot(c1$st_sfc.st_buffer.st_point.c.0..0....1.., add=TRUE)

plot(diferenca_sym$st_sfc.st_buffer.st_point.c.0..0....1.., add=TRUE, col="black")

# União
uniao <- st_union(c1, c2)

plot(c2$st_sfc.st_buffer.st_point.c.1..0....1..)
plot(c1$st_sfc.st_buffer.st_point.c.0..0....1.., add=TRUE)

plot(uniao$st_sfc.st_buffer.st_point.c.0..0....1.., add=TRUE, col="black")


'------------------------------------------------------------------------------
                 Medidas
 ------------------------------------------------------------------------------'
# Área
area_m2 <- st_area(nc_utm[1:5, ])
print(area_m2)

# Junções Espaciais (Spatial Joins)

# Unir atributos baseado na localização
# Exemplo simulado: Árvores dentro de Parques
p1 <- st_polygon(list(rbind(c(0,0), c(2,0), c(2,2), c(0,2), c(0,0))))

p2 <- st_polygon(list(rbind(c(2,0), c(4,0), c(4,2), c(2,2), c(2,0))))

parques <- st_sf(nome = c("Parque A", "Parque B"), geometry = st_sfc(p1, p2))
plot(parques$geometry)
plot(arvores$geometry, add=TRUE)

set.seed(123)
meus_dados <- data.frame(id = 1:5, x = runif(5, 0, 5), y = runif(5, 0, 3))
arvores <- st_as_sf(meus_dados, 
                    coords = c("x", "y"))

# Qual árvore está em qual parque?
join_resultado <- st_join(arvores, parques) 
print(join_resultado)

'------------------------------------------------------------------------------
                           PARTE 2: DADOS DO BRASIL (PACOTE geobr)
 ------------------------------------------------------------------------------'

# Baixar mapa de todos os estados (2010)

br_estados <- read_state(code_state = "all", 
                         year = 2010, showProgress = FALSE)


plot(br_estados$geom)
# Baixar apenas São Paulo
sp_estado <- read_state(code_state = "SP", year = 2010, 
                        showProgress = FALSE)

plot(sp_estado$geom)
# Exemplo: Mancha Urbana
urbano_rj <- read_urban_area(year = 2015, code_state = "RJ", 
                             showProgress = FALSE)
#browseVignettes("geobr")

ggplot() +
  geom_sf(data = br_estados, fill = NA, color = "gray") +
  geom_sf(data = sp_estado, fill = "blue", alpha = 0.5) +
  theme_void()+
  labs(title = "Brasil com destaque para SP")


'------------------------------------------------------------------------------
                           PARTE 3: DADOS GLOBAIS (PACOTE GEODATA)
 ------------------------------------------------------------------------------'

# Nota: path = tempdir() baixa para pasta temporária. Em projetos reais, defina uma pasta.

# Limites Administrativos (GADM) - Ex: Moçambique
moz_sf <- gadm(country = "MOZ", level = 1, path = tempdir()) |> st_as_sf()
plot(moz_sf$geometry)
Mapa_br <- read_sf("/home/almonha/Music/gadm41_BRA_shp/gadm41_BRA_0.shp")

plot(Mapa_br$geometry)
getwd() #ver diretorio
setwd("/home/almonha/webAlex/testando") #trocar local de trabalho
# Dados Climáticos (WorldClim) - Precipitação
prec_moz <- worldclim_country(country = "MOZ", var = "prec", res = 10, path = tempdir())
jan_prec <- prec_moz[[1]] # Apenas Janeiro

#Moz_juan <- st_intersection(prec_moz, jan_prec)
# Plotagem Raster + Vetor
ggplot() +
  geom_spatraster(data = jan_prec) +
  scale_fill_viridis_c(name = "Chuva (mm)", na.value = NA) +
  geom_sf(data = moz_sf, fill = NA, color = "white") +
  theme_void() +
  labs(title = "Precipitação em Moçambique (Janeiro)")

"? Tem áreas extras que não sao MOZ"

pacman::p_load(terra)

moz_vect <- vect(moz_sf)   

jan_prec_moz <- crop(jan_prec, moz_vect) #recortar

plot(jan_prec_moz)

jan_prec_moz <- mask(jan_prec_moz, moz_vect) #remove tudo fora do polígono "moz_vect"

plot(jan_prec_moz)

'------------------------------------------------------------------------------
                           PARTE 4: PACOTE mapsf
 ------------------------------------------------------------------------------'
#https://epsg.io link para ver projeção
# Preparação: Dados do Paraná
pr_mun <- read_municipality(code_muni = "PR", year = 2020, showProgress = FALSE)
pr_mun <- st_transform(pr_mun, crs = 31982) # SIRGAS 2000 / UTM 22S

st_crs(pr_mun)
# Simular dados populacionais
set.seed(123)
pr_mun$populacao <- sample(5000:150000, size = nrow(pr_mun), replace = TRUE)
pr_mun$populacao[pr_mun$name_muni == "Curitiba"] <- 1963726 # Outlier real
pr_mun$area_km2 <- as.numeric(st_area(pr_mun)) / 1e6  # 1000000
pr_mun$densidade <- pr_mun$populacao / pr_mun$area_km2

# defina o tema
mf_theme("iceberg")

mf_map(pr_mun, type="base", col="gray90", border="black")

mf_map(x=pr_mun, 
       var="densidade", 
       type = "choro", 
       breaks = "quantile",
       nbreaks = 6, 
       pal="YlOrRd",  # scico
       leg_title = expression("Densidade (hat/" ~ km^2 ~")"), 
       add=TRUE)

#pacman::p_load(scico)
# scico::scico_palette_show()

# Elementos Cartográficos
mf_title("Densidade Demográfica no Paraná")
mf_scale(size = 100)
mf_arrow(pos = "bottomleft")
mf_credits("Fonte: IBGE/geobr", pos = "bottomleft")


'------------------------------------------------------------------------------
                           PARTE 5: PACOTE ggmapinset
 ------------------------------------------------------------------------------'

# Zoom e Insets
# Foco na Província de Maputo (Moçambique)
maputo_alvo <- moz_sf |> filter(NAME_1 == "Maputo City") |> st_centroid()


config_zoom <- configure_inset(
  shape = shape_circle(centre=maputo_alvo, radius = 50),
  scale = 4,  # Zoom 4x
  translation =  c(400, -100), # Deslocamento,
  units = "km"
)

ggplot(moz_sf)+
  geom_sf_inset(fill=NA, show.legend = TRUE) +
  geom_inset_frame()+   #desenhe uma moldura explicativ
  coord_sf_inset(inset=config_zoom)+
  theme_void()+
  labs(title="Moçambique com Zoom em Maputo")+
  theme(panel.background = element_rect(fill="white", color = NA),
        plot.background = element_rect(fill = "white", color = NA)
        )



maputo_alvo1 <- moz_sf |> filter(NAME_1 == "Maputo City") 

config_zoom1 <- configure_inset(
  shape = shape_sf(maputo_alvo1),
  scale = 4,  # Zoom 4x
  translation =  c(400, -100), # Deslocamento,
  units = "km"
)


ggplot(moz_sf)+
  geom_sf_inset(fill=NA, show.legend = TRUE) +
  geom_inset_frame()+   #desenhe uma moldura explicativ
  coord_sf_inset(inset=config_zoom1)+
  theme_void()+
  labs(title="Moçambique com Zoom em Maputo")+
  theme(panel.background = element_rect(fill="white", color = NA),
        plot.background = element_rect(fill = "white", color = NA)
  )


'------------------------------------------------------------------------------
                           PARTE 5.2: PACOTE ggrepel
 ------------------------------------------------------------------------------'

# Destacar cidades grandes do PR sem sobrepor texto
cidades_grandes <- pr_mun |> filter(populacao > 200000)

ggplot(pr_mun) +
  geom_sf(fill = "gray95", color = "white") +
  geom_sf(data = cidades_grandes, color = "red") +
  geom_text_repel(
    data = cidades_grandes,
    aes(label = name_muni, geometry = geom),
    stat = "sf_coordinates", # Extrai X/Y automaticamente
    min.segment.length = 1
  ) +
  theme_void()


'------------------------------------------------------------------------------
                           PARTE 5.3: PACOTE ggspatial
 ------------------------------------------------------------------------------'

# Mapas Base e Escala
rj_mun <- read_municipality(code_muni = "RJ", year = 2020, showProgress = FALSE)

ggplot(rj_mun) +
  annotation_map_tile(type = "osm", progress = "none") + # Mapa de fundo (Internet necessária)
  geom_sf(fill = "orange", alpha = 0.4) +
  annotation_scale(location = "br") +
  annotation_north_arrow(location = "tl", style = north_arrow_minimal()) +
  theme_minimal()
'------------------------------------------------------------------------------
                           PARTE 6: PACOTE leaflet
 ------------------------------------------------------------------------------'

# Preparar dados: RM de São Paulo
sp_mun <- read_municipality(code_muni = "SP", year = 2020, showProgress = FALSE)
capital <- sp_mun[sp_mun$name_muni == "São Paulo", ]
vizinhos <- st_filter(sp_mun, capital, .predicate = st_touches)
rm_sp <- rbind(vizinhos,capital)

# Simular pontos de interesse (pois dados reais de escolas pesariam no exemplo)
set.seed(123)
pontos_poi <- st_sample(rm_sp, size = 50) |> st_as_sf()

pontos_poi$tipo <- sample(c("Escola", "Hospital"), 50, replace = TRUE)

# Gerar Mapa Web
leaflet(rm_sp)|>
  addTiles(group ="OSM" ) |>
  addProviderTiles(providers$CartoDB.Positron, group = "Clean") |>

  addPolygons(
  color = "blue", weight = 1, fillOpacity = 0.1,
  highlightOptions = highlightOptions(weight = 3, color = "red"),
  label = ~name_muni,
  group = "Municípios"
) |>

addCircleMarkers(
  data = pontos_poi,
  color = ifelse(pontos_poi$tipo=="Escola", "green", "red"),
  radius = 6, stroke = FALSE, fillOpacity = 0.8,
  popup = ~paste("Tipo:", tipo),
  group = "Locais"
)|>
  addLayersControl(
    baseGroups = c("Clean", "OSM"),
    overlayGroups = c("Municípios", "Locais")
  )


#==================FIM =================================
  
p_load(viridis)  #para cores
mf_theme(NULL)


#
mf_map(
  x = pr_mun, 
  var = "area_km2", 
  type = "choro",
  nbreaks = 5,
  border = "white",  
  lwd = 0.5,
  leg_pos = "bottomright",
  leg_title = expression("Área Territorial"~km^2)
)

#
mf_map(
  x = pr_mun,
  var = "populacao",
  type = "prop",
  inches = 0.25,        # Tamanho do maior círculo
  col = "gray50",   
  leg_pos = "topright",
  leg_title = "População Total",
  val_max = max(pr_mun$populacao), 
  add = TRUE
)

#penas para cidades > 300k hab para não poluir
big_cities <- pr_mun[pr_mun$populacao > 300000, ]

mf_label(
  x = big_cities, 
  var = "name_muni", 
  col = "black", 
  cex = 0.7, 
  overlap = FALSE, 
  lines = FALSE
)
mf_scale(size = 100, pos = "bottomleft") # Escala discreta
mf_arrow(pos = "bottomleft",adj = c(1, 1))




# 1. Carregar bases do Brasil
br <- read_country(year = 2020, showProgress = FALSE)
estados <- read_state(year = 2020, showProgress = FALSE)

pr_destaque <- estados[estados$abbrev_state == "PR", ]


p_load(viridis)  #para cores
mf_theme(NULL)


#
mf_map(
  x = pr_mun, 
  var = "area_km2", 
  type = "choro",
  nbreaks = 5,
  border = "white",  
  lwd = 0.5,
  leg_pos = "bottomright",
  leg_title = expression("Área Territorial"~km^2)
)

#
mf_map(
  x = pr_mun,
  var = "populacao",
  type = "prop",
  inches = 0.25,        # Tamanho do maior círculo
  col = "gray50",   
  leg_pos = "topleft",
  leg_title = "População Total",
  val_max = max(pr_mun$populacao), 
  add = TRUE
)

#penas para cidades > 300k hab para não poluir
big_cities <- pr_mun[pr_mun$populacao > 300000, ]

mf_label(
  x = big_cities, 
  var = "name_muni", 
  col = "yellow", 
  cex = 0.7, 
  overlap = FALSE, 
  lines = FALSE
)
mf_scale(size = 100, pos = "bottomleft") # Escala discreta
mf_arrow(pos = "bottomleft",adj = c(1, 1))


#INÍCIO DO INSET
mf_inset_on(x = br, pos = "topright", cex = 0.25)

#Desenhar todos os estados (Fundo)
mf_map(estados, col = "grey90", border = "white", lwd = 0.3)

#Desenhar o destaque
mf_map(pr_destaque, col = "black", border = NA, add = TRUE)

#Caixa
box(col = "grey50")

mf_inset_off()
