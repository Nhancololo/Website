'---------------------------------------------------------------------
                        Script Geostatística
---------------------------------------------------------------------'


## Pacote gstat

'pacote gstat é autor professor Edzer Pebesma

O pacote geoR autores Paulo Justiniano Ribeiro Jr e Peter Diggle
'


'---------------------------------------------------------------------
                                Passo 1
---------------------------------------------------------------------

1.  Dados Observados (Suporte Pontual): As amostras coletadas em campo.'


if (!require("pacman")) install.packages("pacman")
pacman::p_load(gstat, sf,sp, stars, ggplot2,patchwork, viridis, dplyr,gt, gridExtra)

'Saiba mais de gstat'

browseVignettes("gstat")

'Carregamento e conversão dos dados amostrais'

data(meuse)

'O CRS 28992 refere-se à projeção holandesa Amersfoort / RD New,
vc nos seus dados usaraa crs=4326 para graus ou 31983 para metros. Para Geos como
envolve distancia use 31983 que é projem metros'

'veja outras aqui: https://epsg.io/?q=Brazil%20kind:PROJCRS&page=1'

meuse_sf <- st_as_sf(meuse, coords = c("x", "y"), crs = 28992) 
glimpse(meuse_sf)

'2. Criação da Malha de Predição (Grid)'

data(meuse.area)

limite_sf <- st_polygon(list(as.matrix(meuse.area))) |> 
  st_sfc(crs = 28992) |> 
  st_sf() #'provavelmente vc terá um arquivo shapfile, use arquivo .shp'


grid_vetorial <- st_make_grid(limite_sf, cellsize = 40, what = "centers") |> #cellsize ->resolcao
  st_as_sf() |>
  st_filter(limite_sf) # Recorta o seu lol de estudo

#Conversão para STARS (Mais eficiente para o gstat)

grid_stars <- st_as_stars(st_bbox(limite_sf), dx = 40, dy = 40)
grid_stars <- st_crop(grid_stars, limite_sf) 

ggplot() +
  geom_sf(data = grid_vetorial, color="white") + # vc pode trocar 'grid_vetorial' por 'grid_stars'
  geom_sf(data = meuse_sf, color = "red", size = 0.5) +# e aqui trocar 'geom_sf' por geom_stars(data=grid_stars)
  geom_sf(data=limite_sf, color="black", fill=NA)+
  labs(title = "Domínio de Predição (Grade) e Amostras (Vermelho)") +
  theme_void()



'-----------------------------------------------------------------------------
                          Análise exploratória espacial
------------------------------------------------------------------------------
  
Antes de modelar, é necessário verificar a existência de dependência espacial.

`hscat:` Gráficos de dispersão defasados
'

hscat(log(zinc) ~ 1, data=meuse_sf, breaks = c(0, 100, 200, 400, 800))


'---------------------------------------------------------------------------
                          Modelagem da Covariância/semivariograma
----------------------------------------------------------------------------

O Variograma Experimental: `variogram`:

  1. Sintaxe: `variogram(object, locations, ...)`
  
  2. Argumentos:
    
    * formula: dita o tipo de krigagem a usar .
  
  * `cutoff:` A distância máxima de investigação. 
    Por convenção, limita-se a 1/3 da diagonal da área de estudo para garantir representatividade amostral nos lags.
  
  * `width:` A largura do intervalo de classe (tamanho do lag).

* `cloud:` Se TRUE, retorna a nuvem variográfica 

* `map:` Se TRUE, gera um mapa variográfico para inspeção de anisotropia.

* `alpha:` Direção em graus (ex: c(0, 45, 90, 135)) para investigar anisotropia.
'


v_exp <- variogram(log(zinc) ~ 1, meuse_sf, cutoff = 1200, width = 100)

plot(v_exp, main = "Semivariograma Experimental", 
     xlab = "Distância (m)", ylab = "Semivariância")


'-------------------------------------------------------------------------
                     Definição do Modelo Teórico: `vgm` 
-------------------------------------------------------------------------
O variograma experimental fornece pontos discretos. 
A krigagem exige uma função contínua e positiva definida. 
A função vgm estrutura este modelo.

* psill: Patamar parcial (variância estrutural).

* model: Família da curva.

- "Sph" (Esférico): Crescimento linear na origem, atinge patamar definido.

- "Exp" (Exponencial): Crescimento abrupto, atinge patamar assintoticamente.

- "Gau" (Gaussiano): Suave na origem (parabólico), indica alta continuidade.

* range: Alcance.

* nugget: Efeito pepita (erro na origem).

Para visualizar as famílias disponíveis, utiliza-se show.vgms().

'

modelo_inicial <- vgm(psill = 0.6, model = "Sph", range = 800, nugget = 0.05) 

'range 800 porque parece se estabilizar nele
no mesmo lugar que se estabiliza range 800, temos uma patamar (psill) de ~ 0.6 a 0.7
começa meio como linha reta obliquo, então aparenta ser esférico (Sph)
se olhar para onde se interceta o eixo y parece ser ~0.05 esse é efeito pepita (nugget)
'

'--------------------------------------------------------------
           Ajuste de Parâmetros: `fit.variogram `
---------------------------------------------------------------
A função `fit.variogram` ajusta os parâmetros do modelo teórico (`vgm`)
aos pontos experimentais (`v_exp`) utilizando Mínimos Quadrados Ponderados (WLS).
ML, REML, etc, ver aula 3
'


modelo_ajustado <- fit.variogram(v_exp, modelo_inicial)
modelo_ajustado|>
  knitr::kable()

plot(v_exp, modelo_ajustado)



'---------------------------------------------------------------------------
             Opcional: Extração de Valores: variogramLine
----------------------------------------------------------------------------
Se desejar reproduzir a curva teórica no ggplot2, esta função gera os dados da linha.
'

linha_teorica <- variogramLine(modelo_ajustado, maxdist = 1200)

head(linha_teorica) |>
  knitr::kable()



'---------------------------------------------------------------------------------
                Krigagem: Interpolação espacial: krige()
----------------------------------------------------------------------------------
 seleciona automaticamente o método adequado 
(Simples, Ordinária, Universal) baseando-se nos argumentos fornecidos.

* Sintaxe: `krige(formula, locations, newdata, model)`

* Argumentos:
  
  - `formula:` 
  
- `locations:` Objeto `sf` com os dados observados.

- `newdata`: Objeto `sf` ou `stars` com os locais de predição.

- `model:` O modelo de variograma ajustado.

- `block:` (Opcional) Se fornecido um vetor, ex: c(40, 40), 
realiza Krigagem de Bloco, estimando o valor médio dentro da célula
, resultando em mapas mais suaves e menor variância de predição.

'

krigagem_ord <- krige(log(zinc) ~ 1, 
                      locations = meuse_sf,  # Dados
                      newdata =grid_vetorial ,  # Grade de destino, vc poderia usar tambem grid_stars
                      model = modelo_ajustado, debug.level = 0) # Modelo espacial
'
O objeto resultante (krigagem_ord) contém duas variáveis fundamentais:
  
  * var1.pred: O valor predito (estimativa).

  * var1.var: A variância da krigagem (incerteza da estimativa).
'

'----------------------------------------------------------------------
             Visualização de Mapas de Predição e Incerteza
----------------------------------------------------------------------
  
A apresentação correta dos resultados exige a exibição da
estimativa juntamente com sua incerteza associada.
'

pacman::p_load(stars)

krigagem_raster <- st_rasterize(krigagem_ord) |>
  st_crop( limite_sf)  #corta apenas a area do shapfile
#
g1 <- ggplot() +
  geom_stars(data = krigagem_raster, aes(fill = var1.pred, x = x, y = y)) + 
  scale_fill_viridis_c(option = "B", name = "log(Zn)", na.value = "transparent") +
  geom_sf(data = limite_sf, fill = NA, color = "black") +
  labs(title = "Predição (Superfície Raster)") +
  theme_minimal()

g2 <- ggplot() +
  geom_stars(data = krigagem_raster, aes(fill = sqrt(var1.var), x = x, y = y)) + 
  scale_fill_viridis_c(option = "B", name = "SD", na.value = "transparent") +
  geom_sf(data = limite_sf, fill = NA, color = "black") +
  labs(title = "Incerteza (Desvio Padrão)") +
  theme_minimal()+
  theme(axis.title = element_blank()) #remove os eixos x e y (veja no da esquerda existem porque não removemos)

g1+g2


'------------------------------------------------------------------------
                     Alternativamente poderia fazer:
------------------------------------------------------------------------'
# st_bbox pega os limites da área
bb <- st_bbox(limite_sf) # onde limite_sf é seu shapfile

# Criar objeto stars vazio com resolução de 40m
grid_raster <- st_as_stars(bb, dx = 40, dy = 40)

# Recortar (Crop) o raster usando o polígono limite
grid_raster <- st_crop(grid_raster, limite_sf)


krigagem_direta <- krige(log(zinc) ~ 1, 
                         locations = meuse_sf, 
                         newdata = grid_raster, 
                         model = modelo_ajustado, debug.level = 0)

ggplot() +
  geom_stars(data = krigagem_direta, aes(fill = var1.pred)) +
  scale_fill_viridis_c(option = "plasma", name = "log(Zn)", na.value = "transparent") +
  geom_sf(data = limite_sf, fill = NA, color = "black", size = 0.8) +
  labs(title = "Krigagem Ordinária") +
  theme_void() +
  coord_sf(expand = FALSE) # Remove espaços em branco extras nas margens


'-------------------------------------------------------------------------------------
                     Validação Cruzada: `krige.cv`
--------------------------------------------------------------------------------------
Para aferir a qualidade preditiva do modelo, utiliza-se a função `krige.cv`. Esta função executa o procedimento `leave-one-out` (ou k-fold), que remove um ponto, estima-o com os vizinhos, compara o real com o estimado, repete para todos.


* Sintaxe: `krige.cv(formula, locations, model, nfold, ...)`

* Argumentos:
  
  - `nfold:` Se omitido ou igual ao número de observações, faz `leave-one-out`. Se definido (ex: 5 ou 10), faz validação cruzada em k-partes.
'

validacao <- krige.cv(log(zinc) ~ 1, locations = meuse_sf, model = modelo_ajustado, debug.level = 0)


'------------------------------------------------------------------------------
                    Extração de Métricas de Diagnóstico
-------------------------------------------------------------------------------'

# Resíduo = Observado - Predito
# Z-score = Resíduo / Desvio Padrão da Krigagem

metricas <- validacao |>
  st_drop_geometry() |>
  summarise(
    ME = mean(residual),              # Erro Médio (Viés): Ideal -> 0
    RMSE = sqrt(mean(residual^2)),    # Acurácia: Ideal -> Baixo
    MSNE = mean(zscore),              # Viés Normalizado: Ideal -> 0
    RMSNE = sqrt(mean(zscore^2))      # Consistência da Variância: Ideal -> 1
  )


metricas|>
  knitr::kable()

'------------------------------------------------------------------------------
                           Simulação Estocástica
-------------------------------------------------------------------------------
  
A Krigagem suaviza a realidade. 
Para aplicações que exigem a reprodução da textura real da variabilidade 
(ex: fluxo em meios porosos, análise de risco ambiental), utiliza-se simulação.

Basta adicionar o argumento `nsim` e definir `nmax` (para Simulação Sequencial Gaussiana local).
'

# nsim = 4 gera quatro mapas possíveis da realidade
simulacoes <- krige(log(zinc) ~ 1, 
                    locations = meuse_sf, 
                    newdata = grid_stars, 
                    model = modelo_ajustado, 
                    nsim = 4, 
                    nmax = 30, debug.level = 0)

dados_df <- as.data.frame(merge(simulacoes))

ggplot() +
  geom_stars(data = merge(simulacoes)) +
  geom_contour(data=dados_df,aes(x=x, y=y,z=var1),color="white",size=0.2,alpha=0.5)+ #linhas de contorno (isolines)
  facet_wrap(~attributes) +
  geom_sf(data = limite_sf, fill = NA, color = "black", size = 0.5) +
  scale_fill_viridis_c(option = "inferno", name = "log(Zn)", na.value = "transparent") +
  theme_minimal() +
  labs(title = "Simulação com Curvas de Nível")+
  theme(axis.title = element_blank())

'----------------------------------------------------------
                         KS
-----------------------------------------------------------'

data(meuse)
data(meuse.area) 

meuse_sf <- st_as_sf(meuse, coords = c("x", "y"), crs = 28992)

area_sf <- st_polygon(list(as.matrix(meuse.area))) |> 
  st_sfc(crs = 28992) |> 
  st_as_sf()

#Criar Grid de Predição 
# Precisamos definir ONDE queremos estimar
grid_pred <- st_bbox(area_sf) |>       
  st_as_stars(dx = 40, dy = 40) |>     # Define a resolução
  st_crop(area_sf)                     # RECORTA usando o polígono


#Ajuste do Variograma
v_ord <- variogram(log(zinc) ~ 1, meuse_sf)
m_ord <- fit.variogram(v_ord, vgm(0.6, "Sph", 900, 0.05))

media_conhecida <- mean(log(meuse$zinc))

#Krigagem Simples
# O resultado terá duas camadas: var1.pred e var1.var
ks <- krige(log(zinc) ~ 1, 
            locations = meuse_sf, 
            newdata = grid_pred, 
            model = m_ord, 
            beta = media_conhecida, debug.level = 0)

# Mapa de Predição
ks_masked <- ks[area_sf]
p1 <- ggplot() +
  geom_stars(data = ks_masked, aes(fill = var1.pred)) +
  geom_sf(data = area_sf, fill = NA, color = "black", linewidth = 0.5) +
  scale_fill_viridis_c(option = "B", na.value = "transparent") +
  labs(title = "Predição (Log Zinc)") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))

# Mapa de Variância (Erro)
# A variável de erro se chama 'var1.var'
p2 <- ggplot() +
  geom_stars(data = ks_masked, aes(fill = var1.var)) +
  geom_sf(data = area_sf, fill = NA, color = "black", size = 0.5) + # Contorno
  scale_fill_viridis_c(option = "B", na.value = "transparent") + 
  labs(title = "Variância de Krigagem", fill = "Var", x = NULL, y = NULL) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))

p1 + p2


'-----------------------------------------------------------------------
                               KO
------------------------------------------------------------------------'

pacman::p_load(stars, gstat, ggplot2, sf, patchwork, viridis, sp)


data(meuse)
data(meuse.area) 

meuse_sf <- st_as_sf(meuse, coords = c("x", "y"), crs = 28992)

area_sf <- st_polygon(list(as.matrix(meuse.area))) |> 
  st_sfc(crs = 28992) |> 
  st_as_sf()

#Criar Grid de Predição 
# Precisamos definir ONDE queremos estimar
grid_pred <- st_bbox(area_sf) |>       
  st_as_stars(dx = 40, dy = 40) |>     
  st_crop(area_sf)                     

#Krigagem
# modelo_ajustado <- vgm(0.5, "Sph", 900, 0.1)

krigagem <- krige(
  log(zinc) ~ 1,
  locations = meuse_sf,
  newdata   = grid_pred,
  model     = modelo_ajustado, debug.level = 0
)

# var1.pred = Valor Estimado 
# var1.var = Variância de Krigagem (Erro)
# Mapa 1: Predição
p1 <- ggplot() +
  geom_stars(data = krigagem, aes(fill = var1.pred)) +
  geom_sf(data = area_sf, fill = NA, color = "black", size = 0.5) + # Contorno
  scale_fill_viridis_c(option = "B", name = "Log(Zinc)", na.value = "transparent") +
  labs(title = "Predição") +
  theme_void()+
  theme(plot.title = element_text(hjust = 0.5))

# Mapa 2: Variância (Erro)
p2 <- ggplot() +
  geom_stars(data = krigagem, aes(fill = var1.var)) +
  geom_sf(data = area_sf, fill = NA, color = "black", size = 0.5) +
  scale_fill_viridis_c(option = "B", name = "Variância", na.value = "transparent") +
  labs(title = "Incerteza (Variância)") +
  theme_minimal() + #para colocar coordenadas
  theme(plot.title = element_text(hjust = 0.5),
        axis.title = element_blank()
  )

p1 + p2


'----------------------------------------------------------------------
                          KU
----------------------------------------------------------------------'

pacman::p_load(stars, gstat, ggplot2, sf, patchwork, viridis)

data(meuse)
data(meuse.grid)


meuse_sf <- st_as_sf(meuse, coords = c("x", "y"), crs = 28992, remove = FALSE)

grid_sf <- st_as_sf(meuse.grid, coords = c("x", "y"), crs = 28992, remove = FALSE)

#fórmula: ~ x + y (tendência linear nas coordenadas)
v_uni <- variogram(log(zinc) ~ x + y, meuse_sf)
m_uni <- fit.variogram(v_uni, vgm(0.5, "Sph", 800, 0.05))

#Krigagem Universal
ku_points <- krige(log(zinc) ~ x + y, 
                   locations = meuse_sf, 
                   newdata = grid_sf, 
                   model = m_uni, debug.level = 0)

# dx e dy definem o tamanho do pixel (40m para o dataset meuse)
ku_stars <- st_rasterize(ku_points, dx = 40, dy = 40)

# Mapa de Predição
p1 <- ggplot() +
  geom_stars(data = ku_stars, aes(fill = var1.pred)) +
  scale_fill_viridis_c(option = "B", na.value = "transparent") +
  labs(title = "Krigagem Universal (Pred)", fill = "Log(Zn)", x = NULL, y = NULL) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))

# Mapa de Variância
p2 <- ggplot() +
  geom_stars(data = ku_stars, aes(fill = var1.var)) +
  scale_fill_viridis_c(option = "B", na.value = "transparent") +
  labs(title = "Variância (Erro)", fill = "Var", x = NULL, y = NULL) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))

p1 + p2

'---------------------------------------------------------------------
                                    KDE
----------------------------------------------------------------------'

pacman::p_load(stars, gstat, ggplot2, sf, patchwork, viridis)

data(meuse)
data(meuse.grid)

# Precisamos garantir que a coluna 'dist' (nossa covariável x_k) esteja presente
meuse_sf <- st_as_sf(meuse, coords = c("x", "y"), crs = 28992)

#Preparar o Grid (Newdata)
# Aqui usamos covariavel 'dist' pois TAMBÉM precisa existir para todos os pixels
grid_sf <- st_as_sf(meuse.grid, coords = c("x", "y"), crs = 28992)

grid_stars <- st_rasterize(grid_sf, dx = 40, dy = 40)

#Variograma
v_ked <- variogram(log(zinc) ~ dist, meuse_sf)
m_ked <- fit.variogram(v_ked, vgm(0.5, "Exp", 800, 0.05))

plot(v_ked, m_ked, main = "Variograma (KED)")

# Krigagem com Deriva Externa (KED)
ked <- krige(log(zinc) ~ dist, 
             locations = meuse_sf, 
             newdata = grid_stars, 
             model = m_ked, debug.level = 0)

#
# Predição
p1 <- ggplot() +
  geom_stars(data = ked, aes(fill = var1.pred)) +
  scale_fill_viridis_c(option = "plasma", na.value = "transparent") +
  labs(title = "KED Predição (Covariável: Dist)", 
       subtitle = "Tendência guiada pela distância ao rio",
       fill = "Log(Zn)", x = NULL, y = NULL) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(size = 9))

# Variância (Erro)
p2 <- ggplot() +
  geom_stars(data = ked, aes(fill = var1.var)) +
  scale_fill_viridis_c(option = "cividis", na.value = "transparent") +
  labs(title = "Variância KED", fill = "Var", x = NULL, y = NULL) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))

p1 + p2

'---------------------------------------------------------------------
                         co-K
----------------------------------------------------------------------'

pacman::p_load(stars, gstat, ggplot2, sf, patchwork, viridis)

data(meuse)
data(meuse.grid)


meuse_sf <- st_as_sf(meuse, coords = c("x", "y"), crs = 28992)
grid_sf <- st_as_sf(meuse.grid, coords = c("x", "y"), crs = 28992)
grid_stars <- st_rasterize(grid_sf, dx = 40, dy = 40)

# Adicionamos as variáveis uma por uma ao objeto gstat

# Variável 1 (Primária): Zinco
g <- gstat(NULL, id = "zinc", formula = log(zinc) ~ 1, data = meuse_sf); g
# Variável 2 (Secundária): Chumbo (Lead)
g1 <- gstat(g, id = "lead", formula = log(lead) ~ 1, data = meuse_sf);g1

#Variograma Cruzado
# O gstat calcula automaticamente: Var(Zn), Var(Pb) e Cov(Zn, Pb)
v_cross <- variogram(g1)

# Plotar os variogramas (Diretos e Cruzado) para inspeção
plot(v_cross, main = "Variogramas Cruzados: Zinco x Chumbo")

#Ajuste do Modelo Linear de Coregionalização (LMC)
# fit.lmc ajusta o modelo aos variogramas direto e cruzado simultaneamente
model_base <- vgm(0.6, "Exp", 800, 0.05)
m_cross <- fit.lmc(v_cross, g, model = model_base)


plot(v_cross, m_cross, main = "Ajuste do LMC (Zn + Pb)")

#Realizar a Co-Krigagem
ck <- predict(m_cross, newdata = grid_stars)

# Predição
p1 <- ggplot() +
  geom_stars(data = ck, aes(fill = zinc.pred)) +
  scale_fill_viridis_c(option = "plasma", na.value = "transparent") +
  labs(title = "Co-Krigagem Ordinária (Pred)", 
       subtitle = "Zinco auxiliado por Chumbo",
       fill = "Log(Zn)", x = NULL, y = NULL) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(size = 9))

# Variância
p2 <- ggplot() +
  geom_stars(data = ck, aes(fill = zinc.var)) +
  scale_fill_viridis_c(option = "cividis", na.value = "transparent") +
  labs(title = "Variância CK", fill = "Var", x = NULL, y = NULL) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))

p1 + p2


'---------------------------------------------------------------------
                Avaliação da qualidade o ajuste
---------------------------------------------------------------------'
pacman::p_load(stars, gstat, ggplot2, sf, patchwork, viridis, dplyr, tibble, gt)

data(meuse)
meuse_sf <- st_as_sf(meuse, coords = c("x", "y"), crs = 28992)

v_ord <- variogram(log(zinc) ~ 1, meuse_sf)
m_ord <- fit.variogram(v_ord, vgm(0.6, "Exp", 800, 0.05))

'Executar Validação Cruzada (Leave-One-Out)
A função krige.cv faz o loop automaticamente.'

cv_results <- krige.cv(log(zinc) ~ 1, 
                       locations = meuse_sf, 
                       model = m_ord, debug.level = 0); 

cv_results|>
  head()|>
  knitr::kable()

'Estatísticas de Diagnóstico
MSDR (Razão de Desvio Quadrático Médio)'

msdr <- mean(cv_results$zscore^2)

'ME (Erro Médio) - Deve ser próximo de 0 (viés)'

me <- mean(cv_results$residual)

'RMSE (Raiz do Erro Quadrático Médio)'

rmse <- sqrt(mean(cv_results$residual^2))

resultados <- tibble(
  Indicador = c("Mean Error (Viés)", 
                "RMSE (Precisão)", 
                "MSDR (Calibração)"),
  Valor = round(c(me, rmse, msdr), 4)
)

resultados|>
  knitr::kable()


'Interpretação automática simples'

if(msdr > 1.1) {
  cat("MSDR > 1: Subestimação da incerteza (Variograma muito 'otimista' ou outliers).\n")
} else if(msdr < 0.9) {
  cat("MSDR < 1: Superestimação da incerteza (Variograma muito 'pessimista').\n")
} else {
  cat("MSDR ~ 1: Incerteza bem calibrada.\n")
}

#
p1 <- ggplot(cv_results, aes(x = observed, y = var1.pred)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Acurácia: Observado vs Predito",
       subtitle = paste("RMSE:", round(rmse, 3)),
       x = "Log(Zinc) Observado", y = "Log(Zinc) Predito") +
  theme_bw()

#Histograma dos Z-Scores (Deve parecer uma Normal(0,1))
p2 <- ggplot(cv_results, aes(x = zscore)) +
  geom_histogram(aes(y = ..density..), bins = 20, fill = "steelblue", color = "white", alpha = 0.7) +
  stat_function(fun = dnorm, args = list(mean = 0, sd = 1), color = "red", size = 1) +
  labs(title = "Calibração: Z-Scores",
       subtitle = paste("MSDR:", round(msdr, 3), "(Ideal = 1.0)"),
       x = "Resíduo Padronizado", y = "Densidade") +
  theme_bw()

p1 + p2


'--------------------------------------------------------------------
                K-indicatriz (dados categoricos)
----------------------------------------------------------------------'

pacman::p_load(stars, gstat, ggplot2, sf, viridis, dplyr)

data(meuse)
data(meuse.grid)


#Preparação dos Dados
# Definir o corte (Threshold). Vamos usar o 3º quartil do Zinco como "perigo".
threshold <- quantile(meuse$zinc, 0.75) 
meuse$zinc_ind <- ifelse(meuse$zinc > threshold, 1, 0) # 1 se perigoso, 0 se seguro

meuse_sf <- st_as_sf(meuse, coords = c("x", "y"), crs = 28992)
grid_sf <- st_as_sf(meuse.grid, coords = c("x", "y"), crs = 28992)
grid_stars <- st_rasterize(grid_sf, dx = 40, dy = 40)


#Variograma Indicador

v_ind <- variogram(zinc_ind ~ 1, meuse_sf)
m_ind <- fit.variogram(v_ind, vgm(0.15, "Exp", 600, 0.05))

plot(v_ind, m_ind, main = "Variograma Indicador (Zinco > Q75)")


#Krigagem Indicatriz (Ordinária)

# O resultado (var1.pred) será a PROBABILIDADE de ser 1 (acima do corte)
ik <- krige(zinc_ind ~ 1, 
            locations = meuse_sf, 
            newdata = grid_stars, 
            model = m_ind, debug.level = 0)


ik$probabilidade <- pmin(pmax(ik$var1.pred, 0), 1)

ggplot() +
  geom_stars(data = ik, aes(fill = probabilidade)) +
  geom_sf(data = meuse_sf, aes(color = as.factor(zinc_ind)), size = 1) +
  scale_fill_viridis_c(option = "turbo", name = "Prob. > Corte", na.value = "transparent") +
  scale_color_manual(values = c("white", "black"), name = "Dados Reais", labels = c("<= Corte", "> Corte"))+
  labs(title = "",
       subtitle = "Probabilidade do teor de Zinco exceder o limiar de 75%",
       x = NULL, y = NULL) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "right")

'--------------------------------------------------------------------------------------
                                Pacote automap
--------------------------------------------------------------------------------------
Enquanto no pacote `gstat` o usuário deve fornecer estimativas iniciais (chutes) 
para os parâmetros do variograma `(patamar, alcance e efeito pepita)` 
e testar manualmente diferentes funções de covariância `(Esférico, 
Exponencial, Matérn, etc.)`, o pacote `automap` foi desenvolvido
para automatizar essas etapas de ajuste e krigagem.
'

'----------------------------------------------------------------------------------
                        Ajuste Automático do Variograma: `autofitVariogram`
-----------------------------------------------------------------------------------

Esta função elimina a necessidade de tentativa e erro manual. 
Ao contrário da função `fit.variogram` do `gstat`, que exige parâmetros iniciais,
a `autofitVariogram` calcula esses valores automaticamente: o alcance inicial 
é definido como 0,10 vezes a diagonal da área dos dados (`bounding box`), o
`efeito pepita` inicial é o mínimo da semivariância amostral, e o `patamar` 
é uma média entre o máximo e a mediana da semivariância . O algoritmo então 
ajusta e testa automaticamente os modelos Esférico, Exponencial, Gaussiano
e Stein (Matérn), retornando aquele com o melhor ajuste estatístico . 
No canto inferior direito mostra o modelo ajustado e respetivos parâmetros.

'


pacman::p_load(automap, sf, gstat)

data(meuse)
meuse_sf <- st_as_sf(meuse, coords = c("x", "y"), crs = 28992)

variograma_auto <- autofitVariogram(log(zinc) ~ 1, input_data = as(meuse_sf, "Spatial"))

plot(variograma_auto)


'-------------------------------------------------------------------------
                    Interpolação Automática: `autoKrige
------------------------------------------------------------------------
A função autoKrige chama internamente a autofitVariogram 
para ajustar o modelo e, em seguida, utiliza esse modelo otimizado para 
realizar a predição espacial nos novos locais . Isso resolve o problema 
de ter que passar manualmente os parâmetros do variograma para
a função de krigagem. Ela suporta Krigagem Ordinária (padrão), 
Universal (inserindo covariáveis na fórmula) e Krigagem de Bloco .
'

pacman::p_load(stars, sp)
data(meuse)
data(meuse.grid)

meuse_sf <- st_as_sf(meuse, coords = c("x", "y"), crs = 28992)
meuse_grid_sf <- st_as_sf(meuse.grid, coords = c("x", "y"), crs = 28992)
meuse_grid_stars <- st_rasterize(meuse_grid_sf, dx = 40, dy = 40)

krigagem <- autoKrige(log(zinc)~1, 
                      input_data = meuse_sf, 
                      new_data = meuse_grid_stars, debug.level = 0)

plot(krigagem)


'-----------------------------------------------------------------------
                  Extraindo o resultado para plotar com ggplot
-------------------------------------------------------------------------'

pacman::p_load(patchwork)

resultado_sf <- st_as_sf(krigagem$krige_output)

p1 <- ggplot(resultado_sf) +
  geom_sf(aes(fill = var1.pred), color = NA) + # color=NA é crucial aqui
  scale_fill_viridis_c(option = "plasma", name = "Log(Zn)") +
  labs(title = "Predição") + 
  theme_void()+
  theme(plot.title = element_text(hjust = 0.5))

p2 <- ggplot(resultado_sf) +
  geom_sf(aes(fill = sqrt(var1.var)), color = NA) + 
  scale_fill_viridis_c(option = "cividis", name = "SD") +
  labs(title = "Erro Padrão") + 
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))

p1 + p2

'-------------------------------------------------------------------------------------
                    Validação Cruzada Automática: `autoKrige.cv`
------------------------------------------------------------------------------------
Validação cruzada automática (10-fold) para KO
'
cv_ordinaria <- autoKrige.cv(log(zinc) ~ 1,
                             input_data = as(meuse_sf, "Spatial"),
                             nfold = 10)

'Validação cruzada automática para Krigagem Universal (usando distância como covariável)'

cv_universal <- autoKrige.cv(log(zinc) ~ sqrt(dist),
                             input_data = as(meuse_sf, "Spatial"),
                             nfold = 10, debug.level = 0)

summary(cv_ordinaria$krige.cv_output)

'-------------------------------------------------------------------------------
                  Comparação de Modelos: `compare.cv`
-------------------------------------------------------------------------------
Dado que a escolha entre Krigagem Ordinária e Universal pode ser difícil, 
a função `compare.cv` permite comparar diretamente
os resultados de múltiplas validações cruzadas.
Ela gera diagnósticos estatísticos (como RMSE e Correlação) 
e gráficos espaciais de bolhas (`bubble plots`) para identificar 
visualmente qual abordagem automática produziu menores erros. 
O argumento `plot.diff` destaca onde um modelo supera o outro.
'

comparacao <- compare.cv(cv_ordinaria, cv_universal,
                         col.names = c("Ordinária", "Universal"),
                         bubbleplots = TRUE, # Gera os gráficos na janela de plotagem
                         plot.diff = FALSE)   

print(comparacao$spatial)

comparacao1 <- compare.cv(cv_ordinaria, cv_universal,
                          col.names = c("Ordinária", "Universal"),
                          bubbleplots = FALSE, 
                          plot.diff = FALSE)   

comparacao1 |>
  knitr::kable()



'--------------------------------------------------------------------------
         Intervalos de Predição de Posição: `posPredictionInterval`
--------------------------------------------------------------------------
Calcula a posição do intervalo de predição (padrão 95%) em relação a 
um valor limite (cutoff). O mapa resultante classifica as
áreas como potencialmente acima, potencialmente abaixo ou
indistinguível do limite, facilitando a interpretação de 
riscos sem exigir cálculos manuais de intervalos de confiança.
'

'Extrair os resultados da krigagem e remover NA'

resultado_pontos <- st_as_sf(krigagem$krige_output, as_points = TRUE)
resultado_limpo <- resultado_pontos[!is.na(resultado_pontos$var1.pred), ]

krigagem_pontos <- krigagem
krigagem_pontos$krige_output <- resultado_limpo

intervalos <- posPredictionInterval(krigagem_pontos, 
                                    p = 95, 
                                    value = 6.0)

plot(intervalos, main = "Classificação vs Limiar (6.0)")


'---------------------------------------------------------------------------
                              Pacote geoR
----------------------------------------------------------------------------

Veja no capítulo 3 de @scalon2024analise. 

Cupom de desconto de 20% (SCALON20) válido de 08/12 a 12/02 
para compras pelo site da editora-ufla.

* Nota 1: Apenas alunos inscritos, e só foi possivel 50 livros, e sei que a turma >50
infelizmente quem não poder pode recorrer ao preço normal (creio que é acessível)

* Nota 2: É OPCIONAL, NINGUÉM É OBRIGADO A COMPRAR, O MATERIAL DE APOIO TEM O NECESSÁRIO

* NOTA 3: NÃO TENHO VÍNCULO COM A EDIDORE, Prof. Scalon foi meu orientador na UFLA
Qualquer problema comuniquem-o: scalon@ufla.br ou a mím para falar com ele.
'
