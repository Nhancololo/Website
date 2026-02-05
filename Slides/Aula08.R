'---------------------------------------------------------------------
                        Script: Processos Pontuais Espaciais
                 Teoria e Aplicações com R (spatstat)
---------------------------------------------------------------------'

'--------------------------------------------------------------------
                    INSTALAÇÃO E CARREGAMENTO DE PACOTES 
--------------------------------------------------------------------'
if (!require("pacman")) install.packages("pacman")
pacman::p_load(spatstat,      # O pacote principal para análise de PP
               sf,            # Manipulação vetorial
               ggplot2,       # Gráficos
               patchwork,     # Composição de gráficos
               viridis,       # Paletas de cores
               dplyr,         # Manipulação de dados
               automap,       # Krigagem automática
               gstat,         # Krigagem
               stars,         # Raster/Cubo de dados
               terra,         # Manipulação Raster
               mgcv,          # GAMs (Splines)
               gratia,        # Visualização de GAMs
               sp)        

'---------------------------------------------------------------------
              Criação de objetos PP (Point Process) e janela 
---------------------------------------------------------------------
Nota: O pacote spatstat exige objetos do formato ppp 
(Planar Point Pattern). Isso implica que deves transformar sua base 
de dados em objeto ppp e seu shapfile também.
'

# Carrega dados de exemplo (Meuse - Poluição do solo) do pacote sp
data(meuse)
data(meuse.grid)
data(meuse.area) 

'-------------------------------------------------------------------
  Criando a Janela de Observação  e/ou transformando seu shapfile
  em objeto ppp
-------------------------------------------------------------------'
# Se tiver shapfile pule a parte de st_as_sf
meuse_sf <- st_as_sf(meuse, coords = c("x", "y"), crs = 28992)
grid_sf  <- st_as_sf(meuse.grid, coords = c("x", "y"), crs = 28992)

plot(grid_sf$geometry)

janela1   <- as.owin(st_bbox(grid_sf)) # aqui criamos objeto owin

'Nota: usei st_bbox para criar um retangulo (poligono) pegando (x_min, y_min)
e (x_max, y_max). Se tiveres um objeto do tipo polygon não precisa usar st_bbox
'

janela<- owin(poly = list(x = rev(meuse.area[,1]), 
                                    y = rev(meuse.area[,2])))

par(mfrow=c(1,2))
plot(janela1)

plot(janela)

'-------------------------------------------------------------------
            Criando o Objeto PPP (Processo Pontual) sem marca
-------------------------------------------------------------------'

coords <- st_coordinates(meuse_sf)
ppp_meuse_SM <- ppp(x = coords[,1], 
                 y = coords[,2], 
                 window = janela)

par(mfrow=c(1,1))
plot(ppp_meuse_SM, main = "Processo Pontual Marcado (Meuse - Zinco)", 
     cols = "red", chars = 16)
axis(1); axis(2)

'-------------------------------------------------------------------
      Criando o objeto PPP  com marca (Processo Pontual Marcado)
-------------------------------------------------------------------'
# Vamos usar o zinco (var. quantitativa) como marca  (Processo Pontual ) 
coords <- st_coordinates(meuse_sf)
ppp_meuse <- ppp(x = coords[,1], 
                 y = coords[,2], 
                 window = janela, 
                 marks = meuse$zinc)

par(mfrow=c(1,1))
plot(ppp_meuse, 
     cols = "red", chars = 1)
axis(1); axis(2)


# Vamos usar o landuse (var. qualitativa) como marca (Processo Pontual )

ppp_meuse_q <- ppp(x = coords[,1], 
                 y = coords[,2], 
                 window = janela, 
                 marks = meuse$landuse)

par(mfrow=c(1,1))
plot(ppp_meuse_q)
axis(1); axis(2)

'---------------------------------------------------------------------
        ANÁLISE EXPLORATÓRIA - INTENSIDADE de 1ª ORDEM
---------------------------------------------------------------------'

'....................................................................
                    Método de contagem por Quadrats
.....................................................................'

# Teste de Qui-quadrado para Homogeneidade (H0: Intensidade Constante/CSR)
Q <- quadratcount(ppp_meuse, nx = 4, ny = 4)
plot(ppp_meuse, main = "Contagem por Quadrats")
plot(Q, add = TRUE, col = "red", cex = 1.5)

qt_test <- quadrat.test(ppp_meuse, nx=4, ny=4)
print(qt_test) # Se p-valor < 0.05, rejeita-se homogeneidade

'....................................................................
                     Estimativa de Kernel (Suavização)
....................................................................'
#--------A escolha da largura de banda (sigma) é crucial.

#-----a) Seleção de Banda por Verossimilhança (Cross Validation) - bom para PP Poison
bw_ppl <- bw.ppl(ppp_meuse); bw_ppl
#---- b) Seleção de banda por Diggle (MSE) - Bom para Cox/Cluster
bw_dig <- bw.diggle(ppp_meuse); bw_dig

#------- c) Regra de Scott (Baseada na normalidade)
bw_sco <- bw.scott(ppp_meuse) ; bw_sco

par(mfrow=c(1,3))
plot(density(ppp_meuse, sigma = bw_ppl), main = paste("BW PPL:", round(bw_ppl, 1)))
plot(density(ppp_meuse, sigma = bw_dig), main = paste("BW Diggle:", round(bw_dig, 1)))
plot(density(ppp_meuse, sigma = bw_sco), main = "BW Scott")

'....................................................................
                     Kernel Adaptativo
....................................................................'

den_adapt <- densityAdaptiveKernel(ppp_meuse)
par(mfrow=c(1,1))
plot(den_adapt, main = "Kernel Adaptativo")

'---------------------------------------------------------------------
        ANÁLISE EXPLORATÓRIA - 2ª ORDEM (INTERAÇÃO)
---------------------------------------------------------------------
Funções: F, G, J, K, L, g (PCF). Inclui versões Inhomogêneas.
'

# Para esta parte, vamos usar dados "unmark" (sem marcas) para focar na posição
ppp_unmark <- unmark(ppp_meuse)

'........................................................................
    Funções sumárias (Assumindo Homogeneidade/Estacionariedade)
........................................................................
# IMPORTANTE: Se houver tendência forte, estas funções 
  podem dar falso positivo para cluster'

par(mfrow=c(2,3))

#---a) Função F (Espaço Vazio) - Ponto qualquer até evento mais próximo
plot(Fest(ppp_unmark), main = "Função F")

#---b) Função G (Vizinho Mais Próximo) - Evento até evento
plot(Gest(ppp_unmark), main = "Função G")

#---c) Função J (Razão G/F) - J=1 (Aleatório), J>1 (Regular), J<1 (Agrupado)
plot(Jest(ppp_unmark), main = "Função J")

#---d) Função K de Ripley (Cumulativa)
plot(Kest(ppp_unmark), main = "Função K")

#---e) Função L (Linearização de K) - L(r) = r sob CSR
plot(Lest(ppp_unmark), . - r ~ r, main = "Função L (Linearizada)")

#---f)Função g (Pair Correlation Function) - Probabilidade condicional
plot(pcf(ppp_unmark, divisor = "d"), main = "Função g (PCF)")


'--------------------------------INTERPRETAÇÃO DOS GRÁFICOS ---------------------------------
1. As linhas (iso, trans, bord, km, cs, rs, Riply, han) são correções de borda para evitar viés; 
    foca-se na LINHA PRETA SÓLIDA  comparada à AZUL PONTILHADA (Teórico Poisson/Aleatório).
    
2. O conjunto indica um PADRÃO REGULAR (INIBIÇÃO), pois os eventos se repelem:
    - G(r) observados estão ABAIXO da teórica (vizinhos mais distantes que o esperado).
    - J(r) está ACIMA de 1 (repulsão/inibição)
    - L(r)-r é NEGATIVO-até 200m (repulsão).
    - g(r) 
---------------------------------------------------------------------------------------------------'

'...............................................................................
                      Envelopes de Simulação (Monte Carlo)
................................................................................'
# H0: Aleatoriedade Espacial Completa (CSR)

env_L <- envelope(ppp_unmark, Lest, nsim = 39, rank = 1, global = TRUE, verbose = F)
par(mfrow=c(1,1))
plot(env_L, . - r ~ r, main = "Envelope de Simulação (L) - Teste CSR")

# Interpretação: Se a linha preta sai da sombra cinza, rejeita-se CSR.


'------------------------------------------------------------------------
            Funções Inhomogêneas (Lidando com a Tendência)
-------------------------------------------------------------------------
>Essa regularidade é real ou é apenas porque a densidade de 
  pontos varia no terreno (tendência)?

Se sabemos que existe tendência (visto no Kernel), usamos Linhom/Kinhom
para verificar se HÁ CLUSTER além da tendência.

> Ideia por d tras: Remove o efeito da "tendência" (ex: se houver mais pontos 
num canto do mapa por questões ambientais) e verifica se 
a repulsão entre os pontos se mantém.

'

# Primeiro estimamos a intensidade lambda

lambda_est <- density(ppp_unmark, sigma = bw.ppl)

# L Inhomogêneo
L_inh <- Linhom(ppp_unmark, lambda = lambda_est)
par(mfrow=c(1,2))
plot(L_inh, . - r ~ r, main = "L Inhomogêneo (Ajustado p/ Tendência)")

# Envelope Inhomogêneo (Simula Poisson Inhomogêneo, não CSR)

env_inhom <- envelope(ppp_unmark, Linhom, 
                      simulate = expression(rpoispp(lambda_est)),
                      nsim = 19, verbose = F)
plot(env_inhom, . - r ~ r, main = "Envelope Inhomogêneo")


'---------------------------------------------------------------------
            MODELAGEM - MODELOS POISSON (Tendência)
---------------------------------------------------------------------
Modelando a intensidade lambda(u). Ajuste via ppm().
'

#...........Poisson Homogêneo (Beta constante)
fit_pois_homo <- ppm(ppp_unmark ~ 1)

#...........Poisson Não homogêneo (Tendência Log-Linear nas coordenadas)

# lambda(x,y) = exp(beta0 + beta1*x + beta2*y)
fit_pois_lin <- ppm(ppp_unmark ~ x + y)

#........... Poisson Inhomogêneo (Tendência Quadrática)
# lambda(x,y) = exp(polinomio de 2º grau)
fit_pois_quad <- ppm(ppp_unmark ~ polynom(x, y, 2))

#........... Comparação via AIC
AIC(fit_pois_homo, fit_pois_lin, fit_pois_quad)

'........................................................................
                          Diagnóstico do melhor modelo Poisson
------------------------------------------------------------------------'
par(mfrow=c(1,1))
diagnose.ppm(fit_pois_quad,  main = "Resíduos Poisson Quadrático")
qqplot.ppm(fit_pois_quad)

'---------------------------------------------------------------------
            MODELAGEM - MODELOS DE GIBBS (Interação)
---------------------------------------------------------------------
Para padrões REGULARES (Inibição) ou Interações complexas.
OBS: Usaremos o dataset "cells" para estes exemplos, pois "meuse" é agrupado.
'
data(cells)
ppp_cells <- cells # Dados biológicos (inibição competitiva)

#........... Modelo Hard Core (Núcleo Duro)
# Nenhuns dois pontos podem estar a menos de uma distância h

fit_hard <- ppm(ppp_cells ~ 1, Hardcore(h = 0.05)) 

#...........Modelo de Strauss

# Penaliza pontos dentro do raio r com parâmetro gamma (0 < gamma < 1)

fit_strauss <- ppm(ppp_cells ~ 1, Strauss(r = 0.08))

#........... Modelo Strauss-Hard Core
# Mistura: exclusão total até 'hc' e inibição suave até 'r'

fit_strausshard <- ppm(ppp_cells ~ 1, StraussHard(r = 0.1, hc = 0.02))

#........... Modelo Soft Core (Potencial Suave)
# Interação decai gradualmente, não abruptamente.

fit_soft <- ppm(ppp_cells ~ 1, Softcore(kappa = 0.5), correction="isotropic")

'-----------------------------------------------------------------------
                      Modelo Diggle–Gates–Stibbard

Interação senoidal.
-----------------------------------------------------------------------'
  

fit_dgs <- ppm(ppp_cells ~ 1, DiggleGatesStibbard(rho = 0.08))

'-----------------------------------------------------------------------
                               Modelo Diggle–Gratton

Função potencial definida por distância hard core e distância de interação.
Adequado para processos com inibição a curta distância.
Requer escolha cuidadosa dos parâmetros iniciais.
-----------------------------------------------------------------------'
  
fit_dg <- ppm(ppp_cells ~ 1, DiggleGratton(delta=0.02, rho=0.1))

'-----------------------------------------------------------------------
       Modelo de Interação por Área (Area Interaction / Widom-Rowlinson)

 Baseado na área de intersecção de discos ao redor dos pontos.
Capaz de modelar tanto agregação (gamma > 1) quanto inibição (gamma < 1).
------------------------------------------------------------------------'

fit_area <- ppm(ppp_cells ~ 1, AreaInter(r = 0.06))

'---------------------Comparando modelos Gibbs via AIC-----------------'

AIC(fit_strauss, fit_strausshard, fit_area, fit_dgs)

'---------------------------------------------------------------------
            MODELAGEM - MODELOS COX/CLUSTER (Agregação)
---------------------------------------------------------------------
Para padrões AGRUPADOS. Ajuste via kppm() (Minimum Contrast ou Palm).
Usaremos "redwood" (mudas de sequóia) ou o próprio "meuse".
'

data(redwood)

'-----------------------------------------------------------------------
                      Modelo de Thomas (Cluster Gaussiano)

Pais Poisson.
Filhos dispersos isotropicamente (Gaussiana) ao redor dos pais.
-----------------------------------------------------------------------'
  
fit_thomas <- kppm(redwood ~ 1, clusters = "Thomas")

'-----------------------------------------------------------------------
                            Modelo Matérn Cluster

Pais Poisson.
Filhos dispersos uniformemente em um disco ao redor dos pais.
-----------------------------------------------------------------------'
  
fit_matern <- kppm(redwood ~ 1, clusters = "MatClust")

'-----------------------------------------------------------------------
                 Modelo Log-Gaussian Cox Process (LGCP)

Intensidade dirigida por um campo aleatório Gaussiano (Log-Normal).
-----------------------------------------------------------------------'
  
fit_lgcp <- kppm(redwood ~ 1, clusters = "LGCP")

'-----------------------------------------------------------------------
                 Modelo Variância–Gama (VarGamma)

Clusters com caudas mais longas que o modelo de Thomas.
-----------------------------------------------------------------------'
  
fit_vargamma <- kppm(redwood ~ 1, clusters = "VarGamma", nu.ker = -1/4)

'-----------------------------------------------------------------------
                          Modelo Cauchy

Dispersão com caudas muito pesadas (decaimento em lei de potência).
-----------------------------------------------------------------------'
  
fit_cauchy <- kppm(redwood ~ 1, clusters = "Cauchy")

# 
par(mfrow=c(1,2))
plot(fit_thomas, main = "Ajuste Thomas") # Plota L observado vs Teórico do modelo
plot(simulate(fit_thomas), main = "Simulação do Modelo Thomas")

'---------------------------------------------------------------------
            DIAGNÓSTICO E VALIDAÇÃO
---------------------------------------------------------------------
Ferramentas para verificar a qualidade do ajuste e identificar outliers.

Vamos usar o modelo Poisson Quadrático ajustado ao Meuse (fit_pois_quad)
'

'-----------------------------------------------------------------------
                                  Testes

Testes LRT e Score.
Teste de Razão de Verossimilhança para modelos aninhados.
Compara modelo nulo (homogêneo) versus modelo quadrático.
-----------------------------------------------------------------------'

anova(fit_pois_homo, fit_pois_quad, test="LRT")

#------------------Diagnóstico de Resíduos
res <- residuals(fit_pois_quad, type="pearson")

par(mfrow=c(1,1))
plot(Smooth(res), main = "Resíduos de Pearson Suavizados")


#------------- Q-Q Plot de Resíduos
qqplot.ppm(fit_pois_quad, plot.it = TRUE)

#---------------Testes de Monte Carlo (DCLF e MAD)
# Compara envelopes globais do modelo ajustado vs dados
# H0: O dados foram gerados pelo modelo ajustado

dclf.test(fit_pois_quad, Lest, nsim=19)
mad.test(fit_pois_quad, Lest, nsim=19)


'---------------------------------------------------------------------
            INTEGRAÇÃO GEOESTATÍSTICA + GAM + PPM
---------------------------------------------------------------------
1. Krigagem de variável ambiental.
2. Conversão para Imagem.
3. Uso como covariável em modelo PPM usando GAM (Splines) para não-linearidade.
'

if (!require("pacman")) install.packages("pacman")
pacman::p_load(spatstat, gstat, automap, sf, stars, mgcv, 
               gratia, ggplot2, sp)

# Pontos de Coleta para SF
pontos_sf <- st_as_sf(meuse, coords = c("x", "y"), crs = 28992)
grid_sf   <- st_as_sf(meuse.grid, coords = c("x", "y"), crs = 28992)

# Converter para Spatial (Necessário para o automap)
input_sp <- as(pontos_sf, "Spatial")
grid_sp  <- as(grid_sf, "Spatial")
gridded(grid_sp) <- TRUE 

'-----------------------------------------------------------------------------
                      PREPARAÇÃO DA COVARIÁVEL (KRIGAGEM)
                      
Interpolação da concentração de Zinco para toda a área
-------------------------------------------------------------------------------'

krigagem <- autoKrige(log(zinc) ~ 1, input_data = input_sp, new_data = grid_sp, debug.level = 0)

#------ Conversão para Imagem (Covariável preditora)

mapa_zinco_im <- as.im(st_as_stars(krigagem$krige_output)["var1.pred"])

#----------- CONFIGURAÇÃO DO PADRÃO DE PONTOS (PPM)
janela <- Window(mapa_zinco_im)
coords <- st_coordinates(pontos_sf)

#-------------Criação do objeto PPP representando os LOCAIS DE AMOSTRAGEM
pontos_observados <- ppp(x = coords[,1], 
                         y = coords[,2], 
                         window = janela, 
                         checkdup = TRUE)

par(mfrow = c(1, 2), mar=c(1,1,3,1))
plot(mapa_zinco_im, main = "Log(Zinco) no Solo", box = FALSE)

plot(pontos_observados, main = "Locais de\nColeta (Meuse)", pch = 16, cex = 0.5, cols = "black", col = c(NA, NA), 
     box = FALSE)
par(mfrow = c(1, 1))

#-----------MODELAGEM (Investigação do Viés de Coleta)
# ?Pergunta: A escolha dos locais de coleta foi influenciada pelo nível de poluição?
# Modelo: Intensidade_de_Amostragem ~ s(Zinco)

fit_real <- ppm(pontos_observados ~ s(zinco), 
                covariates = list(zinco = mapa_zinco_im), 
                use.gam = TRUE) 

#-------------RESULTADOS

modelo_gam <- fit_real$internal$glmfit

draw(modelo_gam) +
  ggplot2::labs(
    title = "Resposta da densidade de amostras ao gradiente de poluição",
    y = "Efeito na Densidade de Coleta (s(zinco))",
    x = "Log(Concentração de Zinco) [ppm]"
  ) +
  ggplot2::theme_bw() +
  ggplot2::theme(plot.title = element_text(face = "bold"))

#--------- Intensidade Predita (Onde o modelo "espera" que haja amostras)
mapa_predito <- predict(fit_real, type = "trend")

plot(mapa_predito, main = "Densidade de Amostragem Predita (PPM)")
plot(pontos_observados, add = TRUE, pch = ".", cols = "white")

#---------- Diagnóstico de Resíduos
residuos <- residuals(fit_real, type = "pearson")
plot(Smooth(residuos, sigma = 50), 
     main = "Resíduos Espaciais \n(Vermelho = Amostragem \nmais densa que o previsto)")
