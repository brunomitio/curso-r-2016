library(ggplot2)
library(magrittr)
library(tidyr)
library(dplyr)
library(jpeg)
library(tree)

# a) carrega uma imagem jpeg no R 
img <- readJPEG("C:/Users/Mitio/Desktop/Curso R/curso-r-2016/purple_wave.jpg")

# b) transforma o array da imagem em data.frame com infos de posicao (x,y) e cor (r,g,b)
# dimensões da imagem
img_dim <- dim(img)

# RGB para data.frame
img_df <- data.frame(
  x = rep(1:img_dim[2], each = img_dim[1]),
  y = rep(img_dim[1]:1, img_dim[2]),
  r = as.vector(img[,,1]),
  g = as.vector(img[,,2]),
  b = as.vector(img[,,3])
) %>%
  mutate(cor = rgb(r, g, b),
         id = 1:n())

# para reprodução
set.seed(1) 

# Parte 1) x, y, r, g
img_df_parte1 <- img_df %>% 
  sample_frac(3/5) %>% # separando 3/5 do banco
  mutate(b_backup = b, # backup do azul original
         b = 0, # retirando o azul da imagem
         cor = rgb(r, g, b)) # cor da imagem sem o azul

# Parte 2) x, y, r, g, b
img_df_parte2 <- img_df %>% filter(!id%in%img_df_parte1$id) # filtra as linhas que estão na Parte 1

# Imagem sem o azul
sAzul <- ggplot(data = img_df_parte1, aes(x = x, y = y)) + 
  geom_point(colour = img_df_parte1$cor) +
  labs(x = "x", y = "y", title = "Imagem sem B (azul)") +
  coord_fixed(ratio = 1) +
  theme_bw()
sAzul

# Apenas o azul da imagem
azul <- ggplot(data = img_df_parte2, aes(x = x, y = y)) + 
  geom_point(colour = img_df_parte2$cor) +
  labs(x = "x", y = "y", title = "Apenas o B (azul)") +
  coord_fixed(ratio = 1) +
  theme_bw()
azul

#Exercícios
#1: Descritiva
img_dfa <- sample_n(img_df, 500, replace = F)
# matriz de correlação linar
img_dfa %>% select(x, y:b) %>%
  cor %>%
  round(2)
  
#matriz de gráficos de dispersão
img_dfa %>% select(x, y:b) %>%
  pairs()
 
#2: 
lm1 <- lm(b ~ x + y + r + g, data = img_df_parte2)
summary(lm1)



#3
lm2 <- lm(b ~ r + g, data = img_df_parte2)
summary(lm2)

arv1 <- tree(b ~ r + g, img_df_parte2)
summary(arv1)
plot(arv1)
text(arv1, pretty = 0)


#4
predito_arv1p1 <- predict(arv1, img_df_parte1)
predito_lm1p1 <-predict(lm1, img_df_parte1)
predito_arv1p2 <- predict(arv1, img_df_parte2)
predito_lm1p2 <-predict(lm1, img_df_parte2)

img_df_parte2 <- img_df_parte2 %>% 
  mutate(predito_lm1p2,
         predito_arv1p2,
         erro_lm1 = (b - predito_lm1p2)^2,
         erro_arv1 = (b - predito_arv1p2)^2)


soma_erro_lm1 <- sum(img_df_parte2$erro_lm1)
soma_erro_lm1

soma_erro_arv1 <- sum(img_df_parte2$erro_arv1)
soma_erro_arv1

#original
img_original1 <- img_df %>% 
  mutate(cor = rgb(r, g, b)) 

original1 <- ggplot(data = img_original1, aes(x = x, y = y)) + 
  geom_point(colour = img_original1$cor) +
  labs(x = "x", y = "y", title = "Imagem original)") +
  coord_fixed(ratio = 1) +
  theme_bw()
original1

#predito
# Parte 1) x, y, r, g
img_df_parte1 <- img_df_parte1 %>% 
  mutate(predito_lm1p1,
         predito_arv1p1)
img_df_parte1$predito_lm1p1[img_df_parte1$predito_lm1p1<0] <- 0           
img_df_parte2$predito_lm1p2[img_df_parte2$predito_lm1p2<0] <- 0           


img_predito1 <- img_df_parte1 %>% 
    mutate(b = predito_lm1p1,
         cor = rgb(r, g, b))
predito1 <- ggplot(data = img_predito1, aes(x = x, y = y)) + 
  geom_point(colour = img_predito1$cor) +
  labs(x = "x", y = "y", title = "Imagem predita 1") +
  coord_fixed(ratio = 1) +
  theme_bw()
predito1

img_predito2 <- img_df_parte1 %>% 
  mutate(b = predito_arv1p1,
         cor = rgb(r, g, b))
predito2 <- ggplot(data = img_predito2, aes(x = x, y = y)) + 
  geom_point(colour = img_predito2$cor) +
  labs(x = "x", y = "y", title = "Imagem predita 2") +
  coord_fixed(ratio = 1) +
  theme_bw()
predito2

# Para este caso, a predição do modelo linear (predito 1) ficou melhor do que o modelo de árvore de decisão. Aparentemente, isto ocorre pelo fato de se tratar de variaveis continuas, onde o método da arvore de decisão não é tão eficiente.






#5 outra imagem
# a) carrega uma imagem jpeg no R 
img2 <- readJPEG("xadrez_colorido.jpg")

# b) transforma o array da imagem em data.frame com infos de posicao (x,y) e cor (r,g,b)
# dimensões da imagem
img_dim2 <- dim(img2)

# RGB para data.frame
img_df2 <- data.frame(
  x = rep(1:img_dim2[2], each = img_dim2[1]),
  y = rep(img_dim2[1]:1, img_dim2[2]),
  r = as.vector(img2[,,1]),
  g = as.vector(img2[,,2]),
  b = as.vector(img2[,,3])
) %>%
  mutate(cor = rgb(r, g, b),
         id = 1:n())

# para reprodução
set.seed(1) 

# Parte 1) x, y, r, g
img_df_parte1_2 <- img_df2 %>% 
  sample_frac(3/5) %>% # separando 3/5 do banco
  mutate(b_backup = b, # backup do azul original
         b = 0, # retirando o azul da imagem
         cor = rgb(r, g, b)) # cor da imagem sem o azul

# Parte 2) x, y, r, g, b
img_df_parte2_2 <- img_df2 %>% filter(!id%in%img_df_parte1_2$id) # filtra as linhas que estão na Parte 1

# Imagem sem o azul
sAzul2 <- ggplot(data = img_df_parte1_2, aes(x = x, y = y)) + 
  geom_point(colour = img_df_parte1_2$cor) +
  labs(x = "x", y = "y", title = "Imagem sem B (azul)") +
  coord_fixed(ratio = 1) +
  theme_bw()
sAzul2

# Apenas o azul da imagem
azul2 <- ggplot(data = img_df_parte2_2, aes(x = x, y = y)) + 
  geom_point(colour = img_df_parte2_2$cor) +
  labs(x = "x", y = "y", title = "Apenas o B (azul)") +
  coord_fixed(ratio = 1) +
  theme_bw()
azul2

#1: Descritiva
img_dfa2 <- sample_n(img_df2, 500, replace = F)

# matriz de correlação linar
img_dfa2 %>% 
  select(x, y:b) %>%
  cor %>%
  round(2) 

#matriz de gráficos de dispersão
img_dfa2 %>% select(x, y:b) %>%
  pairs()

#2: 
lm3 <- lm(b ~ x + y + r + g, data = img_df_parte2_2)
summary(lm3)


#3
lm4 <- lm(b ~ r + g, data = img_df_parte2_2)
summary(lm4)

arv2 <- tree(b ~ r + g, img_df_parte2_2)
summary(arv2)
plot(arv2)
text(arv2, pretty = 0)


#4
predito_arv2p1 <- predict(arv2, img_df_parte1_2)
predito_lm4p1 <-predict(lm4, img_df_parte1_2)
predito_arv2p2 <- predict(arv2, img_df_parte2_2)
predito_lm4p2 <-predict(lm4, img_df_parte2_2)

img_df_parte2_2 <- img_df_parte2_2 %>% 
  mutate(predito_lm4p2,
         predito_arv2p2,
         erro_lm4 = (b - predito_lm4p2)^2,
         erro_arv2 = (b - predito_arv2p2)^2)

soma_erro_lm4 <- sum(img_df_parte2_2$erro_lm4)
soma_erro_lm4


soma_erro_arv2 <- sum(img_df_parte2_2$erro_arv2)
soma_erro_arv2


#original
img_original2 <- img_df2 %>% 
  mutate(cor = rgb(r, g, b)) 

original2 <- ggplot(data = img_original2, aes(x = x, y = y)) + 
  geom_point(colour = img_original2$cor) +
  labs(x = "x", y = "y", title = "Imagem original)") +
  coord_fixed(ratio = 1) +
  theme_bw()
original2

#predito
# Parte 1) x, y, r, g
img_df_parte1_2 <- img_df_parte1_2 %>% 
  mutate(predito_lm4p1,
         predito_arv2p1)
img_df_parte1_2$predito_lm4p1[img_df_parte1_2$predito_lm4p1<0] <- 0           
img_df_parte2_2$predito_lm4p2[img_df_parte2_2$predito_lm4p2<0] <- 0   

img_predito3 <- img_df_parte1_2 %>% 
  mutate(b = predito_lm4p1,
         cor = rgb(r, g, b))
predito3 <- ggplot(data = img_predito3, aes(x = x, y = y)) + 
  geom_point(colour = img_predito3$cor) +
  labs(x = "x", y = "y", title = "Imagem predita 3") +
  coord_fixed(ratio = 1) +
  theme_bw()
predito3

img_predito4 <- img_df_parte1_2 %>% 
  mutate(b = predito_arv2p1,
         cor = rgb(r, g, b))
predito4 <- ggplot(data = img_predito4, aes(x = x, y = y)) + 
  geom_point(colour = img_predito4$cor) +
  labs(x = "x", y = "y", title = "Imagem predita 4") +
  coord_fixed(ratio = 1) +
  theme_bw()
predito4

# Para este caso, a predição da cor azul peloo modelo de árvore de decisão (predito4) ficou melhor do que o modelo linear. Aparentemente, isto ocorre pelo fato de se tratar de variaveis discretas, onde cada pixel apresenta ausencia ou presença de determinada cor em um valor fixo.

link_tree <- 'https://janusaureus.files.wordpress.com/2012/05/checked_scrapbook_paper_by_polstars_stock.jpg'
link_tree
link_lm<- 'http://lounge.obviousmag.org/sphere/2012/01/27/o-vilao-esqueleto-do-desenho-he-man-e-os-defensores-do-universo-1273175358804_300x300.jpg'
link_lm