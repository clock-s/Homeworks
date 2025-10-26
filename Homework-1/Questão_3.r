bikee <- read.csv("/Users/artur/Downloads/HW1_bike_sharing.csv")

bikee$total_users <- bikee$casual + bikee$registered
if (!inherits(bikee$dteday, "Date")) {
  bikee$dteday <- as.Date(bikee$dteday)
}

cat("\n--- Item 1 ---\n")
cat("Classificações:\n")
cat("instant  - Numérica Discreta (ID)\n")
cat("temp     - Numérica Contínua\n")
cat("casual   - Numérica Discreta\n")
cat("registered - Numérica Discreta\n")
cat("dteday   - Qualitativa Temporal\n")
cat("season   - Qualitativa Categórica\n")
cat("weathersit - Qualitativa Categórica\n")

cat("\nInformações gerais:\n")
tamanho <- length(bikee$instant)
data_inicio <- min(bikee$dteday)
data_fim <- max(bikee$dteday)
print(paste("Total de observações:", tamanho))
print(paste("Período de", data_inicio, "a", data_fim))


cat("\n--- Item 2 ---\n")

media_instant <- mean(bikee$instant)
mediana_instant <- median(bikee$instant)
quartis_instant <- quantile(bikee$instant, probs = c(0.25, 0.5, 0.75))
print(paste("instant - Média:", media_instant))
print(paste("instant - Mediana:", mediana_instant))
print("instant - Quartis:")
print(quartis_instant)

media_temp <- mean(bikee$temp)
mediana_temp <- median(bikee$temp)
quartis_temp <- quantile(bikee$temp, probs = c(0.25, 0.5, 0.75))
print(paste("temp - Média:", media_temp))
print(paste("temp - Mediana:", mediana_temp))
print("temp - Quartis:")
print(quartis_temp)

media_casual <- mean(bikee$casual)
mediana_casual <- median(bikee$casual)
quartis_casual <- quantile(bikee$casual, probs = c(0.25, 0.5, 0.75))
print(paste("casual - Média:", media_casual))
print(paste("casual - Mediana:", mediana_casual))
print("casual - Quartis:")
print(quartis_casual)

media_registered <- mean(bikee$registered)
mediana_registered <- median(bikee$registered)
quartis_registered <- quantile(bikee$registered, probs = c(0.25, 0.5, 0.75))
print(paste("registered - Média:", media_registered))
print(paste("registered - Mediana:", mediana_registered))
print("registered - Quartis:")
print(quartis_registered)


cat("\n--- Item 3 ---\n")

bikee$season_label <- factor(bikee$season,
                             levels = c(1, 2, 3, 4),
                             labels = c("Primavera", "Verão", "Outono", "Inverno"))

bikee$weathersit_label <- factor(bikee$weathersit,
                                 levels = c(1, 2, 3, 4),
                                 labels = c("Céu Limpo", "Nublado/Nevoeiro", "Chuva/Neve Fraca", "Chuva Forte"))
print("Variáveis season e weathersit recodificadas com rótulos descritivos.")

demanda_por_season <- aggregate(total_users ~ season_label, data = bikee, FUN = sum)
demanda_por_weathersit <- aggregate(total_users ~ weathersit_label, data = bikee, FUN = sum)

print("\nDemanda Total (Frequência Absoluta) por Estação:")
print(demanda_por_season)
print("\nDemanda Total (Frequência Absoluta) por Condição Climática:")
print(demanda_por_weathersit)

alturas_season <- setNames(demanda_por_season$total_users, demanda_por_season$season_label)
alturas_weathersit <- setNames(demanda_por_weathersit$total_users, demanda_por_weathersit$weathersit_label)

# CONFIGURAÇÃO DE LAYOUT: 2 LINHAS, 2 COLUNAS
par(mfrow=c(2, 2))

# GRÁFICO 1 (Gráfico de Barras - Estação)
barplot(alturas_season,
        main = "Demanda Total de Bicicletas por Estação do Ano",
        xlab = "Estação do Ano",
        ylab = "Total de Usuários",
        col = c("lightgreen", "yellow", "orange", "lightblue"),
        ylim = c(0, max(alturas_season) * 1.1))

# GRÁFICO 2 (Gráfico de Barras - Clima)
barplot(alturas_weathersit,
        main = "Demanda Total de Bicicletas por Condição Climática",
        xlab = "Condição Climática",
        ylab = "Total de Usuários",
        col = c("blue", "gray", "red", "darkred"),
        ylim = c(0, max(alturas_weathersit) * 1.1))


moda_season <- demanda_por_season[which.max(demanda_por_season$total_users), ]
moda_weathersit <- demanda_por_weathersit[which.max(demanda_por_weathersit$total_users), ]

cat("\n--- Respostas do Item 3 ---\n")
print(paste("Estação com Maior Número de Usuários (Moda):", moda_season$season_label))
print(paste("Condição Climática Mais Favorável (Moda):", moda_weathersit$weathersit_label))


cat("\n--- Item 4 ---\n")

bikee$temp_real <- bikee$temp * 41
print("Variável 'temp_real' criada com sucesso (temperatura em °C).")

# GRÁFICO 3 (Série Temporal - Temperatura)
plot(bikee$dteday, bikee$temp_real, type = "l",
     col = "blue", lwd = 2,
     xlab = "Dia", ylab = "Temperatura (°C)",
     main = "Série Temporal - Temperatura")

# GRÁFICO 4 (Série Temporal - Total de Usuários)
plot(bikee$dteday, bikee$total_users, type = "l",
     col = "darkred", lwd = 2,
     xlab = "Dia", ylab = "Total de Usuários",
     main = "Série Temporal - Total de Usuários")

# RESTAURA O LAYOUT GRÁFICO PARA O PADRÃO (1 figura por tela)
par(mfrow=c(1, 1))