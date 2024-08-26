
  cat("INICIO DA FUNCAO OBJETIVO \n")
  
  rm(list = ls())
  
  # Importar dados do excel como vetores
  data <- read_excel('Dados_medium.xlsx')
  
  Dia <- data$Dia
  mes <- data$mes
  hora <- data$hora
  hora_p <- data$hora_p
  Irradiacao <- data$Irradiacao
  CicloCorrido <- data$CicloCorrido
  Demanda_h <- data$Demanda_h
  
  ### Para testar ###
  Q_pv <- 600 # Quantidade de painéis instalados - Variável para otimização
  Q_mod <- 150 # Quantidade de baterias novas instaladas - Variável para otimização
  Dem_TDC_fp <- 56 # Demanda contratada fora ponta em kW - variável móvel para o mínimo
  Dem_TDC_p <- Dem_TDC_fp # Demanda contratada na ponta em kW - variável móvel para o mínimo
  Dem_TDG <- 198 # Demanda contratada de geração em kW - variável móvel para o mínimo

  tx_G <- 0.15 # Porcentagem da TUSD cobrada na geracao
  
  REC <- 0.8 # Porcentagem do SoH que pode recarregar com energia da rede em horário fp
  
  # Parâmetros de tempo
  anos_t <- 25 # Quantidade de anos para o sistema
  dt <- 1 # Intervalo de tempo dos dados
  
  # Taxas Econômicas
  IPCA <- 0.05 # Taxa de inflação estimada
  tx_r <- 0.15 # Taxa de retorno estimada
  
  # Dados do Sistema de Geração Fotovoltaica
  POT_un <- 520 # Potência de cada painel em W_pico - Fonte: BYD
  A_pv <- 2.38 # Área de incidência em cada painel em m2 - Fonte: BYD
  Ef_pv_i <- 0.18 # Eficiência de conversão da irradiação em corrente DC dos painéis FV - Fonte: Deotti
  Deg_pv <- 7.78e-7 # Perda de eficiência diária Fonte: BYD
  
  Inst_pv <- 4.41 # Custo de instalação de GD em R$/W_pico
  OM_pv <- 0.01 # Gastos de O&M por ano
  Dep <- 0.008 # Depreciação de investimento
  
  Ef_inv <- 0.98 # Eficiência de conversão do inversor - Fonte: Kamath Evaluation
  
  # Dados de demanda energética diária
  E_dia <- sum(Demanda_h[1:24]) # Demanda energética em Wh a cada dia
  E_p <- sum(Demanda_h[19:21]) # Demanda energetica no horario de ponta
  
  # Parâmetros da bateria
  C_mod <- 1 # Capacidade de cada módulo em kWh
  EO1L <- 1 # SoH no fim da primeira vida
  T1l <- 3000 # Ciclos na primeira vida
  SoHInicial <- Q_mod * C_mod * EO1L # Estado inicial de saúde em Wh
  SoC_max <- 0.9 # SoC máximo em porcentagem
  SoC_min <- 0.1 # SoC mínimo em porcentagem
  P_bat <- SoHInicial/2 # Potencia da bateria em kW
  EUL <- 0.8 # Porcentagem do SoH que define o fim da vida util
  Ef_b_in <- 0.98 # Eficiencia de carga/descarga inicial da bateria
  Deg_efbt <- (0.05 / (5 * 365)) # Degradacao da eficiencia a cada ciclo da bateria
  P_preco <- 1 #Poercentagem do preço da bateria nova
  Inst_bat <- 5000*P_preco # Custo de instalacao em R$/kWh
  OM_bat <- 0.02 # Custo de O&M anual para BESS em % do CAPEX
  
  
  # Parametros de degradacao da bateria
  a0 <- -4464    # parametro linear para fitting de Cyclos x DoD da bateria do Nissan Leaf. FOnte: Xiao X. e Assunção
  a1 <- 4167     # parametro linear para fitting de Cyclos x DoD da bateria do Nissan Leaf. FOnte: Xiao X. e Assunção
  a2 <- 7991     # parametro linear para fitting de Cyclos x DoD da bateria do Nissan Leaf. FOnte: Xiao X. e Assunção
  a3 <- -12120     # parametro linear para fitting de Cyclos x DoD da bateria do Nissan Leaf. FOnte: Xiao X. e Assunção
  a4 <- 4929     # parametro linear para fitting de Cyclos x DoD da bateria do Nissan Leaf. FOnte: Xiao X. e Assunção
  
  Tvida <- 3650 # Tempo de vida declarado pelo fabricante em dias
  L100 <- 2500 # Quantidade de ciclos que a bateria suporta com ciclos de 100% DoD dispon'ível no data sheet da bateria
  L_a <- 38200 # parametro linear para determinar a função L_DoD - Fonte: Deoti
  L_b <- -0.02686 # parametro exponencial para determinar a função L_DoD - Fonte: Deoti
  Deg_t <- 1 - 0.8^(1 / Tvida) # Componente de degradação pelo tempo de vida (calendric life time degradation)
  Deg_cyc <- 1 - 0.8^(1 / L100) # Componente de degradação pela ciclagem (calendric life time degradation)
  #neq <- 1 #abre a variável neq
  
  # Parametros de tarifas de energia. Fonte: ANEEL - Ranking das tarifas
  TE_fp <- 302.56/1000 # Tarifa de energia em R$/kWh
  TE_p <- 484.11/1000 # Tarifa de energia em R$/kWh
  TUSD_fp <- 110.49/1000 # Tarifa de Uso do Sistema de Distribuiçao em R$/kWh
  TUSD_p <- 1273.64/1000 # Tarifa de Uso do Sistema de Distribuiçao em R$/kWh
  TD_C_fp <- 16.66 # Tarifa de Demanda do Sistema de Distribuiçao em R$/kW
  TD_C_p <- 16.66 # Tarifa de Demanda do Sistema de Distribuiçao em R$/kW
  TD_G <- 6.61 # Tarifa de Demanda para GD em R$/kW
  Fee_fp <- 2 * TD_C_fp # Multa por excesso de demanda Fora Ponta 
  Fee_p <- 2 * TD_C_p # Multa por excesso de demanda Fora Ponta
  Fee_G <- 2 * TD_G # Multa por excesso de demanda Fora Ponta 
  Dem_min <- 30 # Demanda mínima contratada
 
  tol <- 1.05 # Tolerancia da ANEEL para passar da demanda contratada
  
  PIS <- 0.005 # Imposto PIS em porcentagem
  COFINS <- 0.003 # Imposto COFINS em Porcentagem
  ICMS <- 0.027 # Imposto ICMS em porcentagem
  Impostos <- 1 / (1 - PIS - COFINS) / (1 - ICMS) # Calculo de impostos incidentes
  
  rej <- 0.0681 # Reajuste anual médio além da inflaçao [ANEEL - CPFL Paulista]
  
  # Variaveis horárias
  P_pv_h <- rep(0, length(CicloCorrido)) # Energia gerada a cada hora
  P_pl_h <- rep(0, length(CicloCorrido)) # Quanto de energia vai da GD para a demanda a cada hora
  P_pb_h <- rep(0, length(CicloCorrido)) # Quanto de energia vai da GD para a BATERIA a cada hora
  P_pg_h <- rep(0, length(CicloCorrido)) # Quanto de energia vai da GD para a rede (grid) a cada hora
  P_gl_h <- rep(0, length(CicloCorrido)) # Quanto de energia vai da REDE para a demanda a cada hora
  P_gb_h <- rep(0, length(CicloCorrido)) # Quanto de energia vai da REDE para a BATERIA a cada hora
  P_bl_h <- rep(0, length(CicloCorrido)) # Quanto de energia vai da BATERIA para a demanda a cada hora
  SoC_h <- rep(0, length(CicloCorrido)) # State of Charge da BATERIA a cada hora a cada hora
  Cred_h <- rep(0, length(CicloCorrido)) # Creditos de compensaçao de energia
  C_gl_h <- rep(0, length(CicloCorrido)) #Custo da energia horária da rede para demanda
  C_pg_h <- rep(0, length(CicloCorrido)) #Custo da GD injetada na rede 
  C_D_fp_h <- rep(0, length(CicloCorrido)) # Custo de excedente de demanda a cada dt fora ponta
  C_D_p_h <- rep(0, length(CicloCorrido)) # Custo de excedente de demanda a cada dt ponta
  C_G_h <- rep(0, length(CicloCorrido)) # Custo de excedente de demanda de geracao a cada dt
  
  # Variaveis Contínuas
  irrad_c <- rep(0, anos_t * 365) # Irradiacao por dia
  Demanda_c <- rep(0, anos_t * 365) # Demanda diaria
  P_pv_c <- rep(0, anos_t * 365) # Energia gerada acumulada no mes
  P_pl_c <- rep(0, anos_t * 365) # Quanto de energia vai da GD para a demanda a cada dia (ciclo)
  P_pb_c <- rep(0, anos_t * 365) # Quanto de energia vai da GD para a BATERIA a cada dia (ciclo)
  P_pg_c <- rep(0, anos_t * 365) # Quanto de energia vai da GD para a rede (grid) a cada dia (ciclo)
  P_gl_c <- rep(0, anos_t * 365) # Quanto de energia vai da REDE para a demanda a cada dia (ciclo)
  P_gb_c <- rep(0, anos_t * 365) # Quanto de energia vai da REDE para a bateria a cada dia (ciclo)
  P_bl_c <- rep(0, anos_t * 365) # Quanto de energia vai da BATERIA para a demanda a cada dia (ciclo)
  DoD_c <- rep(0, anos_t * 365) # Profundidade de descarga para cada ciclo
  C_gl_c <- rep(0, anos_t * 365) # Custo da energia da rede para demanda a cada ciclo
  C_pg_c <- rep(0, anos_t * 365) # Custo da injecao de energia GD na rede a cada ciclo
  C_D_fp_c <- rep(0, anos_t * 365) # Custo de excedente de demanda a cada ciclo fora ponta
  C_D_p_c <- rep(0, anos_t * 365) # Custo de excedente de demanda a cada ciclo ponta
  C_G_c <- rep(0, anos_t * 365) # Custo de excedente de geracao a cada ciclo ponta
  
  Troca <- rep(0, anos_t * 365) #Registra dia de troca de bateria
  
  Dia_c <- rep(0, anos_t * 365)
  mes_c <- rep(0, anos_t * 365)
  ano_c <- rep(0, anos_t * 365)
  Ciclobat <- rep(0, anos_t * 365)
  CicloTotal <- rep(0, anos_t * 365)
  # Inicializações
  SoH <- rep(0, anos_t * 365) # Vetor para armazenar o estado de saúde a cada dia (Deoti)
  
  # Inicio de calculo dos parametros tecnicos
  Ciclobat[1] <- T1l
  
  SoC_h[1] <- SoHInicial * 0.6 # Declara o primeiro SoC
  SoH[1] <- SoHInicial # Define o estado inicial
  ano_c[1] <- 1
  
  i <- 1 # progressao de tempo que vai até 24(horas)*365 (dias)
  c <- 1 # progressao de ciclos que vai até 20(anos)*365(dias)
  print("c")
  repeat {
    
    # if (c > 7299) {
    
    #  print(c)
    #}
    
    
    P_pv_h[i] <- Irradiacao[i] * A_pv * Q_pv * (Ef_pv_i * (1 - Deg_pv*c)) * Ef_inv / 1000 # Calcula a geraçao PV no ano 1 em kWh
    P_res <- P_pv_h[i] - Demanda_h[i]
    
    if (P_pv_h[i] >= Demanda_h[i]) {
      P_pl_h[i] <- Demanda_h[i] # Energia PV->load é a demanda
      P_gl_h[i] <- 0 #Nao usa energia da rede
      
      if (SoC_h[i] < SoH[c] * SoC_max) {
        P_pb_h[i] <- min(P_bat, min(abs(P_res), SoH[c] * SoC_max - SoC_h[i])) # Carrega O que for menor entre A energia que falta para carregar, A energia da Gd disponível & a potencia máxima da bateria
        P_pg_h[i] <- P_pv_h[i] - P_pl_h[i] - P_pb_h[i] # SoC_h[i+1] = SoC_h[i] # O SoC mantém no máximo
      } else {
        P_pb_h[i] <- 0 #Nao precisa carregar a bateria
        P_pg_h[i] <- P_pv_h[i] - P_pl_h[i] #entrega toda energia rpa rede
      }
    } else {
      P_pl_h[i] <- P_pv_h[i] # usa a energia só na carga local
      P_pb_h[i] <- 0 # Nao carrega a bateria
      P_pg_h[i] <- 0 # Nao injeta energia na rede
      
      if (hora_p[i] == 1) {
        if (SoC_h[i] <= SoC_min * SoH[c]) {
          P_bl_h[i] <- 0 #Nao uso nada da bateria
        } else {
          P_bl_h[i] <- min(P_bat, min(Demanda_h[i] - P_pl_h[i], (SoC_h[i] - SoC_min * SoH[c]) / dt)) #Cubro a demanda com energia da bateria até onde pode entre toda a demanda ou o que falta para chegar no mínimo, limitado pela potencia da bat
        }
        P_gl_h[i] <- Demanda_h[i] - P_pl_h[i] - P_bl_h[i] # Rede completa a demanda no horário que nao foi coberta pela PV e BESS
        C_gl_h[i] <- max(P_gl_h[i] - Cred_h[i] * (TE_fp / TE_p), 0) * dt * (TE_p + TUSD_p)
        C_D_p_h[i] <- max(P_gl_h[i] - Dem_TDC_p, 0) * Fee_p # Custo de excedente de demanda é calculado se passa da demanda contratada
      } else {
        if (Demanda_h[i] - P_pl_h[i] > Dem_TDC_fp) {
          if (SoC_h[i] - ((Demanda_h[i] - P_pl_h[i] - Dem_TDC_fp) / ((Ef_b_in - Deg_efbt * (Ciclobat[c] - 1)) * Ef_inv)) * dt >= SoC_min * SoH[c]) {
            P_gl_h[i] <- Dem_TDC_fp #Consumo o máximo que posso da rede
            P_bl_h[i] <- Demanda_h[i] - P_pl_h[i] - P_gl_h[i] #Cubro a demanda com energia da bateria para peak shaving
          } else if (SoC_h[i] > SoC_min * SoH[c]) {
            P_bl_h[i] <- ((SoC_h[i] - SoC_min * SoH[c]) / ((Ef_b_in - Deg_efbt * (Ciclobat[c] - 1)) * Ef_inv)) / dt #Uso a bateria até chegar no limite inferior do SoC
            P_gl_h[i] <- Demanda_h[i] - P_pl_h[i] - P_bl_h[i] # Rede completa a demanda no horário que nao foi coberta pela PV e BESS
          } else {
            P_gl_h[i] <- Demanda_h[i] - P_pl_h[i] # Uso energia mesmo passando da minha potencia contratada
            P_bl_h[i] <- 0 # Nao descarrego nada da minha bateria
          }
        } else {
          P_gl_h[i] <- Demanda_h[i] - P_pl_h[i] # Uso somente energia da rede
          P_bl_h[i] <- 0 # Nao descarrego nada da minha bateria
          
          Irrad_op <- (max(0.60* SoH[c] - SoC_h[i], 0)  + 0.5 * (E_dia - E_p)) / (A_pv * Q_pv * (Ef_pv_i * (1 - Deg_pv)^(c - 1)) * Ef_inv / 1000) #Irradiacao minima para cubrir demanda
          if ((sum(Irradiacao[(i):(min(i + 24, length(Irradiacao)))]) < Irrad_op)) {
            if (hora[i] < 11 && SoC_h[i] < REC * SoC_max * SoH[c]) {
              P_gb_h[i] <- min(P_bat, min(((REC * SoC_max * SoH[c] - SoC_h[i]) / (11 - hora[i])) / dt, REC * SoC_max * SoH[c]) / dt)
            }
          }
        }
      }
      C_D_fp_h[i] <- max(P_gl_h[i] - tol*Dem_TDC_fp, 0) * Fee_fp # Custo de excedente de demanda é calculado se passa da demanda contratada
      C_gl_h[i] <- max(P_gl_h[i] + P_gb_h[i] - Cred_h[i], 0) * (TE_fp + TUSD_fp) #Custo da energia vai ser energia da rede para demanda mais a energia da rede para recarga da bateria descontao os créditos
    }
    C_pg_h[i] <- P_pg_h[i] * dt * TUSD_fp * tx_G #Custo de TUSD da energia injetada na rede
    C_G_h[i] <- max(P_pg_h[i] - tol*Dem_TDG, 0) * Fee_G # Custo de excedente de demanda da potencia de geracao contratado
    
    if (i + 1 > length(CicloCorrido)) {
      if (Q_mod > 0) {
        SoC_h[1] <- max(min(SoC_max * SoH[c], (SoC_h[i] + (P_pb_h[i] + P_gb_h[i]) * (Ef_b_in - Deg_efbt * (Ciclobat[c] - 1)) - P_bl_h[i] / ((Ef_b_in - Deg_efbt * (Ciclobat[c] - 1)) * Ef_inv))), SoC_min * SoH[c])
        # SoC horario vai ser o que carregou menos o que descarregou
        Ciclobat[c + 1] <- Ciclobat[c] + 1
        DoD_c[c] <- (P_bl_c[c]) / SoH[c] #Calcula DoD do ciclo
        
        # Calcular o efeito de degraçao da bateria 
        
        #LDOD <- a0*DoD_c[c]^4+a1*DoD_c[c]^3+a2*DoD_c[c]^2+a3*DoD_c[c]+a4 # Calcula o parâmetro L para ciclos diferentes de 100% DoD
        LDOD <- L_a * exp(L_b * DoD_c[c]) # Calcula o parâmetro L para ciclos diferentes de 100% DoD
        neq <- L100 / LDOD # Representa a ciclagem equivalente em relaçção à capacidade máxima
        
        # Equação de degradação da bateria com profundidade de descarga sem influˆência da temperatura e corrente
        SoH[c + 1] <- SoH[c] * (1 - (Deg_t + neq * Deg_cyc))
        
        #Descobrir qual ciclo a bateria passa de 80%
        if (SoH[c + 1] < EUL * SoHInicial || SoH[c + 1] * (SoC_max - SoC_min) < E_p) {
          SoH[c + 1] <- SoHInicial #Reposiçao de baterias
          SoC_h[i + 1] <- SoHInicial * 0.6
          Troca[c] <- Troca[c] + 1 #Registra a troca de bateria
          Ciclobat[c + 1] <- T1l #Reinicia contagem dos ciclos da bateria
        }
      }
      Cred_h[1] <- max((Cred_h[i] + P_pg_h[i] - P_gb_h[i] - P_gl_h[i]), 0) #Computo os creditos a cada hora
      ano_c[c + 1] <- ano_c[c] + 1
      CicloTotal[c+1] <- c + 1
      c <- c + 1 #Aumentou o valor do ciclo contínuo
      
      # Somatória do ciclo inicia novamente
      irrad_c[c] <- 0 # Irradiacao por dia
      P_pv_c[c] <- 0 # Energia acumulada no Ciclos
      P_pl_c[c] <- 0 # Quanto de energia vai da GD para a demanda a cada Ciclos
      P_pb_c[c] <- 0 # Quanto de energia vai da GD para a BATERIA a cada Ciclos
      P_pg_c[c] <- 0 # Quanto de energia vai da GD para a rede (grid) a cada Ciclos
      P_gl_c[c] <- 0 # Quanto de energia vai da REDE para a demanda a cada Ciclos
      P_gb_c[c] <- 0 # Quanto de energia vai da REDE para a bateria a cada Ciclos
      P_bl_c[c] <- 0 # Quanto de energia vai da BATERIA para a demanda a cada hora
      C_gl_c[c] <- 0 # Quanto paga de energia a cada ciclo
      C_pg_c[c] <- 0
      C_D_fp_c[c] <- 0 # Quanto paga por excedente de demanda fora ponta a cada ciclo
      C_D_p_c[c] <- 0 # Quanto paga por excedente de demanda fora ponta a cada ciclo
      C_G_c[c] <- 0 # Quanto paga por excedente de geracao a cada ciclo
      Demanda_c[c] <- 0
      i <- 1
    } else {
      SoC_h[i + 1] <- SoC_h[i] + (P_pb_h[i] + P_gb_h[i]) * (Ef_b_in - Deg_efbt * (Ciclobat[c] - 1)) - P_bl_h[i] / ((Ef_b_in - Deg_efbt * (Ciclobat[c] - 1)) * Ef_inv)
      # SoC horario vai ser o que carregou menos o que descarregou
      Cred_h[i + 1] <- max((Cred_h[i] + P_pg_h[i] - P_gb_h[i] - P_gl_h[i]), 0) #Computo os creditos a cada hora
      ano_c[c] <- ano_c[c]
      
      ## Fazer as somas de quantidades diarias e efeito de degradaçao no SoH
      if (Dia[i + 1] == Dia[i]) {
        irrad_c[c] <- irrad_c[c] + Irradiacao[i] # Irradiacao por dia
        P_pv_c[c] <- P_pv_c[c] + P_pv_h[i] # Energia acumulada no Ciclos
        P_pl_c[c] <- P_pl_c[c] + P_pl_h[i] # Quanto de energia vai da GD para a demanda a cada Ciclos
        P_pb_c[c] <- P_pb_c[c] + P_pb_h[i] # Quanto de energia vai da GD para a BATERIA a cada Ciclos
        P_pg_c[c] <- P_pg_c[c] + P_pg_h[i] # Quanto de energia vai da GD para a rede (grid) a cada Ciclos
        P_gl_c[c] <- P_gl_c[c] + P_gl_h[i] # Quanto de energia vai da REDE para a demanda a cada Ciclos
        P_gb_c[c] <- P_gb_c[c] + P_gb_h[i] # Quanto de energia vai da REDE para a bateria a cada Ciclos
        P_bl_c[c] <- P_bl_c[c] + P_bl_h[i] # Quanto de energia vai da BATERIA para a demanda a cada hora
        C_gl_c[c] <- C_gl_c[c] + C_gl_h[i] # Quanto paga de energia a cada ciclo
        C_pg_c[c] <- C_pg_c[c] + C_pg_h[i]
        C_D_fp_c[c] <- C_D_fp_c[c] + C_D_fp_h[i] # Quanto paga por excedente de demanda fora ponta a cada ciclo
        C_D_p_c[c] <- C_D_p_c[c] + C_D_p_h[i] # Quanto paga por excedente de demanda fora ponta a cada ciclo
        C_G_c[c] <- C_G_c[c] + C_G_h[i] # Quanto paga por excedente de geracao a cada ciclo
        Demanda_c[c] <- Demanda_c[c] + Demanda_h[i] # Demanda acumulada
      } else { # trocou o dia, troca o ciclo
        
        CicloTotal[c + 1] <- c + 1
        ano_c[c + 1] <- ano_c[c]
        
        if (Q_mod > 0) {
          # Calcular o efeito de degraçao da bateria 
          Ciclobat[c + 1] <- Ciclobat[c] + 1
          DoD_c[c] <- (P_bl_c[c]) / SoH[c] #Calcula DoD do ciclo        
          
          #LDOD <- a0*DoD_c[c]^4+a1*DoD_c[c]^3+a2*DoD_c[c]^2+a3*DoD_c[c]+a4 # Calcula o parâmetro L para ciclos diferentes de 100% DoD
          LDOD <- L_a * exp(L_b * DoD_c[c]) # Calcula o parâmetro L para ciclos diferentes de 100% DoD
          neq <- L100 / LDOD # Representa a ciclagem equivalente em relaçção à capacidade máxima
          
          # Equação de degradação da bateria com profundidade de descarga sem influˆência da temperatura e corrente
          SoH[c + 1] <- SoH[c] * (1 - (Deg_t + neq * Deg_cyc))
          
          #Descobrir qual ciclo a bateria passa de 80%
          if (SoH[c + 1] < EUL * SoHInicial || SoH[c + 1] * (SoC_max - SoC_min) < E_p) {
            SoH[c + 1] <- SoHInicial #Reposiçao de baterias
            SoC_h[i + 1] <- SoHInicial * 0.6
            Troca[c] <- Troca[c] + 1 #Registra a troca de bateria
            Ciclobat[c + 1] <- T1l #Reinicia contagem dos ciclos da bateria
          }
        }
        #CicloTotal[c] <- c
        c <- c + 1 #Aumentou o valor do ciclo contínuo
        
        # Somatória do ciclo inicia novamente
        irrad_c[c] <- Irradiacao[i] # Irradiacao por dia
        P_pv_c[c] <- P_pv_h[i] # Energia acumulada no Ciclos
        P_pl_c[c] <- P_pl_h[i] # Quanto de energia vai da GD para a demanda a cada Ciclos
        P_pb_c[c] <- P_pb_h[i] # Quanto de energia vai da GD para a BATERIA a cada Ciclos
        P_pg_c[c] <- P_pg_h[i] # Quanto de energia vai da GD para a rede (grid) a cada Ciclos
        P_gl_c[c] <- P_gl_h[i] # Quanto de energia vai da REDE para a demanda a cada Ciclos
        P_gb_c[c] <- P_gb_h[i] # Quanto de energia vai da REDE para a bateria a cada Ciclos
        P_bl_c[c] <- P_bl_h[i] # Quanto de energia vai da BATERIA para a demanda a cada hora
        C_gl_c[c] <- C_gl_h[i] # Quanto paga de energia a cada ciclo
        C_pg_c[c] <- C_pg_h[i]
        C_D_fp_c[c] <- C_D_fp_h[i] # Quanto paga por excedente de demanda fora ponta a cada ciclo
        C_D_p_c[c] <- C_D_p_h[i] # Quanto paga por excedente de demanda fora ponta a cada ciclo
        C_G_c[c] <- C_G_h[i] # Quanto paga por excedente de geracao a cada ciclo
        Demanda_c[c] <- Demanda_h[i] # Demanda acumulada
      }
      i <- i + 1
    }
    
    if (c >= (anos_t * 365)) {
      cat ("25 anos completos", "\n")
      
      break
    }
    
  }
  
  #cat ("Tabela \n")
  Tb_c <- data.frame(ano_c, mes_c, Dia_c, CicloTotal, irrad_c, Ciclobat, Demanda_c, 
                     P_pv_c, P_pl_c, P_pb_c,P_bl_c, P_gb_c, P_gl_c, P_pg_c, 
                     Troca, SoH,
                     C_gl_c, C_pg_c, C_D_fp_c, C_D_p_c, C_G_c)
  
  #cat ("Agrega \n")
  T_yrl <- aggregate(. ~ ano_c, data = Tb_c, FUN = sum)
  new_row <- rep(0,ncol(T_yrl))
  
  T_yrl <- rbind(new_row,T_yrl)
  #cat ("Custo de energia \n")
  C_energy_yrl <- Impostos * rowSums(T_yrl[, 17:ncol(T_yrl)]) / ((1 - rej) ^ (T_yrl$ano_c - 1))
  C_energy_in <- Impostos * (T_yrl[, 17]) / ((1 - rej) ^ (T_yrl$ano_c - 1))
  C_energy_out <- Impostos * (T_yrl[, 18]) / ((1 - rej) ^ (T_yrl$ano_c - 1))
  C_pen <- Impostos * rowSums(T_yrl[, 19:ncol(T_yrl)]) / ((1 - rej) ^ (T_yrl$ano_c - 1))
  
  #cat ("Capex PV \n")
  CAPEX_pv <- rep(0,nrow (T_yrl))
  CAPEX_pv[1] <- POT_un * Q_pv * Inst_pv
  
  #cat("Capex bat \n")
  CAPEX_bat <- Q_mod * C_mod * Inst_bat * T_yrl$Troca
  CAPEX_bat [1] <- Q_mod * C_mod * Inst_bat
  
  cat ("O&M \n")
  C_OM_pv <- OM_pv * CAPEX_pv[1] / ((1 - IPCA) ^ (T_yrl$ano_c - 1))
  
  C_OM_bat <- OM_bat * max(CAPEX_bat) / ((1 - IPCA) ^ (T_yrl$ano_c - 1))
  
  C_DEM_fp <- Dem_TDC_fp * TD_C_fp / ((1 - IPCA) ^ (T_yrl$ano_c - 1))
  #C_DEM_p <- Dem_TDC_p * TD_C_p / ((1 - IPCA) ^ (T_yrl$ano_c - 1))
  
  if (max(Dem_TDC_fp, Dem_TDC_p) >= Dem_TDG) {
    C_DEM_G <- Dem_TDG * TD_G / ((1 - IPCA) ^ (T_yrl$ano_c - 1))
  } else {
    C_DEM_G <-(Dem_TDG - Dem_TDC_fp) * TD_G / ((1 - IPCA) ^ (T_yrl$ano_c - 1))
  }  
  C_dem <- C_DEM_fp + C_DEM_G
  
  ## Zerar as primeiras linhas ##
  C_OM_pv[1] <- 0
  C_OM_bat[1] <- 0
  C_DEM_fp[1] <- 0
  #C_DEM_p[1] <- 0
  C_DEM_G[1] <- 0
  C_dem[1] <- 0
  
  ## Faz uma tabela para comparar dados ##
  Tb_custos <- data.frame(T_yrl[, 1], CAPEX_pv, CAPEX_bat, C_OM_pv, C_OM_bat, #Custos de equipamentos
                          C_energy_yrl, C_energy_in, C_energy_out, C_pen,  #Custos variávei
                          C_DEM_fp, C_DEM_G) #Custos fixos
  
  NPV_sist <- npv(tx_r, C_dem) + npv(tx_r, C_energy_yrl) + npv(tx_r, CAPEX_pv) +
    npv(tx_r,CAPEX_bat) + npv(tx_r, C_OM_bat) + npv(tx_r, C_OM_pv)
  
  LCOE <- NPV_sist / (npv(tx_r, T_yrl$Demanda_c))
  
  cat("LCOE é", LCOE, "\n")
  
  if (FALSE) {
  suf <- (sum(Tb_c$P_pl_c)+sum(Tb_c$P_bl_c))/(sum(Tb_c$Demanda_c)+sum(Tb_c$P_gb_c))
  cat("Autosufucuencia é", suf, "\n")
  
  BESS <- T_yrl$Troca
  BESS[1] <- 1
  
  Emi_BR <- 61.7/1000 #Emissões de CO2 em kg CO2eq/kWh
  perdas <- 0.15 #Perda de energia na rede
  Emi_pv <- 749.8/1000 #Emissoes de CO2 em kg CO2eq/Wp produzido
  Emi_LFP <- 73.5 #Emissoes de CO2 para fabriar a bateria em kg CO2eq/kWh de bateria
  
  Co2 <- ((T_yrl$P_gl_c+T_yrl$P_gb_c) * Emi_BR*(1+perdas) #Emissoes por uso de energia da rede
          + (SoHInicial/EO1L)*BESS*Emi_LFP)  #Emissoes por uso da bateria
  Co2[1] <- Co2[1] + Q_pv*POT_un*Emi_pv
  tx_co2 <- sum(Co2)/(sum(T_yrl$Demanda_c)+sum(T_yrl$P_gb_c))
  
  tb_plot <- melt(Tb_custos, id.vars = "T_yrl...1.")
  # Plot a stacked bar graph
  ggplot(tb_plot, aes(x = T_yrl...1., y= value, fill = variable)) +
    geom_bar(stat = "identity") +
    labs(title = "", x = "Anos", y = "BRL") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  }